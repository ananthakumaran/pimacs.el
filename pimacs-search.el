;;; pimacs-search.el --- Historical session search -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Anantha Kumaran.

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Historical Pi session search.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'parse-time)
(require 'pimacs-core)
(require 'pimacs-utils)
(require 'pimacs-section)
(require 'pimacs-ui)

(defcustom pimacs-search-rg-executable "rg"
  "Ripgrep executable used for historical session searches."
  :type 'string
  :group 'pimacs)

(defcustom pimacs-search-jq-executable "jq"
  "Jq executable used for historical session searches."
  :type 'string
  :group 'pimacs)

(defcustom pimacs-search-render-idle-delay 0.1
  "Idle time required before rendering another result batch."
  :type 'number
  :group 'pimacs)

(defcustom pimacs-search-default-directory
  (expand-file-name "sessions/" "~/.pi/agent/")
  "Default directory containing Pi session directories."
  :type 'directory
  :group 'pimacs)

(defconst pimacs-search--filter-types
  '(user assistant thinking tool-call tool-result bash compact))

(defconst pimacs-search--default-filters
  '(user assistant))

(defconst pimacs-search--jq-filter
  "def selected($kind): ($filters | index($kind)) != null;

def has_content_type($kind):
  (.message.content? // []) as $content
  | if ($content | type) == \"array\"
    then any($content[]; .type == $kind)
    else false
    end;


def text_content:
  if type == \"array\" then
    [.[] | select(.type == \"text\") | {type, text}]
  elif type == \"string\" then .
  else []
  end;

def assistant_content:
  if (.message.content | type) == \"array\" then
    [.message.content[]
     | select((.type == \"text\" and selected(\"assistant\"))
              or (.type == \"thinking\" and selected(\"thinking\"))
              or (.type == \"toolCall\" and selected(\"tool-call\")))
     | if .type == \"text\" then {type, text}
       elif .type == \"thinking\" then {type, thinking}
       else {type, name, arguments}
       end]
  else .message.content
  end;

def projected:
  if .type == \"message\" then
    if .message.role == \"user\" then
      {type, message: {role: \"user\", content: (.message.content | text_content)}}
    elif .message.role == \"assistant\" then
      {type, message: {role: \"assistant\", content: assistant_content}}
    elif .message.role == \"toolResult\" then
      {type, message: {role: \"toolResult\", toolName: .message.toolName,
                       content: (.message.content | text_content)}}
    elif .message.role == \"bashExecution\" then
      {type, message: {role: \"bashExecution\", command: .message.command,
                       output: (.message.output | text_content)}}
    else {type}
    end
  elif .type == \"compaction\" then
    {type, summary, tokensBefore}
  else {type}
  end;
def accepted:
  if .type == \"message\" then
    if .message.role == \"user\" then selected(\"user\")
    elif .message.role == \"assistant\" then
      (selected(\"assistant\") and
       ((.message.content | type) == \"string\" or has_content_type(\"text\"))) or
      (selected(\"thinking\") and has_content_type(\"thinking\")) or
      (selected(\"tool-call\") and has_content_type(\"toolCall\"))
    elif .message.role == \"toolResult\" then selected(\"tool-result\")
    elif .message.role == \"bashExecution\" then selected(\"bash\")
    else false
    end
  elif .type == \"compaction\" then selected(\"compact\")
  else false
  end;

select(.type == \"match\")
| .data as $match
| ($match.lines.text | fromjson) as $entry
| if $entry.type == \"session\" then
    {kind: \"session\", path: $match.path.text, id: $entry.id, cwd: $entry.cwd, timestamp: $entry.timestamp}
  elif $entry.type == \"session_info\" then
    {kind: \"session-info\", path: $match.path.text, name: $entry.name}
  elif ($entry | accepted) then
    {kind: \"entry\", path: $match.path.text, offset: $match.absolute_offset, entry: ($entry | projected)}
  else
    empty
  end
")

(defconst pimacs-search--buffer-name "*Pimacs Session Search*")

(defvar-local pimacs-search--controls-section nil)
(defvar-local pimacs-search--status-section nil)
(defvar-local pimacs-search--request nil)
(defvar-local pimacs-search--pipeline-process nil)
(defvar-local pimacs-search--generation 0)
(defvar-local pimacs-search--partial-output "")
(defvar-local pimacs-search--render-context nil)
(defconst pimacs-search--render-batch-size 25)
(defvar-local pimacs-search--entry-queue nil)
(defvar-local pimacs-search--entry-queue-tail nil)
(defvar-local pimacs-search--drain-timer nil)
(defvar-local pimacs-search--rendered-count 0)
(defvar-local pimacs-search--session-sections nil)
(defvar-local pimacs-search--session-metadata nil)
(defvar-local pimacs-search--parse-error nil)

(defvar pimacs-search--query-history nil)

(defvar-keymap pimacs-search-mode-map
  :parent special-mode-map
  "<left-fringe> <mouse-1>" #'pimacs-mouse-toggle-section
  "<left-fringe> <mouse-2>" #'pimacs-mouse-toggle-section
  "TAB" #'pimacs-toggle-section
  "C-i" #'pimacs-toggle-section
  "<backtab>" #'pimacs-search-cycle-sections
  "1" #'pimacs-section-show-level-1
  "2" #'pimacs-section-show-level-2
  "3" #'pimacs-section-show-level-3
  "M-1" #'pimacs-section-show-level-1-all
  "M-2" #'pimacs-section-show-level-2-all
  "M-3" #'pimacs-section-show-level-3-all
  "n" #'pimacs-goto-next-section
  "M-n" #'pimacs-goto-next-section
  "p" #'pimacs-goto-previous-section
  "M-p" #'pimacs-goto-previous-section
  "M-g l" #'pimacs-goto-last-section
  "l" #'pimacs-goto-last-section
  "g" #'pimacs-search-refresh)

(defun pimacs-search-cycle-sections ()
  "Cycle visibility of all sections in the current search buffer."
  (interactive)
  (pimacs-section--cycle-global))

(define-derived-mode pimacs-search-mode special-mode "Pimacs Search"
  "Major mode for browsing historical Pi session search results."
  (setq-local truncate-lines t)
  (setq-local bidi-paragraph-direction 'left-to-right)
  (setq-local bidi-inhibit-bpa t)
  (setq-local buffer-undo-list t)
  (font-lock-mode -1)
  (visual-line-mode -1)
  (when (bound-and-true-p display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (setq-local pimacs-section-autohide-count nil))

(defface pimacs-search-control-face
  '((t :underline t))
  "Face used for editable session search controls."
  :group 'pimacs)

(defface pimacs-search-active-control-face
  '((t :inherit font-lock-keyword-face))
  "Face used for selected session search control options."
  :group 'pimacs)

(defun pimacs-search--insert-button (text action &rest properties)
  (let ((face (if (plist-member properties 'face)
                  (plist-get properties 'face)
                'pimacs-search-control-face)))
    (setq properties (plist-put properties 'face face))
    (apply #'insert-text-button
           text
           'action action
           'follow-link t
           properties)))

(defun pimacs-search--option-label (option)
  (replace-regexp-in-string "-" " " (symbol-name option)))

(defun pimacs-search--insert-options (control selected action options)
  (let ((first t))
    (dolist (option options)
      (unless first
        (insert " "))
      (let ((text (pimacs-search--option-label option)))
        (if (eq option selected)
            (insert (propertize text
                                'face 'pimacs-search-active-control-face
                                'pimacs-search-focus control))
          (pimacs-search--insert-button
           text action 'pimacs-search-value option)))
      (setq first nil))))

(defun pimacs-search--insert-filters (filters)
  (let ((first t))
    (dolist (filter pimacs-search--filter-types)
      (unless first
        (insert "   "))
      (let ((active (memq filter filters))
            (text (pimacs-search--option-label filter)))
        (pimacs-search--insert-button
         (if active "[x] " "[ ] ")
         #'pimacs-search--toggle-filter
         'face (and active 'pimacs-search-active-control-face)
         'pimacs-search-filter filter
         'pimacs-search-focus filter)
        (pimacs-search--insert-button
         text #'pimacs-search--toggle-filter
         'pimacs-search-filter filter))
      (setq first nil))))

(defun pimacs-search--insert-context-button (text direction context)
  (pimacs-search--insert-button
   text
   #'pimacs-search--set-context
   'pimacs-search-context-direction direction
   'pimacs-search-focus
   (and context
        (if (eq direction 'before)
            (> (car context) 0)
          (> (cdr context) 0))
        'context)))

(defun pimacs-search--insert-controls ()
  (let* ((request pimacs-search--request)
         (search-type (pimacs-search-request-search-type request))
         (case (pimacs-search-request-case request))
         (scope (pimacs-search-request-scope request))
         (context (pimacs-search-request-context request)))
    (insert "Search term: ")
    (insert (propertize
             (if (equal (pimacs-search-request-query request) "")
                 "<empty>"
               (pimacs-search-request-query request))
             'face 'pimacs-search-active-control-face))
    (insert " ")
    (pimacs-search--insert-button
     "change" #'pimacs-search--edit-query
     'pimacs-search-focus 'query)
    (insert "\nSearch type: ")
    (pimacs-search--insert-options
     'search-type search-type #'pimacs-search--set-search-type
     '(string words regexp))
    (insert "\nCase: ")
    (pimacs-search--insert-options
     'case case #'pimacs-search--set-case
     '(smart sensitive ignore))
    (insert "\nContext: ")
    (if context
        (pimacs-search--insert-button
         "none" #'pimacs-search--clear-context)
      (insert (propertize "none"
                          'face 'pimacs-search-active-control-face
                          'pimacs-search-focus 'context)))
    (insert " ")
    (pimacs-search--insert-context-button "before" 'before context)
    (when context
      (insert (format ":%d" (car context))))
    (insert " ")
    (pimacs-search--insert-context-button "after" 'after context)
    (when context
      (insert (format ":%d" (cdr context))))
    (insert "\n\nDirectory: ")
    (pimacs-search--insert-button
     (abbreviate-file-name
      (pimacs-search-request-directory request))
     #'pimacs-search--edit-directory
     'pimacs-search-focus 'directory)
    (insert "\nProjects: ")
    (pimacs-search--insert-options
     'scope scope #'pimacs-search--set-scope
     '(current all))
    (insert "\nTypes: ")
    (pimacs-search--insert-filters
     (pimacs-search-request-filters request))
    (insert "\n")))

(defun pimacs-search--render-controls (&optional control)
  (let ((inhibit-read-only t))
    (pimacs-section--replace-section pimacs-search--controls-section
      (pimacs-search--insert-controls))
    (when-let ((position (and control
                              (text-property-any
                               (point-min) (point-max)
                               'pimacs-search-focus control))))
      (goto-char position))))

(defun pimacs-search--refresh-after-control-change (control)
  (pimacs-search--render-controls control)
  (pimacs-search-refresh))

(defun pimacs-search--set-status (message)
  (let ((inhibit-read-only t))
    (save-excursion
      (pimacs-section--replace-section pimacs-search--status-section
        (insert message)))))

(defun pimacs-search--set-search-type (button)
  (setf (pimacs-search-request-search-type pimacs-search--request)
        (button-get button 'pimacs-search-value))
  (pimacs-search--refresh-after-control-change 'search-type))

(defun pimacs-search--set-case (button)
  (setf (pimacs-search-request-case pimacs-search--request)
        (button-get button 'pimacs-search-value))
  (pimacs-search--refresh-after-control-change 'case))

(defun pimacs-search--set-scope (button)
  (setf (pimacs-search-request-scope pimacs-search--request)
        (button-get button 'pimacs-search-value))
  (pimacs-search--refresh-after-control-change 'scope))

(defun pimacs-search--toggle-filter (button)
  (let* ((filter (button-get button 'pimacs-search-filter))
         (filters (pimacs-search-request-filters pimacs-search--request)))
    (setf (pimacs-search-request-filters pimacs-search--request)
          (if (memq filter filters)
              (delq filter filters)
            (append filters (list filter))))
    (pimacs-search--refresh-after-control-change filter)))

(defun pimacs-search--set-context (button)
  (let* ((direction (button-get button 'pimacs-search-context-direction))
         (context (or (pimacs-search-request-context pimacs-search--request)
                      '(0 . 0)))
         (value (read-number (format "Lines %s: " direction)
                             (if (eq direction 'before)
                                 (car context)
                               (cdr context))))
         (updated-context
          (if (eq direction 'before)
              (cons value (cdr context))
            (cons (car context) value)))
         (updated-context (unless (equal updated-context '(0 . 0))
                            updated-context)))
    (setf (pimacs-search-request-context pimacs-search--request)
          updated-context)
    (pimacs-search--refresh-after-control-change 'context)))

(defun pimacs-search--clear-context (&optional _button)
  (setf (pimacs-search-request-context pimacs-search--request) nil)
  (pimacs-search--refresh-after-control-change 'context))

(defun pimacs-search--edit-query (&optional _button)
  "Edit the session search query."
  (interactive)
  (setf (pimacs-search-request-query pimacs-search--request)
        (read-string "Search query: "
                     (pimacs-search-request-query pimacs-search--request)
                     'pimacs-search--query-history))
  (pimacs-search--refresh-after-control-change 'query))

(defun pimacs-search--edit-directory (&optional _button)
  "Edit the session directory to search."
  (interactive)
  (setf (pimacs-search-request-directory pimacs-search--request)
        (expand-file-name
         (read-directory-name
          "Session directory: "
          (pimacs-search-request-directory pimacs-search--request))))
  (pimacs-search--refresh-after-control-change 'directory))

(defun pimacs-search--initialize-buffer ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq pimacs-section--root-section nil)
    (let ((root (pimacs-section--create-root-section)))
      (setq pimacs-search--request (pimacs-search--default-request))
      (setq pimacs-search--session-sections (make-hash-table :test 'equal))
      (setq pimacs-search--session-metadata (make-hash-table :test 'equal))
      (setq pimacs-search--controls-section
            (pimacs-section--create-section 'search-control root
              (pimacs-search--insert-controls)))
      (setq pimacs-search--status-section
            (pimacs-section--create-section 'search-info root
              (insert "No search started."))))))

(defun pimacs-search--buffer ()
  (let ((buffer (get-buffer-create pimacs-search--buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'pimacs-search-mode)
        (pimacs-search-mode)
        (pimacs-search--initialize-buffer)))
    buffer))

(defun pimacs-search--dwim-query ()
  (or (and (use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end)))
      (thing-at-point 'symbol t)
      (thing-at-point 'word t)
      ""))

;;;###autoload
(defun pimacs-search-sessions (query)
  "Search historical Pi sessions for QUERY."
  (interactive
   (list (read-string "Search sessions: "
                      (pimacs-search--dwim-query)
                      'pimacs-search--query-history)))
  (let ((buffer (pimacs-search--buffer)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (setf (pimacs-search-request-query pimacs-search--request) query)
      (pimacs-search--refresh-after-control-change 'query))))

(cl-defstruct pimacs-search-request
  directory scope query search-type case context filters project-root)

(cl-defstruct pimacs-search-render-context
  regexp ignore-case before after)

(defun pimacs-search--project-session-directory (directory project-root)
  (let* ((project-root (directory-file-name (expand-file-name project-root)))
         (path (replace-regexp-in-string "\\`[/\\\\]+" "" project-root))
         (path (replace-regexp-in-string "[:/\\\\]" "-" path)))
    (expand-file-name (format "--%s--" path) directory)))

(defun pimacs-search--default-request ()
  (make-pimacs-search-request
   :directory (expand-file-name pimacs-search-default-directory)
   :scope 'current
   :query ""
   :search-type 'string
   :case 'smart
   :context '(1 . 1)
   :filters (copy-sequence pimacs-search--default-filters)
   :project-root (pimacs--project-root)))

(defun pimacs-search--request-directory (request)
  (pcase (pimacs-search-request-scope request)
    ('current
     (pimacs-search--project-session-directory
      (pimacs-search-request-directory request)
      (pimacs-search-request-project-root request)))
    ('all (pimacs-search-request-directory request))
    (_ (error "Unknown search scope: %S"
              (pimacs-search-request-scope request)))))

(defun pimacs-search--rg-arguments (request)
  (append
   '("--json" "--no-ignore" "--glob" "*.jsonl")
   (pcase (pimacs-search-request-search-type request)
     ('string '("--fixed-strings"))
     ('words '("--fixed-strings" "--word-regexp"))
     ('regexp nil)
     (_ (error "Unknown search type: %S"
               (pimacs-search-request-search-type request))))
   (pcase (pimacs-search-request-case request)
     ('smart '("--smart-case"))
     ('sensitive '("--case-sensitive"))
     ('ignore '("--ignore-case"))
     (_ (error "Unknown search case: %S"
               (pimacs-search-request-case request))))
   (list "-e"
         (pimacs-search-request-query request)
         "-e" "\"type\":\"session\""
         "-e" "\"type\":\"session_info\""
         "--"
         (pimacs-search--request-directory request))))

(defun pimacs-search--jq-arguments (request)
  (list "--unbuffered" "-c"
        "--argjson" "filters"
        (json-serialize
         (vconcat
          (mapcar #'symbol-name
                  (pimacs-search-request-filters request))))
        pimacs-search--jq-filter))

(defun pimacs-search--startup-error (request)
  (cond
   ((equal (pimacs-search-request-query request) "")
    "Enter a search term.")
   ((not (executable-find pimacs-search-rg-executable))
    (format "Cannot find ripgrep executable: %s"
            pimacs-search-rg-executable))
   ((not (executable-find pimacs-search-jq-executable))
    (format "Cannot find jq executable: %s"
            pimacs-search-jq-executable))
   ((not (file-directory-p (pimacs-search--request-directory request)))
    (format "Session directory does not exist: %s"
            (abbreviate-file-name
             (pimacs-search--request-directory request))))))

(defun pimacs-search--shell-command (program arguments)
  (mapconcat #'shell-quote-argument (cons program arguments) " "))

(defun pimacs-search--pipeline-command (request)
  (format "%s | %s"
          (pimacs-search--shell-command
           pimacs-search-rg-executable
           (pimacs-search--rg-arguments request))
          (pimacs-search--shell-command
           pimacs-search-jq-executable
           (pimacs-search--jq-arguments request))))

(defun pimacs-search--start-pipeline (request output-filter sentinel)
  (if-let ((message (pimacs-search--startup-error request)))
      (progn
        (pimacs-search--set-status message)
        nil)
    (let (process)
      (condition-case-unless-debug error-data
          (progn
            (setq process
                  (make-process
                   :name "pimacs-search-pipeline"
                   :buffer nil
                   :command (list shell-file-name shell-command-switch
                                  (pimacs-search--pipeline-command request))
                   :connection-type 'pipe
                   :coding 'utf-8-unix
                   :filter output-filter
                   :sentinel sentinel
                   :noquery t))
            (setq pimacs-search--pipeline-process process)
            (pimacs-search--set-status "Searching…")
            process)
        (error
         (when (and process (process-live-p process))
           (delete-process process))
         (pimacs-search--set-status (error-message-string error-data))
         nil)))))

(defun pimacs-search--reset-results ()
  (let ((inhibit-read-only t))
    (dolist (section (copy-sequence
                      (pimacs-section-children pimacs-section--root-section)))
      (when (eq (pimacs-section-type section) 'search-session)
        (pimacs-section--delete-section section)))
    (setq pimacs-search--session-sections (make-hash-table :test 'equal)
          pimacs-search--session-metadata (make-hash-table :test 'equal))))

(defun pimacs-search--reset-stream-state ()
  (when (timerp pimacs-search--drain-timer)
    (cancel-timer pimacs-search--drain-timer))
  (setq pimacs-search--partial-output ""
        pimacs-search--entry-queue nil
        pimacs-search--entry-queue-tail nil
        pimacs-search--drain-timer nil
        pimacs-search--rendered-count 0
        pimacs-search--render-context nil
        pimacs-search--parse-error nil))

(defun pimacs-search--cancel-pipeline ()
  (when-let ((process pimacs-search--pipeline-process))
    (set-process-filter process #'ignore)
    (set-process-sentinel process #'ignore)
    (when (process-live-p process)
      (interrupt-process process t)))
  (setq pimacs-search--pipeline-process nil))


(defun pimacs-search--session-short-id (path)
  (or (plist-get (gethash path pimacs-search--session-metadata) :id)
      (when (string-match "_\\(.+\\)\\'" (file-name-base path))
        (match-string 1 (file-name-base path)))
      "unknown"))

(defun pimacs-search--format-session-timestamp (timestamp)
  (when (stringp timestamp)
    (condition-case nil
        (format-time-string "%F %R" (parse-iso8601-time-string timestamp))
      (error nil))))

(defun pimacs-search--insert-session-info (path)
  (let* ((metadata (gethash path pimacs-search--session-metadata))
         (name (plist-get metadata :name))
         (id (pimacs-search--session-short-id path))
         (timestamp (pimacs-search--format-session-timestamp
                     (plist-get metadata :timestamp)))
         (cwd (plist-get metadata :cwd)))
    (insert (propertize (if (and (stringp name) (> (length name) 0))
                            name
                          (pimacs--short-uuid id))
                        'face 'font-lock-type-face))
    (when timestamp
      (insert "  " timestamp))
    (when (stringp cwd)
      (insert "  "
              (propertize (abbreviate-file-name cwd)
                          'face 'dired-directory)))))

(defun pimacs-search--render-session-heading (path section)
  (pimacs-section--replace-section-body section
    (pimacs-search--insert-session-info path)))

(defun pimacs-search--update-session-metadata (record)
  (let* ((path (plist-get record :path))
         (metadata (copy-sequence (gethash path pimacs-search--session-metadata))))
    (pcase (plist-get record :kind)
      ("session"
       (setq metadata (plist-put metadata :id (plist-get record :id))
             metadata (plist-put metadata :cwd (plist-get record :cwd))
             metadata (plist-put metadata :timestamp (plist-get record :timestamp))))
      ("session-info"
       (setq metadata (plist-put metadata :name (plist-get record :name)))))
    (puthash path metadata pimacs-search--session-metadata)
    (when-let ((section (gethash path pimacs-search--session-sections)))
      (pimacs-search--render-session-heading path section))))

(defun pimacs-search--session-section (path)
  (or (gethash path pimacs-search--session-sections)
      (let ((section
             (pimacs-section--new-section
              'search-session pimacs-section--root-section :padding "\n")))
        (pimacs-section--insert-section section
          (pimacs-search--insert-session-info path))
        (puthash path section pimacs-search--session-sections)
        section)))

(defun pimacs-search--plain-text (content)
  (cond
   ((stringp content) content)
   ((listp content)
    (mapconcat
     (lambda (item)
       (if (equal (plist-get item :type) "text")
           (or (plist-get item :text) "")
         ""))
     content ""))
   (t "")))

(defun pimacs-search--make-preview-regexp (request)
  (let ((query (pimacs-search-request-query request)))
    (pcase (pimacs-search-request-search-type request)
      ('string (pimacs--grep-pattern-regexp query t))
      ('words (concat "\\_<" (pimacs--grep-pattern-regexp query t) "\\_>"))
      ('regexp (pimacs--grep-pattern-regexp query nil)))))

(defun pimacs-search--make-preview-ignore-case-p (request)
  (pcase (pimacs-search-request-case request)
    ('smart (not (string-match-p "[[:upper:]]"
                                 (pimacs-search-request-query request))))
    ('sensitive nil)
    ('ignore t)))

(defun pimacs-search--prepare-render-context ()
  (let* ((request pimacs-search--request)
         (context (or (pimacs-search-request-context request) '(0 . 0))))
    (setq pimacs-search--render-context
          (make-pimacs-search-render-context
           :regexp (pimacs-search--make-preview-regexp request)
           :ignore-case (pimacs-search--make-preview-ignore-case-p request)
           :before (car context)
           :after (cdr context)))))

(defun pimacs-search--merge-line-ranges (ranges)
  (let (merged)
    (dolist (range ranges)
      (if (and merged (<= (car range) (1+ (cdr (car merged)))))
          (setcdr (car merged) (max (cdr (car merged)) (cdr range)))
        (push range merged)))
    (nreverse merged)))

(defun pimacs-search--preview-ranges (lines render-context)
  (let (ranges)
    (when-let ((regexp (pimacs-search-render-context-regexp render-context)))
      (let ((case-fold-search
             (pimacs-search-render-context-ignore-case render-context)))
        (dotimes (index (length lines))
          (when (string-match-p regexp (aref lines index))
            (push (cons index index) ranges)))))
    (when ranges
      (let* ((before (pimacs-search-render-context-before render-context))
             (after (pimacs-search-render-context-after render-context))
             (line-count (length lines)))
        (pimacs-search--merge-line-ranges
         (mapcar (lambda (range)
                   (cons (max 0 (- (car range) before))
                         (min (1- line-count) (+ (cdr range) after))))
                 (nreverse ranges)))))))

(defun pimacs-search--insert-text-preview (lines ranges render-context)
  (let ((first t))
    (dolist (range ranges)
      (unless first
        (insert "\n…\n"))
      (let ((beginning (point)))
        (cl-loop for index from (car range) to (cdr range)
                 do (insert (aref lines index))
                 unless (= index (cdr range))
                 do (insert "\n"))
        (pimacs--fontify-grep-matches
         beginning
         (point)
         (pimacs-search-render-context-regexp render-context)
         (pimacs-search-render-context-ignore-case render-context)))
      (setq first nil))))

(defun pimacs-search--render-text-entry (result role text render-context)
  (let* ((lines (vconcat (split-string text "\n" nil)))
         (ranges (pimacs-search--preview-ranges lines render-context)))
    (when ranges
      (let ((session (pimacs-search--session-section (plist-get result :path))))
        (let ((entry-section
               (pimacs-section--new-section 'search-entry session :padding "\n")))
          (pimacs-section--insert-section entry-section
            (when role
              (pimacs-ui--insert-role-prefix role))
            (pimacs-search--insert-text-preview lines ranges render-context)))
        t))))

(defun pimacs-search--render-content-entry (result role content render-context)
  (let ((text (pimacs-search--plain-text content)))
    (unless (string-empty-p text)
      (pimacs-search--render-text-entry result role text render-context))))

(defun pimacs-search--message-content-items (message type)
  (let ((content (plist-get message :content)))
    (and (listp content)
         (cl-remove-if-not (lambda (item)
                             (equal (plist-get item :type) type))
                           content))))

(defun pimacs-search--json-string (value)
  (condition-case nil
      (json-serialize value)
    (error (format "%s" value))))

(defun pimacs-search--render-tool-call-entry (result item render-context)
  (let* ((name (or (plist-get item :name) "unknown"))
         (arguments (plist-get item :arguments))
         (text (concat (propertize (format "%s " name)
                                   'face 'pimacs-tool-name-face)
                       (if arguments
                           (pimacs-search--json-string arguments)
                         ""))))
    (pimacs-search--render-text-entry result nil text render-context)))

(defun pimacs-search--render-assistant-entry (result entry render-context)
  (let* ((message (plist-get entry :message))
         (filters (pimacs-search-request-filters pimacs-search--request))
         rendered)
    (when (memq 'assistant filters)
      (setq rendered
            (or (pimacs-search--render-content-entry
                 result "assistant" (plist-get message :content) render-context)
                rendered)))
    (when (memq 'thinking filters)
      (dolist (item (pimacs-search--message-content-items message "thinking"))
        (setq rendered
              (or (pimacs-search--render-text-entry
                   result "assistant" (plist-get item :thinking) render-context)
                  rendered))))
    (when (memq 'tool-call filters)
      (dolist (item (pimacs-search--message-content-items message "toolCall"))
        (setq rendered
              (or (pimacs-search--render-tool-call-entry result item render-context)
                  rendered))))
    rendered))

(defun pimacs-search--render-tool-result-entry (result entry render-context)
  (let* ((message (plist-get entry :message))
         (text (pimacs-search--plain-text (plist-get message :content))))
    (unless (string-empty-p text)
      (pimacs-search--render-text-entry
       result nil text render-context))))

(defun pimacs-search--render-bash-entry (result entry render-context)
  (let* ((message (plist-get entry :message))
         (command (or (plist-get message :command) ""))
         (output (pimacs-search--plain-text (plist-get message :output)))
         (text (concat (propertize "bash " 'face 'pimacs-tool-name-face)
                       command
                       (unless (string-empty-p output)
                         (concat "\n" output)))))
    (pimacs-search--render-text-entry result nil text render-context)))

(defun pimacs-search--render-compaction-entry (result entry render-context)
  (let ((summary (plist-get entry :summary))
        (tokens-before (plist-get entry :tokensBefore)))
    (when (stringp summary)
      (pimacs-search--render-text-entry
       result "assistant"
       (concat (when tokens-before
                 (format "Compacted from %s tokens\n" tokens-before))
               summary)
       render-context))))

(defun pimacs-search--render-entry (result render-context)
  (let* ((entry (plist-get result :entry))
         (message (plist-get entry :message)))
    (pcase (plist-get entry :type)
      ("message"
       (pcase (plist-get message :role)
         ("user"
          (when (memq 'user (pimacs-search-request-filters pimacs-search--request))
            (pimacs-search--render-content-entry
             result "user" (plist-get message :content) render-context)))
         ("assistant"
          (pimacs-search--render-assistant-entry result entry render-context))
         ("toolResult"
          (when (memq 'tool-result (pimacs-search-request-filters pimacs-search--request))
            (pimacs-search--render-tool-result-entry result entry render-context)))
         ("bashExecution"
          (when (memq 'bash (pimacs-search-request-filters pimacs-search--request))
            (pimacs-search--render-bash-entry result entry render-context)))))
      ("compaction"
       (when (memq 'compact (pimacs-search-request-filters pimacs-search--request))
         (pimacs-search--render-compaction-entry result entry render-context))))))


(defun pimacs-search--render-record (record render-context)
  (pcase (plist-get record :kind)
    ((or "session" "session-info")
     (pimacs-search--update-session-metadata record)
     nil)
    ("entry"
     (pimacs-search--render-entry record render-context))))

(defun pimacs-search--update-status ()
  (unless pimacs-search--parse-error
    (pimacs-search--set-status
     (format "%s: %d matches in %d sessions%s"
             (if pimacs-search--pipeline-process "Searching" "Finished")
             pimacs-search--rendered-count
             (hash-table-count pimacs-search--session-sections)
             (if pimacs-search--entry-queue "…" ".")))))

(defun pimacs-search--schedule-drain ()
  (unless pimacs-search--drain-timer
    (setq pimacs-search--drain-timer
          (run-with-idle-timer
           pimacs-search-render-idle-delay nil
           #'pimacs-search--drain-queue
           (current-buffer) pimacs-search--generation))))

(defun pimacs-search--enqueue-entry (entry)
  (let ((cell (list entry)))
    (if pimacs-search--entry-queue-tail
        (setcdr pimacs-search--entry-queue-tail cell)
      (setq pimacs-search--entry-queue cell))
    (setq pimacs-search--entry-queue-tail cell))
  (pimacs-search--schedule-drain))

(defun pimacs-search--drain-queue (buffer generation)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (= generation pimacs-search--generation)
        (setq pimacs-search--drain-timer nil)
        (let ((count 0)
              (render-context pimacs-search--render-context)
              (inhibit-read-only t))
          (save-excursion
            (while (and pimacs-search--entry-queue
                        (< count pimacs-search--render-batch-size))
              (let ((entry (pop pimacs-search--entry-queue)))
                (unless pimacs-search--entry-queue
                  (setq pimacs-search--entry-queue-tail nil))
                (when (pimacs-search--render-record entry render-context)
                  (cl-incf pimacs-search--rendered-count))
                (cl-incf count))))
          (when pimacs-search--entry-queue
            (pimacs-search--schedule-drain))
          (pimacs-search--update-status))))))

(defun pimacs-search--parse-entry (line)
  (unless (equal line "")
    (condition-case error-data
        (progn
          (pimacs-search--enqueue-entry
           (json-parse-string line :object-type 'plist :array-type 'list)))
      (error
       (setq pimacs-search--parse-error
             (error-message-string error-data))))))

(defun pimacs-search--consume-output (output finished)
  (let ((output (concat pimacs-search--partial-output output))
        (start 0)
        end)
    (setq pimacs-search--partial-output "")
    (while (setq end (string-match "\n" output start))
      (pimacs-search--parse-entry (substring output start end))
      (setq start (1+ end)))
    (setq pimacs-search--partial-output (substring output start))
    (when finished
      (pimacs-search--parse-entry pimacs-search--partial-output)
      (setq pimacs-search--partial-output ""))))

(defun pimacs-search--process-output (buffer generation output &optional finished)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (= generation pimacs-search--generation)
        (pimacs-search--consume-output output finished)))))

(defun pimacs-search--pipeline-sentinel (buffer generation process event)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (= generation pimacs-search--generation)
        (pimacs-search--process-output buffer generation "" t)
        (when (eq process pimacs-search--pipeline-process)
          (setq pimacs-search--pipeline-process nil))
        (cond
         (pimacs-search--parse-error
          (pimacs-search--set-status pimacs-search--parse-error))
         ((string= event "finished\n")
          (pimacs-search--update-status))
         (t
          (pimacs-search--set-status
           (format "Search failed: %s"
                   (replace-regexp-in-string "[\n\r]+\\'" "" event)))))))))

(defun pimacs-search-refresh ()
  "Restart the current session search."
  (interactive)
  (pimacs-search--cancel-pipeline)
  (cl-incf pimacs-search--generation)
  (pimacs-search--reset-stream-state)
  (pimacs-search--prepare-render-context)
  (pimacs-search--reset-results)
  (let ((buffer (current-buffer))
        (generation pimacs-search--generation))
    (pimacs-search--start-pipeline
     pimacs-search--request
     (lambda (_process output)
       (pimacs-search--process-output buffer generation output))
     (lambda (process event)
       (pimacs-search--pipeline-sentinel buffer generation process event)))))

(provide 'pimacs-search)

;;; pimacs-search.el ends here
