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
(require 'pimacs-core)
(require 'pimacs-section)

(defcustom pimacs-search-rg-executable "rg"
  "Ripgrep executable used for historical session searches."
  :type 'string
  :group 'pimacs)

(defcustom pimacs-search-jq-executable "jq"
  "Jq executable used for historical session searches."
  :type 'string
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
| select($entry | accepted)
| {path: $match.path.text, offset: $match.absolute_offset, entry: $entry}
")

(defconst pimacs-search--buffer-name "*Pimacs Session Search*")

(defvar-local pimacs-search--controls-section nil)
(defvar-local pimacs-search--status-section nil)
(defvar-local pimacs-search--results-section nil)
(defvar-local pimacs-search--request nil)

(define-derived-mode pimacs-search-mode special-mode "Pimacs Search"
  "Major mode for browsing historical Pi session search results."
  (setq-local truncate-lines t))

(defface pimacs-search-control-face
  '((t :underline t))
  "Face used for editable session search controls."
  :group 'pimacs)

(defface pimacs-search-active-control-face
  '((t :inherit font-lock-keyword-face))
  "Face used for selected session search control options."
  :group 'pimacs)

(defun pimacs-search--insert-button (text action &rest properties)
  (apply #'insert-text-button
         text
         'action action
         'follow-link t
         'face 'pimacs-search-control-face
         properties))

(defun pimacs-search--insert-options (control selected action options)
  (let ((first t))
    (dolist (option options)
      (unless first
        (insert " "))
      (let ((text (replace-regexp-in-string "-" " " (symbol-name option))))
        (if (eq option selected)
            (insert (propertize text
                                'face 'pimacs-search-active-control-face
                                'pimacs-search-focus control))
          (pimacs-search--insert-button
           text action 'pimacs-search-value option)))
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
     '(current-project all-projects))
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

(defun pimacs-search--set-search-type (button)
  (setf (pimacs-search-request-search-type pimacs-search--request)
        (button-get button 'pimacs-search-value))
  (pimacs-search--render-controls 'search-type))

(defun pimacs-search--set-case (button)
  (setf (pimacs-search-request-case pimacs-search--request)
        (button-get button 'pimacs-search-value))
  (pimacs-search--render-controls 'case))

(defun pimacs-search--set-scope (button)
  (setf (pimacs-search-request-scope pimacs-search--request)
        (button-get button 'pimacs-search-value))
  (pimacs-search--render-controls 'scope))

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
    (pimacs-search--render-controls 'context)))

(defun pimacs-search--clear-context (&optional _button)
  (setf (pimacs-search-request-context pimacs-search--request) nil)
  (pimacs-search--render-controls 'context))

(defun pimacs-search--edit-query (&optional _button)
  "Edit the session search query."
  (interactive)
  (setf (pimacs-search-request-query pimacs-search--request)
        (read-string "Search query: "
                     (pimacs-search-request-query pimacs-search--request)))
  (pimacs-search--render-controls 'query))

(defun pimacs-search--edit-directory (&optional _button)
  "Edit the session directory to search."
  (interactive)
  (setf (pimacs-search-request-directory pimacs-search--request)
        (expand-file-name
         (read-directory-name
          "Session directory: "
          (pimacs-search-request-directory pimacs-search--request))))
  (pimacs-search--render-controls 'directory))

(defun pimacs-search--initialize-buffer ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq pimacs-section--root-section nil)
    (let ((root (pimacs-section--create-root-section)))
      (setq pimacs-search--request (pimacs-search--default-request))
      (setq pimacs-search--controls-section
            (pimacs-section--create-section 'search root
              (pimacs-search--insert-controls)))
      (setq pimacs-search--status-section
            (pimacs-section--create-section 'info root
              (insert "No search started.")))
      (setq pimacs-search--results-section
            (pimacs-section--create-section 'custom root)))))

(defun pimacs-search--buffer ()
  (let ((buffer (get-buffer-create pimacs-search--buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'pimacs-search-mode)
        (pimacs-search-mode)
        (pimacs-search--initialize-buffer)))
    buffer))

;;;###autoload
(defun pimacs-search-sessions ()
  "Display the persistent buffer for searching historical Pi sessions."
  (interactive)
  (pop-to-buffer (pimacs-search--buffer)))

(cl-defstruct pimacs-search-request
  directory scope query search-type case context filters project-root)

(defun pimacs-search--project-session-directory (directory project-root)
  (let* ((project-root (directory-file-name (expand-file-name project-root)))
         (path (replace-regexp-in-string "\\`[/\\\\]+" "" project-root))
         (path (replace-regexp-in-string "[:/\\\\]" "-" path)))
    (expand-file-name (format "--%s--" path) directory)))

(defun pimacs-search--default-request ()
  (make-pimacs-search-request
   :directory (expand-file-name pimacs-search-default-directory)
   :scope 'current-project
   :query ""
   :search-type 'string
   :case 'smart
   :context nil
   :filters (copy-sequence pimacs-search--default-filters)
   :project-root (pimacs--project-root)))

(defun pimacs-search--request-directory (request)
  (pcase (pimacs-search-request-scope request)
    ('current-project
     (pimacs-search--project-session-directory
      (pimacs-search-request-directory request)
      (pimacs-search-request-project-root request)))
    ('all-projects (pimacs-search-request-directory request))
    (_ (error "Unknown search scope: %S"
              (pimacs-search-request-scope request)))))

(provide 'pimacs-search)

;;; pimacs-search.el ends here
