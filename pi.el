;;; pi.el --- Emacs Client for Pi -*- lexical-binding: t -*-

;; Copyright (C) 2026 Anantha Kumaran.

;; Author: Anantha kumaran <ananthakumaran@gmail.com>
;; URL: https://github.com/ananthakumaran/pi.el
;; Version: 0.1
;; Keywords: convenience processes
;; Package-Requires: ((emacs "28.1") (compat "31.0") (markdown-mode "2.8") (timeout "2.1.7") (pcre2el "1.12") (spinner "1.7"))

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by

;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; pi.el provides an Emacs client for interacting with Pi, an AI coding agent.

;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'project)
(require 'widget)
(require 'wid-edit)
(require 'ring)
(require 'subr-x)
(require 'parse-time)
(require 'timeout)
(require 'spinner)
(require 'pcre2el)
(require 'thingatpt)
(require 'ffap)
(require 'ansi-color)
(require 'diff-mode)
(require 'imenu)

(defgroup pi nil
  "Emacs client for Pi."
  :prefix "pi-"
  :group 'tools)

(require 'pi-section)
(require 'pi-edit)
(require 'pi-utils)

(defface pi-chat-role-face
  '((t :inherit font-lock-builtin-face))
  "Face used for generic chat role labels."
  :group 'pi)

(defface pi-chat-user-role-face
  '((t :inherit font-lock-keyword-face))
  "Face used for user chat message role labels."
  :group 'pi)

(defface pi-chat-assistant-role-face
  '((t :inherit font-lock-constant-face))
  "Face used for assistant chat message role labels."
  :group 'pi)

(defface pi-chat-title-face
  '((t :inherit font-lock-builtin-face))
  "Face used for titles."
  :group 'pi)

(defface pi-error-face
  '((t :inherit error))
  "Face used for Pi widget error messages."
  :group 'pi)

(defface pi-thinking-face
  '((t :inherit font-lock-comment-face))
  "Face used for assistant thinking content."
  :group 'pi)

(defface pi-tool-name-face
  '((t :inherit font-lock-function-name-face))
  "Face used for tool names in tool execution events."
  :group 'pi)

(defface pi-grep-match-face
  '((t :inherit match))
  "Face used to highlight matching text in grep tool results."
  :group 'pi)

(defface pi-notify-info-face
  '((t :inherit font-lock-comment-face))
  "Face used for info notification messages."
  :group 'pi)

(defface pi-notify-warning-face
  '((t :inherit warning))
  "Face used for warning notification messages."
  :group 'pi)

(defface pi-notify-error-face
  '((t :inherit error))
  "Face used for error notification messages."
  :group 'pi)

(defface pi-widget-face
  '((t :inherit shadow))
  "Face used for extension widgets."
  :group 'pi)

(defface pi-status-face
  '((t :inherit shadow))
  "Face used for extension status."
  :group 'pi)

(defvar pi--minimum-version "0.80.3"
  "The minimum supported Pi agent version.")

(defcustom pi-use-ansi-colors t
  "Whether to render ANSI colors in widget and status output."
  :type 'boolean
  :group 'pi)

(defcustom pi-sync-request-timeout 2
  "The number of seconds to wait for a sync response."
  :type 'integer
  :group 'pi)

(defcustom pi-executable "pi"
  "Pi command executable name."
  :type 'string
  :group 'pi)

(defcustom pi-process-environment '()
  "List of extra environment variables to use when starting pi."
  :type '(repeat string)
  :group 'pi)

(defcustom pi-flags '()
  "List of additional flags to provide when starting pi."
  :type '(repeat string)
  :group 'pi)

(defcustom pi-log-rpc nil
  "When non-nil, log all RPC JSON to `pi-log-rpc-file'."
  :type 'boolean
  :group 'pi)

(defcustom pi-log-rpc-file (expand-file-name "pi.el.log" (temporary-file-directory))
  "File to write RPC JSON log entries to."
  :type 'file
  :group 'pi)

(defcustom pi-file-completion-backend 'project
  "Completion backend for @-prefixed file paths in prompts.
`project' uses `project-files' to list files in the current project.
`file' uses `file-name-all-completions' to list files under the project root."
  :type '(choice (const :tag "Project files" project)
                 (const :tag "Default file system" file))
  :group 'pi)

(defcustom pi-prompt-history-max-size 500
  "Maximum number of prompt history entries to keep."
  :type 'integer
  :group 'pi)

(defcustom pi-resume-max-sessions 100
  "Maximum number of recent sessions to list when resuming a session."
  :type 'integer
  :group 'pi)

(defcustom pi-prompt-streaming-behavior 'followUp
  "Default streaming behavior for prompts.

`steer': Queue the message while the agent is running.  It is delivered
after the current assistant turn finishes executing its tool calls,
before the next LLM call.

`followUp': Wait until the agent finishes.  Message is delivered only
when agent stops."
  :type '(choice (const :tag "Follow up" followUp)
                 (const :tag "Steer" steer))
  :group 'pi)

(defcustom pi-slash-commands
  '(("model" pi-select-model 0)
    ("new" pi-new-session 0)
    ("reload" pi-reload 0)
    ("resume" pi-resume 0)
    ("compact" pi-compact 1)
    ("set-auto-compaction" pi-set-auto-compaction 0)
    ("set-auto-retry" pi-set-auto-retry 0)
    ("session" pi-session-stats 0)
    ("name" pi-set-session-name 1)
    ("set-thinking-level" pi-set-thinking-level 0)
    ("cycle-model" pi-cycle-model 0)
    ("cycle-thinking-level" pi-cycle-thinking-level 0)
    ("set-steering-mode" pi-set-steering-mode 0)
    ("set-follow-up-mode" pi-set-follow-up-mode 0)
    ("fork" pi-fork 0)
    ("clone" pi-clone 0)
    ("copy" pi-copy 0)
    ("export" pi-export 1)
    ("quit" pi-quit-chat 0)
    ("exit" pi-quit-chat 0))
  "Alist mapping slash command names to command specs.

Each entry is (NAME COMMAND MAX-ARGS) where NAME is the command
string without the leading slash, COMMAND is a command symbol,
and MAX-ARGS is 0 or 1 indicating the number of optional string
arguments the command accepts."
  :type '(repeat (list string symbol integer))
  :group 'pi)

(defcustom pi-insert-tool-args-functions
  '(("read" . pi--insert-read-args)
    ("write" . pi--insert-write-args)
    ("edit" . pi--insert-edit-args)
    ("bash" . pi--insert-bash-args)
    ("grep" . pi--insert-grep-args)
    ("find" . pi--insert-find-args)
    ("ls" . pi--insert-ls-args))
  "Alist mapping tool names to inserter functions.

Each entry is (TOOL-NAME . FUNCTION) where FUNCTION is called
with ARGS plist to insert formatted tool call arguments."
  :type '(alist :key-type string :value-type function)
  :group 'pi)

(defcustom pi-insert-tool-result-functions
  '(("bash" . pi--insert-bash-result)
    ("read" . pi--insert-read-result)
    ("write" . pi--insert-write-result)
    ("edit" . pi--insert-edit-result)
    ("grep" . pi--insert-grep-result)
    ("find" . pi--insert-find-result)
    ("ls" . pi--insert-ls-result))
  "Alist mapping tool names to result inserter functions.

Each entry is (TOOL-NAME . FUNCTION) where FUNCTION is called
with (RESULT-TEXT DETAILS ARGS) to insert the tool execution result."
  :type '(alist :key-type string :value-type function)
  :group 'pi)

(defcustom pi-visit-tool-result-functions
  '(("read" . pi--visit-read-result)
    ("write" . pi--visit-write-result)
    ("edit" . pi--visit-edit-result)
    ("grep" . pi--visit-grep-result))
  "Alist mapping tool names to result visitor functions.

Each entry is (TOOL-NAME . FUNCTION) where FUNCTION is called
with (DETAILS ARGS) to visit the relevant location of the tool result."
  :type '(alist :key-type string :value-type function)
  :group 'pi)

(defcustom pi-visit-tool-call-functions
  '(("read" . pi--visit-read-call)
    ("write" . pi--visit-write-call)
    ("edit" . pi--visit-edit-call))
  "Alist mapping tool names to call visitor functions.

Each entry is (TOOL-NAME . FUNCTION) where FUNCTION is called
with (ARGS) to visit the relevant location of the tool call."
  :type '(alist :key-type string :value-type function)
  :group 'pi)

(defcustom pi-insert-custom-message-functions '()
  "Alist mapping custom message types to inserter functions.

Each entry is (CUSTOM-TYPE . FUNCTION) where FUNCTION is called
with the message plist to insert the custom message content."
  :type '(alist :key-type string :value-type function)
  :group 'pi)

(defcustom pi-send-pop-to-chat t
  "Whether to pop to the chat buffer after sending region, filename or errors."
  :type 'boolean
  :group 'pi)

(defvar-local pi--project-file-cache nil)

(defun pi-clear-project-file-cache ()
  "Clear the project file cache."
  (interactive)
  (setq pi--project-file-cache nil))

(defun pi--maybe-log-rpc (type json)
  (when pi-log-rpc
    (write-region (concat "{\"type\": \"" type "\", \"message\": " json "}\n") nil pi-log-rpc-file t 'inhibit-message)))

;;; Widget

(defconst pi--empty-widget-text (propertize " " 'invisible t))

(defun pi--widget-item-value-create (widget)
  (let ((value (widget-get widget :value)))
    (if pi-use-ansi-colors
        (insert (ansi-color-apply value))
      (insert (propertize (ansi-color-filter-apply value)
                          'face
                          (widget-get widget :face))))))

(define-widget 'pi-item 'item
  "Item widget with font face support."
  :value-create #'pi--widget-item-value-create
  :format "%v")

(defun pi--widget-end-if-inside (widget)
  "Move to end of text field if point is inside WIDGET."
  (when (and (<= (widget-get widget :from) (point))
             (<= (point) (widget-get widget :to)))
    (goto-char (widget-field-text-end widget))
    (when-let (window (get-buffer-window (current-buffer) t))
      (set-window-point window (widget-field-text-end widget)))))


(defun pi--response-success-p (response)
  (and response
       (plist-get response :success)
       (not (eq (plist-get response :success) 'json-false))))

(defmacro pi--on-response-success (response &rest body)
  (declare (indent 1))
  (let ((resp-sym (gensym "resp")))
    `(let ((,resp-sym ,response))
       (if (pi--response-success-p ,resp-sym)
           (progn ,@body)
         (when-let (err (plist-get ,resp-sym :error))
           (pi--widget-save-excursion
             (pi-section--create-section 'error pi-section--root-section
               (pi--insert-error (format "%s" err))))
           nil)))))

(defmacro pi--on-response-success-callback (response &rest body)
  (declare (indent 1))
  `(lambda (,response)
     (pi--on-response-success ,response
       ,@body)))

(defmacro pi--unless-cancelled (resp operation &rest body)
  (declare (indent 2))
  `(let ((cancelled (plist-get (plist-get ,resp :data) :cancelled)))
     (if (eq cancelled t)
         (pi--widget-save-excursion
           (pi-section--create-section 'error pi-section--root-section
             (pi--insert-error (format "%s cancelled." ,operation))))
       ,@body)))


;;; State management

(cl-defstruct pi-tool-call
  call-section result-section prev-text tool-name args)

(defvar pi--agents (make-hash-table :test 'equal))
(defvar pi--chats (make-hash-table :test 'equal))
(defvar pi--response-callbacks (make-hash-table :test 'equal))

(pi--def-permanent-buffer-local pi--project-root nil)
(pi--def-permanent-buffer-local pi--project-key nil)
(pi--def-permanent-buffer-local pi--prompt-widget nil)
(pi--def-permanent-buffer-local pi--prompt-before-widget nil)
(pi--def-permanent-buffer-local pi--prompt-after-widget nil)
(pi--def-permanent-buffer-local pi--prompt-widget-lines nil)
(pi--def-permanent-buffer-local pi--status-widget nil)
(pi--def-permanent-buffer-local pi--status-widget-texts nil)
(pi--def-permanent-buffer-local pi--text-section nil)
(pi--def-permanent-buffer-local pi--thinking-section nil)
(pi--def-permanent-buffer-local pi--header-line-state nil)
(pi--def-permanent-buffer-local pi--tool-calls nil)
(pi--def-permanent-buffer-local pi--cleanup-callback-fn nil)
(pi--def-permanent-buffer-local pi--agent-state nil)
(pi--def-permanent-buffer-local pi--spinner nil)
(pi--def-permanent-buffer-local pi--retry-in-progress nil)
(pi--def-permanent-buffer-local pi--commands nil)

(defvar pi--event-listeners (make-hash-table :test 'equal))

(defvar pi--request-counter 0)

;;; History

(pi--def-permanent-buffer-local pi--prompt-history nil)
(pi--def-permanent-buffer-local pi--prompt-history-index 0)

(defun pi-previous-prompt ()
  "Navigate to the previous prompt in history."
  (interactive)
  (let ((len (ring-length pi--prompt-history)))
    (when (< pi--prompt-history-index len)
      (cl-incf pi--prompt-history-index)
      (widget-value-set pi--prompt-widget
                        (ring-ref pi--prompt-history (- pi--prompt-history-index 1))))))

(defun pi-next-prompt ()
  "Navigate to the next prompt in history."
  (interactive)
  (cond
   ((> pi--prompt-history-index 1)
    (cl-decf pi--prompt-history-index)
    (widget-value-set pi--prompt-widget
                      (ring-ref pi--prompt-history (1- pi--prompt-history-index))))
   ((= pi--prompt-history-index 1)
    (setq pi--prompt-history-index 0)
    (widget-value-set pi--prompt-widget ""))))

(defun pi-search-prompt ()
  "Search prompt history and select an entry."
  (interactive)
  (let ((items (ring-elements pi--prompt-history)))
    (if (null items)
        (message "No prompt history")
      (let ((selected (completing-read "Search prompt: " items nil t)))
        (widget-value-set pi--prompt-widget selected)
        (setq pi--prompt-history-index 0)
        (pi-focus-prompt)))))

;;; Core

(defun pi--project-root ()
  (or
   pi--project-root
   (let ((project (project-current))
         (path default-directory))
     (if project
         (setq path (project-root project))
       (message "Couldn't find project root folder. Using '%s' as project root." default-directory))
     (let ((full-path (expand-file-name path)))
       (setq pi--project-root full-path)
       full-path))))

(defun pi--project-name ()
  (file-name-nondirectory (directory-file-name (pi--project-root))))

(defun pi--agent-buffer-name ()
  (format "*pi-agent:%s*" (pi--project-key)))

(defun pi--chat-buffer-name (&optional title)
  (if title
      (format "*pi-chat:%s:%s*" (pi--project-name) title)
    (format "*pi-chat:%s*" (pi--project-name))))

(defun pi--project-key ()
  "Unique key for the current project, used for internal hash tables."
  (or
   pi--project-key
   (md5 (pi--project-root))))

(defun pi--recenter-chat ()
  (when-let (window (get-buffer-window (current-buffer) t))
    (with-selected-window window
      (recenter (- -1 scroll-margin (pi--widget-lines pi--prompt-widget) (pi--extra-widget-lines))))))

(defmacro pi--widget-save-excursion (&rest body)
  "Insert before PROMPT-WIDGET and restore focus.  BODY is the content."
  (declare (indent 0))
  `(let* ((inhibit-read-only t)
          (window (get-buffer-window (current-buffer) t))
          (follow-p
           (and window
                (>= (window-point window)
                    (widget-get pi--prompt-widget :from)))))
     (save-excursion
       (goto-char (widget-get pi--prompt-widget :from))
       ,@body)
     (when follow-p
       (pi--recenter-chat))))

(defmacro pi--with-chat-buffer (&rest body)
  "Execute BODY in the current chat buffer."
  (declare (indent 0))
  `(let ((buffer (pi--current-chat)))
     (if buffer
         (with-current-buffer buffer
           (progn ,@body))
       (error "Chat doesn't exist, start a new chat using M-x pi-chat"))))

(defun pi--current-agent ()
  (gethash (pi--project-key) pi--agents))

(defun pi--current-chat ()
  (gethash (pi--project-key) pi--chats))

(defun pi--next-request-id ()
  (number-to-string (cl-incf pi--request-counter)))

;;; Agent

(defun pi--dispatch-response (response)
  (let* ((request-id (plist-get response :id))
         (callback (gethash request-id pi--response-callbacks)))
    (when callback
      (let ((buffer (car callback)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (apply (cdr callback) (list response)))))
      (remhash request-id pi--response-callbacks))))

(defun pi--dispatch-event (event)
  (let ((key (pi--project-key)))
    (when-let (all-listener (gethash (cons key t) pi--event-listeners))
      (with-current-buffer (car all-listener)
        (apply (cdr all-listener) (list event))))
    (when-let (listener (gethash (cons key (plist-get event :type)) pi--event-listeners))
      (with-current-buffer (car listener)
        (apply (cdr listener) (list event))))))

(defun pi--set-event-listener (name listener)
  "Set event listener NAME for all events.  LISTENER is the callback."
  (puthash (cons (pi--project-key) name) (cons (current-buffer) listener) pi--event-listeners))

(defun pi--dispatch (response)
  (cl-case (intern (plist-get response :type))
    ((response) (pi--dispatch-response response))
    (t (pi--dispatch-event response))))

(defun pi--send-command (type args &optional callback)
  (unless (pi--current-agent)
    (error "Agent does not exist.  Run M-x pi-restart-chat to start it again"))

  (let* ((request-id (pi--next-request-id))
         (command (pi--plist-merge (list :id request-id :type type) args))
         (encoded-command (pi--json-encode command))
         (payload (concat encoded-command "\n")))
    (pi--maybe-log-rpc "input" encoded-command)
    (process-send-string (pi--current-agent) payload)
    (when callback
      (puthash request-id (cons (current-buffer) callback) pi--response-callbacks))))

(defun pi--send-command-sync (name args)
  (let* ((start-time (current-time))
         (response nil))
    (pi--send-command name args (lambda (resp) (setq response resp)))
    (while (not response)
      (accept-process-output nil 0.01)
      (when (> (pi--seconds-elapsed-since start-time) pi-sync-request-timeout)
        (error "Sync request timed out %s" name)))
    response))

(defun pi--net-sentinel (process message)
  (let ((project-name (process-get process 'project-name)))
    (message "(%s) pi exits: %s." project-name (string-trim message))
    (ignore-errors
      (kill-buffer (process-buffer process)))
    (pi--cleanup-agent process)))

(defun pi--net-filter (process data)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert (format "%s" data)))
  (pi--decode-response process))

(defun pi--enough-response-p ()
  (goto-char (point-min))
  (save-excursion
    (when (search-forward "{")
      (search-forward "\n" nil t))))

(defun pi--decode-response (process)
  (with-current-buffer (process-buffer process)
    (when (pi--enough-response-p)
      (search-forward "{")
      (backward-char 1)
      (let* ((raw-start (point))
             (response (pi--json-read-object)))
        (when pi-log-rpc
          (pi--maybe-log-rpc "output" (buffer-substring-no-properties raw-start (point))))
        (delete-region (point-min) (point))
        (when response
          (ignore-error quit
            (pi--dispatch response))))
      (when (>= (buffer-size) 16)
        (pi--decode-response process)))))

(defun pi--agent-version ()
  (with-temp-buffer
    (let* ((process-arguments (append pi-flags '("--version")))
           (command-line (mapconcat #'shell-quote-argument (cons pi-executable process-arguments) " "))
           (exit-code (apply #'call-process pi-executable nil (current-buffer) nil process-arguments)))
      (if (zerop exit-code)
          (string-trim (buffer-string))
        (let ((output (buffer-string)))
          (with-current-buffer (get-buffer-create "*pi-version-error*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "Failed to run `%s`.\n" command-line))
              (insert (format "\nExit code: %d\n" exit-code))
              (insert (format "\nOutput:\n%s" output)))
            (special-mode)
            (goto-char (point-min))
            (pop-to-buffer (current-buffer)))
          (error "Failed to run `%s' (exit code %d)" command-line exit-code))))))

(defun pi--check-agent-version (version)
  (unless (version-list-<= (version-to-list pi--minimum-version)
                           (version-to-list version))
    (error "Pi agent version %s is older than minimum supported version %s"
           version pi--minimum-version)))

(defun pi--start-agent (key)
  (when (pi--current-agent)
    (error "Agent already exist"))

  (let* ((default-directory (pi--project-root))
         (version (pi--agent-version)))
    (pi--check-agent-version version)
    (message "(%s) Starting pi version %s..." (pi--project-name) version)
    (let* ((process-environment (append pi-process-environment process-environment))
           (buf (generate-new-buffer (pi--agent-buffer-name)))
           ;; Use a pipe to communicate with the subprocess. This fixes a hang
           ;; when a >1k message is sent on macOS.
           (process-connection-type nil)
           (process-arguments (append pi-flags '("--mode" "rpc")))
           (process
            (apply #'start-file-process "pi" buf pi-executable process-arguments)))
      (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
      (set-process-filter process #'pi--net-filter)
      (set-process-sentinel process #'pi--net-sentinel)
      (set-process-query-on-exit-flag process nil)
      (with-current-buffer (process-buffer process)
        (buffer-disable-undo)
        (setq-local pi--project-key key))
      (process-put process 'project-key key)
      (process-put process 'project-root default-directory)
      (process-put process 'project-name (pi--project-name))
      (puthash key process pi--agents)
      (message "(%s) pi agent started successfully." (pi--project-name)))))


(defun pi--agent-add-cleanup (process fn)
  "Register FN as a cleanup callback for PROCESS.
Cleanup callbacks are run when the agent process exits."
  (push fn (process-get process 'pi--agent-cleanup-fns)))

(defun pi--agent-remove-cleanup (process fn)
  "Remove FN from the cleanup callbacks of PROCESS."
  (let ((cleanup-fns (process-get process 'pi--agent-cleanup-fns)))
    (process-put process 'pi--agent-cleanup-fns
                 (delq fn cleanup-fns))))

(defun pi--cleanup-agent (process)
  "Run cleanup callbacks registered on PROCESS and remove from agents table."
  (let ((project-key (process-get process 'project-key)))
    (when project-key
      (remhash project-key pi--agents))
    (dolist (fn (process-get process 'pi--agent-cleanup-fns))
      (ignore-errors (funcall fn)))
    (process-put process 'pi--agent-cleanup-fns nil)))

;;; Utility commands

(defun pi--kill-agent (&optional skip-cleanup-fn)
  "Kill the agent process.
When SKIP-CLEANUP-FN is non-nil, that cleanup callback is
removed before killing, so it won't run when the process exits."
  (when-let (agent (pi--current-agent))
    (when skip-cleanup-fn
      (pi--agent-remove-cleanup agent skip-cleanup-fn))
    (delete-process agent)))

;;; Completion

(defun pi--project-file-completions (_prefix)
  (or pi--project-file-cache
      (when-let (project (project-current))
        (let ((default-directory (pi--project-root))
              (project-files-relative-names t))
          (setq pi--project-file-cache (project-files project))))))

(defun pi--native-file-completions (prefix)
  (let* ((dir (or (file-name-directory prefix) ""))
         (file (file-name-nondirectory prefix))
         (full-dir (expand-file-name dir (pi--project-root))))
    (when (file-directory-p full-dir)
      (let ((candidates (file-name-all-completions file full-dir)))
        (mapcar (lambda (c) (concat dir c)) candidates)))))

(defun pi--completion-at-point-file ()
  (let ((end (point)))
    (save-excursion
      (when (re-search-backward "@\\([^\t\n ]*\\)" (line-beginning-position) t)
        (let* ((start (match-beginning 1))
               (prefix (match-string 1))
               (completions
                (if (eq pi-file-completion-backend 'project)
                    (pi--project-file-completions prefix)
                  (pi--native-file-completions prefix))))
          (when completions
            (list start end completions :category 'file :company-prefix-length t)))))))

(defun pi--completion-at-point-slash ()
  (let ((end (point)))
    (save-excursion
      (when (re-search-backward "\\([ \t]*\\)/\\([-a-zA-Z0-9]*\\)" (line-beginning-position) t)
        (let* ((match-start (match-beginning 0))
               (cmd-start (match-beginning 2))
               (slash-names (mapcar #'car pi-slash-commands))
               (command-names (mapcar #'car pi--commands))
               (all-names (append slash-names command-names)))
          (when (string-match "^user>[ \t]*$" (buffer-substring-no-properties (widget-get pi--prompt-widget :from) match-start))
            (list cmd-start end all-names
                  :company-prefix-length t
                  :annotation-function
                  (lambda (c)
                    (cond
                     ((member c slash-names) "(emacs)")
                     (pi--commands
                      (when-let ((cmd (assoc c pi--commands #'string=))
                                 (desc (plist-get (cdr cmd) :description)))
                        (format "(%s)" (plist-get (cdr cmd) :source)))))))))))))

;;; Chat

(defun pi--message-role (message)
  (or (plist-get message :role) "unknown"))

(defun pi--content-join (message type)
  (let ((content (plist-get message :content)))
    (if (stringp content)
        (if (string= type "text")
            content
          "")
      (mapconcat
       (lambda (item)
         (when (equal (plist-get item :type) type)
           (plist-get item (intern (concat ":" type)))))
       content
       ""))))

(defun pi--content-text (message)
  (pi--content-join message "text"))

(defun pi--content-thinking (message)
  (pi--content-join message "thinking"))

(defun pi--content-tool-calls (message)
  (cl-remove-if-not
   (lambda (item)
     (equal (plist-get item :type) "toolCall"))
   (plist-get message :content)))

(defun pi--role-face (role)
  (pcase role
    ("user" 'pi-chat-user-role-face)
    ("assistant" 'pi-chat-assistant-role-face)
    (_ 'pi-chat-role-face)))

(defun pi--insert-role-prefix (role)
  (insert (propertize (format "%s> " role) 'face (pi--role-face role))))

(defun pi--fill-string (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (not (eobp))
      (fill-region (point) (line-end-position))
      (forward-line 1))
    (buffer-string)))

(defun pi--insert-thinking (text)
  (insert (propertize text 'face 'pi-thinking-face)))

(defun pi--insert-tool-name (tool-name)
  (insert (propertize (format "%s " tool-name) 'face 'pi-tool-name-face)))

(defun pi--extract-truncation-notice (result-text)
  (if (string-match "\n\\(\\[[^]]* Use \\(?:offset=[^]]* to [Cc]ontinue\\|bash: [^]]*\\)\\.?\\]\\)$" result-text)
      (cons (replace-match "" nil nil result-text)
            (match-string 1 result-text))
    (cons result-text nil)))

(defun pi--insert-model-change (provider model-id)
  (pi-section--create-section 'model pi-section--root-section
    (insert (format "Switched to model: (%s) %s" provider model-id))))

(defun pi--insert-thinking-level-change (level)
  (pi-section--create-section 'thinking pi-section--root-section
    (insert (format "Thinking level set to: %s" level))))

(defun pi--insert-compaction (summary tokens-before)
  (let ((header (format "**Compacted from %s tokens**\n"
                        (pi--format-number-short tokens-before))))
    (pi-section--create-section 'compact pi-section--root-section
      (pi--insert-role-prefix "assistant")
      (insert (pi--render-markdown (concat header summary))))))

(defun pi--insert-session-info (name)
  (pi-section--create-section 'info pi-section--root-section
    (insert (format "Session renamed to: %s" name))))

(defun pi--insert-custom-message (message)
  (let ((display (plist-get message :display))
        (custom-type (plist-get message :customType)))
    (unless (eq display 'json-false)
      (if-let ((inserter (alist-get custom-type pi-insert-custom-message-functions nil nil #'equal)))
          (funcall inserter message)
        ;; Default rendering: use customType as role, render content
        (pi--widget-save-excursion
          (pi-section--create-section 'custom pi-section--root-section
            (pi--insert-role-prefix (or custom-type "custom"))
            (insert (pi--render-markdown (pi--content-text message)))))))))

(defun pi--insert-message (message)
  (pcase (pi--message-role message)
    ("user"
     (let* ((text (pi--content-text message))
            (header (pi--section-header text)))
       (unless (string-empty-p text)
         (pi--widget-save-excursion
           (let ((section (pi-section--create-section 'user pi-section--root-section
                            (pi--insert-role-prefix "user")
                            (insert text))))
             (pi-section--set-info section (make-pi-section-user-info
                                            :header header
                                            :message text)))))))

    ("assistant"
     (let ((thinking-text (pi--content-thinking message))
           (text (pi--content-text message))
           (tool-calls (pi--content-tool-calls message)))
       (unless (string-empty-p thinking-text)
         (pi--widget-save-excursion
           (let ((section (pi-section--create-section 'thinking pi-section--root-section
                            (pi--insert-role-prefix "assistant")
                            (pi--insert-thinking (pi--fill-string thinking-text)))))
             (pi-section--set-info section (make-pi-section-assistant-info
                                            :header (pi--section-header thinking-text)
                                            :message thinking-text
                                            :type 'thinking)))))
       (unless (string-empty-p text)
         (pi--widget-save-excursion
           (let ((section (pi-section--create-section 'assistant pi-section--root-section
                            (pi--insert-role-prefix "assistant")
                            (insert (pi--render-markdown text)))))
             (pi-section--set-info section (make-pi-section-assistant-info
                                            :header (pi--section-header text)
                                            :message text
                                            :type 'text)))))
       (dolist (tool-call tool-calls)
         (let ((tool-call-id (plist-get tool-call :id))
               (tool-name (plist-get tool-call :name))
               (args (plist-get tool-call :arguments)))
           (pi--widget-save-excursion
             (let ((call-section (pi-section--new-section 'tool-call pi-section--root-section :padding "\n")))
               (pi--insert-tool-call call-section tool-name args)
               (let ((result-section (pi-section--new-section 'tool-result call-section)))
                 (pi-section--insert-section result-section)
                 (puthash tool-call-id
                          (make-pi-tool-call
                           :call-section call-section
                           :result-section result-section
                           :prev-text ""
                           :tool-name tool-name
                           :args args)
                          pi--tool-calls))))))))
    ("toolResult"
     (let ((tool-call-id (plist-get message :toolCallId))
           (tool-name (plist-get message :toolName))
           (result-text (pi--content-text message))
           (is-error (plist-get message :isError))
           (details (plist-get message :details)))
       (when-let ((entry (gethash tool-call-id pi--tool-calls)))
         (let ((result-section (pi-tool-call-result-section entry))
               (args (pi-tool-call-args entry)))
           (pi--widget-save-excursion
             (pi-section--replace-section result-section
               (pi--insert-tool-result tool-name result-text is-error details args))
             (pi-section--set-info result-section (make-pi-section-tool-result-info :tool-name tool-name :details details :args args))))
         (remhash tool-call-id pi--tool-calls))))

    ("bashExecution"
     (let* ((args (list :command (plist-get message :command)))
            (output (plist-get message :output)))
       (pi--widget-save-excursion
         (let* ((call-section (pi-section--new-section 'tool-call pi-section--root-section :padding "\n"))
                (result-section (pi-section--new-section 'tool-result call-section)))
           (pi--insert-tool-call call-section "bash" args)
           (pi-section--insert-section result-section
             (pi--insert-tool-result "bash" output nil message))
           (pi-section--set-info result-section (make-pi-section-tool-result-info :tool-name "bash" :details nil :args args))))))

    ("custom"
     (pi--insert-custom-message message))))

(defun pi--handle-message-update (event)
  (let* ((assistant-message-event (plist-get event :assistantMessageEvent))
         (event-type (plist-get assistant-message-event :type))
         (delta (plist-get assistant-message-event :delta))
         (message (plist-get event :message))
         (role (pi--message-role message)))
    (when (member role '("assistant" "user"))
      (pcase event-type
        ("thinking_delta"
         (unless (string-empty-p delta)
           (pi--widget-save-excursion
             (if pi--thinking-section
                 (pi-section--append-section pi--thinking-section
                   (pi--insert-thinking delta))
               (setq pi--thinking-section (pi-section--new-section 'thinking pi-section--root-section))
               (pi-section--insert-section pi--thinking-section
                 (pi--insert-role-prefix role)
                 (pi--insert-thinking delta))))))
        ("text_delta"
         (unless (string-empty-p delta)
           (pi--widget-save-excursion
             (if pi--text-section
                 (pi-section--append-section pi--text-section
                   (insert delta))
               (setq pi--text-section (pi-section--new-section (if (equal role "user") 'user 'assistant)
                                                               pi-section--root-section))
               (pi-section--insert-section pi--text-section
                 (pi--insert-role-prefix role)
                 (insert delta))))))
        ("toolcall_end"
         (let* ((tool-call (plist-get assistant-message-event :toolCall))
                (tool-call-id (plist-get tool-call :id))
                (tool-name (plist-get tool-call :name))
                (args (plist-get tool-call :arguments)))
           (pi--widget-save-excursion
             (let ((call-section (pi-section--new-section 'tool-call pi-section--root-section :padding "\n")))
               (pi--insert-tool-call call-section tool-name args)
               (let ((result-section (pi-section--new-section 'tool-result call-section)))
                 (pi-section--insert-section result-section)
                 (puthash tool-call-id
                          (make-pi-tool-call
                           :call-section call-section
                           :result-section result-section
                           :prev-text ""
                           :tool-name tool-name
                           :args args)
                          pi--tool-calls))))))))))


(defun pi--handle-message-end (event)
  (let* ((message (plist-get event :message))
         (error-message (plist-get message :errorMessage))
         (role (pi--message-role message))
         (thinking-text (pi--content-thinking message))
         (text (pi--content-text message)))
    (when (member role '("assistant" "user"))
      (unless (string-empty-p thinking-text)
        (pi--widget-save-excursion
          (let ((section (pi-section--create-or-replace-section pi--thinking-section 'thinking pi-section--root-section
                           (pi--insert-role-prefix role)
                           (pi--insert-thinking (pi--fill-string thinking-text)))))
            (when (equal role "assistant")
              (pi-section--set-info section (make-pi-section-assistant-info
                                             :header (pi--section-header thinking-text)
                                             :message thinking-text
                                             :type 'thinking))))))

      (unless (string-empty-p text)
        (pi--widget-save-excursion
          (let ((section (pi-section--create-or-replace-section pi--text-section (if (equal role "user") 'user 'assistant) pi-section--root-section
                           (pi--insert-role-prefix role)
                           (insert text))))
            (when (equal role "user")
              (pi-section--set-info section (make-pi-section-user-info
                                             :header (pi--section-header text)
                                             :message text)))))))
    (when (and (equal role "assistant") (not (string-empty-p text)))
      (pi--widget-save-excursion
        (let ((section (pi-section--replace-section pi--text-section
                         (pi--insert-role-prefix role)
                         (insert (pi--render-markdown text)))))
          (pi-section--set-info section (make-pi-section-assistant-info
                                         :header (pi--section-header text)
                                         :message text
                                         :type 'text)))))
    (when (equal role "custom")
      (pi--insert-custom-message message))
    (when (and error-message (not (string-empty-p error-message)))
      (pi--widget-save-excursion
        (pi-section--create-section 'error pi-section--root-section
          (pi--insert-error error-message))))
    ;; Cleanup tracking state
    (setq pi--text-section nil
          pi--thinking-section nil)))

;; read
(defun pi--insert-read-args (args)
  (when-let ((path (plist-get args :path)))
    (let* ((offset (plist-get args :offset))
           (limit (plist-get args :limit))
           (start-line (or offset 1))
           (suffix (cond
                    ((and (null offset) (null limit)) "")
                    ((null limit) (format ":%d" start-line))
                    (t (let ((end-line (+ start-line limit -1)))
                         (format ":%d-%d" start-line end-line))))))
      (pi--insert-file-link (expand-file-name path (pi--project-root)) suffix))))

(defun pi--insert-read-result (result-text _details args)
  (when-let ((path (plist-get args :path)))
    (when (not (string-empty-p result-text))
      (pcase-let ((`(,clean-text . ,truncated-line) (pi--extract-truncation-notice result-text)))
        (insert (pi--render-content (expand-file-name path (pi--project-root)) clean-text))
        (when truncated-line
          (insert truncated-line))))))

(defun pi--visit-read-result (_details args)
  (when-let ((path (plist-get args :path)))
    (let* ((section-line (pi-section--section-line))
           (file (expand-file-name path (pi--project-root)))
           (line (+ (or (plist-get args :offset) 1) section-line))
           (column (pi--current-column-1-based)))
      (list :file file :line line :column column))))

(defun pi--visit-read-call (args)
  (when-let ((path (plist-get args :path)))
    (list :file (expand-file-name path (pi--project-root))
          :line (or (plist-get args :offset) 1))))

;; write
(defun pi--insert-write-args (args)
  (when-let ((path (plist-get args :path))
             (content (plist-get args :content)))
    (pi--insert-file-link (expand-file-name path (pi--project-root)))
    (when (not (string-empty-p content))
      (insert "\n")
      (insert (pi--render-content path content)))))

(defun pi--insert-write-result (result-text _details _args)
  (when (not (string-empty-p result-text))
    (insert (format "%s" result-text))))

(defun pi--visit-write-result (_details args)
  (when-let ((path (plist-get args :path)))
    (list :file (expand-file-name path (pi--project-root)))))

(defun pi--visit-write-call (args)
  (when-let ((path (plist-get args :path)))
    (let ((section-line (pi-section--section-line)))
      (list :file (expand-file-name path (pi--project-root))
            :line (max 1 section-line)
            :column (if (zerop section-line)
                        1
                      (pi--current-column-1-based))))))

;; edit
(defun pi--insert-edit-args (args)
  (when-let ((path (plist-get args :path)))
    (pi--insert-file-link (expand-file-name path (pi--project-root)))))

(defun pi--insert-edit-result (result-text details _args)
  (when-let ((patch (plist-get details :patch)))
    (insert (pi--render-diff patch)))
  (when (not (string-empty-p result-text))
    (insert (format "\n\n%s" result-text))))

(defun pi--visit-edit-call (args)
  (when-let ((path (plist-get args :path)))
    (list :file (expand-file-name path (pi--project-root)) :line 1)))

(defun pi--visit-edit-result (_details args)
  (when-let ((path (plist-get args :path)))
    (let* ((section (pi-section--current-section))
           (reverse (not (save-excursion (beginning-of-line) (looking-at "[-<]"))))
           (location
            (save-restriction
              (narrow-to-region (pi-section-beginning section)
                                (pi-section-end section))
              (condition-case nil
                  (pcase-let ((`(,buffer ,_line-offset ,pos ,src ,_dst ,_switched)
                               (diff-find-source-location nil reverse)))
                    (with-current-buffer buffer
                      (let ((visit-pos (+ (car pos) (cdr src))))
                        (save-excursion
                          (goto-char visit-pos)
                          (list :line (line-number-at-pos)
                                :column (pi--current-column-1-based))))))
                (error nil)))))
      (list :file (expand-file-name path (pi--project-root))
            :line (or (plist-get location :line) 1)
            :column (plist-get location :column)))))

;; bash
(defun pi--insert-bash-args (args)
  (when-let ((command (plist-get args :command)))
    (insert (pi--render-content "tmp.sh" command))))

(defun pi--insert-bash-result (result-text details _args)
  (let* ((exit-code (plist-get details :exitCode))
         (cancelled (plist-get details :cancelled))
         (full-output-path (plist-get details :fullOutputPath)))
    (when (not (string-empty-p result-text))
      (insert (format "%s" result-text)))
    (when (eq cancelled t)
      (pi--insert-error "Cancelled"))
    (when (and (numberp exit-code) (not (zerop exit-code)))
      (pi--insert-error (format "Command exited with code %d" exit-code)))
    (when full-output-path
      (insert "Output truncated. See full output at: ")
      (pi--insert-file-link full-output-path))))

;; grep
(defun pi--insert-grep-args (args)
  (let ((pattern (plist-get args :pattern))
        (path (plist-get args :path))
        (glob (plist-get args :glob))
        (ignore-case (plist-get args :ignoreCase))
        (literal (plist-get args :literal))
        (context (plist-get args :context))
        (limit (plist-get args :limit)))
    (insert (propertize (format "/%s/" pattern) 'face 'font-lock-string-face))
    (when path
      (insert (format " in %s" path)))
    (when glob
      (insert (format " (%s)" glob)))
    (when ignore-case
      (insert " --ignore-case"))
    (when literal
      (insert " --literal"))
    (when context
      (insert (format " -C %d" context)))
    (when limit
      (insert (format " limit %d" limit)))))

(defun pi--insert-grep-highlighted (text pattern &optional ignore-case literal)
  (let* ((regexp (if literal
                     (regexp-quote pattern)
                   (condition-case nil
                       (rxt-pcre-to-elisp pattern)
                     (error nil))))
         (case-fold-search (if ignore-case t nil)))
    (if (null regexp)
        (insert text)
      (insert (replace-regexp-in-string
               regexp
               (lambda (match)
                 (propertize match 'face 'pi-grep-match-face))
               text)))))

(defconst pi--grep-line-regexp "^\\(.*\\):\\([0-9]+\\): \\(.*\\)$")
(defconst pi--grep-line-alt-regexp "^\\(.*\\)\\([-:]\\)\\([0-9]+\\)\\([-:] ?\\)\\(.*\\)$")

(defun pi--insert-grep-result (result-text _details args)
  (if (string-empty-p result-text)
      (insert result-text)
    (let ((pattern (plist-get args :pattern))
          (ignore-case (plist-get args :ignoreCase))
          (literal (plist-get args :literal)))
      (if (or (null pattern) (string-empty-p pattern))
          (insert result-text)
        (let ((lines (split-string result-text "\n")))
          (dolist (line lines)
            (cond
             ((string-match pi--grep-line-regexp line)
              (insert (propertize (match-string 1 line) 'face 'compilation-info) ":")
              (insert (propertize (match-string 2 line) 'face 'compilation-line-number))
              (insert ": ")
              (pi--insert-grep-highlighted (match-string 3 line) pattern ignore-case literal))
             ((string-match pi--grep-line-alt-regexp line)
              (insert (propertize (match-string 1 line) 'face 'compilation-info))
              (insert (match-string 2 line))
              (insert (propertize (match-string 3 line) 'face 'compilation-line-number))
              (insert (match-string 4 line))
              (insert (match-string 5 line)))
             (t
              (insert line)))
            (insert "\n"))
          (delete-char -1))))))

(defun pi--normalize-grep-file (file args)
  (if-let ((path (plist-get args :path)))
      (if (file-directory-p path)
          (expand-file-name file path)
        path)
    file))

(defun pi--visit-grep-match (args cursor-column file-group line-group content-group)
  (let ((content-column (save-excursion
                          (goto-char (match-beginning content-group))
                          (current-column))))
    (list :file (pi--normalize-grep-file (match-string file-group) args)
          :line (string-to-number (match-string line-group))
          :column (max 1 (1+ (- cursor-column content-column))))))

(defun pi--visit-grep-result (_details args)
  (let ((cursor-column (current-column)))
    (save-excursion
      (beginning-of-line)
      (cond
       ((looking-at pi--grep-line-regexp)
        (pi--visit-grep-match args cursor-column 1 2 3))
       ((looking-at pi--grep-line-alt-regexp)
        (pi--visit-grep-match args cursor-column 1 3 5))))))

;; find
(defun pi--insert-find-args (args)
  (let ((pattern (plist-get args :pattern))
        (path (plist-get args :path))
        (limit (plist-get args :limit)))
    (insert (propertize (format "/%s/" pattern) 'face 'font-lock-string-face))
    (when path
      (insert (format " in %s" path)))
    (when limit
      (insert (format " limit %d" limit)))))

(defun pi--insert-find-result (result-text _details _args)
  (when (not (string-empty-p result-text))
    (insert result-text)))

;; ls
(defun pi--insert-ls-args (args)
  (when-let ((path (plist-get args :path)))
    (insert path))
  (when-let ((limit (plist-get args :limit)))
    (insert (format " limit %d" limit))))

(defun pi--insert-ls-result (result-text _details _args)
  (when (not (string-empty-p result-text))
    (insert result-text)))

(defun pi--format-tool-args (tool-name args)
  (with-temp-buffer
    (if-let ((inserter (alist-get tool-name pi-insert-tool-args-functions nil nil #'equal)))
        (funcall inserter args)
      (when args
        (insert (format "%S" args))))
    (buffer-string)))

(defun pi--insert-tool-call (section tool-name args)
  "Format and insert a tool call into SECTION for TOOL-NAME with ARGS."
  (let ((formatted-args (pi--format-tool-args tool-name args)))
    (pi--widget-save-excursion
      (pi-section--insert-section section
        (pi--insert-tool-name tool-name)
        (insert formatted-args))
      (pi-section--set-info section (make-pi-section-tool-call-info
                                     :tool-name tool-name
                                     :args args
                                     :header (pi--section-header (substring-no-properties formatted-args)))))))

(defun pi--insert-tool-result (tool-name result-text is-error &optional details args)
  (if (eq is-error t)
      (when (not (string-empty-p result-text))
        (pi--insert-error (format "%s" result-text)))
    (if-let ((inserter (alist-get tool-name pi-insert-tool-result-functions nil nil #'equal)))
        (funcall inserter result-text details args)
      (when (not (string-empty-p result-text))
        (insert (format "%s" result-text))))))

(defun pi--handle-tool-execution-update (event)
  (let* ((tool-call-id (plist-get event :toolCallId))
         (partial-result (plist-get event :partialResult))
         (new-text (pi--content-text partial-result))
         (entry (gethash tool-call-id pi--tool-calls)))
    (when (and entry new-text)
      (let ((prev-text (pi-tool-call-prev-text entry))
            (result-section (pi-tool-call-result-section entry)))
        (pi--widget-save-excursion
          (if (string-prefix-p prev-text new-text)
              (let ((diff (substring new-text (length prev-text))))
                (unless (string-empty-p diff)
                  (pi-section--append-section result-section
                    (insert diff))))
            (pi-section--replace-section result-section
              (insert new-text)))
          (setf (pi-tool-call-prev-text entry) new-text))))))

(defun pi--handle-tool-execution-end (event)
  (let* ((tool-call-id (plist-get event :toolCallId))
         (result (plist-get event :result))
         (result-text (pi--content-text result))
         (is-error (plist-get event :isError))
         (tool-name (plist-get event :toolName))
         (entry (gethash tool-call-id pi--tool-calls)))
    (when entry
      (let ((result-section (pi-tool-call-result-section entry))
            (details (plist-get result :details))
            (args (pi-tool-call-args entry)))
        (pi--widget-save-excursion
          (pi-section--replace-section result-section
            (pi--insert-tool-result tool-name result-text is-error
                                    details
                                    args)))
        (pi-section--set-info result-section (make-pi-section-tool-result-info :tool-name tool-name :details details :args args)))
      (remhash tool-call-id pi--tool-calls))))

(defun pi--handle-auto-retry-start (event)
  (setq pi--retry-in-progress t)
  (let ((attempt (plist-get event :attempt))
        (max-attempts (plist-get event :maxAttempts))
        (delay-ms (plist-get event :delayMs))
        (error-message (plist-get event :errorMessage)))
    (when (and error-message (not (string-empty-p error-message)))
      (pi--widget-save-excursion
        (pi-section--create-section 'error pi-section--root-section
          (pi--insert-error (format "Error: %s\n\n" error-message))
          (insert
           (propertize (format "Retrying %d/%d (waiting %ds)…" attempt max-attempts (/ delay-ms 1000))
                       'face 'pi-thinking-face)))))))

(defun pi--handle-auto-retry-end (event)
  (setq pi--retry-in-progress nil)
  (let ((attempt (plist-get event :attempt))
        (final-error (plist-get event :finalError)))
    (unless (pi--response-success-p event)
      (pi--widget-save-excursion
        (pi-section--create-section 'error pi-section--root-section
          (pi--insert-error
           (format "Error: Retry failed after %d attempts: %s" attempt final-error)))))))

(defun pi--handle-queue-update (event)
  (let* ((steering (plist-get event :steering))
         (follow-up (plist-get event :followUp))
         (has-content (or (consp steering)
                          (consp follow-up))))
    (when has-content
      (pi--widget-save-excursion
        (pi-section--create-section 'queue pi-section--root-section
          (insert (propertize "queue" 'face 'bold))
          (dolist (item steering)
            (insert (propertize (format "\n Steering: %s" item) 'face 'pi-thinking-face)))
          (dolist (item follow-up)
            (insert (propertize (format "\n Follow-up: %s" item) 'face 'pi-thinking-face))))))))

(defun pi--handle-compaction-end (event)
  (let* ((result (plist-get event :result))
         (error-message (plist-get event :errorMessage)))
    (cond
     (error-message
      (pi--widget-save-excursion
        (pi-section--create-section 'error pi-section--root-section
          (pi--insert-error error-message))))
     (result
      (let ((summary (plist-get result :summary))
            (tokens-before (plist-get result :tokensBefore)))
        (pi--widget-save-excursion
          (pi--insert-compaction summary tokens-before)))))))

(defun pi--notify (message &optional notify-type)
  (let ((face (pcase (or notify-type "info")
                ("warning" 'pi-notify-warning-face)
                ("error" 'pi-notify-error-face)
                (_ 'pi-notify-info-face))))
    (pi--widget-save-excursion
      (pi-section--create-section 'notify pi-section--root-section
        (insert (propertize message 'face face))))))

(defun pi--handle-notify (event)
  (pi--notify (plist-get event :message)
              (plist-get event :notifyType)))

(defun pi--widget-lines (widget)
  (let ((text (widget-value widget)))
    (if (or (string-empty-p text)
            (equal text pi--empty-widget-text))
        0
      (cl-count ?\n text))))

(defun pi--extra-widget-lines ()
  (+ (pi--widget-lines pi--prompt-after-widget)
     (pi--widget-lines pi--status-widget)))

(defun pi--widget-ensure-trailing-newline (text)
  (if (string-empty-p text)
      pi--empty-widget-text
    (if (= (aref text (1- (length text))) ?\n)
        text
      (concat text "\n"))))

(defun pi--update-widget-by-entries (widget entries)
  (widget-value-set widget
                    (pi--widget-ensure-trailing-newline (pi--join (pi--sort-entries-by-key entries)))))

(defun pi--update-prompt-widgets ()
  (let ((above '())
        (below '()))
    (when pi--prompt-widget-lines
      (maphash (lambda (key val)
                 (let ((lines (pi--join (car val)))
                       (placement (cdr val)))
                   (pcase placement
                     ("aboveEditor"
                      (push (cons key lines) above))
                     ("belowEditor"
                      (push (cons key lines) below)))))
               pi--prompt-widget-lines))
    (pi--update-widget-by-entries pi--prompt-before-widget above)
    (pi--update-widget-by-entries pi--prompt-after-widget below)))

(defun pi--handle-set-widget (event)
  (let* ((widget-key (plist-get event :widgetKey))
         (widget-lines (plist-get event :widgetLines))
         (widget-placement (or (plist-get event :widgetPlacement) "aboveEditor")))
    (if (or (not widget-lines)
            (null widget-lines))
        (remhash widget-key pi--prompt-widget-lines)
      (puthash widget-key (cons widget-lines widget-placement) pi--prompt-widget-lines))
    (pi--update-prompt-widgets)))

(defun pi--update-status-widget ()
  (let (entries)
    (when pi--status-widget-texts
      (maphash (lambda (key text)
                 (push (cons key text) entries))
               pi--status-widget-texts))
    (widget-value-set pi--status-widget
                      (pi--widget-ensure-trailing-newline
                       (pi--join (pi--sort-entries-by-key entries) " ")))))

(defun pi--handle-set-status (event)
  (let* ((status-key (plist-get event :statusKey))
         (status-text (plist-get event :statusText)))
    (if (or (not status-text) (string-empty-p status-text))
        (remhash status-key pi--status-widget-texts)
      (puthash status-key status-text pi--status-widget-texts))
    (pi--update-status-widget)))

(defun pi--handle-set-editor-text (event)
  (let ((text (plist-get event :text))
        (current (widget-value pi--prompt-widget)))
    (unless (string-empty-p current)
      (pi--clear-prompt current))
    (widget-value-set pi--prompt-widget text)))

(defun pi--handle-extension-ui-prompt (event prompt-fn)
  (let ((id (plist-get event :id)))
    (condition-case nil
        (funcall prompt-fn)
      (quit
       (pi--send-command "extension_ui_response"
                         (list :id id :cancelled t))))))

(defun pi--handle-select (event)
  (let* ((id (plist-get event :id))
         (title (plist-get event :title))
         (options (plist-get event :options)))
    (pi--widget-save-excursion
      (pi-section--create-section 'select pi-section--root-section
        (insert (propertize (format "%s:" title) 'face 'pi-chat-title-face))
        (dolist (option options)
          (insert "\n")
          (insert (propertize (format "  • %s" option) 'face 'pi-notify-info-face)))))
    (pi--handle-extension-ui-prompt
     event
     (lambda ()
       (let ((selected (completing-read (concat title ": ") options nil t)))
         (pi--send-command "extension_ui_response"
                           (list :id id :value selected)))))))

(defun pi--handle-confirm (event)
  (let* ((id (plist-get event :id))
         (title (plist-get event :title))
         (message (plist-get event :message)))
    (pi--widget-save-excursion
      (pi-section--create-section 'confirm pi-section--root-section
        (insert (propertize (format "%s:" title) 'face 'pi-chat-title-face))
        (insert "\n")
        (insert (propertize message 'face 'pi-notify-info-face))))
    (pi--handle-extension-ui-prompt
     event
     (lambda ()
       (let ((confirmed (y-or-n-p (concat message " "))))
         (pi--send-command "extension_ui_response"
                           (list :id id :confirmed (if confirmed t 'json-false))))))))

(defun pi--handle-input (event)
  (let* ((id (plist-get event :id))
         (title (plist-get event :title))
         (placeholder (plist-get event :placeholder)))
    (pi--widget-save-excursion
      (pi-section--create-section 'input pi-section--root-section
        (insert (propertize (format "%s:" title) 'face 'pi-chat-title-face))
        (when placeholder
          (insert "\n")
          (insert (propertize placeholder 'face 'pi-notify-info-face)))))
    (pi--handle-extension-ui-prompt
     event
     (lambda ()
       (let ((value (read-from-minibuffer
                     (concat title
                             (if placeholder (format " (%s) " placeholder) ": ")))))
         (pi--send-command "extension_ui_response"
                           (list :id id :value value)))))))

(defun pi--handle-editor (event)
  (let* ((id (plist-get event :id))
         (title (plist-get event :title))
         (prefill (plist-get event :prefill)))
    (pi--widget-save-excursion
      (pi-section--create-section 'input pi-section--root-section
        (insert (propertize (format "%s:" title) 'face 'pi-chat-title-face))))
    (pi--handle-extension-ui-prompt
     event
     (lambda ()
       (pi-edit--with-editor
        (lambda (value)
          (pi--send-command "extension_ui_response"
                            (list :id id :value value)))
        (lambda ()
          (pi--send-command "extension_ui_response"
                            (list :id id :cancelled t)))
        prefill)))))

(defun pi--handle-set-title (event)
  (let ((title (plist-get event :title)))
    (when title
      (rename-buffer (pi--chat-buffer-name title) t))))

(defun pi--handle-extension-ui-request (event)
  (pcase (plist-get event :method)
    ("notify" (pi--handle-notify event))
    ("select" (pi--handle-select event))
    ("confirm" (pi--handle-confirm event))
    ("input" (pi--handle-input event))
    ("editor" (pi--handle-editor event))
    ("set_editor_text" (pi--handle-set-editor-text event))
    ("setWidget" (pi--handle-set-widget event))
    ("setStatus" (pi--handle-set-status event))
    ("setTitle" (pi--handle-set-title event))))

(defun pi--register-event-listeners ()
  (pi--set-event-listener "message_update" #'pi--handle-message-update)
  (pi--set-event-listener "message_end" #'pi--handle-message-end)

  (pi--set-event-listener "tool_execution_update" #'pi--handle-tool-execution-update)
  (pi--set-event-listener "tool_execution_end" #'pi--handle-tool-execution-end)

  (pi--set-event-listener "auto_retry_start" #'pi--handle-auto-retry-start)
  (pi--set-event-listener "auto_retry_end" #'pi--handle-auto-retry-end)

  (pi--set-event-listener "queue_update" #'pi--handle-queue-update)
  (pi--set-event-listener "compaction_end" #'pi--handle-compaction-end)
  (pi--set-event-listener "extension_ui_request" #'pi--handle-extension-ui-request)
  (pi--set-event-listener t #'pi--handle-agent-state))

(defun pi--register-agent-cleanup ()
  "Register a cleanup callback on the agent to kill the chat buffer on exit."
  (when-let (agent (pi--current-agent))
    (let* ((buf (current-buffer))
           (fn (lambda ()
                 (when (buffer-live-p buf)
                   (kill-buffer buf)))))
      (pi--agent-add-cleanup agent fn)
      (setq-local pi--cleanup-callback-fn fn))))

(defun pi-focus-prompt ()
  "Move point to the chat prompt input field."
  (interactive)
  (goto-char (widget-field-text-end pi--prompt-widget)))

(defun pi--format-tool-state (tools)
  (pcase tools
    (`(,first ,second ,_ . ,rest)
     (format "%s, %s + %d more" first second (1+ (length rest))))
    (_
     (mapconcat #'identity tools ", "))))

(defun pi--format-state ()
  (if pi--agent-state
      (let ((state pi--agent-state))
        (if (consp state)
            (format "Pi %s(%s)"
                    (car state)
                    (pi--format-tool-state (cdr state)))
          (format "Pi %s" state)))
    "Pi"))

(defun pi--format-header ()
  "Format the header line from `pi--header-line-state'.
Shows context usage and model info."
  (let* ((model (plist-get pi--header-line-state :model))
         (provider (plist-get model :provider))
         (model-id (plist-get model :id))
         (thinking-level (plist-get pi--header-line-state :thinkingLevel))
         (auto-compact (plist-get pi--header-line-state :autoCompactionEnabled))
         (session-stats (plist-get pi--header-line-state :sessionStats))
         (context-usage (plist-get session-stats :contextUsage))
         (ctx-tokens (plist-get context-usage :tokens))
         (ctx-window-usage (plist-get context-usage :contextWindow))
         (ctx-str (pi--format-number-short ctx-window-usage))
         (usage-str (pi--format-number-short ctx-tokens)))
    (let* ((left (format "%s/%s (%s)"
                         usage-str ctx-str
                         (if auto-compact "auto" "manual")))
           (right (format "(%s) %s • %s"
                          (or provider "?")
                          (or model-id "?")
                          (or thinking-level "?"))))
      (format "%s%s%s"
              left
              (make-string (max 1 (- (window-width) (length left) (length right))) ?\s)
              right))))

(defun pi--format-mode-line ()
  (let ((spinner-str (and pi--agent-state (spinner-print pi--spinner)))
        (state-str (pi--format-state)))
    (if spinner-str
        (format " %s %s" state-str spinner-str)
      (format " %s" state-str))))

(defun pi--update-header-line ()
  (let* ((state-result nil)
         (stats-result nil)
         (try-update
          (lambda ()
            (when (and state-result stats-result)
              (setq pi--header-line-state
                    (plist-put state-result :sessionStats stats-result))
              (force-mode-line-update)))))
    (pi--send-command
     "get_state" '()
     (pi--on-response-success-callback resp
       (setq state-result (plist-get resp :data))
       (funcall try-update)))
    (pi--send-command
     "get_session_stats" '()
     (pi--on-response-success-callback resp
       (setq stats-result (plist-get resp :data))
       (funcall try-update)))))

(timeout-debounce 'pi--update-header-line 1)

(defun pi--update-agent-state (state)
  (setq pi--agent-state state)
  (setq imenu--index-alist nil)
  (if pi--agent-state
      (unless (spinner--active-p pi--spinner)
        (spinner-start pi--spinner))
    (spinner-stop pi--spinner)))

(defun pi--agent-state-tools ()
  (if (and (consp pi--agent-state)
           (eq (car pi--agent-state) 'tool))
      (cdr pi--agent-state)
    nil))

(defun pi--agent-state-add-tool (tool-name)
  (cons 'tool
        (cons tool-name (pi--agent-state-tools))))

(defun pi--agent-state-remove-tool (tool-name)
  (let ((tools (cl-remove tool-name (pi--agent-state-tools) :count 1 :test #'equal)))
    (if tools
        (cons 'tool tools)
      'thinking)))

(defun pi--handle-agent-state (event)
  (cl-case (intern (plist-get event :type))
    (agent_start (pi--update-agent-state 'thinking))
    (agent_end (pi--update-agent-state nil)
               (pi-clear-project-file-cache))
    (turn_start (pi--update-agent-state 'thinking))
    (turn_end (pi--update-agent-state nil))
    (tool_execution_start
     (pi--update-agent-state
      (pi--agent-state-add-tool (plist-get event :toolName))))
    (tool_execution_end
     (pi--update-agent-state
      (pi--agent-state-remove-tool (plist-get event :toolName))))
    (compaction_start (pi--update-agent-state 'compacting))
    (compaction_end (pi--update-agent-state nil))
    (auto_retry_start (pi--update-agent-state 'retrying))
    (auto_retry_end (pi--update-agent-state nil)))
  (when (string-suffix-p "_end" (plist-get event :type))
    (pi--autohide-sections))
  (pi--update-header-line))

(defun pi--autohide-sections ()
  (pi-section-autohide)
  (pi--recenter-chat))

(defun pi--cleanup-chat-buffer ()
  (let ((project-key (pi--project-key)))
    (remhash project-key pi--chats)
    (pi--hash-remove-if (lambda (k _v) (equal (car k) project-key)) pi--event-listeners)
    (ignore-errors
      (pi--kill-agent))))


;;; Commands

(defun pi--parse-slash-command (prompt)
  (when (string-match "\\`[ \t\n]*/\\([-a-zA-Z0-9]+\\)\\([ \t].*\\)?$" prompt)
    (let* ((name (match-string-no-properties 1 prompt))
           (raw (and (match-beginning 2)
                     (string-trim-left (match-string-no-properties 2 prompt))))
           (args (and (not (string-empty-p raw)) raw))
           (cell (assoc name pi-slash-commands #'string=)))
      (when cell
        (let ((cmd (cadr cell))
              (max-args (nth 2 cell)))
          (when (and args (not (eq max-args 1)))
            (error "Slash command \"/%s\" does not accept arguments" name))
          (cons cmd args))))))

(defun pi--parse-bang-command (prompt)
  (pi--parse-bang-command-with-regex prompt "\\`[ \t\n]*!\\([^!].*\\)$"))

(defun pi--parse-double-bang-command (prompt)
  (pi--parse-bang-command-with-regex prompt "\\`[ \t\n]*!!\\(.+\\)$"))

(defun pi--parse-bang-command-with-regex (prompt regex)
  (when (string-match regex prompt)
    (let ((result (match-string-no-properties 1 prompt)))
      (when (not (string-match-p "^[ \t]+$" result))
        result))))

(defun pi--clear-prompt (prompt)
  (let ((current (widget-value pi--prompt-widget)))
    (when (string= current prompt)
      (widget-value-set pi--prompt-widget "")))
  (unless (and (> (ring-length pi--prompt-history) 0)
               (equal prompt (ring-ref pi--prompt-history 0)))
    (ring-insert pi--prompt-history prompt))
  (setq pi--prompt-history-index 0)
  (pi-section-autohide))

(defun pi-send-prompt (&optional prompt streaming-behavior)
  "Send PROMPT with optional STREAMING-BEHAVIOR to the agent."
  (interactive "sPrompt: ")
  (if (or (null prompt) (string-empty-p prompt))
      (message "No prompt to send")
    (let ((slash (pi--parse-slash-command prompt))
          (bang (pi--parse-bang-command prompt))
          (double-bang (pi--parse-double-bang-command prompt)))
      (pi--with-chat-buffer
        (cond
         (slash
          (let ((cmd (car slash))
                (args (cdr slash)))
            (if (null args)
                (call-interactively cmd)
              (apply cmd (list args)))
            (when (not (eq cmd 'pi-quit-chat))
              (pi--clear-prompt prompt))))
         (double-bang
          (pi-bash double-bang t)
          (pi--clear-prompt prompt))
         (bang
          (pi-bash bang)
          (pi--clear-prompt prompt))
         (t
          (pi--send-command
           "prompt" (list :message prompt
                          :streamingBehavior (when-let (behavior (or streaming-behavior pi-prompt-streaming-behavior))
                                               (symbol-name behavior)))
           (pi--on-response-success-callback resp
             (pi--clear-prompt prompt)))))))))


(defun pi-send-prompt-alternate (&optional prompt)
  "Send PROMPT with the alternative streaming behavior.

If `pi-prompt-streaming-behavior' is `followUp', use `steer' and vice versa."
  (interactive)
  (let* ((alt-behavior (if (eq pi-prompt-streaming-behavior 'followUp)
                           'steer
                         'followUp))
         (prompt-text (or prompt (widget-value pi--prompt-widget))))
    (when (and prompt-text (not (string-empty-p prompt-text)))
      (pi-send-prompt prompt-text alt-behavior))))

(defun pi--prompt-append (text)
  (pi--with-chat-buffer
    (let ((current-value (widget-value pi--prompt-widget)))
      (pi--widget-save-excursion
        (if (string-empty-p current-value)
            (widget-value-set pi--prompt-widget text)
          (widget-value-set pi--prompt-widget
                            (concat current-value "\n" text))))
      (pi--widget-end-if-inside pi--prompt-widget))))

(defun pi--pop-to-chat ()
  (when pi-send-pop-to-chat
    (let ((buffer (pi--current-chat)))
      (when buffer (pop-to-buffer buffer)))))

(defun pi--project-relative-name ()
  (let ((root (pi--project-root)))
    (file-relative-name buffer-file-name root)))

(defun pi-send-region (start end)
  "Append the region delimited by START and END to the pi prompt input."
  (interactive "r")
  (let* ((string (buffer-substring-no-properties start end))
         (header (when buffer-file-name
                   (let* ((relative (pi--project-relative-name))
                          (line-start (line-number-at-pos start))
                          (line-end (line-number-at-pos end)))
                     (format "@%s#L%d-%d\n" relative line-start line-end))))
         (full-string (if header (concat header string) string)))
    (pi--prompt-append full-string)
    (deactivate-mark)
    (pi--pop-to-chat)))

(defun pi-send-filename ()
  "Append the current buffer's filename to the pi prompt input."
  (interactive)
  (when buffer-file-name
    (let* ((relative (pi--project-relative-name))
           (header (format "@%s" relative)))
      (pi--prompt-append header)
      (pi--pop-to-chat))))

(declare-function flycheck-overlay-errors-at "ext:flycheck")
(declare-function flycheck-error-line "ext:flycheck")
(declare-function flycheck-error-level "ext:flycheck")
(declare-function flycheck-error-message "ext:flycheck")
(declare-function flycheck-error-buffer "ext:flycheck")
(declare-function flycheck-error-format-position "ext:flycheck")
(defvar flycheck-current-errors)

(defun pi-send-flycheck-errors ()
  "Append flycheck errors at point to the pi prompt input."
  (interactive)
  (let ((errors (or (flycheck-overlay-errors-at (point))
                    flycheck-current-errors)))
    (when (and errors buffer-file-name)
      (let* ((relative (pi--project-relative-name))
             (error-lines
              (mapconcat
               (lambda (err)
                 (let* ((level (flycheck-error-level err))
                        (message (flycheck-error-message err))
                        (header (format "@%s#%s" relative (flycheck-error-format-position err)))
                        (line (pi--get-line-contents (flycheck-error-buffer err) (flycheck-error-line err))))
                   (format "%s\n%s\n%s: %s"
                           header
                           line
                           (capitalize (symbol-name level))
                           message)))
               errors "\n")))
        (pi--prompt-append error-lines)
        (pi--pop-to-chat)))))

(defun pi-abort ()
  "Abort the current agent operation."
  (interactive)
  (pi--with-chat-buffer
    (when (or pi--retry-in-progress pi--agent-state)
      (pi--send-command
       (cond
        (pi--retry-in-progress "abort_retry")
        ((eq pi--agent-state 'bash) "abort_bash")
        (pi--agent-state "abort"))
       '()
       (pi--on-response-success-callback resp
         (pi--widget-save-excursion
           (pi-section--create-section 'error pi-section--root-section
             (pi--insert-error "Aborted")))))))
  (keyboard-quit))

(defun pi--insert-stats-section (header plist fields)
  "Insert a stats section with HEADER (bold), extracting integers from PLIST.
FIELDS is a list of (LABEL . KEY) where KEY is a plist key."
  (insert (propertize (concat header "\n") 'face 'bold))
  (pcase-dolist (`(,label . ,key) fields)
    (insert (format " %s: %d\n" label (plist-get plist key)))))

(defun pi-session-stats ()
  "Show current session statistics."
  (interactive)
  (pi--with-chat-buffer
    (pi--send-command
     "get_session_stats" '()
     (pi--on-response-success-callback resp
       (let* ((data (plist-get resp :data))
              (tokens (plist-get data :tokens))
              (cost (plist-get data :cost)))
         (pi--widget-save-excursion
           (pi-section--create-section 'session pi-section--root-section
             (insert
              (propertize "Session Info\n" 'face 'bold))

             (insert " File: ")
             (pi--insert-file-link (plist-get data :sessionFile))
             (insert "\n")

             (insert
              (format " ID: %s\n\n"
                      (plist-get data :sessionId)))

             (pi--insert-stats-section
              "Messages"
              data
              '(("User" . :userMessages)
                ("Assistant" . :assistantMessages)
                ("Tool Calls" . :toolCalls)
                ("Tool Results" . :toolResults)
                ("Total" . :totalMessages)))

             (insert "\n")

             (pi--insert-stats-section
              "Tokens"
              tokens
              '(("Input" . :input)
                ("Output" . :output)
                ("Cache Read" . :cacheRead)
                ("Total" . :total)))

             (insert "\n")

             (insert
              (propertize "Cost\n" 'face 'bold))

             (insert
              (format " Total: %.4f\n" cost)))))))))

(defun pi-select-model ()
  "Select a different model for the session."
  (interactive)
  (pi--with-chat-buffer
    (pi--send-command
     "get_available_models" '()
     (pi--on-response-success-callback resp
       (let* ((models (plist-get (plist-get resp :data) :models))
              (items
               (mapcar
                (lambda (m)
                  (cons (format "(%s) %s" (plist-get m :provider) (plist-get m :id))
                        m))
                models)))
         (if (null items)
             (message "No models available.")
           (let* ((selected (completing-read "Select model: " items nil t))
                  (model (alist-get selected items nil nil #'equal))
                  (provider (plist-get model :provider))
                  (model-id (plist-get model :id)))
             (pi--send-command
              "set_model" (list :provider provider :modelId model-id)
              (pi--on-response-success-callback resp
                (pi--update-header-line)
                (pi--widget-save-excursion
                  (pi--insert-model-change provider model-id)))))))))))

(defvar pi--thinking-level-descriptions
  '((:off     . "No reasoning")
    (:minimal . "Very brief reasoning ~1k tokens")
    (:low     . "Light reasoning ~2k tokens")
    (:medium  . "Moderate reasoning ~8k tokens")
    (:high    . "Deep reasoning ~16k tokens")
    (:xhigh   . "Extra high reasoning ~32k tokens")
    (:max     . "Maximum reasoning")))

(defvar pi--prompt-modes
  '((:one-at-a-time . "One at a time")
    (:all . "All")))

(defun pi--get-supported-thinking-levels (model)
  (let ((thinking-level-map (plist-get model :thinkingLevelMap))
        (reasoning (plist-get model :reasoning))
        result)
    (when (and reasoning (not (eq reasoning 'json-false)))
      (dolist (level '(:minimal :low :medium :high :xhigh :max))
        (let ((mapped (plist-get thinking-level-map level)))
          (unless (eq mapped 'json-null)
            (if (member level '(:xhigh :max))
                (when mapped
                  (push level result))
              (push level result))))))
    (cons :off (nreverse result))))

(defun pi-set-thinking-level ()
  "Set the thinking level for the agent."
  (interactive)
  (pi--with-chat-buffer
    (pi--send-command
     "get_state" '()
     (pi--on-response-success-callback resp
       (let* ((data (plist-get resp :data))
              (model (plist-get data :model))
              (current-level (plist-get data :thinkingLevel))
              (supported-levels (pi--get-supported-thinking-levels model)))
         (if (null supported-levels)
             (message "No thinking levels available for this model.")
           (let* ((options
                   (mapcar
                    (lambda (level)
                      (let* ((name (pi--keyword-name level))
                             (desc (alist-get level pi--thinking-level-descriptions)))
                        (cons level
                              (if desc
                                  (format "%s (%s)" name desc)
                                name))))
                    supported-levels))
                  (choice (pi--read-option options current-level "Set thinking level")))
             (when choice
               (pi--send-command
                "set_thinking_level" (list :level (car choice))
                (pi--on-response-success-callback resp
                  (pi--update-header-line)
                  (pi--widget-save-excursion
                    (pi--insert-thinking-level-change (car choice)))))))))))))

(defun pi-cycle-model ()
  "Cycle to the next available model."
  (interactive)
  (pi--with-chat-buffer
    (pi--send-command
     "cycle_model" '()
     (pi--on-response-success-callback resp
       (let ((data (plist-get resp :data)))
         (if (null data)
             (message "No more models to cycle through.")
           (let ((model (plist-get data :model))
                 (thinking-level (plist-get data :thinkingLevel))
                 (is-scoped (plist-get data :isScoped)))
             (pi--update-header-line)
             (pi--widget-save-excursion
               (pi-section--create-section 'model pi-section--root-section
                 (insert (format "Cycled to model: (%s) %s · thinking level: %s%s"
                                 (plist-get model :provider)
                                 (plist-get model :id)
                                 (or thinking-level "?")
                                 (if (eq is-scoped t) " (scoped)" ""))))))))))))

(defun pi-set-steering-mode ()
  "Switch to steering mode for prompt delivery."
  (interactive)
  (pi--with-chat-buffer
    (pi--send-command
     "get_state" '()
     (pi--on-response-success-callback resp
       (let* ((data (plist-get resp :data))
              (current-mode (plist-get data :steeringMode))
              (choice (pi--read-option pi--prompt-modes current-mode "Set steering mode")))
         (when choice
           (pi--send-command
            "set_steering_mode" (list :mode (car choice))
            (pi--on-response-success-callback resp
              (pi--update-header-line)
              (pi--widget-save-excursion
                (pi-section--create-section 'info pi-section--root-section
                  (insert (format "Steering mode set to: %s" (cdr choice)))))))))))))

(defun pi-set-follow-up-mode ()
  "Switch to follow-up mode for prompt delivery."
  (interactive)
  (pi--with-chat-buffer
    (pi--send-command
     "get_state" '()
     (pi--on-response-success-callback resp
       (let* ((data (plist-get resp :data))
              (current-mode (plist-get data :followUpMode))
              (choice (pi--read-option pi--prompt-modes current-mode "Set follow-up mode")))
         (when choice
           (pi--send-command
            "set_follow_up_mode" (list :mode (car choice))
            (pi--on-response-success-callback resp
              (pi--update-header-line)
              (pi--widget-save-excursion
                (pi-section--create-section 'info pi-section--root-section
                  (insert (format "Follow-up mode set to: %s" (cdr choice)))))))))))))

(defun pi-cycle-thinking-level ()
  "Cycle to the next thinking level."
  (interactive)
  (pi--with-chat-buffer
    (pi--send-command
     "cycle_thinking_level" '()
     (pi--on-response-success-callback resp
       (let ((data (plist-get resp :data)))
         (if (null data)
             (message "No more thinking levels to cycle through.")
           (let ((level (plist-get data :level)))
             (pi--update-header-line)
             (pi--widget-save-excursion
               (pi-section--create-section 'thinking pi-section--root-section
                 (insert (format "Cycled thinking level to: %s" level)))))))))))

(cl-defstruct pi-session-choice
  id message timestamp cwd path parent-id name)

(defun pi--read-session-choice (filename)
  (with-temp-buffer
    (insert-file-contents filename nil 0 10000)
    (goto-char (point-min))
    (let ((id nil)
          (timestamp nil)
          (cwd nil)
          (parent-id nil)
          (first-text nil)
          (name nil)
          (lines-read 0))
      (while (and (< lines-read 20) (not (eobp)))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (unless (string-empty-p line)
            (condition-case nil
                (let ((json (json-parse-string line :object-type 'plist)))
                  (pcase (intern (plist-get json :type))
                    ('session
                     (setq id (plist-get json :id)
                           timestamp (plist-get json :timestamp)
                           cwd (plist-get json :cwd)
                           parent-id (when-let ((ps (plist-get json :parentSession)))
                                       (file-name-sans-extension
                                        (file-name-nondirectory ps))))
                     (when parent-id
                       (setq parent-id (car (last (split-string parent-id "_"))))))
                    ('session_info
                     (setq name (plist-get json :name)))
                    ('message
                     (unless first-text
                       (let ((first-msg-text (pi--content-text (plist-get json :message))))
                         (when (and (not (string-empty-p first-msg-text))
                                    (null first-text))
                           (setq first-text (pi--section-header first-msg-text))))))))
              (error nil))))
        (forward-line 1)
        (cl-incf lines-read))
      (make-pi-session-choice :id id
                              :path filename
                              :timestamp (when timestamp
                                           (condition-case nil
                                               (parse-iso8601-time-string timestamp)
                                             (error nil)))
                              :cwd cwd
                              :parent-id parent-id
                              :message first-text
                              :name name))))

(defun pi-resume ()
  "Resume a previous session."
  (interactive)
  (pi--with-chat-buffer
    (pi--send-command
     "get_state" '()
     (lambda (resp)
       (when (pi--response-success-p resp)
         (let* ((data (plist-get resp :data))
                (session-file (plist-get data :sessionFile))
                (session-dir (file-name-directory session-file))
                (files (when session-dir
                         (seq-take
                          (sort (directory-files session-dir t "\\.jsonl$")
                                #'string>)
                          pi-resume-max-sessions)))
                (sessions (mapcar #'pi--read-session-choice files)))
           (if (null sessions)
               (message "No session files found in %s" session-dir)
             (let* ((candidates
                     (mapcar
                      (lambda (s)
                        (let* ((ts (pi-session-choice-timestamp s))
                               (formatted-time (if ts
                                                   (format-time-string "%F %R" ts)
                                                 ""))
                               (short-id (substring (pi-session-choice-id s) -8))
                               (short-parent (when-let ((pid (pi-session-choice-parent-id s)))
                                               (substring pid -8))))
                          (cons (format "%s  %s  %s%s%s" short-id formatted-time
                                        (if (pi-session-choice-name s)
                                            (format "[%s] " (pi-session-choice-name s))
                                          "")
                                        (pi-session-choice-message s)
                                        (if short-parent (format " (parent: %s)" short-parent) ""))
                                s)))
                      sessions))
                    (selected (pi--completing-read "Resume session: " candidates))
                    (choice (alist-get selected candidates nil nil #'equal))
                    (session-path (pi-session-choice-path choice)))
               (pi--switch-session session-path "Resumed session")))))))))

(defun pi--clear-sections ()
  (dolist (child (copy-sequence (pi-section-children pi-section--root-section)))
    (pi-section--delete-section child))
  (setq pi--text-section nil
        pi--thinking-section nil)
  (clrhash pi--tool-calls))

(defun pi--clear-session-widgets ()
  (when pi--prompt-widget-lines
    (clrhash pi--prompt-widget-lines))
  (when pi--status-widget-texts
    (clrhash pi--status-widget-texts))
  (pi--update-prompt-widgets)
  (pi--update-status-widget))

(defun pi-refresh-session (&optional callback)
  "Refresh the current session state.
CALLBACK is called after a successful refresh."
  (interactive)
  (pi--with-chat-buffer
    (pi--send-command
     "get_entries" '()
     (pi--on-response-success-callback resp
       (let ((entries (plist-get (plist-get resp :data) :entries)))
         (pi--widget-save-excursion
           (pi--clear-sections)
           (dolist (entry entries)
             (pcase (plist-get entry :type)
               ("message"
                (pi--insert-message (plist-get entry :message)))
               ("compaction"
                (pi--insert-compaction (plist-get entry :summary)
                                       (plist-get entry :tokensBefore)))
               ("model_change"
                (pi--insert-model-change (plist-get entry :provider)
                                         (plist-get entry :modelId)))
               ("thinking_level_change"
                (pi--insert-thinking-level-change (plist-get entry :thinkingLevel)))
               ("custom_message"
                (pi--insert-custom-message entry))
               ("session_info"
                (when-let ((name (plist-get entry :name)))
                  (pi--insert-session-info name)))
               (_ nil)))))
       (pi-section-autohide)
       (when callback
         (funcall callback))))))

(defun pi-clone ()
  "Clone the current session."
  (interactive)
  (pi--with-chat-buffer
    (pi--widget-save-excursion
      (pi--clear-session-widgets))
    (pi--send-command
     "clone" '()
     (pi--on-response-success-callback resp
       (pi--unless-cancelled resp "Clone"
         (pi-refresh-session
          (lambda ()
            (pi--update-header-line)
            (pi--notify "Cloned to new session"))))))))

(defun pi-new-session ()
  "Start a new session."
  (interactive)
  (pi--with-chat-buffer
    (pi--widget-save-excursion
      (pi--clear-sections)
      (pi--clear-session-widgets))
    (pi--send-command
     "new_session" '()
     (pi--on-response-success-callback resp
       (pi--update-header-line)
       (pi--unless-cancelled resp "New session"
         (pi--notify "New session started."))))))

(defun pi-set-session-name (name)
  "Set the session name to NAME."
  (interactive "sSession name: ")
  (let ((trimmed (string-trim name)))
    (if (string-empty-p trimmed)
        (message "Session name cannot be empty")
      (pi--with-chat-buffer
        (pi--send-command
         "set_session_name" (list :name trimmed)
         (pi--on-response-success-callback resp
           (rename-buffer (pi--chat-buffer-name trimmed) t)
           (pi--update-header-line)
           (pi--widget-save-excursion
             (pi--insert-session-info trimmed))))))))

(defun pi-export (&optional output-path)
  "Export the current session to OUTPUT-PATH."
  (interactive
   (list (when current-prefix-arg
           (expand-file-name
            (read-file-name "Export to file: ")))))
  (pi--with-chat-buffer
    (let ((args (if (and output-path (not (string-empty-p output-path)))
                    (list :outputPath (expand-file-name output-path))
                  '())))
      (pi--send-command
       "export_html" args
       (pi--on-response-success-callback resp
         (pi--update-header-line)
         (let ((path (plist-get (plist-get resp :data) :path)))
           (pi--widget-save-excursion
             (pi-section--create-section 'info pi-section--root-section
               (insert "Session exported to: ")
               (pi--insert-file-link path)))))))))

(defun pi-copy ()
  "Copy the last assistant message to the clipboard."
  (interactive)
  (pi--with-chat-buffer
    (pi--send-command
     "get_last_assistant_text" '()
     (pi--on-response-success-callback resp
       (let ((text (plist-get (plist-get resp :data) :text)))
         (if text
             (progn
               (kill-new text)
               (message "Copied last assistant message to clipboard."))
           (message "No assistant message available to copy.")))))))

(defun pi-fork ()
  "Fork the current session from this point."
  (interactive)
  (pi--with-chat-buffer
    (pi--send-command
     "get_fork_messages" '()
     (pi--on-response-success-callback resp
       (let* ((messages (plist-get (plist-get resp :data) :messages))
              (items
               (mapcar
                (lambda (m)
                  (cons (truncate-string-to-width (plist-get m :text) 80 nil nil t) m))
                messages)))
         (if (null items)
             (message "No fork points available.")
           (let* ((selected (pi--completing-read "Fork at message: " (reverse items)))
                  (message (alist-get selected items nil nil #'equal))
                  (entry-id (plist-get message :entryId))
                  (message-text (plist-get message :text)))
             (pi--widget-save-excursion
               (pi--clear-session-widgets))
             (pi--send-command
              "fork" (list :entryId entry-id)
              (pi--on-response-success-callback resp
                (pi--unless-cancelled resp "Fork"
                  (pi-refresh-session
                   (lambda ()
                     (pi--widget-save-excursion
                       (widget-value-set pi--prompt-widget (or message-text "")))
                     (pi-focus-prompt)
                     (pi--notify "Forked to new session")))
                  (pi--update-header-line)))))))))))

(defun pi-compact (&optional custom-instructions)
  "Compact the current session to reduce context usage.

With prefix argument, prompt for CUSTOM-INSTRUCTIONS to guide the
summarization."
  (interactive
   (list (when current-prefix-arg
           (read-string "Custom instructions for compaction: "))))
  (pi--with-chat-buffer
    (let ((args (if custom-instructions
                    (list :customInstructions custom-instructions)
                  '())))
      (pi--send-command
       "compact"
       args
       (pi--on-response-success-callback resp
         (pi--update-header-line))))))

(defun pi-set-auto-compaction (enabled)
  "Toggle auto compaction with ENABLED."
  (interactive (list (y-or-n-p "Enable auto compaction? ")))
  (pi--with-chat-buffer
    (pi--send-command
     "set_auto_compaction" (list :enabled (if enabled t 'json-false))
     (pi--on-response-success-callback resp
       (pi--update-header-line)
       (pi--widget-save-excursion
         (pi-section--create-section 'info pi-section--root-section
           (insert (format "Compaction set to: %s" (if enabled "auto" "manual")))))))))

(defun pi-set-auto-retry (enabled)
  "Toggle auto retry with ENABLED."
  (interactive (list (y-or-n-p "Enable auto retry? ")))
  (pi--with-chat-buffer
    (pi--send-command
     "set_auto_retry" (list :enabled (if enabled t 'json-false))
     (pi--on-response-success-callback resp
       (pi--update-header-line)
       (pi--widget-save-excursion
         (pi-section--create-section 'info pi-section--root-section
           (insert (format "Auto retry set to: %s" (if enabled "enabled" "disabled")))))))))

(defun pi-bash (command &optional exclude-from-context)
  "Run a bash COMMAND with optional EXCLUDE-FROM-CONTEXT flag."
  (interactive "sBash command: ")
  (unless (string-empty-p (string-trim command))
    (pi--with-chat-buffer
      (pi--update-agent-state 'bash)
      (let ((args (list :command command))
            (call-section (pi-section--new-section 'tool-call pi-section--root-section :padding "\n")))
        (when exclude-from-context
          (setq args (nconc args (list :excludeFromContext t))))
        (pi--insert-tool-call call-section "bash" args)
        (pi--send-command
         "bash" args
         (lambda (resp)
           (pi--on-response-success resp
             (pi--update-header-line)
             (let* ((data (plist-get resp :data))
                    (output (plist-get data :output)))
               (pi--widget-save-excursion
                 (pi-section--create-section 'tool-result call-section
                   (pi--insert-tool-result "bash" output nil data)))))
           (pi--update-agent-state nil)))))))

;;; Chat mode

(defun pi--fetch-commands ()
  (pi--send-command
   "get_commands" '()
   (pi--on-response-success-callback resp
     (setq pi--commands
           (mapcar (lambda (c) (cons (plist-get c :name) c))
                   (plist-get (plist-get resp :data) :commands))))))


(defun pi--visit-file (result &optional other-window)
  (let ((file (plist-get result :file))
        (line (plist-get result :line))
        (column (plist-get result :column))
        (find-file-func (if other-window #'find-file-other-window #'find-file)))
    (when file
      (funcall find-file-func file)
      (when line
        (goto-char (point-min))
        (forward-line (1- line)))
      (when column
        (move-to-column (max 0 (1- column)))))))

(defun pi--visit-file-at-point (other-window)
  (when-let (file (pi--file-at-point))
    (pi--visit-file (list :file file) other-window)))

(defun pi-visit-item (&optional other-window)
  "Visit current item.
With a prefix argument OTHER-WINDOW, visit in other window."
  (interactive (list current-prefix-arg))
  (pi-section--section-case
      ((tool-result)
       (if-let* ((info (pi-section-info (pi-section--current-section)))
                 (tool-name (pi-section-tool-result-info-tool-name info))
                 (visitor (alist-get tool-name pi-visit-tool-result-functions nil nil #'equal)))
           (let* ((details (pi-section-tool-result-info-details info))
                  (args (pi-section-tool-result-info-args info)))
             (pi--visit-file (funcall visitor details args) other-window))
         (pi--visit-file-at-point other-window)))
    ((tool-call)
     (if-let* ((info (pi-section-info (pi-section--current-section)))
               (tool-name (pi-section-tool-call-info-tool-name info))
               (visitor (alist-get tool-name pi-visit-tool-call-functions nil nil #'equal)))
         (let ((args (pi-section-tool-call-info-args info)))
           (pi--visit-file (funcall visitor args) other-window))
       (pi--visit-file-at-point other-window)))
    (t
     (pi--visit-file-at-point other-window))))

(defun pi-visit-item-other-window ()
  "Visit current item in other window."
  (interactive)
  (pi-visit-item t))

(defun pi-goto-next-user-message ()
  "Go to the next user message."
  (interactive)
  (pi--with-chat-buffer
    (pi-section--goto-next-section-of-type 'user)))

(defun pi-goto-previous-user-message ()
  "Go to the previous user message."
  (interactive)
  (pi--with-chat-buffer
    (pi-section--goto-previous-section-of-type 'user)))

(defvar-keymap pi-chat-mode-map
  :doc "Keymap for `pi-chat-mode'."
  :parent special-mode-map
  "<remap> <keyboard-quit>" #'pi-abort
  "<left-fringe> <mouse-1>" #'pi-mouse-toggle-section
  "<left-fringe> <mouse-2>" #'pi-mouse-toggle-section
  "RET" #'pi-visit-item
  "M-RET" #'pi-visit-item-other-window
  "TAB" #'pi-toggle-section
  "C-i" #'pi-toggle-section
  "n" #'pi-goto-next-section
  "M-n" #'pi-goto-next-section
  "N" #'pi-goto-next-user-message
  "p" #'pi-goto-previous-section
  "M-p" #'pi-goto-previous-section
  "P" #'pi-goto-previous-user-message
  "M-g l" #'pi-goto-last-section
  "l" #'pi-goto-last-section
  "i" #'pi-focus-prompt
  "q" #'pi-quit-chat)

(defvar pi-chat-widget-field-keymap
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<remap> <keyboard-quit>" #'pi-abort)
    (keymap-set map "M-p" #'pi-previous-prompt)
    (keymap-set map "M-n" #'pi-next-prompt)
    (keymap-set map "C-r" #'pi-search-prompt)
    (keymap-set map "M-RET" #'pi-send-prompt-alternate)
    (keymap-set map "M-g l" #'pi-goto-last-section)
    (keymap-set map "C-k" #'widget-kill-line)
    (keymap-set map "C-e" #'widget-end-of-line)
    (keymap-set map "C-m" #'widget-field-activate)
    map))

(defun pi--imenu-create-index ()
  "Build an imenu index from the top-level chat sections."
  (let ((groups (make-hash-table :test 'equal))
        result)
    (dolist (section (pi-section-children pi-section--root-section))
      (when-let ((pair (pcase (pi-section-type section)
                         ('user
                          (when-let ((info (pi-section-info section)))
                            (cons "user" (pi-section-user-info-header info))))
                         ((or 'assistant 'thinking)
                          (when-let ((info (pi-section-info section)))
                            (cons "assistant" (pi-section-assistant-info-header info))))
                         ('tool-call
                          (when-let ((info (pi-section-info section)))
                            (cons (pi-section-tool-call-info-tool-name info)
                                  (pi-section-tool-call-info-header info)))))))
        (push (cons (cdr pair) (marker-position (pi-section-beginning section)))
              (gethash (car pair) groups))))
    (maphash (lambda (name entries)
               (push (cons name (nreverse entries)) result))
             groups)
    result))

(define-derived-mode pi-chat-mode nil "pi-chat"
  "Major mode for pi chat.

\\{pi-chat-mode-map}"
  (buffer-disable-undo)
  (setq header-line-format '(:eval (pi--format-header)))
  (setq pi--tool-calls (make-hash-table :test 'equal))
  (pi-section--create-root-section)
  (setq imenu-create-index-function #'pi--imenu-create-index)
  (setq pi--prompt-history (make-ring pi-prompt-history-max-size))
  (setq-local completion-at-point-functions
              (append (list #'pi--completion-at-point-slash
                            #'pi--completion-at-point-file)
                      completion-at-point-functions))
  (setq pi--spinner (spinner-create 'progress-bar))
  (setq pi--prompt-before-widget (widget-create 'pi-item :face 'pi-widget-face pi--empty-widget-text))
  (setq pi--prompt-widget
        (widget-create 'editable-field
                       :keymap pi-chat-widget-field-keymap
                       :help-echo ""
                       :format "%[user>%] %v"
                       :button-face 'pi-chat-user-role-face
                       :action (lambda (widget &optional _event)
                                 (pi-send-prompt (widget-value widget)))))
  (setq pi--prompt-after-widget (widget-create 'pi-item :face 'pi-widget-face pi--empty-widget-text))
  (setq pi--prompt-widget-lines (make-hash-table :test 'equal))
  (setq pi--status-widget (widget-create 'pi-item :face 'pi-status-face pi--empty-widget-text))
  (setq pi--status-widget-texts (make-hash-table :test 'equal))
  (widget-setup)
  (pi-focus-prompt)
  (add-hook 'kill-buffer-hook #'pi--cleanup-chat-buffer nil t)
  (pi--register-agent-cleanup)
  (pi--register-event-listeners)
  (setq-local mode-line-misc-info
              (append (list '(:eval (pi--format-mode-line)))
                      mode-line-misc-info))
  (pi--update-header-line)
  (pi--fetch-commands))

;;;###autoload
(defun pi-chat (&optional name)
  "Start a chat window with optional session NAME."
  (interactive
   (list (when current-prefix-arg
           (read-string "Session name: "))))
  (let* ((root (pi--project-root))
         (key (if name
                  (md5 (concat (pi--project-root) name))
                (pi--project-key)))
         (pi--project-key key))
    (unless (pi--current-agent)
      (pi--start-agent key))
    (let ((chat-buffer (or (pi--current-chat)
                           (progn
                             (let ((buffer (generate-new-buffer (pi--chat-buffer-name name))))
                               (with-current-buffer buffer
                                 (setq-local pi--project-key key)
                                 (puthash key buffer pi--chats)
                                 (pi-chat-mode)
                                 (setq-local default-directory root)
                                 (when name
                                   (pi-set-session-name name)))
                               buffer)))))
      (pop-to-buffer chat-buffer))))

(defun pi-toggle-chat ()
  "Toggle chat window."
  (interactive)
  (if-let* ((chat-buffer (pi--current-chat))
            (chat-window (get-buffer-window chat-buffer t)))
      (with-selected-window chat-window
        (if (one-window-p)
            (bury-buffer)
          (delete-window chat-window)))
    (pi-chat)))

(defun pi-quit-chat ()
  "Quit the current chat window."
  (interactive)
  (when-let (buffer (pi--current-chat))
    (kill-buffer buffer))
  (pi--kill-agent))

(defun pi--switch-session (session-file message &optional cb)
  "Switch to an existing session file and refresh.
SESSION-FILE is the path to the session file to switch to.
MESSAGE is shown as a notification when complete.
If non-nil, call CB after the session refresh finishes."
  (pi--widget-save-excursion
    (pi--clear-session-widgets))
  (pi--send-command
   "switch_session" (list :sessionPath session-file)
   (pi--on-response-success-callback resp
     (pi--update-header-line)
     (pi--unless-cancelled resp "Session switch"
       (pi-refresh-session (lambda ()
                             (pi--notify message)
                             (when cb
                               (funcall cb))))))))

(defun pi-reload ()
  "Reload agent configuration by restarting the agent process."
  (interactive)
  (pi--with-chat-buffer
    (pi--send-command
     "get_state" '()
     (pi--on-response-success-callback resp
       (let* ((data (plist-get resp :data))
              (session-file (plist-get data :sessionFile))
              (project-key (pi--project-key))
              (chat-buffer (current-buffer)))
         (run-at-time
          0 nil
          (lambda ()
            (when (buffer-live-p chat-buffer)
              (with-current-buffer chat-buffer
                (pi--kill-agent pi--cleanup-callback-fn)
                (pi--widget-save-excursion
                  (pi--clear-sections)
                  (pi--clear-session-widgets))
                (pi--start-agent project-key)
                (pi--register-agent-cleanup)
                (pi--switch-session
                 session-file
                 "Reloaded extensions, skills and prompts."
                 (lambda ()
                   (pi--fetch-commands))))))))))))

(defun pi-restart-chat ()
  "Exit the current chat and restart."
  (interactive)
  (let ((root default-directory))
    (pi-quit-chat)
    (let ((default-directory root))
      (pi-chat))))

(provide 'pi)

;;; pi.el ends here
