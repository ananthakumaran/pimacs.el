;;; pi.el --- Emacs UI for Pi -*- lexical-binding: t -*-

;; Copyright (C) 2026 Anantha Kumaran.

;; Author: Anantha kumaran <ananthakumaran@gmail.com>
;; URL: http://github.com/ananthakumaran/pi.el
;; Version: 0.1
;; Keywords: pi agent
;; Package-Requires: ((emacs "28.1") (compat  "31.0") (markdown-mode "2.8") (timeout "2.1.7"))

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

;;; Code:

(require 'project)
(require 'widget)
(require 'wid-edit)
(require 'ring)
(require 'markdown-mode)
(require 'cl-lib)
(require 'pi-section)
(require 'subr-x)
(require 'parse-time)
(require 'timeout)

(defgroup pi nil
  "Emacs UI for Pi."
  :prefix "pi-"
  :group 'tools)

(defface pi-chat-role-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face used for chat message role labels."
  :group 'pi)

(defface pi-error-face
  '((t :inherit error))
  "Face used for Pi widget error messages."
  :group 'pi)

(defface pi-thinking-face
  '((t :inherit shadow :italic t))
  "Face used for assistant thinking content."
  :group 'pi)

(defface pi-tool-name-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face used for tool names in tool execution events."
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

(defvar pi-slash-commands
  '(("model" pi-select-model 0)
    ("new" pi-new-session 0)
    ("resume" pi-resume 0)
    ("compact" pi-compact 1)
    ("session" pi-session-stats 0)
    ("name" pi-set-session-name 1)
    ("thinking-level" pi-set-thinking-level 0)
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
arguments the command accepts.")

(defvar pi-log-rpc-file "/tmp/pi.el.log"
  "File to write RPC JSON log entries to.")

(defvar-local pi-project-file-cache nil)

(defun pi-maybe-log-rpc (json)
  (when pi-log-rpc
    (write-region (concat json "\n") nil pi-log-rpc-file t 'inhibit-message)))

;;; Utilities

(defun pi-json-read-object ()
  (json-parse-buffer :object-type 'plist :null-object 'json-null :false-object 'json-false :array-type 'list))

(defun pi-json-encode (obj)
  "Encode OBJ into a JSON string. JSON arrays must be represented with vectors."
  (json-serialize obj :null-object 'json-null :false-object 'json-false))

(defun pi-format-number-short (n)
  "Format number N into a short human-readable string with K/M/B suffixes."
  (cond
   ((not (numberp n)) "?")
   ((>= n 1000000000)
    (format "%.1fB" (/ n 1000000000.0)))
   ((>= n 1000000)
    (format "%.1fM" (/ n 1000000.0)))
   ((>= n 1000)
    (format "%.1fk" (/ n 1000.0)))
   (t
    (number-to-string n))))

(defmacro pi-def-permanent-buffer-local (name &optional init-value)
  "Declare NAME as buffer local variable."
  `(progn
     (defvar ,name ,init-value)
     (make-variable-buffer-local ',name)
     (put ',name 'permanent-local t)))

(defun pi-join (list)
  (mapconcat 'identity list ""))

(defun pi-insert-error (text)
  "Insert TEXT with `pi-error-face'."
  (insert (propertize text 'face 'pi-error-face)))

(defun pi-insert-file-link (path &optional suffix)
  (widget-create 'file-link
                 :button-prefix ""
                 :button-suffix (or suffix "")
                 path))

(defun pi-keyword-name (keyword)
  "Return the name of KEYWORD as a string without the leading colon."
  (substring (symbol-name keyword) 1))

(defun pi-seconds-elapsed-since (time)
  (time-to-seconds (time-subtract (current-time) time)))

(defun pi-hash-remove-if (pred table)
  "Remove entries from TABLE for which PRED returns non-nil.

PRED is called with KEY VALUE."
  (maphash
   (lambda (k v)
     (when (funcall pred k v)
       (remhash k table)))
   table))

(defun pi-response-success-p (response)
  (and response
       (plist-get response :success)
       (not (eq (plist-get response :success) 'json-false))))

(defmacro pi-on-response-success (response &rest body)
  (declare (indent 1))
  (let ((resp-sym (gensym "resp")))
    `(let ((,resp-sym ,response))
       (if (pi-response-success-p ,resp-sym)
           (progn ,@body)
         (when-let (err (plist-get ,resp-sym :error))
           (pi-widget-save-excursion
             (pi-create-section "error" 'error pi-root-section
               (pi-insert-error (format "%s\n\n" err))))
           nil)))))

(defmacro pi-on-response-success-callback (response &rest body)
  (declare (indent 1))
  `(lambda (,response)
     (pi-on-response-success ,response
       ,@body)))

(defmacro pi-unless-cancelled (resp operation &rest body)
  (declare (indent 2))
  `(let ((cancelled (plist-get (plist-get ,resp :data) :cancelled)))
     (if (eq cancelled t)
         (pi-widget-save-excursion
           (pi-create-section "error" 'error pi-root-section
             (pi-insert-error (format "%s cancelled.\n\n" ,operation))))
       ,@body)))

(defun pi-render-markdown (text)
  (with-temp-buffer
    (insert text)
    (delay-mode-hooks
      (markdown-view-mode))
    (font-lock-ensure)
    (buffer-string)))

(defun pi-render-content (filename content)
  (with-temp-buffer
    ;; Use a fake temp filename preserving extension only.
    (setq-local
     buffer-file-name
     (expand-file-name
      (concat "pi-fontify"
              (when-let ((ext (file-name-extension filename t)))
                ext))
      temporary-file-directory))

    (insert content)

    (let ((delay-mode-hooks t)
          (enable-local-variables nil)
          (enable-local-eval nil))
      (set-auto-mode)
      (font-lock-ensure))

    ;; Prevent save prompts
    (set-buffer-modified-p nil)

    ;; Preserve text properties
    (buffer-string)))

(defun pi-render-diff (diff)
  (with-temp-buffer
    (insert diff)
    (let ((delay-mode-hooks t))
      (diff-mode)
      (font-lock-ensure))
    (set-buffer-modified-p nil)
    (buffer-string)))

;;; State management


(defvar pi-agents (make-hash-table :test 'equal))
(defvar pi-chats (make-hash-table :test 'equal))
(defvar pi-response-callbacks (make-hash-table :test 'equal))

(pi-def-permanent-buffer-local pi-project-root nil)
(pi-def-permanent-buffer-local pi-prompt-widget nil)
(pi-def-permanent-buffer-local pi-text-section nil)
(pi-def-permanent-buffer-local pi-thinking-section nil)
(pi-def-permanent-buffer-local pi-header-line-state nil)
(pi-def-permanent-buffer-local pi-current-tool-read-filename nil)
(pi-def-permanent-buffer-local pi-current-tool-section nil)
(pi-def-permanent-buffer-local pi-agent-state nil)
(pi-def-permanent-buffer-local pi-bash-in-progress nil)

(defvar pi-event-listeners (make-hash-table :test 'equal))

(defvar pi-request-counter 0)

;;; History

(pi-def-permanent-buffer-local pi-prompt-history nil)
(pi-def-permanent-buffer-local pi-prompt-history-index 0)

(defun pi-previous-prompt ()
  "Navigate to the previous prompt in history."
  (interactive)
  (let ((len (ring-length pi-prompt-history)))
    (when (< pi-prompt-history-index len)
      (cl-incf pi-prompt-history-index)
      (widget-value-set pi-prompt-widget
                        (ring-ref pi-prompt-history (- pi-prompt-history-index 1))))))

(defun pi-next-prompt ()
  "Navigate to the next prompt in history."
  (interactive)
  (cond
   ((> pi-prompt-history-index 1)
    (cl-decf pi-prompt-history-index)
    (widget-value-set pi-prompt-widget
                      (ring-ref pi-prompt-history (1- pi-prompt-history-index))))
   ((= pi-prompt-history-index 1)
    (setq pi-prompt-history-index 0)
    (widget-value-set pi-prompt-widget ""))))

(defun pi-search-prompt ()
  "Search prompt history and select an entry."
  (interactive)
  (let ((items (ring-elements pi-prompt-history)))
    (if (null items)
        (message "No prompt history")
      (let ((selected (completing-read "Search prompt: " items nil t)))
        (widget-value-set pi-prompt-widget selected)
        (setq pi-prompt-history-index 0)
        (pi-focus-prompt)))))

;;; Core

(defun pi-project-root ()
  (or
   pi-project-root
   (let ((project (project-current))
         (path default-directory))
     (if project
         (setq path (project-root project))
       (message (pi-join (list "Couldn't find project root folder. Using '" default-directory "' as project root."))))
     (let ((full-path (expand-file-name path)))
       (setq pi-project-root full-path)
       full-path))))

(defun pi-project-name ()
  (file-name-nondirectory (directory-file-name (pi-project-root))))

(defun pi-agent-buffer-name ()
  (format "*pi-agent:%s*" (pi-project-name)))

(defun pi-chat-buffer-name ()
  (format "*pi-chat:%s*" (pi-project-name)))

(defun pi-project-key ()
  "Unique key for the current project, used for internal hash tables."
  (md5 (pi-project-root)))

(defmacro pi-widget-save-excursion (&rest body)
  "Insert content before PROMPT-WIDGET and restore focus afterward."
  (declare (indent 0) (debug t))
  `(let* ((inhibit-read-only t)
          (window (get-buffer-window (current-buffer) t))
          (follow-p
           (and window
                (>= (window-point window)
                    (widget-get pi-prompt-widget :from)))))
     (save-excursion
       (goto-char (widget-get pi-prompt-widget :from))
       ,@body)
     (when follow-p
       (with-selected-window window
         (recenter (- -1 scroll-margin))))))

(defmacro pi-with-chat-buffer (&rest body)
  "Execute the body in the current chat buffer"
  (declare (indent 0) (debug t))
  `(let ((buffer (pi-current-chat)))
     (if buffer
         (with-current-buffer buffer
           (progn ,@body))
       (error "Chat doesn't exist, start a new chat using M-x pi-chat"))))

(defun pi-current-agent ()
  (gethash (pi-project-key) pi-agents))

(defun pi-current-chat ()
  (gethash (pi-project-key) pi-chats))

(defun pi-next-request-id ()
  (number-to-string (cl-incf pi-request-counter)))

;;; Agent

(defun pi-dispatch-response (response)
  (let* ((request-id (plist-get response :id))
         (callback (gethash request-id pi-response-callbacks)))
    (when callback
      (let ((buffer (car callback)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (apply (cdr callback) (list response)))))
      (remhash request-id pi-response-callbacks))))

(defun pi-dispatch-event (event)
  (let ((key (pi-project-key)))
    (when-let (all-listener (gethash (cons key t) pi-event-listeners))
      (with-current-buffer (car all-listener)
        (apply (cdr all-listener) (list event))))
    (when-let (listener (gethash (cons key (plist-get event :type)) pi-event-listeners))
      (with-current-buffer (car listener)
        (apply (cdr listener) (list event))))))

(defun pi-set-event-listener (name listener)
  "Set `name' to t to receive all events"
  (puthash (cons (pi-project-key) name) (cons (current-buffer) listener) pi-event-listeners))

(defun pi-dispatch (response)
  (cl-case (intern (plist-get response :type))
    ((response) (pi-dispatch-response response))
    (t (pi-dispatch-event response))))

(defun pi-send-command (type args &optional callback)
  (unless (pi-current-agent)
    (error "Agent does not exist. Run M-x pi-restart-chat to start it again"))

  (let* ((request-id (pi-next-request-id))
         (command (append (list :id request-id :type type) args))
         (encoded-command (pi-json-encode command))
         (payload (concat encoded-command "\n")))
    (pi-maybe-log-rpc encoded-command)
    (process-send-string (pi-current-agent) payload)
    (when callback
      (puthash request-id (cons (current-buffer) callback) pi-response-callbacks))))

(defun pi-send-command-sync (name args)
  (let* ((start-time (current-time))
         (response nil))
    (pi-send-command name args (lambda (resp) (setq response resp)))
    (while (not response)
      (accept-process-output nil 0.01)
      (when (> (pi-seconds-elapsed-since start-time) pi-sync-request-timeout)
        (error "Sync request timed out %s" name)))
    response))

(defun pi-net-sentinel (process message)
  (let ((project-name (process-get process 'project-name)))
    (message "(%s) pi exits: %s." project-name (string-trim message))
    (ignore-errors
      (kill-buffer (process-buffer process)))
    (pi-cleanup-agent process)))

(defun pi-net-filter (process data)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert (format "%s" data)))
  (pi-decode-response process))

(defun pi-enough-response-p ()
  (goto-char (point-min))
  (save-excursion
    (when (search-forward "{")
      (search-forward "\n" nil t))))

(defun pi-decode-response (process)
  (with-current-buffer (process-buffer process)
    (when (pi-enough-response-p)
      (search-forward "{")
      (backward-char 1)
      (let* ((raw-start (point))
             (response (pi-json-read-object)))
        (when pi-log-rpc
          (pi-maybe-log-rpc (buffer-substring-no-properties raw-start (point))))
        (delete-region (point-min) (point))
        (when response
          (pi-dispatch response)))
      (when (>= (buffer-size) 16)
        (pi-decode-response process)))))

(defun pi-agent-version ()
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

(defun pi-start-agent ()
  (when (pi-current-agent)
    (error "Agent already exist"))

  (let* ((default-directory (pi-project-root))
         (version (pi-agent-version)))
    (message "(%s) Starting pi version %s..." (pi-project-name) version)
    (let* ((process-environment (append pi-process-environment process-environment))
           (buf (generate-new-buffer (pi-agent-buffer-name)))
           ;; Use a pipe to communicate with the subprocess. This fixes a hang
           ;; when a >1k message is sent on macOS.
           (process-connection-type nil)
           (process-arguments (append pi-flags '("--mode" "rpc")))
           (process
            (apply #'start-file-process "pi" buf pi-executable process-arguments)))
      (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
      (set-process-filter process #'pi-net-filter)
      (set-process-sentinel process #'pi-net-sentinel)
      (set-process-query-on-exit-flag process nil)
      (with-current-buffer (process-buffer process)
        (buffer-disable-undo))
      (process-put process 'project-key (pi-project-key))
      (process-put process 'project-root default-directory)
      (process-put process 'project-name (pi-project-name))
      (puthash (pi-project-key) process pi-agents)
      (message "(%s) pi agent started successfully." (pi-project-name)))))


(defun pi-cleanup-agent (process)
  (let ((project-key (process-get process 'project-key)))
    (when project-key
      (remhash project-key pi-agents)
      (when-let (buffer (gethash project-key pi-chats))
        (kill-buffer buffer)))))

;;; Utility commands

(defun pi-kill-agent ()
  "Kill the agent in the current buffer."
  (when-let (agent (pi-current-agent))
    (delete-process agent)))

;;; Completion

(defun pi-project-file-completions (_prefix)
  (or pi-project-file-cache
      (when-let (project (project-current))
        (let ((default-directory (pi-project-root))
              (project-files-relative-names t))
          (setq pi-project-file-cache (project-files project))))))

(defun pi-native-file-completions (prefix)
  (let* ((dir (or (file-name-directory prefix) ""))
         (file (file-name-nondirectory prefix))
         (full-dir (expand-file-name dir (pi-project-root))))
    (when (file-directory-p full-dir)
      (let ((candidates (file-name-all-completions file full-dir)))
        (mapcar (lambda (c) (concat dir c)) candidates)))))

(defun pi-completion-at-point-file ()
  (let ((end (point)))
    (save-excursion
      (when (re-search-backward "@\\([^\t\n ]*\\)" (line-beginning-position) t)
        (let* ((start (match-beginning 1))
               (prefix (match-string 1))
               (completions
                (if (eq pi-file-completion-backend 'project)
                    (pi-project-file-completions prefix)
                  (pi-native-file-completions prefix))))
          (when completions
            (list start end completions :category 'file :company-prefix-length t)))))))

(defun pi-completion-at-point-slash ()
  (let ((end (point)))
    (save-excursion
      (when (re-search-backward "\\([ \t]*\\)/\\([-a-zA-Z0-9]*\\)" (line-beginning-position) t)
        (let ((match-start (match-beginning 0))
              (cmd-start (match-beginning 2)))
          (when (string-match "^user>[ \t]*$" (buffer-substring-no-properties (widget-get pi-prompt-widget :from) match-start))
            (list cmd-start end (mapcar #'car pi-slash-commands) :company-prefix-length t)))))))

;;; Chat

(defun pi-message-role (message)
  (or (plist-get message :role) "unknown"))

(defun pi-content-join (message type)
  (mapconcat
   (lambda (item)
     (when (equal (plist-get item :type) type)
       (plist-get item (intern (concat ":" type)))))
   (plist-get message :content)
   ""))

(defun pi-content-text (message)
  (pi-content-join message "text"))

(defun pi-content-thinking (message)
  (pi-content-join message "thinking"))

(defun pi-content-tool-call (message)
  (cl-find-if
   (lambda (item)
     (equal (plist-get item :type) "toolCall"))
   (plist-get message :content)))

(defun pi-insert-role-prefix (role)
  (insert (propertize (format "%s> " role) 'face 'pi-chat-role-face)))

(defun pi-insert-thinking (text)
  (insert (propertize text 'face 'pi-thinking-face)))

(defun pi-insert-message-tail ()
  (insert "\n\n"))

(defun pi-insert-tool-name (tool-name)
  (insert (propertize (format "%s " tool-name) 'face 'pi-tool-name-face)))

(defun pi-insert-tool-result (tool-name result-text is-error &optional details)
  (cond
   ((string= tool-name "bash")
    (let* ((exit-code (plist-get details :exitCode))
           (full-output-path (plist-get details :fullOutputPath)))
      (cond
       ((eq is-error t)
        (when (not (string-empty-p result-text))
          (pi-insert-error (format "%s\n" result-text))))
       (t
        (when (not (string-empty-p result-text))
          (insert (format "%s\n" result-text)))))
      (when (and (numberp exit-code) (not (zerop exit-code)))
        (pi-insert-error (format "Command exited with code %d\n\n" exit-code)))
      (when full-output-path
        (insert "Output truncated. See full output at: ")
        (pi-insert-file-link full-output-path)
        (pi-insert-message-tail))))
   ((eq is-error t)
    (when (not (string-empty-p result-text))
      (pi-insert-error (format "%s\n" result-text))))
   ((string= tool-name "read")
    (when (and (not (string-empty-p result-text)) pi-current-tool-read-filename)
      (let ((truncated-line nil))
        (when (string-match "\n\\(\\[.*more lines.*continue.\\]\\)$" result-text)
          (setq truncated-line (match-string 1 result-text)
                result-text (replace-match "" nil nil result-text)))
        (insert (pi-render-content pi-current-tool-read-filename result-text))
        (insert (format "%s\n" (or truncated-line ""))))))
   ((string= tool-name "edit")
    (when-let ((diff (plist-get details :diff)))
      (insert (pi-render-diff diff))
      (insert "\n"))
    (when (not (string-empty-p result-text))
      (insert (format "%s\n" result-text))))
   (t
    (when (not (string-empty-p result-text))
      (insert (format "%s\n" result-text))))))

(defun pi-insert-message (message)
  (pcase (pi-message-role message)
    ("user"
     (let ((text (pi-content-text message)))
       (unless (string-empty-p text)
         (pi-widget-save-excursion
           (pi-create-section "user" 'user pi-root-section
             (pi-insert-role-prefix "user")
             (insert text)
             (pi-insert-message-tail))))))

    ("assistant"
     (let ((thinking-text (pi-content-thinking message))
           (text (pi-content-text message))
           (tool-call (pi-content-tool-call message)))
       (unless (string-empty-p thinking-text)
         (pi-widget-save-excursion
           (pi-create-section "thinking" 'thinking pi-root-section
             (pi-insert-role-prefix "assistant")
             (pi-insert-thinking thinking-text)
             (pi-insert-message-tail))))
       (unless (string-empty-p text)
         (pi-widget-save-excursion
           (pi-create-section "text" 'text pi-root-section
             (pi-insert-role-prefix "assistant")
             (insert (pi-render-markdown text))
             (pi-insert-message-tail))))
       (when tool-call
         (let ((tool-name (plist-get tool-call :name))
               (args (plist-get tool-call :arguments)))
           (when (string= tool-name "read")
             (setq pi-current-tool-read-filename (plist-get args :path)))
           (pi-widget-save-excursion
             (setq pi-current-tool-section (pi-new-section tool-name 'tool pi-root-section))
             (pi-insert-section pi-current-tool-section
               (pi-insert-tool-name tool-name)
               (pi-format-tool-args tool-name args)))))))

    ("toolResult"
     (let ((tool-name (plist-get message :toolName))
           (result-text (pi-content-text message))
           (is-error (plist-get message :isError))
           (details (plist-get message :details)))
       (when pi-current-tool-section
         (pi-widget-save-excursion
           (pi-append-section pi-current-tool-section
             (pi-insert-tool-result tool-name result-text is-error details))))
       (setq pi-current-tool-section nil
             pi-current-tool-read-filename nil)))))


(defun pi-handle-message-update (event)
  (let* ((message (plist-get event :message))
         (role (pi-message-role message))
         (type (plist-get event :type))
         (thinking-text (pi-content-thinking message))
         (text (pi-content-text message)))
    (when (member role '("assistant" "user"))
      (unless (string-empty-p thinking-text)
        (pi-widget-save-excursion
          (if pi-thinking-section
              (pi-replace-section pi-thinking-section
                (pi-insert-role-prefix role)
                (pi-insert-thinking thinking-text)
                (pi-insert-message-tail))
            (setq pi-thinking-section (pi-new-section "thinking" 'thinking pi-root-section))
            (pi-insert-section pi-thinking-section
              (pi-insert-role-prefix role)
              (pi-insert-thinking thinking-text)
              (pi-insert-message-tail)))))

      (unless (string-empty-p text)
        (pi-widget-save-excursion
          (if pi-text-section
              (pi-replace-section pi-text-section
                (pi-insert-role-prefix role)
                (insert text)
                (pi-insert-message-tail))
            (setq pi-text-section (pi-new-section "text" 'text pi-root-section))
            (pi-insert-section pi-text-section
              (pi-insert-role-prefix role)
              (insert text)
              (pi-insert-message-tail))))))
    (when (equal type "message_end")
      (when (and (equal role "assistant") (not (string-empty-p text)))
        (pi-widget-save-excursion
          (pi-replace-section pi-text-section
            (pi-insert-role-prefix role)
            (insert (pi-render-markdown text))
            (pi-insert-message-tail))))
      ;; Cleanup tracking state
      (setq pi-text-section nil
            pi-thinking-section nil))))


(defun pi-format-tool-args (tool-name args)
  (pcase tool-name
    ("read"
     (when-let ((path (plist-get args :path)))
       (let* ((offset (plist-get args :offset))
              (limit (plist-get args :limit))
              (start-line (or offset 1))
              (suffix (cond
                       ((and (null offset) (null limit)) "")
                       ((null limit) (format ":%d" start-line))
                       (t (let ((end-line (+ start-line limit -1)))
                            (format ":%d-%d" start-line end-line))))))
         (pi-insert-file-link (expand-file-name path (pi-project-root)) suffix)
         (insert "\n"))))
    ("write"
     (when-let ((path (plist-get args :path))
                (content (plist-get args :content)))
       (pi-insert-file-link (expand-file-name path (pi-project-root)))
       (when (not (string-empty-p content))
         (insert "\n")
         (insert (pi-render-content path content)))
       (insert "\n")))
    ("edit"
     (when-let ((path (plist-get args :path)))
       (pi-insert-file-link (expand-file-name path (pi-project-root)))
       (insert "\n")))
    ("bash"
     (when-let ((command (plist-get args :command)))
       (insert (format "%s\n" command))))
    (_
     (insert (format "%S\n" args)))))

(defun pi-handle-tool-execution-start (event)
  (let* ((tool-name (plist-get event :toolName))
         (args (plist-get event :args)))
    (when (string= tool-name "read")
      (setq pi-current-tool-read-filename (plist-get args :path)))
    (pi-widget-save-excursion
      (setq pi-current-tool-section (pi-new-section tool-name 'tool pi-root-section))
      (pi-insert-section pi-current-tool-section
        (pi-insert-tool-name tool-name)
        (pi-format-tool-args tool-name args)))))

(defun pi-handle-tool-execution-end (event)
  (let* ((result (plist-get event :result))
         (result-text (pi-content-text result))
         (is-error (plist-get event :isError))
         (tool-name (plist-get event :toolName)))
    (when pi-current-tool-section
      (pi-widget-save-excursion
        (pi-append-section pi-current-tool-section
          (pi-insert-tool-result tool-name result-text is-error
                                 (plist-get result :details)))))
    (setq pi-current-tool-read-filename nil
          pi-current-tool-section nil)))

(defun pi-handle-auto-retry-start (event)
  (let ((attempt (plist-get event :attempt))
        (max-attempts (plist-get event :maxAttempts))
        (delay-ms (plist-get event :delayMs))
        (error-message (plist-get event :errorMessage)))
    (when (and error-message (not (string-empty-p error-message)))
      (pi-widget-save-excursion
        (pi-create-section "error" 'error pi-root-section
          (pi-insert-error (format "Error: %s\n\n" error-message))
          (insert
           (propertize (format "Retrying %d/%d (waiting %ds)…\n\n" attempt max-attempts (/ delay-ms 1000))
                       'face 'pi-thinking-face)))))))

(defun pi-handle-auto-retry-end (event)
  (let ((attempt (plist-get event :attempt))
        (final-error (plist-get event :finalError)))
    (unless (pi-response-success-p event)
      (pi-widget-save-excursion
        (pi-create-section "error" 'error pi-root-section
          (pi-insert-error
           (format "Error: Retry failed after %d attempts: %s\n\n" attempt final-error)))))))

(defun pi-handle-queue-update (event)
  (let* ((steering (plist-get event :steering))
         (follow-up (plist-get event :followUp))
         (has-content (or (consp steering)
                          (consp follow-up))))
    (when has-content
      (pi-widget-save-excursion
        (pi-create-section "queue" 'queue pi-root-section
          (insert (propertize "queue\n" 'face 'bold))
          (dolist (item steering)
            (insert (propertize (format " Steering: %s\n" item) 'face 'pi-thinking-face)))
          (dolist (item follow-up)
            (insert (propertize (format " Follow-up: %s\n" item) 'face 'pi-thinking-face)))
          (insert "\n"))))))

(defun pi-handle-compaction-end (event)
  (let* ((result (plist-get event :result))
         (error-message (plist-get event :errorMessage)))
    (cond
     (error-message
      (pi-widget-save-excursion
        (pi-create-section "error" 'error pi-root-section
          (pi-insert-error error-message)
          (pi-insert-message-tail))))
     (result
      (let* ((summary (plist-get result :summary))
             (tokens-before (plist-get result :tokensBefore))
             (header (format "**Compacted from %s tokens**\n\n"
                             (pi-format-number-short tokens-before))))
        (pi-widget-save-excursion
          (pi-create-section "compact" 'compact pi-root-section
            (pi-insert-role-prefix "assistant")
            (insert (pi-render-markdown (concat header summary)))
            (pi-insert-message-tail))))))))

(defun pi-register-event-listeners ()
  (pi-set-event-listener "message_update" #'pi-handle-message-update)
  (pi-set-event-listener "message_end" #'pi-handle-message-update)

  (pi-set-event-listener "tool_execution_start" #'pi-handle-tool-execution-start)
  (pi-set-event-listener "tool_execution_end" #'pi-handle-tool-execution-end)

  (pi-set-event-listener "auto_retry_start" #'pi-handle-auto-retry-start)
  (pi-set-event-listener "auto_retry_end" #'pi-handle-auto-retry-end)

  (pi-set-event-listener "queue_update" #'pi-handle-queue-update)
  (pi-set-event-listener "compaction_end" #'pi-handle-compaction-end)
  (pi-set-event-listener t #'pi-handle-agent-state))

(defun pi-focus-prompt ()
  (interactive)
  (goto-char (widget-get pi-prompt-widget :from))
  (forward-char 6)
  (widget-end-of-line))

(defun pi-format-state ()
  (if pi-agent-state
      (let ((state pi-agent-state))
        (if (consp state)
            (format "Pi %s(%s)" (car state) (cdr state))
          (format "Pi %s" state)))
    "Pi"))

(defun pi-format-header ()
  "Format the header line from `pi-header-line-state'."
  (let* ((model (plist-get pi-header-line-state :model))
         (provider (plist-get model :provider))
         (model-id (plist-get model :id))
         (thinking-level (plist-get pi-header-line-state :thinkingLevel))
         (auto-compact (plist-get pi-header-line-state :autoCompactionEnabled))
         (session-stats (plist-get pi-header-line-state :sessionStats))
         (context-usage (plist-get session-stats :contextUsage))
         (ctx-tokens (plist-get context-usage :tokens))
         (ctx-window-usage (plist-get context-usage :contextWindow))
         (ctx-str (pi-format-number-short ctx-window-usage))
         (usage-str (pi-format-number-short ctx-tokens)))
    (let* ((state-str (pi-format-state))
           (left (format "%s/%s (%s) • %s"
                         usage-str ctx-str
                         (if auto-compact "auto" "manual")
                         state-str))
           (right (format "(%s) %s • %s"
                          (or provider "?")
                          (or model-id "?")
                          (or thinking-level "?"))))
      (format "%s%s%s"
              left
              (make-string (max 1 (- (window-width) (length left) (length right))) ?\s)
              right))))

(defun pi-update-header-line ()
  (let* ((state-result nil)
         (stats-result nil)
         (try-update
          (lambda ()
            (when (and state-result stats-result)
              (setq pi-header-line-state
                    (plist-put state-result :sessionStats stats-result))
              (force-mode-line-update)))))
    (pi-send-command
     "get_state" '()
     (pi-on-response-success-callback resp
       (setq state-result (plist-get resp :data))
       (funcall try-update)))
    (pi-send-command
     "get_session_stats" '()
     (pi-on-response-success-callback resp
       (setq stats-result (plist-get resp :data))
       (funcall try-update)))))

(timeout-debounce 'pi-update-header-line 1)

(defun pi-handle-agent-state (event)
  (cl-case (intern (plist-get event :type))
    (agent_start (setq pi-agent-state 'thinking))
    (agent_end (setq pi-agent-state nil))
    (turn_start (setq pi-agent-state 'thinking))
    (turn_end (setq pi-agent-state nil))
    (tool_execution_start (setq pi-agent-state (cons 'tool (plist-get event :toolName))))
    (tool_execution_end (setq pi-agent-state nil))
    (compaction_start (setq pi-agent-state 'compacting))
    (compaction_end (setq pi-agent-state nil))
    (auto_retry_start (setq pi-agent-state 'retrying))
    (auto_retry_end (setq pi-agent-state nil)))
  (pi-update-header-line))

(defun pi-cleanup-chat-buffer ()
  (let ((project-key (pi-project-key)))
    (remhash project-key pi-chats)
    (pi-hash-remove-if (lambda (k _v) (equal (car k) project-key)) pi-event-listeners)
    (ignore-errors
      (pi-kill-agent))))


;;; Commands

(defun pi-parse-slash-command (prompt)
  (when (string-match "^[ \t]*/\\([-a-zA-Z0-9]+\\)\\([ \t].*\\)?$" prompt)
    (let* ((name (match-string-no-properties 1 prompt))
           (raw (and (match-beginning 2)
                     (string-trim-left (match-string-no-properties 2 prompt))))
           (args (and (not (string-empty-p raw)) raw))
           (cell (assoc name pi-slash-commands #'string-equal)))
      (when cell
        (let ((cmd (cadr cell))
              (max-args (nth 2 cell)))
          (when (and args (not (eq max-args 1)))
            (error "Slash command \"/%s\" does not accept arguments" name))
          (cons cmd args))))))

(defun pi-parse-bang-command (prompt)
  (pi-parse-bang-command-with-regex prompt "^[ \t]*!\\([^!].*\\)$"))

(defun pi-parse-double-bang-command (prompt)
  (pi-parse-bang-command-with-regex prompt "^[ \t]*!!\\(.+\\)$"))

(defun pi-parse-bang-command-with-regex (prompt regex)
  (when (string-match regex prompt)
    (let ((result (match-string-no-properties 1 prompt)))
      (when (not (string-match-p "^[ \t]+$" result))
        result))))

(defun pi-clear-prompt (prompt)
  (widget-value-set pi-prompt-widget "")
  (unless (and (> (ring-length pi-prompt-history) 0)
               (equal prompt (ring-ref pi-prompt-history 0)))
    (ring-insert pi-prompt-history prompt))
  (setq pi-prompt-history-index 0))

(defun pi-send-prompt (&optional prompt streaming-behavior)
  (interactive "sPrompt: ")
  (if (or (null prompt) (string-empty-p prompt))
      (message "No prompt to send")
    (let ((slash (pi-parse-slash-command prompt))
          (bang (pi-parse-bang-command prompt))
          (double-bang (pi-parse-double-bang-command prompt)))
      (cond
       (slash
        (let ((cmd (car slash))
              (args (cdr slash)))
          (if (null args)
              (call-interactively cmd)
            (apply cmd (list args))))
        (pi-clear-prompt prompt))
       (double-bang
        (pi-bash double-bang t)
        (pi-clear-prompt prompt))
       (bang
        (pi-bash bang)
        (pi-clear-prompt prompt))
       (t
        (pi-with-chat-buffer
          (pi-send-command
           "prompt" (list :message prompt
                          :streamingBehavior (when-let (behavior (or streaming-behavior pi-prompt-streaming-behavior))
                                               (symbol-name behavior)))
           (pi-on-response-success-callback resp
             (pi-clear-prompt prompt)))))))))


(defun pi-send-prompt-alternate (&optional prompt)
  "Send PROMPT with the alternative streaming behavior.

If `pi-prompt-streaming-behavior' is `followUp', use `steer' and vice versa."
  (interactive)
  (let* ((alt-behavior (if (eq pi-prompt-streaming-behavior 'followUp)
                           'steer
                         'followUp))
         (prompt-text (or prompt (widget-value pi-prompt-widget))))
    (when (and prompt-text (not (string-empty-p prompt-text)))
      (pi-send-prompt prompt-text alt-behavior))))

(defun pi-abort ()
  (interactive)
  (pi-with-chat-buffer
    (pi-send-command
     (if pi-bash-in-progress "abort_bash" "abort") '()
     (pi-on-response-success-callback resp
       (pi-widget-save-excursion
         (pi-create-section "error" 'error pi-root-section
           (pi-insert-error "Aborted.\n\n"))))))
  (keyboard-quit))

(defun pi-insert-stats-section (header plist fields)
  "Insert a stats section with HEADER (bold), extracting integers from PLIST.
FIELDS is a list of (LABEL . KEY) where KEY is a plist key."
  (insert (propertize (concat header "\n") 'face 'bold))
  (pcase-dolist (`(,label . ,key) fields)
    (insert (format " %s: %d\n" label (plist-get plist key)))))

(defun pi-session-stats ()
  (interactive)
  (pi-with-chat-buffer
    (pi-send-command
     "get_session_stats" '()
     (pi-on-response-success-callback resp
       (let* ((data (plist-get resp :data))
              (tokens (plist-get data :tokens))
              (cost (plist-get data :cost)))
         (pi-widget-save-excursion
           (pi-create-section "session" 'session pi-root-section
             (insert
              (propertize "Session Info\n" 'face 'bold))

             (insert " File: ")
             (pi-insert-file-link (plist-get data :sessionFile))
             (insert "\n")

             (insert
              (format " ID: %s\n\n"
                      (plist-get data :sessionId)))

             (pi-insert-stats-section
              "Messages"
              data
              '(("User" . :userMessages)
                ("Assistant" . :assistantMessages)
                ("Tool Calls" . :toolCalls)
                ("Tool Results" . :toolResults)
                ("Total" . :totalMessages)))

             (insert "\n")

             (pi-insert-stats-section
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
              (format " Total: %.4f\n" cost))

             (insert "\n\n"))))))))

(defun pi-select-model ()
  (interactive)
  (pi-with-chat-buffer
    (pi-send-command
     "get_available_models" '()
     (pi-on-response-success-callback resp
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
             (pi-send-command
              "set_model" (list :provider provider :modelId model-id)
              (pi-on-response-success-callback resp
                (pi-update-header-line)
                (pi-widget-save-excursion
                  (pi-create-section "model" 'model pi-root-section
                    (insert (format "Switched to model: (%s) %s\n\n" provider model-id)))))))))))))

(defvar pi-thinking-level-descriptions
  '((:off     . "No reasoning")
    (:minimal . "Very brief reasoning (~1k tokens)")
    (:low     . "Light reasoning (~2k tokens)")
    (:medium  . "Moderate reasoning (~8k tokens)")
    (:high    . "Deep reasoning (~16k tokens)")
    (:xhigh   . "Maximum reasoning (~32k tokens)")))

(defun pi-get-supported-thinking-levels (model)
  (let ((thinking-level-map (plist-get model :thinkingLevelMap))
        (reasoning (plist-get model :reasoning))
        result)
    (when (and reasoning (not (eq reasoning 'json-false)))
      (dolist (level '(:minimal :low :medium :high :xhigh))
        (let ((mapped (plist-get thinking-level-map level)))
          (unless (eq mapped 'json-null)
            (if (eq level :xhigh)
                (when mapped
                  (push level result))
              (push level result))))))
    (cons :off (nreverse result))))

(defun pi-set-thinking-level ()
  (interactive)
  (pi-with-chat-buffer
    (pi-send-command
     "get_state" '()
     (pi-on-response-success-callback resp
       (let* ((data (plist-get resp :data))
              (model (plist-get data :model))
              (current-level (when-let ((l (plist-get data :thinkingLevel)))
                               (intern (concat ":" l))))
              (supported-levels (pi-get-supported-thinking-levels model))
              (items
               (mapcar
                (lambda (level)
                  (let* ((name (pi-keyword-name level))
                         (desc (alist-get level pi-thinking-level-descriptions)))
                    (cons (if desc
                              (format "%s — %s" name desc)
                            name)
                          level)))
                supported-levels)))
         (if (null supported-levels)
             (message "No thinking levels available for this model.")
           (let* ((default (or current-level :off))
                  (default-display (alist-get default items))
                  (selected-display (completing-read
                                     (format "Set thinking level (current: %s): "
                                             (or (plist-get data :thinkingLevel) "?"))
                                     items nil t nil nil default-display))
                  (selected (alist-get selected-display items nil nil #'equal))
                  (selected-str (pi-keyword-name selected)))
             (pi-send-command
              "set_thinking_level" (list :level selected-str)
              (pi-on-response-success-callback resp
                (pi-update-header-line)
                (pi-widget-save-excursion
                  (pi-create-section "thinking" 'thinking pi-root-section
                    (insert (format "Thinking level set to: %s\n\n" selected-str)))))))))))))

(cl-defstruct pi-session-choice
  id message timestamp cwd path parent-id)

(defun pi-read-session-choice (filename)
  (with-temp-buffer
    (insert-file-contents filename nil 0 5000)
    (goto-char (point-min))
    (let ((id nil)
          (timestamp nil)
          (cwd nil)
          (parent-id nil)
          (first-text nil)
          (lines-read 0))
      (while (and (null first-text) (< lines-read 10) (not (eobp)))
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
                    ('message
                     (let ((first-msg-text (pi-content-text (plist-get json :message))))
                       (when (and (not (string-empty-p first-msg-text))
                                  (null first-text))
                         (setq first-text (truncate-string-to-width first-msg-text 80 nil nil t)))))))
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
                              :message first-text))))

(defun pi-resume ()
  (interactive)
  (pi-with-chat-buffer
    (pi-send-command
     "get_state" '()
     (lambda (resp)
       (when (pi-response-success-p resp)
         (let* ((data (plist-get resp :data))
                (session-file (plist-get data :sessionFile))
                (session-dir (file-name-directory session-file))
                (files (when session-dir
                         (seq-take
                          (sort (directory-files session-dir t "\\.jsonl$")
                                #'string>)
                          pi-resume-max-sessions)))
                (sessions (mapcar #'pi-read-session-choice files)))
           (if (null sessions)
               (message "No session files found in %s" session-dir)
             (let* ((candidates
                     (mapcar
                      (lambda (s)
                        (let* ((ts (pi-session-choice-timestamp s))
                               (formatted-time (if ts
                                                   (format-time-string "%Y-%m-%d %H:%M" ts)
                                                 ""))
                               (short-id (substring (pi-session-choice-id s) -8))
                               (short-parent (when-let ((pid (pi-session-choice-parent-id s)))
                                               (substring pid -8))))
                          (cons (format "%s  %s  %s%s" short-id formatted-time (pi-session-choice-message s)
                                        (if short-parent (format " (parent: %s)" short-parent) ""))
                                s)))
                      sessions))
                    (selected (completing-read "Resume session: "
                                               (lambda (string pred action)
                                                 (if (eq action 'metadata)
                                                     '(metadata (display-sort-function . identity))
                                                   (complete-with-action action candidates string pred)))
                                               nil t))
                    (choice (alist-get selected candidates nil nil #'equal))
                    (session-path (pi-session-choice-path choice)))
               (pi-send-command
                "switch_session" (list :sessionPath session-path)
                (pi-on-response-success-callback resp
                  (pi-unless-cancelled resp "Session switch"
                    (pi-refresh-session))))))))))))

(defun pi-clear-sections ()
  (dolist (child (copy-sequence (pi-section-children pi-root-section)))
    (pi-delete-section child))
  (setq pi-text-section nil
        pi-thinking-section nil
        pi-current-tool-section nil
        pi-current-tool-read-filename nil))

(defun pi-refresh-session ()
  (interactive)
  (pi-with-chat-buffer
    (pi-send-command
     "get_messages" '()
     (pi-on-response-success-callback resp
       (let ((messages (plist-get (plist-get resp :data) :messages)))
         (pi-widget-save-excursion
           (pi-clear-sections)
           (dolist (message messages)
             (pi-insert-message message))))))))

(defun pi-clone ()
  (interactive)
  (pi-with-chat-buffer
    (pi-send-command
     "clone" '()
     (pi-on-response-success-callback resp
       (pi-unless-cancelled resp "Clone"
         (pi-refresh-session)
         (pi-widget-save-excursion
           (pi-create-section "info" 'info pi-root-section
             (insert "Session cloned.\n\n"))))))))

(defun pi-new-session ()
  (interactive)
  (pi-with-chat-buffer
    (pi-send-command
     "new_session" '()
     (pi-on-response-success-callback resp
       (pi-unless-cancelled resp "New session"
         (pi-widget-save-excursion
           (pi-clear-sections)))))))

(defun pi-set-session-name (name)
  (interactive "sSession name: ")
  (let ((trimmed (string-trim name)))
    (if (string-empty-p trimmed)
        (message "Session name cannot be empty")
      (pi-with-chat-buffer
        (pi-send-command
         "set_session_name" (list :name trimmed)
         (pi-on-response-success-callback resp
           (pi-widget-save-excursion
             (pi-create-section "info" 'info pi-root-section
               (insert (format "Session renamed to: %s\n\n" trimmed))))))))))


(defun pi-export (&optional output-path)
  (interactive
   (list (when current-prefix-arg
           (expand-file-name
            (read-file-name "Export to file: ")))))
  (pi-with-chat-buffer
    (let ((args (if (and output-path (not (string-empty-p output-path)))
                    (list :outputPath (expand-file-name output-path))
                  '())))
      (pi-send-command
       "export_html" args
       (pi-on-response-success-callback resp
         (let ((path (plist-get (plist-get resp :data) :path)))
           (pi-widget-save-excursion
             (pi-create-section "export" 'info pi-root-section
               (insert "Session exported to: ")
               (pi-insert-file-link path)
               (pi-insert-message-tail)))))))))

(defun pi-copy ()
  "Copy the last assistant message to the clipboard."
  (interactive)
  (pi-with-chat-buffer
    (pi-send-command
     "get_last_assistant_text" '()
     (pi-on-response-success-callback resp
       (let ((text (plist-get (plist-get resp :data) :text)))
         (if text
             (progn
               (kill-new text)
               (message "Copied last assistant message to clipboard."))
           (message "No assistant message available to copy.")))))))

(defun pi-fork ()
  (interactive)
  (pi-with-chat-buffer
    (pi-send-command
     "get_fork_messages" '()
     (pi-on-response-success-callback resp
       (let* ((messages (plist-get (plist-get resp :data) :messages))
              (items
               (mapcar
                (lambda (m)
                  (cons (truncate-string-to-width (plist-get m :text) 80 nil nil t) m))
                messages)))
         (if (null items)
             (message "No fork points available.")
           (let* ((selected (completing-read "Fork at message: " items nil t))
                  (message (alist-get selected items nil nil #'equal))
                  (entry-id (plist-get message :entryId)))
             (pi-send-command
              "fork" (list :entryId entry-id)
              (pi-on-response-success-callback resp
                (pi-unless-cancelled resp "Fork"
                  (let ((text (plist-get (plist-get resp :data) :text)))
                    (pi-refresh-session)
                    (pi-widget-save-excursion
                      (pi-create-section "fork" 'fork pi-root-section
                        (insert text)
                        (pi-insert-message-tail))))))))))))))

(defun pi-compact (&optional custom-instructions)
  "Compact the current session to reduce context usage.

With prefix argument, prompt for custom instructions to guide the
summarization."
  (interactive
   (list (when current-prefix-arg
           (read-string "Custom instructions for compaction: "))))
  (pi-with-chat-buffer
    (let ((args (if custom-instructions
                    (list :customInstructions custom-instructions)
                  '())))
      (pi-send-command "compact" args))))

(defun pi-bash (command &optional exclude-from-context)
  (interactive "sBash command: ")
  (unless (string-empty-p (string-trim command))
    (pi-with-chat-buffer
      (setq pi-bash-in-progress t)
      (let ((args (list :command command))
            (section (pi-new-section "bash" 'tool pi-root-section)))
        (when exclude-from-context
          (setq args (nconc args (list :excludeFromContext t))))
        (pi-widget-save-excursion
          (pi-insert-section section
            (pi-insert-tool-name "bash")
            (insert (format "%s\n" command))))
        (pi-send-command
         "bash" args
         (lambda (resp)
           (pi-on-response-success resp
             (let* ((data (plist-get resp :data))
                    (exit-code (plist-get data :exitCode))
                    (cancelled (plist-get data :cancelled))
                    (is-error (or (and exit-code
                                       (not (eq exit-code 'json-null))
                                       (not (zerop exit-code)))
                                  (and cancelled
                                       (not (eq cancelled 'json-false))))))
               (pi-widget-save-excursion
                 (pi-append-section section
                   (pi-insert-tool-result
                    "bash"
                    (plist-get data :output)
                    is-error
                    data)))))
           (setq pi-bash-in-progress nil)))))))

;;; Chat mode

(defvar-keymap pi-chat-mode-map
  :doc "Keymap for `pi-chat-mode'."
  :parent special-mode-map
  "C-g" #'pi-abort
  "TAB" #'pi-toggle-section
  "C-i" #'pi-toggle-section
  "n" #'pi-goto-next-section
  "M-n" #'pi-goto-next-section
  "p" #'pi-goto-previous-section
  "M-p" #'pi-goto-previous-section
  "i" #'pi-focus-prompt
  "q" #'pi-quit-chat)

(defvar pi-chat-widget-field-keymap
  (let ((map (make-composed-keymap nil widget-field-keymap)))
    (keymap-set map "C-g" #'pi-abort)
    (keymap-set map "M-p" #'pi-previous-prompt)
    (keymap-set map "M-n" #'pi-next-prompt)
    (keymap-set map "C-r" #'pi-search-prompt)
    (keymap-set map "M-RET" #'pi-send-prompt-alternate)
    map))

(define-derived-mode pi-chat-mode nil "pi-chat"
  "Major mode for pi chat.

\\{pi-chat-mode-map}"
  (buffer-disable-undo)
  (setq header-line-format '(:eval (pi-format-header)))
  (pi-create-root-section)
  (setq pi-prompt-history (make-ring pi-prompt-history-max-size))
  (setq-local completion-at-point-functions
              (append (list #'pi-completion-at-point-slash
                            #'pi-completion-at-point-file)
                      completion-at-point-functions))
  (setq pi-prompt-widget
        (widget-create 'editable-field
                       :keymap pi-chat-widget-field-keymap
                       :help-echo ""
                       :format "%[user>%] %v"
                       :button-face 'pi-chat-role-face
                       :action (lambda (widget &optional _event)
                                 (pi-send-prompt (widget-value widget)))))
  (widget-setup)
  (pi-focus-prompt)
  (add-hook 'kill-buffer-hook 'pi-cleanup-chat-buffer nil t)
  (pi-register-event-listeners)
  (pi-update-header-line))

;;;###autoload
(defun pi-chat ()
  "Start a chat window"
  (interactive)
  (unless (pi-current-agent)
    (pi-start-agent))
  (let ((chat-buffer (or (pi-current-chat)
                         (progn
                           (let ((buffer (generate-new-buffer (pi-chat-buffer-name)))
                                 (root (pi-project-root)))
                             (with-current-buffer buffer
                               (pi-chat-mode)
                               (setq-local default-directory root))
                             (puthash (pi-project-key) buffer pi-chats)
                             buffer)))))
    (pop-to-buffer chat-buffer)))


(defun pi-quit-chat ()
  "Quit the current chat window."
  (interactive)
  (when-let (buffer (pi-current-chat))
    (kill-buffer buffer)))

(defun pi-restart-chat ()
  "Exit the current chat and restart"
  (interactive)
  (when-let (buffer (pi-current-chat))
    (kill-buffer buffer))
  (pi-kill-agent)
  (pi-chat))


(provide 'pi)

;;; pi.el ends here
