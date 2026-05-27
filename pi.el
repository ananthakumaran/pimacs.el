;;; pi.el --- Emacs UI for Pi -*- lexical-binding: t -*-

;; Copyright (C) 2026 Anantha Kumaran.

;; Author: Anantha kumaran <ananthakumaran@gmail.com>
;; URL: http://github.com/ananthakumaran/pi.el
;; Version: 0.1
;; Keywords: pi agent
;; Package-Requires: ((emacs "28.1"))

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

(defgroup pi nil
  "Emacs UI for Pi."
  :prefix "pi-"
  :group 'tools)

(defface pi-chat-role-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face used for chat message role labels."
  :group 'pi)

(defface pi-widget-error-face
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

;;; Utilities

(defun pi-json-read-object ()
  (json-parse-buffer :object-type 'plist :null-object 'json-null :false-object 'json-false :array-type 'list))

(defun pi-json-encode (obj)
  "Encode OBJ into a JSON string. JSON arrays must be represented with vectors."
  (json-serialize obj :null-object 'json-null :false-object 'json-false))

(defun pi-format-number-short (n)
  "Format number N into a short human-readable string with K/M/B suffixes."
  (cond
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

;;; State management

(pi-def-permanent-buffer-local pi-project-root nil)

(defvar pi-agents (make-hash-table :test 'equal))
(defvar pi-chats (make-hash-table :test 'equal))
(defvar pi-response-callbacks (make-hash-table :test 'equal))

(pi-def-permanent-buffer-local pi-prompt-widget nil)
(pi-def-permanent-buffer-local pi-message-widget nil)
(pi-def-permanent-buffer-local pi-thinking-widget nil)
(pi-def-permanent-buffer-local pi-header-line-state nil)

(defvar pi-event-listeners (make-hash-table :test 'equal))

(defvar pi-agent-buffer-name "*pi-agent*")
(defvar pi-chat-buffer-name "*pi-chat*")
(defvar pi-request-counter 0)

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
  (let ((full-path (directory-file-name (pi-project-root))))
    (concat (file-name-nondirectory full-path) "-" (substring (md5 full-path) 0 10))))

(defmacro pi-widget-save-excursion (&rest body)
  "Insert content before PROMPT-WIDGET and restore focus afterward."
  (declare (indent 0) (debug t))
  `(progn
     (goto-char (widget-get pi-prompt-widget :from))
     ,@body
     (widget-setup)
     (pi-focus-prompt)
     ;; (recenter -4)
     ))

(defmacro pi-with-chat-buffer (&rest body)
  "Execute the body in the current chat buffer"
  (declare (indent 0) (debug t))
  `(let ((buffer (pi-current-chat)))
     (if buffer
         (with-current-buffer buffer
           (progn ,@body))
       (error "Chat doesn't exist, start a new chat using M-x pi-chat"))))

(defun pi-current-agent ()
  (gethash (pi-project-name) pi-agents))

(defun pi-current-chat ()
  (gethash (pi-project-name) pi-chats))

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
  (if-let (listener (gethash (cons (pi-project-name) (plist-get event :type)) pi-event-listeners))
    (with-current-buffer (car listener)
      (apply (cdr listener) (list event)))
    (message "Unhandled event %S" event)))

(defun pi-set-event-listener (name listener)
  (puthash (cons (pi-project-name) name) (cons (current-buffer) listener) pi-event-listeners))

(defun pi-dispatch (response)
  (cl-case (intern (plist-get response :type))
    ((response) (pi-dispatch-response response))
    (t (pi-dispatch-event response))))

(defun pi-send-command (type args &optional callback)
  (unless (pi-current-agent)
    (error "Agent does not exist. Run M-x pi-restart-agent to start it again"))

  (let* ((request-id (pi-next-request-id))
         (command (append (list :id request-id :type type) args))
         (encoded-command (pi-json-encode command))
         (payload (concat encoded-command "\n")))
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
    (pi-cleanup-agent project-name)))

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
      (let ((response (pi-json-read-object)))
        (delete-region (point-min) (point))
        (when response
          (pi-dispatch response)))
      (when (>= (buffer-size) 16)
        (pi-decode-response process)))))

(defun pi-start-agent ()
  (when (pi-current-agent)
    (error "Agent already exist"))

  (message "(%s) Starting pi..." (pi-project-name))
  (let* ((default-directory (pi-project-root))
         (process-environment (append pi-process-environment process-environment))
         (buf (generate-new-buffer pi-agent-buffer-name))
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
    (process-put process 'project-name (pi-project-name))
    (process-put process 'project-root default-directory)
    (puthash (pi-project-name) process pi-agents)
    (message "(%s) pi agent started successfully." (pi-project-name))))


(defun pi-cleanup-agent (project-name)
  (remhash project-name pi-agents))

;;; Utility commands

(defun pi-kill-agent ()
  "Kill the agent in the current buffer."
  (interactive)
  (when-let (agent (pi-current-agent))
    (delete-process agent)))

(defun pi-restart-agent ()
  "Restarts pi agent."
  (interactive)
  (pi-kill-agent)
  (pi-start-agent))


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

(defun pi-handle-noop (_event)
  nil)

(defun pi-handle-message-update (event)
  (let* ((message (plist-get event :message))
         (role (pi-message-role message))
         (type (plist-get event :type))
         (thinking-text (pi-content-thinking message))
         (text (pi-content-text message)))
    (when (member role '("assistant" "user"))
     (unless (string-empty-p thinking-text)
       (pi-widget-save-excursion
         (unless pi-thinking-widget
           (widget-insert
            (propertize
             (format "%s> " role)
             'face 'pi-chat-role-face))
           (setq pi-thinking-widget (widget-create 'item
                                                   :format "%[%v%]\n\n"
                                                   :button-face 'pi-thinking-face
                                                   "")))
         (widget-value-set pi-thinking-widget thinking-text)))

     (unless (string-empty-p text)
       (pi-widget-save-excursion
         (unless pi-message-widget
           (widget-insert
            (propertize
             (format "%s> " role)
             'face 'pi-chat-role-face))
           (setq pi-message-widget (widget-create 'item
                                                  :format "%v\n\n"
                                                  "")))
         (widget-value-set pi-message-widget text))))
    (when (equal type "message_end")
      ;; Cleanup tracking state
      (setq pi-thinking-widget nil
            pi-message-widget nil))))


(defun pi-format-tool-args (tool-name args)
  "Format ARGS for display based on TOOL-NAME.
For read/write/edit, the path is rendered as a file-link widget."
  (pcase tool-name
    ((or "read" "write")
     (when-let ((path (plist-get args :path)))
       (widget-create 'file-link
                      :button-prefix ""
                      :button-suffix ""
                      (expand-file-name path (pi-project-root)))
       (widget-insert "\n")))
    ("edit"
     (when-let ((path (plist-get args :path)))
       (widget-create 'file-link
                      :button-prefix ""
                      :button-suffix ""
                      (expand-file-name path (pi-project-root)))
       (widget-insert "\n")))
    ("bash"
     (when-let ((command (plist-get args :command)))
       (widget-insert (format "%s \n" command))))
    (_
     (widget-insert (format "%S\n" args)))))


(defun pi-handle-tool-execution-start (event)
  (let* ((tool-name (plist-get event :toolName))
         (args (plist-get event :args)))
    (pi-widget-save-excursion
      (widget-insert
       (propertize (format "%s " tool-name) 'face 'pi-tool-name-face))
      (pi-format-tool-args tool-name args))))

(defun pi-handle-tool-execution-end (event)
  (let* ((result (plist-get event :result))
         (result-text (pi-content-text result)))
    (pi-widget-save-excursion
      (when (not (string-empty-p result-text))
        (widget-insert (format "%s\n" result-text))))))

(defun pi-handle-header-line-update (_event)
  (pi-update-header-line))


(defun pi-register-event-listeners ()
  (pi-set-event-listener "message_start" #'pi-handle-noop)
  (pi-set-event-listener "message_update" #'pi-handle-message-update)
  (pi-set-event-listener "message_end" #'pi-handle-message-update)

  (pi-set-event-listener "agent_start" #'pi-handle-noop)
  (pi-set-event-listener "agent_end" #'pi-handle-header-line-update)

  (pi-set-event-listener "turn_start" #'pi-handle-noop)
  (pi-set-event-listener "turn_end" #'pi-handle-header-line-update)

  (pi-set-event-listener "tool_execution_start" #'pi-handle-tool-execution-start)
  (pi-set-event-listener "tool_execution_update" #'pi-handle-noop)
  (pi-set-event-listener "tool_execution_end" #'pi-handle-tool-execution-end))

(defun pi-focus-prompt ()
  (goto-char (widget-get pi-prompt-widget :from))
  (forward-char 6)
  (widget-end-of-line))

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
         (ctx-str (if ctx-window-usage
                      (pi-format-number-short ctx-window-usage)
                    "?"))
         (usage-str (if ctx-tokens
                        (pi-format-number-short ctx-tokens)
                      "?")))
    (let ((left (format "%s/%s (%s)"
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
     (lambda (resp)
       (when (plist-get resp :success)
         (setq state-result (plist-get resp :data))
         (funcall try-update))))
    (pi-send-command
     "get_session_stats" '()
     (lambda (resp)
       (when (plist-get resp :success)
         (setq stats-result (plist-get resp :data))
         (funcall try-update))))))

(defun pi-cleanup-chat-buffer ()
  (let ((project-name (pi-project-name)))
    (ignore-errors
      (pi-kill-agent))
    (remhash project-name pi-chats)
    (pi-hash-remove-if (lambda (k _v) (equal (car k) project-name)) pi-event-listeners)))

(define-derived-mode pi-chat-mode nil "pi-chat"
  "Major mode for pi chat.

\\{pi-chat-mode-map}"
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (setq header-line-format '(:eval (pi-format-header)))
  (setq pi-prompt-widget
        (widget-create 'editable-field
                       :help-echo ""
                       :format "%[user>%] %v"
                       :button-face 'pi-chat-role-face
                       :action (lambda (widget &optional _event)
                                 (pi-send-prompt (widget-value widget)))))
  (use-local-map widget-keymap)
  (widget-setup)
  (pi-focus-prompt)
  (add-hook 'kill-buffer-hook 'pi-cleanup-chat-buffer nil t)
  (pi-register-event-listeners)
  (pi-update-header-line))

(defvar pi-chat-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(defun pi-chat ()
  "Start a chat window"
  (interactive)
  (unless (pi-current-agent)
    (pi-start-agent))
  (let ((chat-buffer (or (pi-current-chat)
                         (progn
                           (let ((buffer (generate-new-buffer pi-chat-buffer-name))
                                 (root (pi-project-root)))
                             (with-current-buffer buffer
                               (pi-chat-mode)
                               (setq-local default-directory root))
                             (puthash (pi-project-name) buffer pi-chats)
                             buffer)))))
    (pop-to-buffer chat-buffer)))


(defun pi-restart-chat ()
  "Exist the current chat and restart"
  (interactive)
  (when-let (buffer (pi-current-chat))
    (kill-buffer buffer))
  (pi-kill-agent)
  (pi-chat))

;;; Commands

(defun pi-send-prompt (&optional prompt)
  (interactive "sPrompt: ")
  (pi-with-chat-buffer
    (pi-send-command
     "prompt"
     (list :message prompt)
     (lambda (resp)
       (if (equal (plist-get resp :success) 'json-false)
         (pi-widget-save-excursion
           (widget-insert
            (propertize
             (format "%s\n\n" (plist-get resp :error))
             'face 'pi-widget-error-face)))
         (widget-value-set pi-prompt-widget ""))))))

(defun pi-insert-stats-section (header plist fields)
  "Insert a stats section with HEADER (bold), extracting integers from PLIST.
FIELDS is a list of (LABEL . KEY) where KEY is a plist key."
  (widget-insert (propertize (concat header "\n") 'face 'bold))
  (pcase-dolist (`(,label . ,key) fields)
    (widget-insert (format " %s: %d\n" label (plist-get plist key)))))

(defun pi-session-stats ()
  (interactive)
  (pi-with-chat-buffer
    (pi-send-command
     "get_session_stats"
     '()
     (lambda (resp)
       (let* ((data (plist-get resp :data))
              (tokens (plist-get data :tokens))
              (cost (plist-get data :cost)))
         (pi-widget-save-excursion
           (widget-insert
            (propertize "Session Info\n" 'face 'bold))

           (widget-insert " File: ")
           (widget-create 'file-link
                          :button-prefix ""
                          :button-suffix ""
                          (plist-get data :sessionFile))
           (widget-insert "\n")

           (widget-insert
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

           (widget-insert "\n")

           (pi-insert-stats-section
            "Tokens"
            tokens
            '(("Input" . :input)
              ("Output" . :output)
              ("Cache Read" . :cacheRead)
              ("Total" . :total)))

           (widget-insert "\n")

           (widget-insert
            (propertize "Cost\n" 'face 'bold))

           (widget-insert
            (format " Total: %.4f\n" cost))

           (widget-insert "\n\n")))))))

(provide 'pi)

;;; pi.el ends here
