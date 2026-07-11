;;; pi-agent.el --- Agent (RPC) support -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; RPC client for communicating with the Pi coding agent process.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pi-utils)
(require 'pi-core)

(defvar pi--minimum-version "0.80.3"
  "The minimum supported Pi agent version.")

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

(defun pi--maybe-log-rpc (type json)
  (when pi-log-rpc
    (write-region (concat "{\"type\": \"" type "\", \"message\": " json "}\n") nil pi-log-rpc-file t 'inhibit-message)))


(defun pi--response-success-p (response)
  (and response
       (plist-get response :success)
       (not (eq (plist-get response :success) 'json-false))))

(defvar pi--agents (make-hash-table :test 'equal))
(defvar pi--response-callbacks (make-hash-table :test 'equal))

(defvar pi--event-listeners (make-hash-table :test 'equal))

(defvar pi--request-counter 0)

(defun pi--current-agent ()
  (gethash (pi--project-key) pi--agents))

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

(provide 'pi-agent)

;;; pi-agent.el ends here
