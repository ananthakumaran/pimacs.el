;;; pimacs-integration-tests --- This file contains automated integration tests for pimacs.el -*- lexical-binding: t; -*-

;;; Code:

;; Test setup:

(require 'cl-lib)
(require 'ert)

;; development only packages, not declared as a package-dependency
(package-initialize)

(require 'undercover)
(undercover)

(require 'pimacs)

(defun pimacs-project-try-project (dir)
  (let ((root (locate-dominating-file dir ".project")))
    (when root
      (cons 'transient root))))

(add-hook 'project-find-functions #'pimacs-project-try-project)

(defconst pimacs-integration-directory
  (file-name-directory
   (or (and load-file-name
            (file-truename load-file-name))
       (and buffer-file-name
            (file-truename buffer-file-name)))))

(defconst pimacs-tape-directory (expand-file-name "fixture/tapes" pimacs-integration-directory))
(defconst pimacs-project-directory (expand-file-name "project" pimacs-integration-directory))
(defconst pimacs-project-agent-directory (expand-file-name "project/agent" pimacs-integration-directory))

(defun pimacs-fixture-mode ()
  (or (getenv "FIXTURE_MODE") "replay"))

(defconst pimacs-silenced-integration-message-patterns
  '("^(.*) Starting pimacs version .+\.\.\.$"
    "^(.*) pimacs agent started successfully\.$"
    "^(.*) pimacs exits: killed\.$"
    "^Copied last assistant message to clipboard\.$"))

(defmacro pimacs-with-silenced-integration-messages (&rest body)
  (declare (indent 0))
  `(let* ((message-fn (symbol-function 'message))
          (settings-file (expand-file-name "settings.json" pimacs-project-agent-directory))
          (original-settings (with-temp-buffer
                               (insert-file-contents settings-file)
                               (buffer-string))))
     (unwind-protect
         (cl-letf (((symbol-function 'message)
                    (lambda (format-string &rest args)
                      (let ((text (apply #'format-message format-string args)))
                        (unless (cl-some (lambda (pattern)
                                           (string-match-p pattern text))
                                         pimacs-silenced-integration-message-patterns)
                          (funcall message-fn "%s" text))))))
           ,@body)
       ;; Restore settings file to original content
       (write-region original-settings nil settings-file nil 'silent))))

(defmacro pimacs-with-integration-project (scenario &rest body)
  (declare (indent 1))
  `(pimacs-with-silenced-integration-messages
     (let* ((default-directory pimacs-project-directory)
            (pimacs-process-environment (list
                                         (concat "FIXTURE_SCENARIO=" ,scenario)
                                         (concat "PI_CODING_AGENT_DIR=" pimacs-project-agent-directory)
                                         (concat "FIXTURE_MODE=" (pimacs-fixture-mode))))
            (pimacs-flags (list "--tools" "read,bash,edit,write,grep,find,ls" "--extension" (expand-file-name "fixture" pimacs-integration-directory))))
       (let ((sessions-dir (expand-file-name "sessions" pimacs-project-agent-directory)))
         (when (file-exists-p sessions-dir)
           (delete-directory sessions-dir t)
           (make-directory sessions-dir)))
       (pimacs-chat)
       (sleep-for 2)
       ,@body
       (pimacs-drain-process-output)
       (pimacs--with-chat-buffer
         (let* ((tape-file (expand-file-name (concat ,scenario ".txt") pimacs-tape-directory))
                (current-text (pimacs-normalize-buffer-text (buffer-substring (point-min) (point-max))))
                (fixture-mode (pimacs-fixture-mode)))
           (if (or (not (file-exists-p tape-file))
                   (string= fixture-mode "record"))
               (write-region current-text nil tape-file nil 'silent)
             (let ((expected (pimacs-normalize-buffer-text
                              (with-temp-buffer
                                (insert-file-contents tape-file)
                                (buffer-string)))))
               (unless (string= current-text expected)
                 (let ((temp-file (make-temp-file "pimacs-tape-")))
                   (unwind-protect
                       (progn
                         (write-region current-text nil temp-file nil 'silent)
                         (with-temp-buffer
                           (call-process "diff" nil (current-buffer) nil "-u" tape-file temp-file)
                           (message "Tape mismatch for %s:\n%s" ,scenario (buffer-string))
                           (ert-fail (format "Tape mismatch for %s" ,scenario))))
                     (delete-file temp-file))))))))

       (pimacs-quit-chat))))

(defvar pimacs-settle-time (if (getenv "CI") 1 0.1))
(defvar pimacs-poll-interval (if (getenv "CI") 0.5 0.05))

(defun pimacs-drain-process-output (&optional timeout)
  (let* ((timeout (or timeout 120))
         (start (current-time))
         (buffer (pimacs--current-chat)))
    (sleep-for pimacs-settle-time)
    (when buffer
      (with-current-buffer buffer
        (while (and pimacs--agent-state
                    (< (time-to-seconds (time-subtract (current-time) start)) timeout))
          (accept-process-output nil pimacs-poll-interval))))
    (sleep-for pimacs-settle-time)))

(defmacro pimacs-with-editor-buffer (&rest body)
  (declare (indent 0))
  `(progn
     (pimacs-drain-process-output)
     (let ((buffer (get-buffer "*pimacs-edit*")))
       (when buffer
         (with-current-buffer buffer
           ,@body)))
     (pimacs-drain-process-output)))

(defun pimacs-normalize-buffer-text (text)
  (let ((session_dir (concat "--" (replace-regexp-in-string "/" "-"
                                                            (substring pimacs-project-directory 1))
                             "--")))
    (->> text
         (replace-regexp-in-string (regexp-quote pimacs-project-directory) "PROJECT_DIR")
         (replace-regexp-in-string (regexp-quote session_dir) "SESSION_DIR")
         (replace-regexp-in-string "\\b[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}" "UUID")
         (replace-regexp-in-string "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{3\\}Z" "TIMESTAMP"))))

(defun pimacs-send-prompt-and-wait (prompt)
  (pimacs-send-prompt prompt)
  (pimacs-drain-process-output))

(defmacro pimacs-with-minibuffer-input (input &rest body)
  (declare (indent 1))
  `(let ((executing-kbd-macro t)
         (completion-styles '(flex))
         (unread-command-events
          (append (listify-key-sequence ,input)
                  unread-command-events)))
     ,@body))

(ert-deftest pimacs-basics ()
  (pimacs-with-integration-project "basics"
    (pimacs-send-prompt-and-wait "list files")
    (pimacs-send-prompt-and-wait "grep for sample")
    (pimacs-send-prompt-and-wait "create a new filed name test.txt")
    (pimacs-send-prompt-and-wait "delete test.txt")
    (pimacs-send-prompt-and-wait "find files with json extension")
    (pimacs-send-prompt-and-wait "read utils.py file")
    (pimacs-send-prompt-and-wait "create test.txt with some text")
    (pimacs-send-prompt-and-wait "remove the 3rd line using edit tool")
    (pimacs-send-prompt-and-wait "delete text.txt")
    (pimacs-send-prompt-and-wait "/export /tmp/pimacs-session.html")))

(ert-deftest pimacs-slash ()
  (pimacs-with-integration-project "slash"
    (pimacs-send-prompt-and-wait "/new")
    (pimacs-send-prompt-and-wait "/name session1")
    (pimacs-send-prompt-and-wait "/session")
    (pimacs-send-prompt-and-wait "hello")
    (pimacs-send-prompt-and-wait "/copy")
    (pimacs-with-minibuffer-input "n"
      (pimacs-send-prompt-and-wait "/set-auto-compaction"))
    (pimacs-with-minibuffer-input "y"
      (pimacs-send-prompt-and-wait "/set-auto-compaction"))
    (pimacs-with-minibuffer-input "(fixture) qwen3.5:0.8b"
      (pimacs-send-prompt-and-wait "/model"))
    (pimacs-with-minibuffer-input "minimal (Very brief reasoning ~1k tokens)"
      (pimacs-send-prompt-and-wait "/set-thinking-level"))
    (pimacs-send-prompt-and-wait "/cycle-thinking-level")
    (pimacs-with-minibuffer-input "n"
      (pimacs-send-prompt-and-wait "/set-auto-retry"))
    (pimacs-with-minibuffer-input "y"
      (pimacs-send-prompt-and-wait "/set-auto-retry"))
    (pimacs-with-minibuffer-input "One at a time"
      (pimacs-send-prompt-and-wait "/set-steering-mode"))
    (pimacs-with-minibuffer-input "All"
      (pimacs-send-prompt-and-wait "/set-follow-up-mode"))))

(ert-deftest pimacs-session ()
  (pimacs-with-integration-project "session"
    (pimacs-send-prompt-and-wait "/new")
    (pimacs-send-prompt-and-wait "/name test-session")
    (pimacs-send-prompt-and-wait "/session")
    (pimacs-send-prompt-and-wait "say hello")
    (pimacs-send-prompt-and-wait "/session")))

(ert-deftest pimacs-clone ()
  (pimacs-with-integration-project "clone"
    (pimacs-send-prompt-and-wait "/name clone-test")
    (pimacs-send-prompt-and-wait "say hello")
    (pimacs-send-prompt-and-wait "tell me a story, 100 words")
    (pimacs-send-prompt-and-wait "/compact")
    (pimacs-with-minibuffer-input "high (Deep reasoning ~16k tokens)"
      (pimacs-send-prompt-and-wait "/set-thinking-level"))
    (pimacs-send-prompt-and-wait "/clone")
    (pimacs-send-prompt-and-wait "cloned")))

(ert-deftest pimacs-fork ()
  (pimacs-with-integration-project "fork"
    (pimacs-send-prompt-and-wait "hello")
    (pimacs-send-prompt-and-wait "hello again")
    (pimacs-with-minibuffer-input "hello again"
      (pimacs-send-prompt-and-wait "/fork"))
    (pimacs-send-prompt-and-wait "hello fork")))

(ert-deftest pimacs-resume ()
  (pimacs-with-integration-project "resume"
    (pimacs-send-prompt-and-wait "/name sessionv1")
    (pimacs-send-prompt-and-wait "h1")
    (pimacs-send-prompt-and-wait "h2")
    (pimacs-send-prompt-and-wait "!ls -1 | LC_ALL=C sort")
    (pimacs-send-prompt-and-wait "/new")
    (pimacs-send-prompt-and-wait "/name sessionv2")
    (pimacs-with-minibuffer-input (kbd "sessionv1 TAB RET")
      (pimacs-send-prompt-and-wait "/resume"))
    (pimacs-send-prompt-and-wait "h3")))

(ert-deftest pimacs-compact ()
  (pimacs-with-integration-project "compact"
    (pimacs-send-prompt-and-wait "hello")
    (pimacs-send-prompt-and-wait "tell me a story, 100 words")
    (pimacs-send-prompt-and-wait "/compact")
    (pimacs-send-prompt-and-wait "hello again")))

(ert-deftest pimacs-followup ()
  (pimacs-with-integration-project "followup"
    (pimacs-send-prompt "hello")
    (pimacs-send-prompt "follow up 1")
    (pimacs-send-prompt "follow up 2")
    (pimacs-drain-process-output)
    (pimacs-send-prompt-and-wait "hello again")))

(ert-deftest pimacs-steer ()
  (pimacs-with-integration-project "steer"
    (pimacs-send-prompt "hello")
    (pimacs-send-prompt-alternate "hello 1")
    (pimacs-send-prompt-alternate "hello 2")
    (pimacs-drain-process-output)
    (pimacs-send-prompt-and-wait "hello again")))

(ert-deftest pimacs-send-region ()
  (pimacs-with-integration-project "insert-region"
    (pimacs-send-prompt-and-wait "say hello")
    (with-temp-buffer
      (insert "hello again")
      (let ((start (point-min))
            (end (point-max)))
        (pimacs-send-region start end)))
    (pimacs-send-prompt-and-wait (widget-value pimacs--prompt-widget))))

(ert-deftest pimacs-reload ()
  (pimacs-with-integration-project "reload"
    (pimacs-send-prompt-and-wait "hello")
    (pimacs-send-prompt "/reload")
    (sleep-for 3)
    (pimacs-send-prompt-and-wait "hello")))


(ert-deftest pimacs-extension-ui ()
  (pimacs-with-integration-project "extension-ui"
    (pimacs-send-prompt-and-wait "/rpc-notify")

    (pimacs-with-minibuffer-input "test value"
      (pimacs-send-prompt-and-wait "/rpc-input"))

    (pimacs-with-minibuffer-input "y"
      (pimacs-send-prompt-and-wait "/rpc-confirm"))

    (pimacs-with-minibuffer-input "n"
      (pimacs-send-prompt-and-wait "/rpc-confirm"))

    (pimacs-with-minibuffer-input (kbd "C-g")
      (pimacs-send-prompt-and-wait "/rpc-confirm"))

    (pimacs-with-minibuffer-input "Option B"
      (pimacs-send-prompt-and-wait "/rpc-select"))

    (pimacs-with-minibuffer-input (kbd "C-g")
      (pimacs-send-prompt-and-wait "/rpc-select"))

    (pimacs-send-prompt-and-wait "/rpc-set-editor-text")

    (pimacs-send-prompt-and-wait (widget-value pimacs--prompt-widget))

    (pimacs-send-prompt "/rpc-editor")
    (pimacs-with-editor-buffer
      (goto-char (point-max))
      (insert "\nnew line")
      (pimacs-edit-finish))

    (pimacs-send-prompt "/rpc-editor")
    (pimacs-with-editor-buffer
      (pimacs-edit-cancel))

    (pimacs-send-prompt-and-wait "/rpc-set-widget")

    (pimacs-send-prompt-and-wait "/rpc-set-status")

    (pimacs-send-prompt-and-wait "/rpc-set-title")))

;;; pimacs-tests.el ends here
