;;; pi-integration-tests --- This file contains automated integration tests for pi.el -*- lexical-binding: t; -*-

;;; Code:

;; Test setup:

(require 'ert)

;; development only packages, not declared as a package-dependency
(package-initialize)

(require 'undercover)
(undercover "*.el"
            (:report-format 'codecov)
            (:send-report nil)
            (:exclude "*-tests.el"))

(require 'pi)

(defun pi-project-try-project (dir)
  (let ((root (locate-dominating-file dir ".project")))
    (when root
      (cons 'transient root))))

(add-hook 'project-find-functions #'pi-project-try-project)

(defconst pi-integration-directory
  (file-name-directory
   (or (and load-file-name
            (file-truename load-file-name))
       (and buffer-file-name
            (file-truename buffer-file-name)))))

(defconst pi-tape-directory (expand-file-name "fixture/tapes" pi-integration-directory))
(defconst pi-project-directory (expand-file-name "project" pi-integration-directory))
(defconst pi-project-agent-directory (expand-file-name "project/agent" pi-integration-directory))

(defun pi-fixture-mode ()
  (or (getenv "FIXTURE_MODE") "replay"))

(defmacro pi-with-integration-project (scenario &rest body)
  (declare (indent 1))
  `(let* ((default-directory pi-project-directory)
          (pi-process-environment (list
                                   (concat "FIXTURE_SCENARIO=" ,scenario)
                                   (concat "PI_CODING_AGENT_DIR=" pi-project-agent-directory)
                                   (concat "FIXTURE_MODE=" (pi-fixture-mode))))
          (pi-flags (list "--tools" "read,bash,edit,write,grep,find,ls" "--extension" (expand-file-name "fixture" pi-integration-directory))))
     (let ((sessions-dir (expand-file-name "sessions" pi-project-agent-directory)))
       (when (file-exists-p sessions-dir)
         (delete-directory sessions-dir t)
         (make-directory sessions-dir)))
     (pi-chat)
     (sleep-for 2)
     ,@body
     (pi-drain-process-output)
     (pi-with-chat-buffer
       (let* ((tape-file (expand-file-name (concat ,scenario ".txt") pi-tape-directory))
              (current-text (pi-normalize-buffer-text (buffer-substring (point-min) (point-max))))
              (fixture-mode (pi-fixture-mode)))
         (if (or (not (file-exists-p tape-file))
                 (string= fixture-mode "record"))
             (write-region current-text nil tape-file nil 'silent)
           (let ((expected (pi-normalize-buffer-text
                            (with-temp-buffer
                              (insert-file-contents tape-file)
                              (buffer-string)))))
             (unless (string= current-text expected)
               (let ((temp-file (make-temp-file "pi-tape-")))
                 (unwind-protect
                     (progn
                       (write-region current-text nil temp-file nil 'silent)
                       (with-temp-buffer
                         (call-process "diff" nil (current-buffer) nil "-u" tape-file temp-file)
                         (message "Tape mismatch for %s:\n%s" ,scenario (buffer-string))
                         (ert-fail (format "Tape mismatch for %s" ,scenario))))
                   (delete-file temp-file))))))))

     (pi-quit-chat)))

(defvar pi-settle-time (if (getenv "CI") 1 0.1))
(defvar pi-poll-interval (if (getenv "CI") 0.5 0.05))

(defun pi-drain-process-output (&optional timeout)
  (let* ((timeout (or timeout 120))
         (start (current-time))
         (buffer (pi-current-chat)))
    (sleep-for pi-settle-time)
    (when buffer
      (with-current-buffer buffer
        (while (and pi-agent-state
                    (< (time-to-seconds (time-subtract (current-time) start)) timeout))
          (accept-process-output nil pi-poll-interval))))
    (sleep-for pi-settle-time)))

(defmacro pi-with-editor-buffer (&rest body)
  (declare (indent 0))
  `(progn
     (pi-drain-process-output)
     (let ((buffer (get-buffer "*pi-edit*")))
       (when buffer
         (with-current-buffer buffer
           ,@body)))
     (pi-drain-process-output)))

(defun pi-normalize-buffer-text (text)
  (let ((session_dir (concat "--" (replace-regexp-in-string "/" "-"
                                                            (substring pi-project-directory 1))
                             "--")))
    (->> text
         (replace-regexp-in-string (regexp-quote pi-project-directory) "PROJECT_DIR")
         (replace-regexp-in-string (regexp-quote session_dir) "SESSION_DIR")
         (replace-regexp-in-string "\\b[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}" "UUID")
         (replace-regexp-in-string "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{3\\}Z" "TIMESTAMP"))))

(defun pi-send-prompt-and-wait (prompt)
  (pi-send-prompt prompt)
  (pi-drain-process-output))

(defmacro pi-with-minibuffer-input (input &rest body)
  (declare (indent 1))
  `(let ((executing-kbd-macro t)
         (completion-styles '(flex))
         (unread-command-events
          (append (listify-key-sequence ,input)
                  unread-command-events)))
     ,@body))

(ert-deftest pi-basics ()
  (pi-with-integration-project "basics"
    (pi-send-prompt-and-wait "list files")
    (pi-send-prompt-and-wait "grep for sample")
    (pi-send-prompt-and-wait "create a new filed name test.txt")
    (pi-send-prompt-and-wait "delete test.txt")
    (pi-send-prompt-and-wait "find files with json extension")
    (pi-send-prompt-and-wait "read utils.py file")
    (pi-send-prompt-and-wait "create test.txt with some text")
    (pi-send-prompt-and-wait "remove the 3rd line using edit tool")
    (pi-send-prompt-and-wait "delete text.txt")
    (pi-send-prompt-and-wait "/export /tmp/pi-session.html")))

(ert-deftest pi-slash ()
  (pi-with-integration-project "slash"
    (pi-send-prompt-and-wait "/new")
    (pi-send-prompt-and-wait "/name session1")
    (pi-send-prompt-and-wait "/session")
    (pi-send-prompt-and-wait "hello")
    (pi-send-prompt-and-wait "/copy")
    (pi-with-minibuffer-input "n"
      (pi-send-prompt-and-wait "/set-auto-compaction"))
    (pi-with-minibuffer-input "y"
      (pi-send-prompt-and-wait "/set-auto-compaction"))
    (pi-with-minibuffer-input "(fixture) qwen3.5:4b"
      (pi-send-prompt-and-wait "/model"))
    (pi-with-minibuffer-input "minimal (Very brief reasoning ~1k tokens)"
      (pi-send-prompt-and-wait "/set-thinking-level"))
    (pi-send-prompt-and-wait "/cycle-thinking-level")
    (pi-with-minibuffer-input "n"
      (pi-send-prompt-and-wait "/set-auto-retry"))
    (pi-with-minibuffer-input "y"
      (pi-send-prompt-and-wait "/set-auto-retry"))
    (pi-with-minibuffer-input "One at a time"
      (pi-send-prompt-and-wait "/set-steering-mode"))
    (pi-with-minibuffer-input "All"
      (pi-send-prompt-and-wait "/set-follow-up-mode"))))

(ert-deftest pi-session ()
  (pi-with-integration-project "session"
    (pi-send-prompt-and-wait "/new")
    (pi-send-prompt-and-wait "/name test-session")
    (pi-send-prompt-and-wait "/session")
    (pi-send-prompt-and-wait "say hello")
    (pi-send-prompt-and-wait "/session")))

(ert-deftest pi-clone ()
  (pi-with-integration-project "clone"
    (pi-send-prompt-and-wait "say hello")
    (pi-send-prompt-and-wait "/session")
    (pi-send-prompt-and-wait "/clone")
    (pi-send-prompt-and-wait "cloned")))

(ert-deftest pi-fork ()
  (pi-with-integration-project "fork"
    (pi-send-prompt-and-wait "hello")
    (pi-send-prompt-and-wait "hello again")
    (pi-with-minibuffer-input "hello again"
      (pi-send-prompt-and-wait "/fork"))
    (pi-send-prompt-and-wait "hello fork")))

(ert-deftest pi-resume ()
  (pi-with-integration-project "resume"
    (pi-send-prompt-and-wait "/name sessionv1")
    (pi-send-prompt-and-wait "h1")
    (pi-send-prompt-and-wait "h2")
    (pi-send-prompt-and-wait "!ls -1 | LC_ALL=C sort")
    (pi-send-prompt-and-wait "/new")
    (pi-send-prompt-and-wait "/name sessionv2")
    (pi-with-minibuffer-input (kbd "sessionv1 TAB RET")
      (pi-send-prompt-and-wait "/resume"))
    (pi-send-prompt-and-wait "h3")))

(ert-deftest pi-compact ()
  (pi-with-integration-project "compact"
    (pi-send-prompt-and-wait "hello")
    (pi-send-prompt-and-wait "display a sample markdown document with examples, don't create any file")
    (pi-send-prompt-and-wait "!ls -1 | LC_ALL=C sort")
    (pi-send-prompt-and-wait "!cat README.md")
    (pi-send-prompt-and-wait "!cat config.json")
    (pi-send-prompt-and-wait "!cat notes.txt")
    (pi-send-prompt-and-wait "!cat utils.py")
    (pi-send-prompt-and-wait "/compact")
    (pi-send-prompt-and-wait "hello again")))

(ert-deftest pi-followup ()
  (pi-with-integration-project "followup"
    (pi-send-prompt "hello")
    (pi-send-prompt "follow up 1")
    (pi-send-prompt "follow up 2")
    (pi-drain-process-output)
    (pi-send-prompt-and-wait "hello again")))

(ert-deftest pi-steer ()
  (pi-with-integration-project "steer"
    (pi-send-prompt "hello")
    (pi-send-prompt-alternate "hello 1")
    (pi-send-prompt-alternate "hello 2")
    (pi-drain-process-output)
    (pi-send-prompt-and-wait "hello again")))

(ert-deftest pi-send-region ()
  (pi-with-integration-project "insert-region"
    (pi-send-prompt-and-wait "say hello")
    (with-temp-buffer
      (insert "hello again")
      (let ((start (point-min))
            (end (point-max)))
        (pi-send-region start end)))
    (pi-send-prompt-and-wait (widget-value pi-prompt-widget))))

(ert-deftest pi-extension-ui ()
  (pi-with-integration-project "extension-ui"
    (pi-send-prompt-and-wait "/rpc-notify")

    (pi-with-minibuffer-input "test value"
      (pi-send-prompt-and-wait "/rpc-input"))

    (pi-with-minibuffer-input "y"
      (pi-send-prompt-and-wait "/rpc-confirm"))

    (pi-with-minibuffer-input "n"
      (pi-send-prompt-and-wait "/rpc-confirm"))

    (pi-with-minibuffer-input (kbd "C-g")
      (pi-send-prompt-and-wait "/rpc-confirm"))

    (pi-with-minibuffer-input "Option B"
      (pi-send-prompt-and-wait "/rpc-select"))

    (pi-with-minibuffer-input (kbd "C-g")
      (pi-send-prompt-and-wait "/rpc-select"))

    (pi-send-prompt-and-wait "/rpc-set-editor-text")

    (pi-send-prompt-and-wait (widget-value pi-prompt-widget))

    (pi-send-prompt "/rpc-editor")
    (pi-with-editor-buffer
      (goto-char (point-max))
      (insert "\nnew line")
      (pi-edit-finish))

    (pi-send-prompt "/rpc-editor")
    (pi-with-editor-buffer
      (pi-edit-cancel))

    (pi-send-prompt-and-wait "/rpc-set-widget")

    (pi-send-prompt-and-wait "/rpc-set-status")

    (pi-send-prompt-and-wait "/rpc-set-title")))

;;; pi-tests.el ends here
