;;; pi-integration-tests --- This file contains automated integration tests for pi.el -*- lexical-binding: t; -*-

;;; Code:

;; Test setup:

(require 'ert)

;; development only packages, not declared as a package-dependency
(package-initialize)

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

(defmacro pi-with-integration-project (scenario &rest body)
  (declare (indent 1))
  `(let* ((default-directory pi-project-directory)
          (pi-process-environment (list
                                   (concat "FIXTURE_SCENARIO=" ,scenario)
                                   (concat "PI_CODING_AGENT_DIR=" pi-project-agent-directory)
                                   "FIXTURE_MODE=replay"))
          (pi-flags (list "--tools" "read,bash,edit,write,grep,find,ls" "--extension" (expand-file-name "fixture" pi-integration-directory))))
     (pi-chat)
     (sleep-for 2)
     ,@body
     (pi-drain-process-output)
     (pi-with-chat-buffer
       (let* ((tape-file (expand-file-name (concat ,scenario ".txt") pi-tape-directory))
              (current-text (pi-normalize-buffer-text (buffer-substring (point-min) (point-max)))))
         (if (file-exists-p tape-file)
             (let ((expected (pi-normalize-buffer-text (with-temp-buffer
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
                     (delete-file temp-file)))))
           (write-region current-text nil tape-file nil 'silent))))
     (pi-quit-chat)))

(defun pi-drain-process-output (&optional timeout)
  (let* ((timeout (or timeout 30))
         (start (current-time))
         (buffer (pi-current-chat)))
    (sleep-for 0.1)
    (when buffer
      (with-current-buffer buffer
        (while (and pi-agent-state
                    (< (time-to-seconds (time-subtract (current-time) start)) timeout))
          (accept-process-output nil 0.05))))
    (sleep-for 0.1)))

(defun pi-normalize-buffer-text (text)
  (replace-regexp-in-string (regexp-quote pi-project-directory) "PROJECT_DIR" text))

(defun pi-send-prompt-and-wait (prompt)
  (pi-send-prompt prompt)
  (pi-drain-process-output))

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
    (pi-send-prompt-and-wait "delete text.txt")))

;;; pi-tests.el ends here
