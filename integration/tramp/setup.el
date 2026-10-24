;;; setup.el --- Disposable SSH fixture configuration -*- lexical-binding: t; -*-

(require 'auth-source)
(require 'tramp-sh)

(defvar pimacs-integration-remote-directory)

(unless (equal (or (getenv "FIXTURE_MODE") "replay") "replay")
  (error "The SSH integration fixture currently supports replay only"))

(let* ((vec (tramp-dissect-file-name pimacs-integration-remote-directory))
       (auth-file
        (make-temp-file "pimacs-tramp-auth-" nil nil
                        (format "machine %s port ssh login pimacs password pimacs\n"
                                (tramp-file-name-host-port vec)))))
  (setq auth-sources (list auth-file)
        auth-source-do-cache nil
        enable-remote-dir-locals t
        tramp-verbose 2
        vc-handled-backends nil
        tramp-persistency-file-name nil
        tramp-cache-read-persistent-data t)
  (connection-local-set-profile-variables
   'pimacs-integration
   `((pimacs-executable . "/opt/pimacs-test/bin/pi")
     (pimacs-session-directory
      . ,(expand-file-name "project/agent/sessions" pimacs-integration-remote-directory))))
  (connection-local-set-profiles
   `(:application tramp :protocol ,(tramp-file-name-method vec)
     :user ,(tramp-file-name-user-domain vec) :machine ,(tramp-file-name-host-port vec))
   'pimacs-integration)
  (add-hook 'kill-emacs-hook (lambda () (delete-file auth-file)))
  (tramp-set-connection-property
   vec "login-args"
   '(("-F" "/dev/null")
     ("-o" "StrictHostKeyChecking=no")
     ("-o" "UserKnownHostsFile=/dev/null")
     ("-o" "GlobalKnownHostsFile=/dev/null")
     ("-o" "PubkeyAuthentication=no")
     ("-o" "PreferredAuthentications=password")
     ("-o" "ConnectTimeout=5")
     ("-l" "%u") ("-p" "%p") ("-e" "none") ("%h")))
  (dolist (path '("/opt/pimacs-test/bin/pi" "/srv/pimacs-tramp"))
    (when (file-exists-p path)
      (error "Remote-only fixture path exists on the client: %s" path))))
