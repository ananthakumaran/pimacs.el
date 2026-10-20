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
        tramp-persistency-file-name nil
        tramp-cache-read-persistent-data t
        pimacs-executable "/opt/pimacs-test/bin/pi")
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
  (dolist (path (list pimacs-executable "/srv/pimacs-tramp"))
    (when (file-exists-p path)
      (error "Remote-only fixture path exists on the client: %s" path))))
