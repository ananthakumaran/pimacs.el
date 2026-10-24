;;; pimacs-doctor-tests.el --- Tests for pimacs-doctor.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

;; development only packages, not declared as a package-dependency
(package-initialize)

(require 'undercover)
(undercover)

(require 'pimacs-doctor)

(ert-deftest pimacs-doctor-smoke ()
  (save-window-excursion
    (pimacs-doctor)
    (should (derived-mode-p 'pimacs-doctor-mode))))

(ert-deftest pimacs-doctor-find-executable-resolves-explicit-paths ()
  (let* ((file (make-temp-file "pimacs-doctor-"))
         (default-directory (file-name-directory file))
         (relative (concat "./" (file-name-nondirectory file))))
    (unwind-protect
        (progn
          (set-file-modes file #o700)
          (should (equal (pimacs-doctor--find-executable relative) file))
          (should (equal (pimacs-doctor--find-executable file) file))
          (set-file-modes file #o600)
          (should-not (pimacs-doctor--find-executable relative)))
      (delete-file file))))

;;; pimacs-doctor-tests.el ends here
