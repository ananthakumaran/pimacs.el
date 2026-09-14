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
;;; pimacs-doctor-tests.el ends here
