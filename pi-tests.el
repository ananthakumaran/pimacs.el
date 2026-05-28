;;; pi-tests --- This file contains automated tests for pi.el -*- lexical-binding: t; -*-

;;; Code:

;; Test setuup:

(require 'ert)

;; development only packages, not declared as a package-dependency
(package-initialize)

(ert-deftest pi-hello-tests ()
  (should (equal t t)))

(require 'pi)

;;; pi-tests.el ends here

