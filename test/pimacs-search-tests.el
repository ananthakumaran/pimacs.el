;;; pimacs-search-tests --- Tests for historical session search -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'pimacs-search)

(ert-deftest pimacs-search-default-request-uses-session-directory ()
  (let ((pimacs-session-directory "/tmp/sessions/")
        (pimacs-search-default-scope 'current-project))
    (cl-letf (((symbol-function 'pimacs--project-root)
               (lambda () "/tmp/project/")))
      (let ((request (pimacs-search--default-request)))
        (should (equal (pimacs-search-request-directory request)
                       "/tmp/sessions/"))
        (should (equal (pimacs-search--request-directory request)
                       "/tmp/sessions/--tmp-project--"))))))

;;; pimacs-search-tests.el ends here
