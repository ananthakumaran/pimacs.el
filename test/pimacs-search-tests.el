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

(ert-deftest pimacs-search-status-update-preserves-reading-point ()
  (with-temp-buffer
    (pimacs-section--create-root-section)
    (setq pimacs-search--status-section
          (pimacs-section--create-section 'info pimacs-section--root-section
            (insert "Searching...")))
    (goto-char (+ (pimacs-section-beginning pimacs-search--status-section) 5))
    (pimacs-search--set-status "Search complete")
    (should (= (point) (+ (pimacs-section-beginning pimacs-search--status-section) 5)))))

;;; pimacs-search-tests.el ends here
