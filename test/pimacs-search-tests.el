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

(ert-deftest pimacs-search-resume-new-buffer-passes-new-buffer-option ()
  (with-temp-buffer
    (setq pimacs-search--session-metadata (make-hash-table :test 'equal))
    (puthash "/tmp/session.jsonl" '(:cwd "/tmp/project/")
             pimacs-search--session-metadata)
    (let* (arguments
           (pimacs-search-resume-function
            (lambda (&rest args) (setq arguments args))))
      (cl-letf (((symbol-function 'pimacs-search--resume-info-at-point)
                 (lambda () (make-pimacs-search-result-info :path "/tmp/session.jsonl"))))
        (pimacs-search-resume-at-point-new-buffer)
        (should (equal arguments '("/tmp/session.jsonl" "/tmp/project/" t)))))))

;;; pimacs-search-tests.el ends here
