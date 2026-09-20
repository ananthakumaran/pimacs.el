;;; pimacs-session-tests --- Tests for persisted Pi sessions -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'pimacs-session)

(defun pimacs-session-tests--write (file contents)
  (with-temp-file file
    (insert contents)))

(ert-deftest pimacs-session-project-directory-encodes-project-root ()
  (should
   (equal (pimacs-session-project-directory "/tmp/sessions/" "/home/user/project/")
          "/tmp/sessions/--home-user-project--")))

(ert-deftest pimacs-session-read-record-reads-bounded-metadata ()
  (let ((file (make-temp-file "pimacs-session-" nil ".jsonl")))
    (unwind-protect
        (progn
          (pimacs-session-tests--write
           file
           (concat
            "{\"type\":\"session\",\"id\":\"session-id\",\"timestamp\":\"2026-01-02T03:04:05Z\",\"cwd\":\"/tmp/project\",\"parentSession\":\"/tmp/parent_1234.jsonl\"}\n"
            "{\"type\":\"session_info\",\"name\":\"named session\"}\n"
            "{\"type\":\"message\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"first line\\nsecond line\"}]}}\n"))
          (let ((record (pimacs-session-read-record file)))
            (should (equal (pimacs-session-record-id record) "session-id"))
            (should (equal (pimacs-session-record-cwd record) "/tmp/project"))
            (should (equal (pimacs-session-record-parent-id record) "1234"))
            (should (equal (pimacs-session-record-name record) "named session"))
            (should (equal (pimacs-session-record-preview record) "first line"))
            (should (equal (pimacs-session-record-path record) file))
            (should (pimacs-session-record-timestamp record))
            (should (pimacs-session-record-modified record))))
      (delete-file file))))

(ert-deftest pimacs-session-formats-timestamps-and-relative-times ()
  (let ((time (encode-time 0 4 3 2 1 2026)))
    (should (equal (pimacs-session-format-timestamp time)
                   "02 Jan 2026, 03:04"))
    (should (equal (pimacs-session-format-timestamp "2026-01-02T03:04:00Z")
                   (format-time-string "%d %b %Y, %R"
                                       (parse-iso8601-time-string "2026-01-02T03:04:00Z"))))
    (cl-letf (((symbol-function 'pimacs--seconds-elapsed-since)
               (lambda (_time) 65)))
      (should (equal (pimacs-session-format-relative-time time)
                     "1 minute ago")))))

(ert-deftest pimacs-session-read-record-respects-max-bytes ()
  (let ((file (make-temp-file "pimacs-session-" nil ".jsonl")))
    (unwind-protect
        (progn
          (pimacs-session-tests--write
           file
           (concat (make-string 64 ? )
                   "{\"type\":\"session\",\"id\":\"session-id\"}\n"))
          (let ((pimacs-session-record-max-bytes 64))
            (should-not (pimacs-session-record-id
                         (pimacs-session-read-record file)))))
      (delete-file file))))

(ert-deftest pimacs-session-read-record-tolerates-malformed-records ()
  (let ((file (make-temp-file "pimacs-session-" nil ".jsonl")))
    (unwind-protect
        (progn
          (pimacs-session-tests--write file "not json\n")
          (let ((record (pimacs-session-read-record file)))
            (should record)
            (should (equal (pimacs-session-record-path record) file))
            (should-not (pimacs-session-record-id record))))
      (delete-file file))))

(ert-deftest pimacs-session-recent-files-scopes-sorts-and-limits ()
  (let* ((directory (make-temp-file "pimacs-sessions-" t))
         (nested (expand-file-name "nested" directory))
         (older (expand-file-name "older.jsonl" directory))
         (newer (expand-file-name "newer.jsonl" nested))
         (not-a-file (expand-file-name "directory.jsonl" directory)))
    (unwind-protect
        (progn
          (make-directory nested)
          (make-directory not-a-file)
          (pimacs-session-tests--write older "{}\n")
          (pimacs-session-tests--write newer "{}\n")
          (set-file-times older (seconds-to-time 1))
          (set-file-times newer (seconds-to-time 2))
          (should (equal (pimacs-session-recent-files directory nil 10)
                         (list older)))
          (should (equal (pimacs-session-recent-files directory t 1)
                         (list newer))))
      (delete-directory directory t))))

(ert-deftest pimacs-session-read-record-skips-missing-files ()
  (should-not (pimacs-session-read-record
               (expand-file-name "missing.jsonl"
                                 (make-temp-name temporary-file-directory)))))

;;; pimacs-session-tests.el ends here
