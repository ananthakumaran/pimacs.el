;;; pimacs-session.el --- Persisted Pi sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Anantha Kumaran.

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Persisted Pi session support.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'parse-time)
(require 'seq)
(require 'subr-x)
(require 'pimacs-utils)

(defcustom pimacs-session-directory
  (expand-file-name "sessions/" "~/.pi/agent/")
  "Root directory containing persisted Pi session files."
  :type 'directory
  :group 'pimacs)

(defcustom pimacs-session-record-max-bytes (* 10 1024)
  "Maximum number of bytes read from a session file for its record."
  :type 'integer
  :group 'pimacs)

(cl-defstruct pimacs-session-record
  id timestamp modified cwd path parent-id name preview)

(defun pimacs-session--timestamp-time (timestamp)
  (if (stringp timestamp)
      (condition-case nil
          (parse-iso8601-time-string timestamp)
        (error nil))
    timestamp))

(defun pimacs-session-format-timestamp (timestamp)
  (when-let ((time (pimacs-session--timestamp-time timestamp)))
    (condition-case nil
        (format-time-string "%d %b %Y, %R" time)
      (error nil))))

(defun pimacs-session-format-relative-time (timestamp)
  (when-let ((time (pimacs-session--timestamp-time timestamp)))
    (condition-case nil
        (let ((seconds (max 0
                            (floor (pimacs--seconds-elapsed-since time)))))
          (if (< seconds 60)
              "just now"
            (concat
             (cond
              ((< seconds 3600) (format-seconds "%M" seconds))
              ((< seconds 86400) (format-seconds "%H" seconds))
              (t (format-seconds "%D" seconds)))
             " ago")))
      (error nil))))

(defun pimacs-session-project-directory (session-directory project-root)
  (let* ((project-root (directory-file-name (expand-file-name project-root)))
         (path (replace-regexp-in-string "\\`[/\\\\]+" "" project-root))
         (path (replace-regexp-in-string "[:/\\\\]" "-" path)))
    (expand-file-name (format "--%s--" path) session-directory)))

(defun pimacs-session--regular-file-p (file)
  (condition-case nil
      (and (file-regular-p file)
           (string-match-p "\\.jsonl\\'" file))
    (file-error nil)))

(defun pimacs-session-modification-time (file)
  (condition-case nil
      (file-attribute-modification-time (file-attributes file))
    (file-error nil)))

(defun pimacs-session-recent-files (directory recursive limit)
  (let ((files
         (condition-case nil
             (if (not (file-directory-p directory))
                 nil
               (if recursive
                   (directory-files-recursively directory "\\.jsonl\\'" nil nil nil)
                 (directory-files directory t "\\.jsonl\\'" t)))
           (file-error nil))))
    (setq files (seq-filter #'pimacs-session--regular-file-p files))
    (setq files
          (sort files
                (lambda (left right)
                  (time-less-p (or (pimacs-session-modification-time right)
                                   (seconds-to-time 0))
                               (or (pimacs-session-modification-time left)
                                   (seconds-to-time 0))))))
    (seq-take files (max 0 limit))))

(defun pimacs-session--parent-id (parent-session)
  (when parent-session
    (car (last (split-string
                (file-name-sans-extension (file-name-nondirectory parent-session))
                "_")))))

(defun pimacs-session--content-preview (content)
  (let ((items (if (stringp content)
                   (list (list :type "text" :text content))
                 content)))
    (when-let ((item (cl-find-if (lambda (entry)
                                   (member (plist-get entry :type)
                                           '("text" "thinking")))
                                 items)))
      (pimacs--section-header (or (plist-get item :text)
                                  (plist-get item :thinking))))))

(defun pimacs-session-read-record (file)
  (when (pimacs-session--regular-file-p file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file nil 0 (max 0 pimacs-session-record-max-bytes))
          (goto-char (point-min))
          (let ((id nil)
                (timestamp nil)
                (cwd nil)
                (parent-id nil)
                (preview nil)
                (name nil)
                (lines-read 0))
            (while (and (< lines-read 20) (not (eobp)))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
                (unless (string-empty-p line)
                  (condition-case nil
                      (let ((record (json-parse-string line :object-type 'plist)))
                        (pcase (intern (plist-get record :type))
                          ('session
                           (setq id (plist-get record :id)
                                 timestamp (plist-get record :timestamp)
                                 cwd (plist-get record :cwd)
                                 parent-id (pimacs-session--parent-id
                                            (plist-get record :parentSession))))
                          ('session_info
                           (setq name (plist-get record :name)))
                          ('message
                           (unless preview
                             (setq preview
                                   (pimacs-session--content-preview
                                    (plist-get (plist-get record :message) :content)))))))
                    (error nil))))
              (forward-line 1)
              (cl-incf lines-read))
            (make-pimacs-session-record
             :id id
             :timestamp (when timestamp
                          (condition-case nil
                              (parse-iso8601-time-string timestamp)
                            (error nil)))
             :modified (pimacs-session-modification-time file)
             :cwd cwd
             :path file
             :parent-id parent-id
             :name name
             :preview preview)))
      (file-error nil))))

(defun pimacs-session-recent-records (directory recursive limit)
  (delq nil
        (mapcar #'pimacs-session-read-record
                (pimacs-session-recent-files directory recursive limit))))

(provide 'pimacs-session)

;;; pimacs-session.el ends here
