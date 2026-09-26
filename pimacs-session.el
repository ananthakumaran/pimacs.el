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
(require 'pimacs-core)

(defcustom pimacs-session-directory
  (expand-file-name "sessions/" "~/.pi/agent/")
  "Root directory containing persisted Pi session files."
  :type 'directory
  :group 'pimacs)

(defcustom pimacs-session-record-max-bytes (* 100 1024)
  "Maximum number of bytes read from a session file for its record."
  :type 'integer
  :group 'pimacs)

(cl-defstruct pimacs-session-record
  id timestamp modified cwd path parent-path parent-id name preview)

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
      (when-let ((header (car (split-string (or (pimacs--json-get item :text)
                                                (pimacs--json-get item :thinking))
                                            "\n" t))))
        (string-trim header)))))

(defun pimacs-session-read-record (file)
  (when (pimacs-session--regular-file-p file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file nil 0 (max 0 pimacs-session-record-max-bytes))
          (goto-char (point-min))
          (let ((id nil)
                (timestamp nil)
                (cwd nil)
                (parent-path nil)
                (parent-id nil)
                (preview nil)
                (name nil)
                (lines-read 0))
            (while (and (< lines-read 20) (not (eobp)))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
                (unless (string-empty-p line)
                  (condition-case nil
                      (let ((record (pimacs--json-parse-string line)))
                        (pcase (intern (plist-get record :type))
                          ('session
                           (setq id (pimacs--json-get record :id)
                                 timestamp (pimacs--json-get record :timestamp)
                                 cwd (pimacs--json-get record :cwd)
                                 parent-path (when-let ((parent (pimacs--json-get record :parentSession)))
                                               (expand-file-name parent (file-name-directory file)))
                                 parent-id (pimacs-session--parent-id parent-path)))
                          ('session_info
                           (setq name (pimacs--json-get record :name)))
                          ('message
                           (when (and (not preview)
                                      (equal (plist-get (plist-get record :message) :role) "user"))
                             (setq preview
                                   (pimacs-session--content-preview
                                    (pimacs--json-get (pimacs--json-get record :message) :content)))))))
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
             :parent-path parent-path
             :parent-id parent-id
             :name name
             :preview preview)))
      (file-error nil))))

(defun pimacs-session-with-ancestors (records)
  (let ((seen (make-hash-table :test 'equal))
        (pending (copy-sequence records))
        ancestors)
    (dolist (record records)
      (puthash (pimacs-session-record-path record) t seen))
    (while pending
      (when-let ((parent (pimacs-session-record-parent-path (pop pending))))
        (unless (gethash parent seen)
          (puthash parent t seen)
          (when-let ((record (pimacs-session-read-record parent)))
            (push record ancestors)
            (push record pending)))))
    (append records (nreverse ancestors))))

(defun pimacs-session-recent-records (directory recursive limit)
  (delq nil
        (mapcar #'pimacs-session-read-record
                (pimacs-session-recent-files directory recursive limit))))

(defun pimacs-session-tree (records)
  (let ((by-path (make-hash-table :test 'equal))
        (children (make-hash-table :test 'equal))
        (visited (make-hash-table :test 'eq))
        (recent (make-hash-table :test 'eq))
        roots result)
    (dolist (record records)
      (puthash (pimacs-session-record-path record) record by-path))
    (dolist (record records)
      (let ((parent (pimacs-session-record-parent-path record)))
        (if (and parent (gethash parent by-path))
            (push record (gethash parent children))
          (push record roots))))
    (maphash (lambda (key value)
               (puthash key (nreverse value) children))
             children)
    (cl-labels ((latest (record)
                  (or (gethash record recent)
                      (let ((time (or (pimacs-session-record-modified record)
                                      (seconds-to-time 0))))
                        (puthash record time recent)
                        (dolist (child (gethash (pimacs-session-record-path record) children))
                          (let ((child-time (latest child)))
                            (when (time-less-p time child-time)
                              (setq time child-time))))
                        (puthash record time recent))))
                (newer (left right)
                  (time-less-p (latest right) (latest left)))
                (walk (record indent lastp rootp)
                  (unless (gethash record visited)
                    (puthash record t visited)
                    (let* ((parent (pimacs-session-record-parent-path record))
                           (missing (and parent (not (gethash parent by-path))))
                           (prefix (concat indent (unless rootp (if lastp "└─ " "├─ "))
                                           (if missing "↳ " "")))
                           (next-indent (if rootp "" (concat indent (if lastp "   " "│  "))))
                           (descendants (cl-stable-sort
                                         (copy-sequence (gethash (pimacs-session-record-path record) children))
                                         #'newer)))
                      (push (list record prefix missing) result)
                      (cl-loop for child in descendants
                               for tail on descendants
                               do (walk child next-indent (null (cdr tail)) nil))))))
      (let ((roots (cl-stable-sort (nreverse roots) #'newer)))
        (cl-loop for root in roots
                 for tail on roots
                 do (walk root "" (null (cdr tail)) t)))
      (dolist (record records)
        (unless (gethash record visited)
          (walk record "" t t))))
    (nreverse result)))


(defun pimacs-session-resumable-record-p (record)
  (when-let ((cwd (pimacs-session-record-cwd record)))
    (file-directory-p cwd)))

(defun pimacs-session--resume-annotation-suffix (record include-cwd)
  (let ((cwd (pimacs-session-record-cwd record))
        (relative-time (pimacs-session-format-relative-time
                        (pimacs-session-record-modified record))))
    (string-join
     (delq nil
           (list (when (and include-cwd cwd)
                   (propertize (abbreviate-file-name cwd)
                               'face 'pimacs-session-directory-face))
                 (when relative-time
                   (propertize relative-time 'face 'shadow))))
     "  ")))

(defun pimacs-session--resume-candidates (records &optional include-cwd)
  (mapcar
   (lambda (node)
     (pcase-let ((`(,record ,prefix ,missing) node))
       (let* ((modified (pimacs-session-record-modified record))
              (id (propertize (or (pimacs--short-uuid
                                   (pimacs-session-record-id record))
                                  "unknown")
                              'face 'pimacs-session-name-face))
              (date (when-let ((date (pimacs-session-format-timestamp modified)))
                      (propertize date 'face 'shadow)))
              (session-name (when-let ((name (pimacs-session-record-name record)))
                              (propertize (format "[%s]" name)
                                          'face 'pimacs-session-name-face)))
              (text (string-join
                     (delq nil
                           (list session-name
                                 (pimacs-session-record-preview record)
                                 (when missing
                                   (concat "(parent unavailable"
                                           (when-let ((parent-id (pimacs--short-uuid
                                                                  (pimacs-session-record-parent-id record))))
                                             (concat ": " (propertize parent-id
                                                                      'face 'pimacs-session-name-face)))
                                           ")"))))
                     "  "))
              (head (string-join (delq nil (list id date)) "  "))
              (suffix (pimacs-session--resume-annotation-suffix record include-cwd))
              (width (- (window-body-width (minibuffer-window))
                        (if (string-empty-p suffix) 0 (+ 2 (string-width suffix)))
                        (string-width head) 2 (string-width prefix) 3)))
         (when (> (string-width text) (max 0 width))
           (put-text-property (length (truncate-string-to-width text (max 0 (1- width))))
                              (length text) 'display
                              (if (> width 0) (propertize "…" 'face 'shadow) "")
                              text))
         (cons (concat head "  " (propertize prefix 'face 'shadow) text) record))))
   (pimacs-session-tree records)))

(defun pimacs-session--resume-annotation-function (candidates include-cwd)
  (lambda (candidate)
    (when-let ((record (pimacs--alist-get-equal candidate candidates)))
      (let ((suffix (pimacs-session--resume-annotation-suffix record include-cwd)))
        (unless (string-empty-p suffix)
          (concat (propertize " "
                              'display `(space :align-to (- right ,(string-width suffix))))
                  suffix))))))

(defun pimacs-session-read-resume-record (records &optional include-cwd)
  (when records
    (let* ((records (pimacs-session-with-ancestors records))
           (candidates (pimacs-session--resume-candidates records include-cwd))
           (annotation-function
            (pimacs-session--resume-annotation-function candidates include-cwd))
           (selected (let ((completion-styles (cons 'substring completion-styles)))
                       (pimacs--completing-read "Resume session: " candidates
                                                annotation-function))))
      (pimacs--alist-get-equal selected candidates))))

(provide 'pimacs-session)

;;; pimacs-session.el ends here
