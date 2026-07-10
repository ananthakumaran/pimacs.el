;;; pi-utils.el --- Utilities -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Utility helpers shared by pi.el.

;;; Code:

(require 'subr-x)
(require 'widget)
(require 'ffap)
(require 'markdown-mode)
(require 'diff-mode)

(defcustom pi-align-markdown-tables t
  "Whether to align markdown tables while rendering assistant output."
  :type 'boolean
  :group 'pi)

(defun pi--json-read-object ()
  (json-parse-buffer :object-type 'plist :null-object 'json-null :false-object 'json-false :array-type 'list))

(defun pi--json-encode (obj)
  "Encode OBJ into a JSON string.  JSON arrays must be represented with vectors."
  (json-serialize obj :null-object 'json-null :false-object 'json-false))

(defun pi--format-number-short (n)
  "Format number N into a short human-readable string with K/M/B suffixes."
  (cond
   ((not (numberp n)) "?")
   ((>= n 1000000000)
    (format "%.1fB" (/ n 1000000000.0)))
   ((>= n 1000000)
    (format "%.1fM" (/ n 1000000.0)))
   ((>= n 1000)
    (format "%.1fk" (/ n 1000.0)))
   (t
    (number-to-string n))))

(defmacro pi--def-permanent-buffer-local (name &optional init-value)
  "Declare NAME as buffer local variable with optional INIT-VALUE."
  `(progn
     (defvar ,name ,init-value)
     (make-variable-buffer-local ',name)
     (put ',name 'permanent-local t)))

(defun pi--join (x &optional join-char)
  (let ((join-char (or join-char "\n")))
    (cond
     ((stringp x) x)
     ((proper-list-p x) (mapconcat (lambda (item) (pi--join item join-char)) x join-char))
     ((consp x) (pi--join (cdr x) join-char))
     (t ""))))

(defun pi--insert-error (text)
  "Insert TEXT with `pi-error-face'."
  (insert (propertize text 'face 'pi-error-face)))

(defun pi--insert-file-link (path &optional suffix)
  (widget-create 'file-link
                 :button-prefix ""
                 :button-suffix (or suffix "")
                 path))

(defun pi--keyword-name (keyword)
  "Return the name of KEYWORD as a string without the leading colon."
  (substring (symbol-name keyword) 1))

(defun pi--current-column-1-based ()
  "Return the current column as a 1-based number."
  (1+ (current-column)))

(defun pi--seconds-elapsed-since (time)
  (time-to-seconds (time-subtract (current-time) time)))

(defun pi--hash-remove-if (pred table)
  "Remove entries from TABLE for which PRED return non-nil.

PRED is called with KEY VALUE."
  (maphash
   (lambda (k v)
     (when (funcall pred k v)
       (remhash k table)))
   table))

(defun pi--file-at-point ()
  (let ((ffap-url-regexp nil))
    (when-let ((file (ffap-file-at-point)))
      (when (file-exists-p file)
        file))))

(defun pi--plist-merge (&rest plists)
  (let (result)
    (dolist (plist plists result)
      (while plist
        (setq result (plist-put result (car plist) (cadr plist))
              plist (cddr plist))))))

(defun pi--sort-entries-by-key (entries)
  (sort entries (lambda (a b) (string< (car a) (car b)))))

(defun pi--completing-read (prompt collection)
  (completing-read prompt
                   (lambda (string pred action)
                     (if (eq action 'metadata)
                         '(metadata (display-sort-function . identity))
                       (complete-with-action action collection string pred)))
                   nil t))

(defun pi--read-option (options current prompt)
  (let* ((items (mapcar (lambda (opt)
                          (cons (cdr opt) (car opt)))
                        options))
         (current-keyword (when current
                            (intern (concat ":" current))))
         (default-display (when current-keyword
                            (cdr (assoc current-keyword options))))
         (selected-display (completing-read
                            (format "%s (current: %s): " prompt (or current "?"))
                            (lambda (string pred action)
                              (if (eq action 'metadata)
                                  '(metadata (display-sort-function . identity))
                                (complete-with-action action items string pred)))
                            nil t nil nil default-display)))
    (when selected-display
      (let ((selected-keyword (alist-get selected-display items nil nil #'equal)))
        (cons (pi--keyword-name selected-keyword)
              (cdr (assoc selected-keyword options)))))))

(defun pi--get-line-contents (buffer line)
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (forward-line (1- line))
        (buffer-substring-no-properties
         (point)
         (line-end-position))))))

(defun pi--align-markdown-tables ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "|" nil t)
      (when (markdown-table-at-point-p)
        (goto-char (markdown-table-begin))
        (markdown-table-align)
        (goto-char (markdown-table-end))))))

(defun pi--render-markdown (text)
  (with-temp-buffer
    (insert text)
    (let ((inhibit-message t))
      (ignore-errors
        (delay-mode-hooks
          (gfm-view-mode))
        (when pi-align-markdown-tables
          (condition-case nil
              (let ((inhibit-read-only t))
                (pi--align-markdown-tables))
            (error nil)))
        (font-lock-ensure))
      (buffer-string))))

(defun pi--render-content (filename content)
  (with-temp-buffer
    ;; Use a fake temp filename preserving extension only.
    (setq-local
     buffer-file-name
     (expand-file-name
      (concat "pi-fontify"
              (when-let ((ext (file-name-extension filename t)))
                ext))
      temporary-file-directory))

    (insert content)

    (let ((inhibit-message t))
      (ignore-errors
        (delay-mode-hooks
          (let ((enable-local-variables nil)
                (enable-local-eval nil))
            (set-auto-mode)
            (font-lock-ensure)))))

    ;; Prevent save prompts
    (set-buffer-modified-p nil)

    ;; Preserve text properties
    (buffer-string)))

(defun pi--section-header (text)
  "Extract a short header from TEXT for use as section info."
  (when-let ((header (car (split-string text "\n" t))))
    (truncate-string-to-width (string-trim header) 80 nil nil t)))

(defun pi--diff-overlay-to-text-properties ()
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (eq (overlay-get ov 'diff-mode) 'fine)
      (put-text-property
       (overlay-start ov)
       (overlay-end ov)
       'face
       (overlay-get ov 'face)))))

(defun pi--render-diff (diff)
  (with-temp-buffer
    (insert diff)
    (delay-mode-hooks
      (diff-mode)
      (font-lock-ensure)
      (goto-char (point-min))
      (while (not (eobp))
        (diff-hunk-next)
        (diff-refine-hunk))
      (pi--diff-overlay-to-text-properties))
    (set-buffer-modified-p nil)
    (buffer-string)))

(provide 'pi-utils)
;;; pi-utils.el ends here
