;;; pi-section.el --- Section support -*- lexical-binding: t; -*-

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

;; A buffer in pi-mode is organized into hierarchical sections.
;; These sections are used for navigation and for hiding parts of the
;; buffer.

(require 'cl-lib)

;;; Code:

(defcustom pi-section-padding "\n\n"
  "String inserted between sections to control the visual gap.
Increase or decrease this value to adjust spacing between sections."
  :type 'string
  :group 'pi)

(defvar pi-section-hidden-default nil)
(defvar-local pi-root-section nil)

(defun pi-prefix-p (prefix list)
  "Return non-nil if PREFIX is a prefix of LIST.
PREFIX and LIST should both be lists.

If the car of PREFIX is the symbol '*, then return non-nil if the cdr of PREFIX
is a sublist of LIST (as if '* matched zero or more arbitrary elements of LIST)"
  (or (null prefix)
      (if (eq (car prefix) '*)
          (or (pi-prefix-p (cdr prefix) list)
              (and (not (null list))
                   (pi-prefix-p prefix (cdr list))))
        (and (not (null list))
             (equal (car prefix) (car list))
             (pi-prefix-p (cdr prefix) (cdr list))))))


(cl-defstruct pi-section
  parent children beginning end type hidden info padding)

(defun pi-set-section-info (section info)
  (setf (pi-section-info section) info))

(defun pi-advance-pointer-maker (marker)
  (let ((m (copy-marker marker)))
    (set-marker-insertion-type m t)
    m))

(defun pi-new-section (type parent &rest args)
  (let* ((padding (or (plist-get args :padding) pi-section-padding))
         (s (make-pi-section :parent parent
                             :type type
                             :hidden pi-section-hidden-default
                             :padding padding)))
    (when parent
      (setf (pi-section-children parent)
            (nconc (pi-section-children parent)
                   (list s))))
    s))

(defun pi-create-root-section ()
  (when pi-root-section
    (error "Root section already exists"))
  (let ((root (pi-new-section 'root nil)))
    (setf (pi-section-beginning root) (point-min))
    (setf (pi-section-end root) (point-min-marker))
    (setq pi-root-section root)
    root))

(defmacro pi-insert-section (section &rest body)
  (declare (indent 1)
           (debug (symbolp body)))
  (let ((s (make-symbol "*section*")))
    `(let* ((,s ,section))
       (goto-char (pi-section-end (pi-section-parent ,s)))
       (setf (pi-section-beginning ,s) (point-marker))
       ,@body
       (insert (pi-section-padding ,s))
       (setf (pi-section-beginning ,s) (pi-advance-pointer-maker (pi-section-beginning ,s)))
       (pi-update-section-end ,s (point-marker))
       (pi-propertize-section ,s)
       ,s)))

(defmacro pi-create-section (type parent &rest body)
  (declare (indent 2)
           (debug (symbolp body)))
  (let ((s (make-symbol "*section*")))
    `(let* ((,s (pi-new-section ,type ,parent)))
       (pi-insert-section ,s
         ,@body)
       ,s)))

(defmacro pi-append-section (section &rest body)
  (declare (indent 1)
           (debug (symbolp body)))
  (let ((s (make-symbol "*section*")))
    `(let* ((,s ,section))
       (goto-char (pi-section-beginning ,s))
       (setf (pi-section-beginning ,s) (point-marker))
       (goto-char (- (pi-section-end ,s) (length (pi-section-padding ,s))))
       ,@body
       (forward-char (length (pi-section-padding ,s)))
       (setf (pi-section-beginning ,s) (pi-advance-pointer-maker (pi-section-beginning ,s)))
       (pi-update-section-end ,s (point-marker))
       (pi-propertize-section ,s)
       ,s)))

(defmacro pi-replace-section (section &rest body)
  (declare (indent 1)
           (debug (symbolp body)))
  (let ((s (make-symbol "*section*")))
    `(let* ((,s ,section))
       (delete-region (pi-section-beginning ,s) (pi-section-end ,s))
       (setf (pi-section-children ,s) nil)
       (goto-char (pi-section-beginning ,s))
       (setf (pi-section-beginning ,s) (point-marker))
       ,@body
       (insert (pi-section-padding ,s))
       (setf (pi-section-beginning ,s) (pi-advance-pointer-maker (pi-section-beginning ,s)))
       (pi-update-section-end ,s (point-marker))
       (pi-propertize-section ,s)
       ,s)))

(defun pi-delete-section (section)
  (let ((beg (pi-section-beginning section))
        (end (pi-section-end section))
        (parent (pi-section-parent section)))
    (delete-region beg end)
    (when parent
      (setf (pi-section-children parent)
            (delq section (pi-section-children parent)))
      (pi-update-section-end parent beg))))

(defmacro pi-create-or-replace-section (section type parent &rest body)
  (declare (indent 3)
           (debug (symbolp body)))
  `(if ,section
       (pi-replace-section ,section ,@body)
     (pi-create-section ,type ,parent ,@body)))

(defun pi-update-section-end (section end)
  (when section
    (let ((current-end (pi-section-end section)))
      (if (or (null current-end)
              (<= (marker-position current-end) (marker-position end)))
          (setf (pi-section-end section) end)))
    (pi-update-section-end (pi-section-parent section) end)))

(defun pi-propertize-section (section)
  "Add text-property needed for SECTION."
  (put-text-property (pi-section-beginning section)
                     (pi-section-end section)
                     'pi-section section))

(defun pi-find-section (path top)
  "Find the section at the path PATH in subsection of section TOP."
  (if (null path)
      top
    (let ((secs (pi-section-children top)))
      (while (and secs (not (eq (car path)
                                (pi-section-type (car secs)))))
        (setq secs (cdr secs)))
      (and (car secs)
           (pi-find-section (cdr path) (car secs))))))

(defun pi-section-path (section)
  "Return the path of SECTION."
  (if (or (not section) (not (pi-section-parent section)))
      '()
    (append (pi-section-path (pi-section-parent section))
            (list (pi-section-type section)))))

(defun pi-current-section ()
  "Return the pi section at point."
  (pi-section-at (point)))

(defun pi-section-at (pos)
  "Return the pi section at position POS."
  (get-text-property pos 'pi-section))

(defun pi-find-section-after (pos secs)
  "Find the first section that begins after POS in the list SECS."
  (while (and secs
              (not (> (pi-section-beginning (car secs)) pos)))
    (setq secs (cdr secs)))
  (car secs))

(defun pi-find-section-before (pos secs)
  "Find the last section that begins before POS in the list SECS."
  (let ((prev nil))
    (while (and secs
                (not (> (pi-section-beginning (car secs)) pos)))
      (setq prev (car secs))
      (setq secs (cdr secs)))
    prev))

(defun pi-next-section (section)
  "Return the section that is after SECTION."
  (let ((parent (pi-section-parent section)))
    (if parent
        (let ((next (cadr (memq section
                                (pi-section-children parent)))))
          (or next
              (pi-next-section parent))))))

(defun pi-goto-next-section ()
  "Go to the next pi section."
  (interactive)
  (let* ((section (pi-current-section))
         (next (and section
                    (or (and (not (pi-section-hidden section))
                             (pi-section-children section)
                             (pi-find-section-after (point)
                                                    (pi-section-children
                                                     section)))
                        (pi-next-section section)))))
    (cond
     (next
      (goto-char (pi-section-beginning next)))
     (t (message "No next section")))))

(defun pi-prev-section (section)
  "Return the section that is before SECTION."
  (let ((parent (pi-section-parent section)))
    (if parent
        (let ((prev (cadr (memq section
                                (reverse (pi-section-children parent))))))
          (cond (prev
                 (while (and (not (pi-section-hidden prev))
                             (pi-section-children prev))
                   (setq prev (car (reverse (pi-section-children prev)))))
                 prev)
                (t
                 parent))))))

(defun pi-goto-previous-section ()
  "Goto the previous pi section."
  (interactive)
  (let ((section (pi-current-section)))
    (cond
     ((null section)
      (if (and pi-root-section
               (not (null (pi-section-children pi-root-section))))
          (goto-char (pi-section-beginning (car (last (pi-section-children pi-root-section)))))
        (message "No previous section")))
     ((= (point) (pi-section-beginning section))
      (let ((prev (pi-prev-section (pi-current-section))))
        (if prev
            (goto-char (pi-section-beginning prev))
          (message "No previous section"))))
     (t
      (let ((prev (pi-find-section-before (point)
                                          (pi-section-children
                                           section))))
        (goto-char (pi-section-beginning (or prev section)))
        (goto-char (pi-section-beginning (or prev section))))))))

(defun pi-goto-last-section ()
  "Go to the last child section of pi-root-section."
  (interactive)
  (if (and pi-root-section
           (pi-section-children pi-root-section))
      (goto-char (pi-section-beginning
                  (car (last (pi-section-children pi-root-section)))))
    (message "No sections")))

(defun pi-section-isearch-open (ov)
  (when-let ((section
              (get-text-property (overlay-start ov) 'pi-section))
             (parent (pi-section-parent section)))
    (while (and parent (not (eq parent pi-root-section)))
      (setq section (pi-section-parent section))
      (setq parent (pi-section-parent section)))
    (pi-section-set-hidden section nil)))

(defun pi-section-set-hidden (section hidden)
  "Hide SECTION if HIDDEN is not nil, show it otherwise."
  (setf (pi-section-hidden section) hidden)
  (let ((inhibit-read-only t)
        (beg (save-excursion
               (goto-char (pi-section-beginning section))
               (forward-line)
               (point-marker)))
        (end (pi-section-end section)))

    ;; Remove any existing hide overlays.
    (remove-overlays beg end 'pi-section-hidden t)

    (when (and hidden (< beg end))
      (let ((ov (make-overlay beg end)))
        (overlay-put ov 'pi-section-hidden t)
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'invisible t)
        (overlay-put ov 'isearch-open-invisible
                     #'pi-section-isearch-open))))

  (unless hidden
    (dolist (child (pi-section-children section))
      (pi-section-set-hidden child
                             (pi-section-hidden child)))))

(defun pi-toggle-section ()
  "Toggle hidden status of current section."
  (interactive)
  (when-let (section (pi-current-section))
    (when (pi-section-parent section)
      (goto-char (pi-section-beginning section))
      (pi-section-set-hidden section (not (pi-section-hidden section))))))

(defun pi-section-show-level-1-all ()
  "Collapse all the sections in the pi status buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp)) (pi-current-section))
      (let ((section (pi-current-section)))
	(pi-section-set-hidden section t))
      (forward-line 1))))


(defmacro pi-section-case (&rest clauses)
  "Make different action depending of current section.

CLAUSES is a list of CLAUSE, each clause is (SECTION-TYPE &BODY)
where SECTION-TYPE describe section where BODY will be run.

This returns non-nil if some section matches.  If the
corresponding body return a non-nil value, it is returned,
otherwise it return t."

  (declare (indent 1)
           (debug (form &rest (sexp body))))
  (let ((section (make-symbol "*section*"))
        (path (make-symbol "*path*")))
    `(let* ((,section (pi-current-section))
            (,path (pi-section-path ,section)))
       (cond ,@(mapcar (lambda (clause)
                         (let ((prefix (car clause))
                               (body (cdr clause)))
                           `(,(if (eq prefix t)
                                  `t
                                `(pi-prefix-p ',(reverse prefix) (reverse ,path)))
                             (or (progn ,@body)
                                 t))))
                       clauses)))))


(defun pi-demo ()
  "Create a demo buffer with nested pi sections."
  (interactive)
  (let ((buf (get-buffer-create "*pi-demo*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (let* ((pi-section-padding "\n")
             (root (pi-create-root-section))
             (build (pi-new-section 'build root))
             (compile (pi-new-section 'compile build))
             (tests (pi-new-section 'test build))
             (unit-tests (pi-new-section 'test tests))
             (integration-tests (pi-new-section 'integration-tests tests))
             (logs (pi-new-section 'logs root))
             (server-log (pi-new-section 'server-log logs))
             (worker-log (pi-new-section 'worker-log logs))
             (deploy (pi-new-section 'deploy root)))
        (pi-insert-section build
          (insert "[-] Build\n"))
        (pi-insert-section compile
          (insert "  [-] Compile\n")
          (insert "      Compiling foo.c\n")
          (insert "      Compiling bar.c\n"))
        (pi-insert-section tests
          (insert "  [-] Tests\n"))
        (pi-insert-section unit-tests
          (insert "      [-] Unit Tests\n")
          (insert "          test-auth ... ok\n")
          (insert "          test-db ... ok\n"))
        (pi-insert-section integration-tests
          (insert "      [-] Integration Tests\n")
          (insert "          api-flow ... running\n"))
        (pi-insert-section logs
          (insert "[-] Logs\n"))
        (pi-insert-section server-log
          (insert "  [-] Server\n")
          (insert "      Listening on :8080\n")
          (insert "      Connected client #42\n"))
        (pi-insert-section worker-log
          (insert "  [-] Worker\n")
          (insert "      Job started\n")
          (insert "      Job completed\n"))
        (pi-insert-section deploy
          (insert "[-] Deploy\n")
          (insert "    Uploading artifacts...\n")
          (insert "    Restarting services...\n"))
        (pi-append-section server-log
          (insert "      Connected client #43\n")
          (insert "      Connected client #44\n")
          (insert "      Connected client #45\n"))
        (pi-replace-section worker-log
          (insert "  [-] Worker\n")
          (insert "      Restarted\n")
          (insert "      Processing queue...\n")
          (insert "      Queue drained\n"))
        (pi-append-section server-log
          (insert "      Connected client #46\n")
          (insert "      Connected client #47\n")
          (insert "      Connected client #48\n")))

      (setq buffer-read-only t)
      (goto-char (point-min)))

    (pop-to-buffer buf)))

(defun pi-describe-section (section &optional indent)
  "Pretty print SECTION and its children with INDENT.
Does not recurse into the parent."
  (interactive (list (pi-current-section) 0))
  (let ((prefix (make-string (* indent 2) ?\s))
        (parent (pi-section-parent section)))
    (princ (format "%sSection: %s\n" prefix
                   (pi-section-type section)))
    (when parent
      (princ (format "%s  parent: %s\n" prefix
                     (pi-section-type parent))))
    (princ (format "%s  beginning: %s, end: %s\n" prefix
                   (pi-section-beginning section)
                   (pi-section-end section)))
    (princ (format "%s  hidden: %s\n" prefix
                   (pi-section-hidden section)))
    (when (pi-section-info section)
      (princ (format "%s  info: %s\n" prefix
                     (pi-section-info section))))
    (let ((children (pi-section-children section)))
      (when children
        (princ (format "%s  Children:\n" prefix))
        (dolist (child children)
          (pi-describe-section child (1+ indent)))))))

(defun pi-section-line ()
  "Return the 0-based line number of point within the current section.
Returns 0 if point is on the first line of the section or if there is
no current section."
  (if-let ((section (pi-current-section)))
      (count-lines (pi-section-beginning section) (point))
    0))

(provide 'pi-section)

;;; pi-section.el ends here
