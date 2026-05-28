;;; pi-section.el --- Section support -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar pi-section-hidden-default nil)

;; A buffer in pi-mode is organized into hierarchical sections.
;; These sections are used for navigation and for hiding parts of the
;; buffer.
;;
;; Most sections also represent the objects that Monky works with,
;; such as files, diffs, hunks, commits, etc.  The 'type' of a section
;; identifies what kind of object it represents (if any), and the
;; parent and grand-parent, etc provide the context.

(cl-defstruct pi-section
  parent children beginning end type title hidden info)

(defun pi-set-section-info (info &optional section)
  (setf (pi-section-info section) info))

(defun pi-advance-pointer-maker (marker)
  (let ((m (copy-marker marker)))
    (set-marker-insertion-type m t)
    m))

(defun pi-new-section (title type parent)
  (let* ((s (make-pi-section :parent parent
                             :title title
                             :type type
                             :hidden pi-section-hidden-default)))
    (when parent
      (setf (pi-section-children parent)
            (nconc (pi-section-children parent)
                   (list s))))
    s))

(defun pi-create-root-section ()
  (let ((root (pi-new-section "Root" 'root nil)))
    (setf (pi-section-beginning root) (point-min))
    (setf (pi-section-end root) (point-min-marker))
    root))

(defmacro pi-insert-section (section &rest body)
  (declare (indent 1)
           (debug (symbolp body)))
  (let ((s (make-symbol "*section*")))
    `(let* ((,s ,section))
       (setf (pi-section-beginning ,s) (point-marker))
       ,@body
       (setf (pi-section-beginning ,s) (pi-advance-pointer-maker (pi-section-beginning ,s)))
       (pi-update-section-end ,s (point-marker))
       (pi-propertize-section ,s)
       ,s)))

(defmacro pi-append-section (section &rest body)
  (declare (indent 1)
           (debug (symbolp body)))
  (let ((s (make-symbol "*section*")))
    `(let* ((,s ,section))
       (goto-char (pi-section-end ,s))
       (pi-update-section-end ,s (pi-advance-pointer-maker (pi-section-end ,s)))
       ,@body
       (pi-update-section-end ,s (point-marker))
       (pi-propertize-section ,s)
       ,s)))

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
      (while (and secs (not (equal (car path)
                                   (pi-section-title (car secs)))))
        (setq secs (cdr secs)))
      (and (car secs)
           (pi-find-section (cdr path) (car secs))))))

(defun pi-section-path (section)
  "Return the path of SECTION."
  (if (not (pi-section-parent section))
      '()
    (append (pi-section-path (pi-section-parent section))
            (list (pi-section-title section)))))

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
         (next (or (and (not (pi-section-hidden section))
                        (pi-section-children section)
                        (pi-find-section-after (point)
                                                  (pi-section-children
                                                   section)))
                   (pi-next-section section))))
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
    (cond ((= (point) (pi-section-beginning section))
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


(defun pi-section-set-hidden (section hidden)
  "Hide SECTION if HIDDEN is not nil, show it otherwise."
  (setf (pi-section-hidden section) hidden)
  (let ((inhibit-read-only t)
        (beg (save-excursion
               (goto-char (pi-section-beginning section))
               (forward-line)
               (point)))
        (end (pi-section-end section)))
    (if (< beg end)
        (put-text-property beg end 'invisible hidden)))
  (if (not hidden)
      (dolist (c (pi-section-children section))
        (pi-section-set-hidden c (pi-section-hidden c)))))

(defun pi-toggle-section ()
  "Toggle hidden status of current section."
  (interactive)
  (let ((section (pi-current-section)))
    (when (pi-section-parent section)
      (goto-char (pi-section-beginning section))
      (pi-section-set-hidden section (not (pi-section-hidden section))))))

(defun pi-section-show-level-1-all ()
  "Collapse all the sections in the pi status buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((section (pi-current-section)))
	(pi-section-set-hidden section t))
      (forward-line 1))))


(defun pi-demo ()
  "Create a demo buffer with nested pi sections."
  (interactive)
  (let ((buf (get-buffer-create "*pi-demo*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (let* ((root (pi-create-root-section))
             (build (pi-new-section "Build" 'build root))
             (compile (pi-new-section "Compile" 'compile build))
             (tests (pi-new-section "Tests" 'test build))
             (unit-tests (pi-new-section "Unit Tests" 'test tests))
             (integration-tests (pi-new-section "Integration Tests" 'integration-tests tests))
             (logs (pi-new-section "Logs" 'logs root))
             (server-log (pi-new-section "Server" 'server-log logs))
             (worker-log (pi-new-section "Worker" 'worker-log logs))
             (deploy (pi-new-section "Deploy" 'deploy root)))
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
          (insert "      Connected client #45\n")))

      (setq buffer-read-only t)
      (goto-char (point-min)))

    (pop-to-buffer buf)))

(defun pi-describe-section (section &optional indent)
  "Pretty print SECTION and its children with INDENT.
Does not recurse into the parent."
  (interactive (list (pi-current-section) 0))
  (let ((prefix (make-string (* indent 2) ?\s))
        (parent (pi-section-parent section)))
    (princ (format "%sSection: %S (type: %s)\n" prefix
                   (pi-section-title section)
                   (pi-section-type section)))
    (when parent
      (princ (format "%s  parent: %S (type: %s)\n" prefix
                     (pi-section-title parent)
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
        (dolist (child (reverse children))
          (pi-describe-section child (1+ indent)))))))

(provide 'pi-section)
