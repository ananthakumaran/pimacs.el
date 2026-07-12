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

;;; Code:

(require 'cl-lib)

(defcustom pi-section-autohide-count 2
  "Automatically hide older chat sections beyond this count.
This helps reduce clutter by collapsing earlier responses when the
conversation grows long.  When nil, auto hiding is disabled and no
sections are hidden automatically."
  :type '(choice (const :tag "Disable" nil)
                 integer)
  :group 'pi)

(defcustom pi-section-padding "\n\n"
  "String inserted between sections to control the visual gap.
Increase or decrease this value to adjust spacing between sections."
  :type 'string
  :group 'pi)

(defcustom pi-section-visibility-indicators
  '(pi-section-fringe-bitmap> . pi-section-fringe-bitmapv)
  "Fringe bitmaps used to indicate section visibility.

The car is used for hidden sections and the cdr for visible sections.
Set this to nil to disable fringe indicators."
  :type '(choice
          (const :tag "No indicators" nil)
          (cons :tag "Fringe indicators"
                (symbol :tag "Hidden section bitmap")
                (symbol :tag "Visible section bitmap")))
  :group 'pi)

(defvar pi-section--visibility-default :autoshow)
(defvar-local pi-section--root-section nil)

(define-fringe-bitmap 'pi-section-fringe-bitmap>
  [#b01100000
   #b00110000
   #b00011000
   #b00001100
   #b00011000
   #b00110000
   #b01100000
   #b00000000])

(define-fringe-bitmap 'pi-section-fringe-bitmapv
  [#b00000000
   #b10000010
   #b11000110
   #b01101100
   #b00111000
   #b00010000
   #b00000000
   #b00000000])

(defun pi-section--visible-p (section)
  (memq (pi-section-visibility section) '(:autoshow :show)))

(defun pi-section--hidden-p (section)
  (memq (pi-section-visibility section) '(:autohide :hide)))

(defun pi-section--user-toggled-p (section)
  (memq (pi-section-visibility section) '(:show :hide)))

(defun pi-section--prefix-p (prefix list)
  "Return non-nil if PREFIX is a prefix of LIST.
PREFIX and LIST should both be lists.

If the car of PREFIX is the symbol '*, then return non-nil if the cdr of PREFIX
is a sublist of LIST (as if '* matched zero or more arbitrary elements of LIST)"
  (or (null prefix)
      (if (eq (car prefix) '*)
          (or (pi-section--prefix-p (cdr prefix) list)
              (and list
                   (pi-section--prefix-p prefix (cdr list))))
        (and list
             (equal (car prefix) (car list))
             (pi-section--prefix-p (cdr prefix) (cdr list))))))


(cl-defstruct pi-section
  parent children beginning end type visibility info padding)

(cl-defstruct pi-section-tool-call-info
  tool-name args header)

(cl-defstruct pi-section-tool-result-info
  tool-name details args)

(cl-defstruct pi-section-user-info
  header content)

(cl-defstruct pi-section-assistant-info
  header content type)

(defun pi-section--set-info (section info)
  (setf (pi-section-info section) info))

(defun pi-section--advance-pointer-maker (marker)
  (let ((m (copy-marker marker)))
    (set-marker-insertion-type m t)
    m))

(defun pi-section--new-section (type parent &rest args)
  (let* ((padding (or (plist-get args :padding) pi-section-padding))
         (s (make-pi-section :parent parent
                             :type type
                             :visibility pi-section--visibility-default
                             :padding padding)))
    (when parent
      (setf (pi-section-children parent)
            (nconc (pi-section-children parent)
                   (list s))))
    s))

(defun pi-section--create-root-section ()
  (when pi-section--root-section
    (error "Root section already exists"))
  (let ((root (pi-section--new-section 'root nil)))
    (setf (pi-section-beginning root) (point-min))
    (setf (pi-section-end root) (point-min-marker))
    (setq pi-section--root-section root)
    root))

(defmacro pi-section--insert-section (section &rest body)
  (declare (indent 1)
           (debug (symbolp body)))
  (let ((s (make-symbol "*section*")))
    `(let* ((,s ,section))
       (goto-char (pi-section-end (pi-section-parent ,s)))
       (setf (pi-section-beginning ,s) (point-marker))
       ,@body
       (insert (pi-section-padding ,s))
       (setf (pi-section-beginning ,s) (pi-section--advance-pointer-maker (pi-section-beginning ,s)))
       (pi-section--update-section-end ,s (point-marker))
       (pi-section--propertize-section ,s)
       (pi-section--update-visibility-indicator ,s)
       ,s)))

(defmacro pi-section--create-section (type parent &rest body)
  (declare (indent 2)
           (debug (symbolp symbolp body)))
  (let ((s (make-symbol "*section*")))
    `(let* ((,s (pi-section--new-section ,type ,parent)))
       (pi-section--insert-section ,s
         ,@body)
       ,s)))

(defmacro pi-section--append-section (section &rest body)
  (declare (indent 1)
           (debug (symbolp body)))
  (let ((s (make-symbol "*section*")))
    `(let* ((,s ,section))
       (goto-char (pi-section-beginning ,s))
       (setf (pi-section-beginning ,s) (point-marker))
       (goto-char (- (pi-section-end ,s) (length (pi-section-padding ,s))))
       ,@body
       (forward-char (length (pi-section-padding ,s)))
       (setf (pi-section-beginning ,s) (pi-section--advance-pointer-maker (pi-section-beginning ,s)))
       (pi-section--update-section-end ,s (point-marker))
       (pi-section--propertize-section ,s)
       (pi-section--update-visibility-indicator ,s)
       ,s)))

(defmacro pi-section--replace-section (section &rest body)
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
       (setf (pi-section-beginning ,s) (pi-section--advance-pointer-maker (pi-section-beginning ,s)))
       (pi-section--update-section-end ,s (point-marker))
       (pi-section--propertize-section ,s)
       (if (pi-section--hidden-p ,s)
           (pi-section--set-visibility ,s (pi-section-visibility ,s))
         (pi-section--update-visibility-indicator ,s))
       ,s)))

(defun pi-section--delete-section (section)
  (let ((beg (pi-section-beginning section))
        (end (pi-section-end section))
        (parent (pi-section-parent section)))
    (delete-region beg end)
    (when parent
      (setf (pi-section-children parent)
            (delq section (pi-section-children parent)))
      (pi-section--update-section-end parent (copy-marker beg)))))

(defun pi-section--update-section-end (section end)
  (when section
    (let ((current-end (pi-section-end section)))
      (when (or (null current-end)
                (<= (marker-position current-end) (marker-position end)))
        (setf (pi-section-end section) end)
        ;; rebuild the overlay if the section is hidden
        (when (pi-section--hidden-p section)
          (pi-section--set-visibility section (pi-section-visibility section)))))
    (pi-section--update-section-end (pi-section-parent section) end)))

(defun pi-section--propertize-section (section)
  "Add text-property needed for SECTION."
  (put-text-property (pi-section-beginning section)
                     (pi-section-end section)
                     'pi-section section))

(defun pi-section--find-section (path top)
  "Find the section at the path PATH in subsection of section TOP."
  (if (null path)
      top
    (let ((secs (pi-section-children top)))
      (while (and secs (not (eq (car path)
                                (pi-section-type (car secs)))))
        (setq secs (cdr secs)))
      (and (car secs)
           (pi-section--find-section (cdr path) (car secs))))))

(defun pi-section--section-path (section)
  "Return the path of SECTION."
  (if (or (not section) (not (pi-section-parent section)))
      '()
    (append (pi-section--section-path (pi-section-parent section))
            (list (pi-section-type section)))))

(defun pi-section--current-section ()
  "Return the pi section at point."
  (pi-section--section-at (point)))

(defun pi-section--section-at (pos)
  "Return the pi section at position POS."
  (get-text-property pos 'pi-section))

(defun pi-section--find-section-after (pos secs)
  "Find the first section that begins after POS in the list SECS."
  (while (and secs
              (not (> (pi-section-beginning (car secs)) pos)))
    (setq secs (cdr secs)))
  (car secs))

(defun pi-section--find-section-before (pos secs)
  "Find the last section that begins before POS in the list SECS."
  (let ((prev nil))
    (while (and secs
                (not (> (pi-section-beginning (car secs)) pos)))
      (setq prev (car secs))
      (setq secs (cdr secs)))
    prev))

(defun pi-section--walk-sections (section step predicate)
  "Walk from SECTION using STEP until PREDICATE matches.
Return the first matching section, or nil if there is none."
  (setq section (and section (funcall step section)))
  (while (and section
              (not (funcall predicate section)))
    (setq section (funcall step section)))
  section)

(defun pi-section--navigable-children (section)
  "Return the child sections of SECTION that should be navigated."
  (and (pi-section--visible-p section)
       (pi-section-children section)))

(defun pi-section--next-after-subtree-step (section)
  "Return the first section after SECTION's subtree in tree order."
  (let ((parent (pi-section-parent section)))
    (if parent
        (let ((next (cadr (memq section
                                (pi-section-children parent)))))
          (or next
              (pi-section--next-after-subtree-step parent))))))

(defun pi-section--next-section-step (section)
  "Return the section immediately after SECTION in tree order."
  (or (car (pi-section--navigable-children section))
      (pi-section--next-after-subtree-step section)))

(defun pi-section--next-section (section)
  "Return the section that is after SECTION."
  (pi-section--walk-sections section #'pi-section--next-section-step
                             (lambda (_section) t)))

(defun pi-section--next-section-of-type (section type)
  "Return the first section after SECTION whose type is TYPE."
  (pi-section--walk-sections section #'pi-section--next-section-step
                             (lambda (next)
                               (eq (pi-section-type next) type))))
(defun pi-section--next-target-at-point ()
  "Return the section `pi-goto-next-section' would jump to from point."
  (let ((section (pi-section--current-section)))
    (and section
         (or (pi-section--find-section-after (point)
                                             (pi-section--navigable-children section))
             (pi-section--next-after-subtree-step section)))))

(defun pi-section--goto-next-section-of-type (type)
  "Go to the next pi section whose type is TYPE."
  (let* ((target (pi-section--next-target-at-point))
         (next (and target
                    (if (eq (pi-section-type target) type)
                        target
                      (pi-section--next-section-of-type target type)))))
    (if next
        (goto-char (pi-section-beginning next))
      (message "No next %s section" type))))

(defun pi-goto-next-section ()
  "Go to the next pi section."
  (interactive)
  (if-let ((next (pi-section--next-target-at-point)))
      (goto-char (pi-section-beginning next))
    (message "No next section")))

(defun pi-section--prev-section-step (section)
  "Return the section immediately before SECTION in tree order."
  (when-let ((parent (pi-section-parent section)))
    (if-let ((prev (cadr (memq section
                               (reverse (pi-section-children parent))))))
        (progn
          (while (pi-section--navigable-children prev)
            (setq prev (car (last (pi-section--navigable-children prev)))))
          prev)
      (and (pi-section-parent parent)
           parent))))

(defun pi-section--prev-section (section)
  "Return the section that is before SECTION."
  (pi-section--walk-sections section #'pi-section--prev-section-step
                             (lambda (_section) t)))

(defun pi-section--prev-section-of-type (section type)
  "Return the first section before SECTION whose type is TYPE."
  (pi-section--walk-sections section #'pi-section--prev-section-step
                             (lambda (prev)
                               (eq (pi-section-type prev) type))))
(defun pi-section--previous-target-at-point ()
  "Return the section `pi-goto-previous-section' would jump to from point."
  (let ((section (pi-section--current-section)))
    (cond
     ((null section)
      (and pi-section--root-section
           (car (last (pi-section-children pi-section--root-section)))))
     ((= (point) (pi-section-beginning section))
      (pi-section--prev-section section))
     (t
      (or (pi-section--find-section-before (point)
                                           (pi-section--navigable-children section))
          section)))))

(defun pi-goto-previous-section ()
  "Goto the previous pi section."
  (interactive)
  (if-let ((prev (pi-section--previous-target-at-point)))
      (goto-char (pi-section-beginning prev))
    (message "No previous section")))

(defun pi-section--goto-previous-section-of-type (type)
  "Go to the previous pi section whose type is TYPE."
  (let* ((target (pi-section--previous-target-at-point))
         (prev (and target
                    (if (eq (pi-section-type target) type)
                        target
                      (pi-section--prev-section-of-type target type)))))
    (if prev
        (goto-char (pi-section-beginning prev))
      (message "No previous %s section" type))))

(defun pi-goto-last-section ()
  "Go to the last child section of `pi-section--root-section'."
  (interactive)
  (if (and pi-section--root-section
           (pi-section-children pi-section--root-section))
      (goto-char (pi-section-beginning
                  (car (last (pi-section-children pi-section--root-section)))))
    (message "No sections")))

(defun pi-section--isearch-open (ov)
  (when-let ((section
              (get-text-property (overlay-start ov) 'pi-section))
             (parent (pi-section-parent section)))
    (while (and parent (not (eq parent pi-section--root-section)))
      (setq section (pi-section-parent section))
      (setq parent (pi-section-parent section)))
    (pi-section--set-visibility section :show)))

(defun pi-section--visibility-indicator ()
  (and (display-graphic-p)
       pi-section-visibility-indicators))

(defun pi-section--update-visibility-indicator (section)
  (when (pi-section-parent section)
    (let ((beg (pi-section-beginning section))
          (eol (save-excursion
                 (goto-char (pi-section-beginning section))
                 (line-end-position))))
      (dolist (ov (overlays-in beg eol))
        (when (overlay-get ov 'pi-section-visibility-indicator)
          (delete-overlay ov)))
      (when-let ((indicator (pi-section--visibility-indicator)))
        (let ((ov (make-overlay beg eol nil t))
              (bitmap (if (pi-section--hidden-p section)
                          (car indicator)
                        (cdr indicator))))
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'pi-section-visibility-indicator t)
          (overlay-put ov 'before-string
                       (propertize "fringe" 'display
                                   `(left-fringe ,bitmap fringe))))))))

(defun pi-section--set-visibility (section visibility)
  "Set the visibility state of SECTION.

VISIBILITY can be one of:
- `:autoshow'  - visible, never toggled by user (initial state)
- `:autohide'  - hidden, auto-managed
- `:show'      - visible, user explicitly toggled
- `:hide'      - hidden, user explicitly toggled"
  (setf (pi-section-visibility section) visibility)
  (let ((inhibit-read-only t)
        (beg (save-excursion
               (goto-char (pi-section-beginning section))
               (forward-line)
               (point-marker)))
        (end (pi-section-end section)))

    ;; Remove any existing hide overlays.
    (remove-overlays beg end 'pi-section-hidden t)

    (when (and (pi-section--hidden-p section) (< beg end))
      (let ((ov (make-overlay beg end)))
        (overlay-put ov 'pi-section-hidden t)
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'invisible t)
        (overlay-put ov 'display "")
        (overlay-put ov 'isearch-open-invisible
                     #'pi-section--isearch-open)))

    (pi-section--update-visibility-indicator section))

  (when (pi-section--visible-p section)
    (dolist (child (pi-section-children section))
      (pi-section--set-visibility child
                                  (pi-section-visibility child)))))

(defun pi-toggle-section ()
  "Toggle visibility of current section."
  (interactive)
  (when-let (section (pi-section--current-section))
    (when (pi-section-parent section)
      (goto-char (pi-section-beginning section))
      (if (pi-section--visible-p section)
          (pi-section--set-visibility section :hide)
        (pi-section--set-visibility section :show)))))

(defun pi-mouse-toggle-section (event)
  "Toggle visibility of the section clicked in the fringe.
EVENT is the mouse event that triggered the toggle."
  (interactive "e")
  (let* ((pos (event-start event))
         (section (pi-section--section-at (posn-point pos))))
    (when (and section (pi-section-parent section))
      (goto-char (pi-section-beginning section))
      (pi-toggle-section))))

(defun pi-section-autohide ()
  "Hide sections beyond `pi-section-autohide-count'."
  (interactive)
  (when-let* ((count pi-section-autohide-count)
              (children (pi-section-children pi-section--root-section)))
    (let ((hide-count (max 0 (- (length children) count))))
      (dolist (child (seq-take children hide-count))
        (when (and (eq (pi-section-visibility child) :autoshow)
                   (not (and (>= (point) (pi-section-beginning child))
                             (< (point) (pi-section-end child)))))
          (pi-section--set-visibility child :autohide))))))

(defun pi-section-show-level-1-all ()
  "Collapse all the sections in the pi status buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp)) (pi-section--current-section))
      (let ((section (pi-section--current-section)))
	(pi-section--set-visibility section :hide))
      (forward-line 1))))


(defmacro pi-section--section-case (&rest clauses)
  "Make different action depending of current section.

CLAUSES is a list of CLAUSE, each clause is (SECTION-TYPE &BODY)
where SECTION-TYPE describe section where BODY will be run.

This returns non-nil if some section matches.  If the
corresponding body return a non-nil value, it is returned,
otherwise it return t."

  (declare (indent 1)
           (debug (&rest (sexp body))))
  (let ((section (make-symbol "*section*"))
        (path (make-symbol "*path*")))
    `(let* ((,section (pi-section--current-section))
            (,path (pi-section--section-path ,section)))
       (cond ,@(mapcar (lambda (clause)
                         (let ((prefix (car clause))
                               (body (cdr clause)))
                           `(,(if (eq prefix t)
                                  `t
                                `(pi-section--prefix-p ',(reverse prefix) (reverse ,path)))
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
             (root (pi-section--create-root-section))
             (build (pi-section--new-section 'build root))
             (compile (pi-section--new-section 'compile build))
             (tests (pi-section--new-section 'test build))
             (unit-tests (pi-section--new-section 'test tests))
             (integration-tests (pi-section--new-section 'integration-tests tests))
             (logs (pi-section--new-section 'logs root))
             (server-log (pi-section--new-section 'server-log logs))
             (worker-log (pi-section--new-section 'worker-log logs))
             (deploy (pi-section--new-section 'deploy root)))
        (pi-section--insert-section build
          (insert "[-] Build\n"))
        (pi-section--insert-section compile
          (insert "  [-] Compile\n")
          (insert "      Compiling foo.c\n")
          (insert "      Compiling bar.c\n"))
        (pi-section--insert-section tests
          (insert "  [-] Tests\n"))
        (pi-section--insert-section unit-tests
          (insert "      [-] Unit Tests\n")
          (insert "          test-auth ... ok\n")
          (insert "          test-db ... ok\n"))
        (pi-section--insert-section integration-tests
          (insert "      [-] Integration Tests\n")
          (insert "          api-flow ... running\n"))
        (pi-section--insert-section logs
          (insert "[-] Logs\n"))
        (pi-section--insert-section server-log
          (insert "  [-] Server\n")
          (insert "      Listening on :8080\n")
          (insert "      Connected client #42\n"))
        (pi-section--insert-section worker-log
          (insert "  [-] Worker\n")
          (insert "      Job started\n")
          (insert "      Job completed\n"))
        (pi-section--insert-section deploy
          (insert "[-] Deploy\n")
          (insert "    Uploading artifacts...\n")
          (insert "    Restarting services...\n"))
        (pi-section--append-section server-log
          (insert "      Connected client #43\n")
          (insert "      Connected client #44\n")
          (insert "      Connected client #45\n"))
        (pi-section--replace-section worker-log
          (insert "  [-] Worker\n")
          (insert "      Restarted\n")
          (insert "      Processing queue...\n")
          (insert "      Queue drained\n"))
        (pi-section--append-section server-log
          (insert "      Connected client #46\n")
          (insert "      Connected client #47\n")
          (insert "      Connected client #48\n")))

      (setq buffer-read-only t)
      (goto-char (point-min)))

    (pop-to-buffer buf)))

(defun pi-describe-section (section &optional indent)
  "Pretty print SECTION and its children with INDENT.
Does not recurse into the parent."
  (interactive (list (pi-section--current-section) 0))
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
    (princ (format "%s  visibility: %s\n" prefix
                   (pi-section-visibility section)))
    (when (pi-section-info section)
      (princ (format "%s  info: %s\n" prefix
                     (pi-section-info section))))
    (let ((children (pi-section-children section)))
      (when children
        (princ (format "%s  Children:\n" prefix))
        (dolist (child children)
          (pi-describe-section child (1+ indent)))))))

(defun pi-section--section-line ()
  "Return the 0-based line number of point within the current section.
Returns 0 if point is on the first line of the section or if there is
no current section."
  (if-let ((section (pi-section--current-section)))
      (- (line-number-at-pos (point))
         (line-number-at-pos (pi-section-beginning section)))
    0))

(provide 'pi-section)

;;; pi-section.el ends here
