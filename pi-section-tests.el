;;; pi-section-tests --- Tests for pi-section.el -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)

;; development only packages, not declared as a package-dependency
(package-initialize)

(require 'undercover)
(undercover "*.el"
            (:report-format 'codecov)
            (:send-report nil)
            (:exclude "*-tests.el"))

(require 'pi-section)
(setq pi-section-padding "")

(defmacro pi-with-root-section (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (pi-section--create-root-section)
     ,@body))

(defmacro pi-section-tests-with-demo-buffer (&rest body)
  (declare (indent 0))
  `(pi-with-root-section
     (let* ((build (pi-section--new-section 'build pi-section--root-section))
            (compile (pi-section--new-section 'compile build))
            (tests (pi-section--new-section 'test build))
            (unit-tests (pi-section--new-section 'unit-tests tests))
            (integration-tests (pi-section--new-section 'integration-tests tests))
            (logs (pi-section--new-section 'logs pi-section--root-section))
            (server-log (pi-section--new-section 'server-log logs))
            (worker-log (pi-section--new-section 'worker-log logs))
            (deploy (pi-section--new-section 'deploy pi-section--root-section)))
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
       (goto-char (point-min))
       ,@body)))

(defun pi-section-tests--visibility-indicator-overlay (section)
  (cl-find-if
   (lambda (ov)
     (overlay-get ov 'pi-section-visibility-indicator))
   (overlays-in (pi-section-beginning section)
                (save-excursion
                  (goto-char (pi-section-beginning section))
                  (line-end-position)))))


;; ─── Basic section creation ────────────────────────────────────────────

(ert-deftest pi-section-create-root ()
  (pi-with-root-section
    (should (pi-section-p pi-section--root-section))
    (should (eq (pi-section-type pi-section--root-section) 'root))
    (should (null (pi-section-parent pi-section--root-section)))
    (should (null (pi-section-children pi-section--root-section)))
    (should (= (pi-section-beginning pi-section--root-section) (point-min)))
    (should (= (pi-section-end pi-section--root-section) (point-min)))))

(ert-deftest pi-section-new-child ()
  (pi-with-root-section
    (let ((child (pi-section--new-section 'child pi-section--root-section)))
      (should (pi-section-p child))
      (should (eq (pi-section-type child) 'child))
      (should (eq (pi-section-parent child) pi-section--root-section))
      (should (memq child (pi-section-children pi-section--root-section))))))

(ert-deftest pi-section-new-nested-children ()
  (pi-with-root-section
    (let* ((build (pi-section--new-section 'build pi-section--root-section))
           (compile (pi-section--new-section 'compile build)))
      (should (eq (pi-section-parent compile) build))
      (should (memq compile (pi-section-children build)))
      (should (eq (pi-section-parent build) pi-section--root-section))
      (should (memq build (pi-section-children pi-section--root-section))))))

(ert-deftest pi-section-default-visibility ()
  (pi-with-root-section
    (let ((child (pi-section--new-section 'child pi-section--root-section)))
      (should (equal (pi-section-visibility child) pi-section--visibility-default))
      (should (eq (pi-section-visibility child) :autoshow)))))


;; ─── pi-section--insert-section ─────────────────────────────────────────────────

(ert-deftest pi-section-insert-sets-beginning-and-end ()
  (pi-with-root-section
    (let ((build (pi-section--new-section 'build pi-section--root-section)))
      (pi-section--insert-section build
        (insert "[-] Build\n"))
      (should (< (pi-section-beginning build) (pi-section-end build)))
      (should (= (pi-section-beginning build) 1))
      (should (= (pi-section-end build) 11)))))

(ert-deftest pi-section-insert-propertizes-text ()
  (pi-with-root-section
    (let ((build (pi-section--new-section 'build pi-section--root-section)))
      (pi-section--insert-section build
        (insert "[-] Build\n"))
      (goto-char 1)
      (should (eq (get-text-property (point) 'pi-section) build)))))

(ert-deftest pi-section-insert-updates-parent-end ()
  (pi-with-root-section
    (let* ((build (pi-section--new-section 'build pi-section--root-section))
           (compile (pi-section--new-section 'compile build)))
      (pi-section--insert-section build
        (insert "[-] Build\n"))
      (pi-section--insert-section compile
        (insert "  [-] Compile\n"))
      (should (>= (pi-section-end build) (pi-section-end compile)))
      (should (>= (pi-section-end pi-section--root-section) (pi-section-end build))))))


;; ─── pi-section--append-section ─────────────────────────────────────────────────

(ert-deftest pi-section-append-extends-existing ()
  (pi-with-root-section
    (let ((log (pi-section--new-section 'log pi-section--root-section)))
      (pi-section--insert-section log
        (insert "[-] Log\n"))
      (let ((original-end (pi-section-end log)))
        (pi-section--append-section log
          (insert "extra line\n"))
        (should (> (pi-section-end log) original-end))))))

(ert-deftest pi-section-append-adds-text-properties ()
  (pi-with-root-section
    (let ((log (pi-section--new-section 'log pi-section--root-section)))
      (pi-section--insert-section log
        (insert "[-] Log\n"))
      (pi-section--append-section log
        (insert "extra line\n"))
      (goto-char (point-max))
      (should (eq (get-text-property (1- (point)) 'pi-section) log)))))


;; ─── pi-section--replace-section ────────────────────────────────────────────────

(ert-deftest pi-section-replace-content ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 18)
    (let* ((worker (pi-section--current-section))
           (old-end (marker-position (pi-section-end worker))))
      (pi-section--replace-section worker
        (insert "  [-] Worker\n")
        (insert "      Restarted\n"))
      (should (eq (pi-section-type worker) 'worker-log))
      (should (< (marker-position (pi-section-end worker)) old-end))
      (goto-char (pi-section-beginning worker))
      (should (looking-at "  \\[-\\] Worker\n"))
      (forward-line 1)
      (should (looking-at "      Restarted\n"))
      (should (not (search-forward "Job started" nil t))))))

(ert-deftest pi-section-replace-clear-children ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let* ((build (pi-section--current-section))
           (old-children (pi-section-children build)))
      (should old-children)
      (pi-section--replace-section build
        (insert "[-] Build\n"))
      (should (null (pi-section-children build))))))

(ert-deftest pi-section-replace-propertizes-text ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 12)
    (let ((worker (pi-section--current-section)))
      (pi-section--replace-section worker
        (insert "  [-] Worker\n")
        (insert "      Restarted\n"))
      (goto-char (pi-section-beginning worker))
      (should (eq (get-text-property (point) 'pi-section) worker))
      (goto-char (1- (pi-section-end worker)))
      (should (eq (get-text-property (point) 'pi-section) worker)))))

(ert-deftest pi-section-replace-updates-parent-end ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 10)
    (let* ((logs (pi-section--current-section))
           (worker (pi-section--find-section '(logs worker-log) pi-section--root-section))
           (server (pi-section--find-section '(logs server-log) pi-section--root-section)))
      (should worker)
      (pi-section--replace-section worker
        (insert "  [-] Worker\n"))
      ;; parent end should still cover the remaining server-log content
      (should (>= (pi-section-end logs) (pi-section-end server))))))

(ert-deftest pi-section-replace-clear-multiple-children ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((tests (pi-section--find-section '(build test) pi-section--root-section)))
      (should (pi-section-children tests))
      (pi-section--replace-section tests
        (insert "  [-] Tests\n"))
      (should (null (pi-section-children tests))))))


;; ─── pi-section--current-section / pi-section--section-at ────────────────────────────────

(ert-deftest pi-section--section-at-returns-correct-section ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((s (pi-section--section-at (point))))
      (should (pi-section-p s))
      (should (eq (pi-section-type s) 'build)))))

(ert-deftest pi-section-current-returns-correct-section ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (should (eq (pi-section-type (pi-section--current-section)) 'build))))

(ert-deftest pi-section--section-at-on-different-lines ()
  (pi-section-tests-with-demo-buffer
    ;; Server log section
    (goto-char (point-min))
    (forward-line 10)
    (should (eq (pi-section-type (pi-section--current-section)) 'logs))
    ;; Worker log section
    (goto-char (point-min))
    (forward-line 17)
    (should (eq (pi-section-type (pi-section--current-section)) 'worker-log))))


;; ─── pi-section--section-path ───────────────────────────────────────────────────

(ert-deftest pi-section--section-path-root ()
  (pi-section-tests-with-demo-buffer
    (let ((root pi-section--root-section))
      (while (pi-section-parent root)
        (setq root (pi-section-parent root)))
      (should (equal (pi-section--section-path root) '())))))

(ert-deftest pi-section--section-path-nested ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 5)
    (let ((s (pi-section--current-section)))
      (should (equal (pi-section--section-path s)
                     '(build test unit-tests))))))


;; ─── pi-section--find-section ───────────────────────────────────────────────────

(ert-deftest pi-section--find-section-by-path ()
  (pi-section-tests-with-demo-buffer
    (let* ((found (pi-section--find-section '(build compile) pi-section--root-section)))
      (should found)
      (should (eq (pi-section-type found) 'compile)))))

(ert-deftest pi-section--find-section-non-existent ()
  (pi-section-tests-with-demo-buffer
    (let* ((root pi-section--root-section)
           (found (pi-section--find-section '(build non-existent) root)))
      (should (null found)))))

(ert-deftest pi-section--find-section-empty-path ()
  (pi-section-tests-with-demo-buffer
    (let* ((root pi-section--root-section)
           (found (pi-section--find-section '() root)))
      (should (eq found root)))))


;; ─── pi-section--next-section / pi-section--prev-section ─────────────────────────────────

(ert-deftest pi-section--next-section-first-child ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((next (pi-section--next-section (pi-section--current-section))))
      (should next)
      (should (eq (pi-section-type next) 'compile)))))

(ert-deftest pi-section--next-section-goes-to-sibling-before-parent ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 5)
    (let ((next (pi-section--next-section (pi-section--current-section))))
      (should next)
      (should (eq (pi-section-type next) 'integration-tests)))))

(ert-deftest pi-section--next-section-goes-to-parent-sibling ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 8)
    (let ((next (pi-section--next-section (pi-section--current-section))))
      (should next)
      (should (eq (pi-section-type next) 'logs)))))

(ert-deftest pi-section--next-section-last ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-max))
    (forward-line -1)
    (let ((section (pi-section--section-at (point))))
      (should (null (pi-section--next-section section))))))

(ert-deftest pi-section--next-section-of-type ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((next (pi-section--next-section-of-type (pi-section--current-section) 'deploy)))
      (should next)
      (should (eq (pi-section-type next) 'deploy)))))

(ert-deftest pi-section--next-section-of-type-missing ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((next (pi-section--next-section-of-type (pi-section--current-section) 'missing)))
      (should (null next)))))

(ert-deftest pi-section--next-section-walks-tree-in-order ()
  (pi-section-tests-with-demo-buffer
    (let* ((compile (pi-section--find-section '(build compile) pi-section--root-section))
           (next1 (pi-section--next-section compile))
           (next2 (pi-section--next-section next1))
           (next3 (pi-section--next-section next2))
           (next4 (pi-section--next-section next3)))
      (should (eq (pi-section-type next1) 'test))
      (should (eq (pi-section-type next2) 'unit-tests))
      (should (eq (pi-section-type next3) 'integration-tests))
      (should (eq (pi-section-type next4) 'logs)))))

(ert-deftest pi-section--prev-section-sibling ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 10)
    (let* ((logs (pi-section--current-section))
           (prev (pi-section--prev-section logs)))
      (should prev)
      (should (eq (pi-section-type prev) 'integration-tests)))))

(ert-deftest pi-section--prev-section-goes-to-parent ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 1)
    (let* ((compile (pi-section--current-section))
           (prev (pi-section--prev-section compile)))
      (should prev)
      (should (eq (pi-section-type prev) 'build)))))

(ert-deftest pi-section--prev-section-of-type ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 10)
    (let ((prev (pi-section--prev-section-of-type (pi-section--current-section) 'build)))
      (should prev)
      (should (eq (pi-section-type prev) 'build)))))

(ert-deftest pi-section--prev-section-of-type-missing ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((prev (pi-section--prev-section-of-type (pi-section--current-section) 'missing)))
      (should (null prev)))))

(ert-deftest pi-section--prev-section-walks-tree-in-reverse-order ()
  (pi-section-tests-with-demo-buffer
    (let* ((worker-log (pi-section--find-section '(logs worker-log) pi-section--root-section))
           (prev1 (pi-section--prev-section worker-log))
           (prev2 (pi-section--prev-section prev1))
           (prev3 (pi-section--prev-section prev2))
           (prev4 (pi-section--prev-section prev3)))
      (should (eq (pi-section-type prev1) 'server-log))
      (should (eq (pi-section-type prev2) 'logs))
      (should (eq (pi-section-type prev3) 'integration-tests))
      (should (eq (pi-section-type prev4) 'unit-tests)))))

(ert-deftest pi-section--goto-next-section-of-type ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (pi-section--goto-next-section-of-type 'deploy)
    (should (eq (pi-section-type (pi-section--current-section)) 'deploy))))

(ert-deftest pi-section--next-section-skips-hidden-children ()
  (pi-section-tests-with-demo-buffer
    (let ((build (pi-section--find-section '(build) pi-section--root-section)))
      (pi-section--set-visibility build :hide)
      (should (eq (pi-section-type (pi-section--next-section build)) 'logs)))))

(ert-deftest pi-section--prev-section-skips-hidden-children ()
  (pi-section-tests-with-demo-buffer
    (let ((logs (pi-section--find-section '(logs) pi-section--root-section))
          (build (pi-section--find-section '(build) pi-section--root-section)))
      (pi-section--set-visibility build :hide)
      (should (eq (pi-section-type (pi-section--prev-section logs)) 'build)))))

(ert-deftest pi-section--goto-previous-section-of-type ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 10)
    (pi-section--goto-previous-section-of-type 'build)
    (should (eq (pi-section-type (pi-section--current-section)) 'build))))

(ert-deftest pi-section--goto-previous-section-of-type-current-section ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 11)
    (pi-section--goto-previous-section-of-type 'logs)
    (should (eq (pi-section-type (pi-section--current-section)) 'logs))
    (should (= (point) (pi-section-beginning (pi-section--current-section))))))

;; ─── pi-section--delete-section ────────────────────────────────────────────

(ert-deftest pi-section-delete-removes-content ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((build (pi-section--current-section)))
      (pi-section--delete-section build)
      (should (not (search-forward "[-] Build" nil t)))
      (should (looking-at (regexp-quote "[-] Logs\n"))))))

(ert-deftest pi-section-delete-removes-from-parent-children ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((build (pi-section--current-section)))
      (pi-section--delete-section build)
      (should (not (memq build (pi-section-children pi-section--root-section))))
      ;; other root children remain
      (let ((remaining-types
             (mapcar #'pi-section-type (pi-section-children pi-section--root-section))))
        (should (equal remaining-types '(logs deploy)))))))

(ert-deftest pi-section-delete-updates-parent-end ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let* ((build (pi-section--current-section))
           (old-parent-end (marker-position (pi-section-end pi-section--root-section)))
           (build-size (- (marker-position (pi-section-end build))
                          (pi-section-beginning build))))
      (pi-section--delete-section build)
      (should (= (marker-position (pi-section-end pi-section--root-section))
                 (- old-parent-end build-size))))))

(ert-deftest pi-section-delete-middle-child ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 10)
    (let ((logs (pi-section--current-section)))
      (pi-section--delete-section logs)
      (goto-char (point-min))
      (should (looking-at (regexp-quote "[-] Build\n")))
      (forward-line 10)
      (should (looking-at (regexp-quote "[-] Deploy\n")))
      (let ((remaining-types
             (mapcar #'pi-section-type (pi-section-children pi-section--root-section))))
        (should (equal remaining-types '(build deploy)))))))

(ert-deftest pi-section-delete-leaf-child ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 1)
    (let* ((compile (pi-section--current-section))
           (build (pi-section-parent compile)))
      (pi-section--delete-section compile)
      (should (not (memq compile (pi-section-children build))))
      (goto-char (pi-section-beginning build))
      (should (looking-at (regexp-quote "[-] Build\n")))
      (forward-line 1)
      (should (looking-at (regexp-quote "  [-] Tests\n"))))))

(ert-deftest pi-section-delete-nested-content-gone ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 10)
    (let ((logs (pi-section--current-section)))
      (pi-section--delete-section logs)
      (goto-char (point-min))
      ;; server and worker content should be gone
      (should (not (search-forward "Connected client" nil t)))
      (should (not (search-forward "Job" nil t))))))


;; ─── pi-section--update-section-end ─────────────────────────────────────────────

(ert-deftest pi-section--update-section-end-expands ()
  (pi-with-root-section
    (let ((child (pi-section--new-section 'child pi-section--root-section)))
      (pi-section--insert-section child
        (insert "  [-] Compile\n")
        (insert "      Compiling foo.c\n")
        (insert "      Compiling bar.c\n"))
      (setf (pi-section-beginning child) (point-min))
      (setf (pi-section-end child) (point-min-marker))
      (let ((m (make-marker)))
        (set-marker m 10)
        (pi-section--update-section-end child m)
        (should (= (pi-section-end child) 10))))))

(ert-deftest pi-section--update-section-end-propagates-to-parent ()
  (pi-with-root-section
    (let ((child (pi-section--new-section 'child pi-section--root-section)))
      (pi-section--insert-section child
        (insert "  [-] Compile\n")
        (insert "      Compiling foo.c\n")
        (insert "      Compiling bar.c\n"))
      (setf (pi-section-beginning child) (set-marker (make-marker) 1))
      (setf (pi-section-end child) (set-marker (make-marker) 5))
      (setf (pi-section-beginning pi-section--root-section) (set-marker (make-marker) 1))
      (setf (pi-section-end pi-section--root-section) (set-marker (make-marker) 5))
      (let ((m (make-marker)))
        (set-marker m 20)
        (pi-section--update-section-end child m)
        (should (= (pi-section-end pi-section--root-section) 20))))))

(ert-deftest pi-section--update-section-end-does-not-shrink ()
  (pi-with-root-section
    (let ((child (pi-section--new-section 'child pi-section--root-section)))
      (pi-section--insert-section child
        (insert "  [-] Compile\n")
        (insert "      Compiling foo.c\n")
        (insert "      Compiling bar.c\n"))
      (setf (pi-section-beginning child) (set-marker (make-marker) 1))
      (setf (pi-section-end child) (set-marker (make-marker) 20))
      (let ((m (make-marker)))
        (set-marker m 5)
        (pi-section--update-section-end child m)
        (should (= (pi-section-end child) 20))))))


;; ─── pi-section--set-visibility / pi-toggle-section ─────────────────────

(ert-deftest pi-section--set-visibility-hides ()
  "Setting visibility to :hide or :autohide makes content invisible."
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((build (pi-section--current-section)))
      (pi-section--set-visibility build :hide)
      (goto-char (pi-section-beginning build))
      (forward-line 1)
      (should (invisible-p (point)))
      (pi-section--set-visibility build :autohide)
      (should (invisible-p (point))))))

(ert-deftest pi-section--set-visibility-shows ()
  "Setting visibility to :show or :autoshow makes content visible."
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((build (pi-section--current-section)))
      (pi-section--set-visibility build :hide)
      (pi-section--set-visibility build :show)
      (goto-char (pi-section-beginning build))
      (forward-line 1)
      (should (not (invisible-p (point))))
      (pi-section--set-visibility build :autoshow)
      (should (not (invisible-p (point)))))))

(ert-deftest pi-toggle-section-toggles-visibility ()
  "Toggle transitions: :autoshow->:hide, :autohide->:show, :show->:hide, :hide->:show."
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((build (pi-section--current-section)))
      (should (eq (pi-section-visibility build) :autoshow))
      ;; :autoshow -> :hide
      (pi-toggle-section)
      (should (eq (pi-section-visibility build) :hide))
      ;; :hide -> :show
      (pi-toggle-section)
      (should (eq (pi-section-visibility build) :show))
      ;; :show -> :hide
      (pi-toggle-section)
      (should (eq (pi-section-visibility build) :hide))
      ;; :hide -> :show
      (pi-toggle-section)
      (should (eq (pi-section-visibility build) :show))
      ;; :autohide -> :show
      (pi-section--set-visibility build :autohide)
      (should (eq (pi-section-visibility build) :autohide))
      (pi-toggle-section)
      (should (eq (pi-section-visibility build) :show)))))

(ert-deftest pi-section--set-visibility-updates-fringe-indicator ()
  (cl-letf (((symbol-function 'display-graphic-p)
             (lambda (&optional _frame) t)))
    (pi-section-tests-with-demo-buffer
      (let* ((build (pi-section--current-section))
             (overlay (pi-section-tests--visibility-indicator-overlay build)))
        (should overlay)
        (should (equal (get-text-property 0 'display
                                          (overlay-get overlay 'before-string))
                       '(left-fringe pi-section-fringe-bitmapv fringe)))
        (pi-section--set-visibility build :hide)
        (setq overlay (pi-section-tests--visibility-indicator-overlay build))
        (should overlay)
        (should (equal (get-text-property 0 'display
                                          (overlay-get overlay 'before-string))
                       '(left-fringe pi-section-fringe-bitmap> fringe)))))))

(ert-deftest pi-section--visibility-indicator-shows-for-leaf-sections ()
  (cl-letf (((symbol-function 'display-graphic-p)
             (lambda (&optional _frame) t)))
    (pi-with-root-section
      (let ((leaf (pi-section--new-section 'leaf pi-section--root-section)))
        (pi-section--insert-section leaf
          (insert "[-] Leaf\n"))
        (should (pi-section-tests--visibility-indicator-overlay leaf))))))

(ert-deftest pi-section--visibility-indicator-skips-root ()
  (cl-letf (((symbol-function 'display-graphic-p)
             (lambda (&optional _frame) t)))
    (pi-with-root-section
      (pi-section--propertize-section pi-section--root-section)
      (should-not (pi-section-tests--visibility-indicator-overlay
                   pi-section--root-section)))))

(ert-deftest pi-section--delete-section-keeps-parent-fringe-indicator ()
  (cl-letf (((symbol-function 'display-graphic-p)
             (lambda (&optional _frame) t)))
    (pi-with-root-section
      (let* ((parent (pi-section--new-section 'parent pi-section--root-section))
             (child (pi-section--new-section 'child parent)))
        (pi-section--insert-section parent
          (insert "[-] Parent\n"))
        (pi-section--insert-section child
          (insert "  [-] Child\n"))
        (should (pi-section-tests--visibility-indicator-overlay parent))
        (pi-section--delete-section child)
        (should (pi-section-tests--visibility-indicator-overlay parent))))))


;; ─── pi-section-autohide ───────────────────────────────────────────────

(ert-deftest pi-section-autohide-nil-count ()
  (pi-with-root-section
    (let ((a (pi-section--new-section 'a pi-section--root-section))
          (b (pi-section--new-section 'b pi-section--root-section)))
      (pi-section--insert-section a (insert "[-] A\n"))
      (pi-section--insert-section b (insert "[-] B\n"))
      (let ((pi-section-autohide-count nil))
        (pi-section-autohide)
        (should (eq (pi-section-visibility a) :autoshow))
        (should (eq (pi-section-visibility b) :autoshow))))))

(ert-deftest pi-section-autohide-skips-middle-section-at-point ()
  (pi-with-root-section
    (let ((a (pi-section--new-section 'a pi-section--root-section))
          (b (pi-section--new-section 'b pi-section--root-section))
          (c (pi-section--new-section 'c pi-section--root-section))
          (d (pi-section--new-section 'd pi-section--root-section)))
      (pi-section--insert-section a (insert "[-] A\n"))
      (pi-section--insert-section b (insert "[-] B\n"))
      (pi-section--insert-section c (insert "[-] C\n"))
      (pi-section--insert-section d (insert "[-] D\n"))
      (let ((pi-section-autohide-count 2))
        (goto-char (pi-section-beginning b))
        (pi-section-autohide)
        (should (eq (pi-section-visibility a) :autohide))
        (should (eq (pi-section-visibility b) :autoshow))
        (should (eq (pi-section-visibility c) :autoshow))
        (should (eq (pi-section-visibility d) :autoshow))))))

(ert-deftest pi-section-autohide-skips-non-autoshow ()
  (pi-with-root-section
    (let ((a (pi-section--new-section 'a pi-section--root-section))
          (b (pi-section--new-section 'b pi-section--root-section))
          (c (pi-section--new-section 'c pi-section--root-section)))
      (pi-section--insert-section a (insert "[-] A\n"))
      (pi-section--insert-section b (insert "[-] B\n"))
      (pi-section--insert-section c (insert "[-] C\n"))
      (pi-section--set-visibility a :show)
      (let ((pi-section-autohide-count 1))
        (pi-section-autohide)
        (should (eq (pi-section-visibility a) :show))
        (should (eq (pi-section-visibility b) :autohide))
        (should (eq (pi-section-visibility c) :autoshow))))))
