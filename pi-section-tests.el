;;; pi-section-tests --- Tests for pi-section.el -*- lexical-binding: t; -*-

;;; Code:

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
     (pi-create-root-section)
     ,@body))

(defmacro pi-section-tests-with-demo-buffer (&rest body)
  (declare (indent 0))
  `(pi-with-root-section
     (let* ((build (pi-new-section 'build pi-root-section))
            (compile (pi-new-section 'compile build))
            (tests (pi-new-section 'test build))
            (unit-tests (pi-new-section 'unit-tests tests))
            (integration-tests (pi-new-section 'integration-tests tests))
            (logs (pi-new-section 'logs pi-root-section))
            (server-log (pi-new-section 'server-log logs))
            (worker-log (pi-new-section 'worker-log logs))
            (deploy (pi-new-section 'deploy pi-root-section)))
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
       (goto-char (point-min))
       ,@body)))


;; ─── Basic section creation ────────────────────────────────────────────

(ert-deftest pi-section-create-root ()
  (pi-with-root-section
    (should (pi-section-p pi-root-section))
    (should (eq (pi-section-type pi-root-section) 'root))
    (should (null (pi-section-parent pi-root-section)))
    (should (null (pi-section-children pi-root-section)))
    (should (= (pi-section-beginning pi-root-section) (point-min)))
    (should (= (pi-section-end pi-root-section) (point-min)))))

(ert-deftest pi-section-new-child ()
  (pi-with-root-section
    (let ((child (pi-new-section 'child pi-root-section)))
      (should (pi-section-p child))
      (should (eq (pi-section-type child) 'child))
      (should (eq (pi-section-parent child) pi-root-section))
      (should (memq child (pi-section-children pi-root-section))))))

(ert-deftest pi-section-new-nested-children ()
  (pi-with-root-section
    (let* ((build (pi-new-section 'build pi-root-section))
           (compile (pi-new-section 'compile build)))
      (should (eq (pi-section-parent compile) build))
      (should (memq compile (pi-section-children build)))
      (should (eq (pi-section-parent build) pi-root-section))
      (should (memq build (pi-section-children pi-root-section))))))

(ert-deftest pi-section-default-visibility ()
  (pi-with-root-section
    (let ((child (pi-new-section 'child pi-root-section)))
      (should (equal (pi-section-visibility child) pi-section-visibility-default))
      (should (eq (pi-section-visibility child) :autoshow)))))


;; ─── pi-insert-section ─────────────────────────────────────────────────

(ert-deftest pi-section-insert-sets-beginning-and-end ()
  (pi-with-root-section
    (let ((build (pi-new-section 'build pi-root-section)))
      (pi-insert-section build
        (insert "[-] Build\n"))
      (should (< (pi-section-beginning build) (pi-section-end build)))
      (should (= (pi-section-beginning build) 1))
      (should (= (pi-section-end build) 11)))))

(ert-deftest pi-section-insert-propertizes-text ()
  (pi-with-root-section
    (let ((build (pi-new-section 'build pi-root-section)))
      (pi-insert-section build
        (insert "[-] Build\n"))
      (goto-char 1)
      (should (eq (get-text-property (point) 'pi-section) build)))))

(ert-deftest pi-section-insert-updates-parent-end ()
  (pi-with-root-section
    (let* ((build (pi-new-section 'build pi-root-section))
           (compile (pi-new-section 'compile build)))
      (pi-insert-section build
        (insert "[-] Build\n"))
      (pi-insert-section compile
        (insert "  [-] Compile\n"))
      (should (>= (pi-section-end build) (pi-section-end compile)))
      (should (>= (pi-section-end pi-root-section) (pi-section-end build))))))


;; ─── pi-append-section ─────────────────────────────────────────────────

(ert-deftest pi-section-append-extends-existing ()
  (pi-with-root-section
    (let ((log (pi-new-section 'log pi-root-section)))
      (pi-insert-section log
        (insert "[-] Log\n"))
      (let ((original-end (pi-section-end log)))
        (pi-append-section log
          (insert "extra line\n"))
        (should (> (pi-section-end log) original-end))))))

(ert-deftest pi-section-append-adds-text-properties ()
  (pi-with-root-section
    (let ((log (pi-new-section 'log pi-root-section)))
      (pi-insert-section log
        (insert "[-] Log\n"))
      (pi-append-section log
        (insert "extra line\n"))
      (goto-char (point-max))
      (should (eq (get-text-property (1- (point)) 'pi-section) log)))))


;; ─── pi-replace-section ────────────────────────────────────────────────

(ert-deftest pi-section-replace-content ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 18)
    (let* ((worker (pi-current-section))
           (old-end (marker-position (pi-section-end worker))))
      (pi-replace-section worker
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
    (let* ((build (pi-current-section))
           (old-children (pi-section-children build)))
      (should old-children)
      (pi-replace-section build
        (insert "[-] Build\n"))
      (should (null (pi-section-children build))))))

(ert-deftest pi-section-replace-propertizes-text ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 12)
    (let ((worker (pi-current-section)))
      (pi-replace-section worker
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
    (let* ((logs (pi-current-section))
           (worker (pi-find-section '(logs worker-log) pi-root-section))
           (server (pi-find-section '(logs server-log) pi-root-section)))
      (should worker)
      (pi-replace-section worker
        (insert "  [-] Worker\n"))
      ;; parent end should still cover the remaining server-log content
      (should (>= (pi-section-end logs) (pi-section-end server))))))

(ert-deftest pi-section-replace-clear-multiple-children ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((tests (pi-find-section '(build test) pi-root-section)))
      (should (pi-section-children tests))
      (pi-replace-section tests
        (insert "  [-] Tests\n"))
      (should (null (pi-section-children tests))))))


;; ─── pi-current-section / pi-section-at ────────────────────────────────

(ert-deftest pi-section-at-returns-correct-section ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((s (pi-section-at (point))))
      (should (pi-section-p s))
      (should (eq (pi-section-type s) 'build)))))

(ert-deftest pi-section-current-returns-correct-section ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (should (eq (pi-section-type (pi-current-section)) 'build))))

(ert-deftest pi-section-at-on-different-lines ()
  (pi-section-tests-with-demo-buffer
    ;; Server log section
    (goto-char (point-min))
    (forward-line 10)
    (should (eq (pi-section-type (pi-current-section)) 'logs))
    ;; Worker log section
    (goto-char (point-min))
    (forward-line 17)
    (should (eq (pi-section-type (pi-current-section)) 'worker-log))))


;; ─── pi-section-path ───────────────────────────────────────────────────

(ert-deftest pi-section-path-root ()
  (pi-section-tests-with-demo-buffer
    (let ((root pi-root-section))
      (while (pi-section-parent root)
        (setq root (pi-section-parent root)))
      (should (equal (pi-section-path root) '())))))

(ert-deftest pi-section-path-nested ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 5)
    (let ((s (pi-current-section)))
      (should (equal (pi-section-path s)
                     '(build test unit-tests))))))


;; ─── pi-find-section ───────────────────────────────────────────────────

(ert-deftest pi-find-section-by-path ()
  (pi-section-tests-with-demo-buffer
    (let* ((found (pi-find-section '(build compile) pi-root-section)))
      (should found)
      (should (eq (pi-section-type found) 'compile)))))

(ert-deftest pi-find-section-non-existent ()
  (pi-section-tests-with-demo-buffer
    (let* ((root pi-root-section)
           (found (pi-find-section '(build non-existent) root)))
      (should (null found)))))

(ert-deftest pi-find-section-empty-path ()
  (pi-section-tests-with-demo-buffer
    (let* ((root pi-root-section)
           (found (pi-find-section '() root)))
      (should (eq found root)))))


;; ─── pi-next-section / pi-prev-section ─────────────────────────────────

(ert-deftest pi-next-section-sibling ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let* ((build (pi-current-section))
           (next (pi-next-section build)))
      (should next)
      (should (eq (pi-section-type next) 'logs)))))

(ert-deftest pi-next-section-goes-to-parent ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 8)
    (let* ((unit-tests (pi-current-section))
           (next (pi-next-section unit-tests)))
      (should next)
      (should (eq (pi-section-type next) 'logs)))))

(ert-deftest pi-next-section-last ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-max))
    (forward-line -1)
    (let ((section (pi-section-at (point))))
      (should (null (pi-next-section section))))))

(ert-deftest pi-prev-section-sibling ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 10)
    (let* ((logs (pi-current-section))
           (prev (pi-prev-section logs)))
      (should prev)
      (should (eq (pi-section-type prev) 'integration-tests)))))

(ert-deftest pi-prev-section-goes-to-parent ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 1)
    (let* ((compile (pi-current-section))
           (prev (pi-prev-section compile)))
      (should prev)
      (should (eq (pi-section-type prev) 'build)))))

;; ─── pi-delete-section ────────────────────────────────────────────

(ert-deftest pi-section-delete-removes-content ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((build (pi-current-section)))
      (pi-delete-section build)
      (should (not (search-forward "[-] Build" nil t)))
      (should (looking-at (regexp-quote "[-] Logs\n"))))))

(ert-deftest pi-section-delete-removes-from-parent-children ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((build (pi-current-section)))
      (pi-delete-section build)
      (should (not (memq build (pi-section-children pi-root-section))))
      ;; other root children remain
      (let ((remaining-types
             (mapcar #'pi-section-type (pi-section-children pi-root-section))))
        (should (equal remaining-types '(logs deploy)))))))

(ert-deftest pi-section-delete-updates-parent-end ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let* ((build (pi-current-section))
           (old-parent-end (marker-position (pi-section-end pi-root-section)))
           (build-size (- (marker-position (pi-section-end build))
                          (pi-section-beginning build))))
      (pi-delete-section build)
      (should (= (marker-position (pi-section-end pi-root-section))
                 (- old-parent-end build-size))))))

(ert-deftest pi-section-delete-middle-child ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 10)
    (let ((logs (pi-current-section)))
      (pi-delete-section logs)
      (goto-char (point-min))
      (should (looking-at (regexp-quote "[-] Build\n")))
      (forward-line 10)
      (should (looking-at (regexp-quote "[-] Deploy\n")))
      (let ((remaining-types
             (mapcar #'pi-section-type (pi-section-children pi-root-section))))
        (should (equal remaining-types '(build deploy)))))))

(ert-deftest pi-section-delete-leaf-child ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 1)
    (let* ((compile (pi-current-section))
           (build (pi-section-parent compile)))
      (pi-delete-section compile)
      (should (not (memq compile (pi-section-children build))))
      (goto-char (pi-section-beginning build))
      (should (looking-at (regexp-quote "[-] Build\n")))
      (forward-line 1)
      (should (looking-at (regexp-quote "  [-] Tests\n"))))))

(ert-deftest pi-section-delete-nested-content-gone ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 10)
    (let ((logs (pi-current-section)))
      (pi-delete-section logs)
      (goto-char (point-min))
      ;; server and worker content should be gone
      (should (not (search-forward "Connected client" nil t)))
      (should (not (search-forward "Job" nil t))))))


;; ─── pi-update-section-end ─────────────────────────────────────────────

(ert-deftest pi-update-section-end-expands ()
  (pi-with-root-section
    (let ((child (pi-new-section 'child pi-root-section)))
      (pi-insert-section child
        (insert "  [-] Compile\n")
        (insert "      Compiling foo.c\n")
        (insert "      Compiling bar.c\n"))
      (setf (pi-section-beginning child) (point-min))
      (setf (pi-section-end child) (point-min-marker))
      (let ((m (make-marker)))
        (set-marker m 10)
        (pi-update-section-end child m)
        (should (= (pi-section-end child) 10))))))

(ert-deftest pi-update-section-end-propagates-to-parent ()
  (pi-with-root-section
    (let ((child (pi-new-section 'child pi-root-section)))
      (pi-insert-section child
        (insert "  [-] Compile\n")
        (insert "      Compiling foo.c\n")
        (insert "      Compiling bar.c\n"))
      (setf (pi-section-beginning child) (set-marker (make-marker) 1))
      (setf (pi-section-end child) (set-marker (make-marker) 5))
      (setf (pi-section-beginning pi-root-section) (set-marker (make-marker) 1))
      (setf (pi-section-end pi-root-section) (set-marker (make-marker) 5))
      (let ((m (make-marker)))
        (set-marker m 20)
        (pi-update-section-end child m)
        (should (= (pi-section-end pi-root-section) 20))))))

(ert-deftest pi-update-section-end-does-not-shrink ()
  (pi-with-root-section
    (let ((child (pi-new-section 'child pi-root-section)))
      (pi-insert-section child
        (insert "  [-] Compile\n")
        (insert "      Compiling foo.c\n")
        (insert "      Compiling bar.c\n"))
      (setf (pi-section-beginning child) (set-marker (make-marker) 1))
      (setf (pi-section-end child) (set-marker (make-marker) 20))
      (let ((m (make-marker)))
        (set-marker m 5)
        (pi-update-section-end child m)
        (should (= (pi-section-end child) 20))))))


;; ─── pi-section-set-visibility / pi-toggle-section ─────────────────────

(ert-deftest pi-section-set-visibility-hides ()
  "Setting visibility to :hide or :autohide makes content invisible."
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((build (pi-current-section)))
      (pi-section-set-visibility build :hide)
      (goto-char (pi-section-beginning build))
      (forward-line 1)
      (should (invisible-p (point)))
      (pi-section-set-visibility build :autohide)
      (should (invisible-p (point))))))

(ert-deftest pi-section-set-visibility-shows ()
  "Setting visibility to :show or :autoshow makes content visible."
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((build (pi-current-section)))
      (pi-section-set-visibility build :hide)
      (pi-section-set-visibility build :show)
      (goto-char (pi-section-beginning build))
      (forward-line 1)
      (should (not (invisible-p (point))))
      (pi-section-set-visibility build :autoshow)
      (should (not (invisible-p (point)))))))

(ert-deftest pi-toggle-section-toggles-visibility ()
  "Toggle transitions: :autoshow->:hide, :autohide->:show, :show->:hide, :hide->:show."
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((build (pi-current-section)))
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
      (pi-section-set-visibility build :autohide)
      (should (eq (pi-section-visibility build) :autohide))
      (pi-toggle-section)
      (should (eq (pi-section-visibility build) :show)))))
