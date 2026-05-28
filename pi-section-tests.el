;;; pi-section-tests --- Tests for pi-section.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

;; development only packages, not declared as a package-dependency
(package-initialize)

(require 'pi-section)

(defvar pi-root-section nil)

(defmacro pi-section-tests-with-demo-buffer (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (let* ((pi-root-section (pi-create-root-section))
            (build (pi-new-section "Build" 'build pi-root-section))
            (compile (pi-new-section "Compile" 'compile build))
            (tests (pi-new-section "Tests" 'test build))
            (unit-tests (pi-new-section "Unit Tests" 'test tests))
            (integration-tests (pi-new-section "Integration Tests" 'integration-tests tests))
            (logs (pi-new-section "Logs" 'logs pi-root-section))
            (server-log (pi-new-section "Server" 'server-log logs))
            (worker-log (pi-new-section "Worker" 'worker-log logs))
            (deploy (pi-new-section "Deploy" 'deploy pi-root-section)))
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
  (with-temp-buffer
    (let ((root (pi-create-root-section)))
      (should (pi-section-p root))
      (should (eq (pi-section-type root) 'root))
      (should (equal (pi-section-title root) "Root"))
      (should (null (pi-section-parent root)))
      (should (null (pi-section-children root)))
      (should (= (pi-section-beginning root) (point-min)))
      (should (= (pi-section-end root) (point-min))))))

(ert-deftest pi-section-new-child ()
  (with-temp-buffer
    (let* ((root (pi-create-root-section))
           (child (pi-new-section "Child" 'child root)))
      (should (pi-section-p child))
      (should (eq (pi-section-type child) 'child))
      (should (equal (pi-section-title child) "Child"))
      (should (eq (pi-section-parent child) root))
      (should (memq child (pi-section-children root))))))

(ert-deftest pi-section-new-nested-children ()
  (with-temp-buffer
    (let* ((root (pi-create-root-section))
           (build (pi-new-section "Build" 'build root))
           (compile (pi-new-section "Compile" 'compile build)))
      (should (eq (pi-section-parent compile) build))
      (should (memq compile (pi-section-children build)))
      (should (eq (pi-section-parent build) root))
      (should (memq build (pi-section-children root))))))

(ert-deftest pi-section-default-hidden ()
  (with-temp-buffer
    (let* ((root (pi-create-root-section))
           (child (pi-new-section "Child" 'child root)))
      (should (equal (pi-section-hidden child) pi-section-hidden-default))
      (should (null (pi-section-hidden child))))))


;; ─── pi-insert-section ─────────────────────────────────────────────────

(ert-deftest pi-section-insert-sets-beginning-and-end ()
  (with-temp-buffer
    (let* ((root (pi-create-root-section))
           (build (pi-new-section "Build" 'build root)))
      (pi-insert-section build
        (insert "[-] Build\n"))
      (should (< (pi-section-beginning build) (pi-section-end build)))
      (should (= (pi-section-beginning build) 1))
      (should (= (pi-section-end build) 11)))))

(ert-deftest pi-section-insert-propertizes-text ()
  (with-temp-buffer
    (let* ((root (pi-create-root-section))
           (build (pi-new-section "Build" 'build root)))
      (pi-insert-section build
        (insert "[-] Build\n"))
      (goto-char 1)
      (should (eq (get-text-property (point) 'pi-section) build)))))

(ert-deftest pi-section-insert-updates-parent-end ()
  (with-temp-buffer
    (let* ((root (pi-create-root-section))
           (build (pi-new-section "Build" 'build root))
           (compile (pi-new-section "Compile" 'compile build)))
      (pi-insert-section build
        (insert "[-] Build\n"))
      (pi-insert-section compile
        (insert "  [-] Compile\n"))
      (should (>= (pi-section-end build) (pi-section-end compile)))
      (should (>= (pi-section-end root) (pi-section-end build))))))


;; ─── pi-append-section ─────────────────────────────────────────────────

(ert-deftest pi-section-append-extends-existing ()
  (with-temp-buffer
    (let* ((root (pi-create-root-section))
           (log (pi-new-section "Log" 'log root)))
      (pi-insert-section log
        (insert "[-] Log\n"))
      (let ((original-end (pi-section-end log)))
        (pi-append-section log
          (insert "extra line\n"))
        (should (> (pi-section-end log) original-end))))))

(ert-deftest pi-section-append-adds-text-properties ()
  (with-temp-buffer
    (let* ((root (pi-create-root-section))
           (log (pi-new-section "Log" 'log root)))
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
      (should (equal (pi-section-title worker) "Worker"))
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
           (worker (pi-find-section '("Logs" "Worker") pi-root-section))
           (server (pi-find-section '("Logs" "Server") pi-root-section)))
      (should worker)
      (pi-replace-section worker
        (insert "  [-] Worker\n"))
      ;; parent end should still cover the remaining server-log content
      (should (>= (pi-section-end logs) (pi-section-end server))))))

(ert-deftest pi-section-replace-clear-multiple-children ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((tests (pi-find-section '("Build" "Tests") pi-root-section)))
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
      (should (equal (pi-section-title s) "Build")))))

(ert-deftest pi-section-current-returns-correct-section ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (should (equal (pi-section-title (pi-current-section)) "Build"))))

(ert-deftest pi-section-at-on-different-lines ()
  (pi-section-tests-with-demo-buffer
    ;; Server log section
    (goto-char (point-min))
    (forward-line 10)
    (should (equal (pi-section-title (pi-current-section)) "Logs"))
    ;; Worker log section
    (goto-char (point-min))
    (forward-line 17)
    (should (equal (pi-section-title (pi-current-section)) "Worker"))))


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
                     '("Build" "Tests" "Unit Tests"))))))


;; ─── pi-find-section ───────────────────────────────────────────────────

(ert-deftest pi-find-section-by-path ()
  (pi-section-tests-with-demo-buffer
    (let* ((found (pi-find-section '("Build" "Compile") pi-root-section)))
      (should found)
      (should (equal (pi-section-title found) "Compile")))))

(ert-deftest pi-find-section-non-existent ()
  (pi-section-tests-with-demo-buffer
    (let* ((root pi-root-section)
           (found (pi-find-section '("Build" "NonExistent") root)))
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
      (should (equal (pi-section-title next) "Logs")))))

(ert-deftest pi-next-section-goes-to-parent ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 8)
    (let* ((unit-tests (pi-current-section))
           (next (pi-next-section unit-tests)))
      (should next)
      (should (equal (pi-section-title next) "Logs")))))

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
      (should (equal (pi-section-title prev) "Integration Tests")))))

(ert-deftest pi-prev-section-goes-to-parent ()
  (pi-section-tests-with-demo-buffer
    (goto-char (point-min))
    (forward-line 1)
    (let* ((compile (pi-current-section))
           (prev (pi-prev-section compile)))
      (should prev)
      (should (equal (pi-section-title prev) "Build")))))

;; ─── pi-update-section-end ─────────────────────────────────────────────

(ert-deftest pi-update-section-end-expands ()
  (with-temp-buffer
    (let* ((root (pi-create-root-section))
           (child (pi-new-section "Child" 'child root)))
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
  (with-temp-buffer
    (let* ((root (pi-create-root-section))
           (child (pi-new-section "Child" 'child root)))
      (pi-insert-section child
         (insert "  [-] Compile\n")
         (insert "      Compiling foo.c\n")
         (insert "      Compiling bar.c\n"))
      (setf (pi-section-beginning child) (set-marker (make-marker) 1))
      (setf (pi-section-end child) (set-marker (make-marker) 5))
      (setf (pi-section-beginning root) (set-marker (make-marker) 1))
      (setf (pi-section-end root) (set-marker (make-marker) 5))
      (let ((m (make-marker)))
        (set-marker m 20)
        (pi-update-section-end child m)
        (should (= (pi-section-end root) 20))))))

(ert-deftest pi-update-section-end-does-not-shrink ()
  (with-temp-buffer
    (let* ((root (pi-create-root-section))
           (child (pi-new-section "Child" 'child root)))
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


;; ─── pi-section-set-hidden / pi-toggle-section ─────────────────────────

(ert-deftest pi-section-set-hidden-makes-invisible ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((build (pi-current-section)))
      (pi-section-set-hidden build t)
      (goto-char (pi-section-beginning build))
      (forward-line 1)
      (should (get-text-property (point) 'invisible)))))

(ert-deftest pi-section-set-hidden-unhide ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((build (pi-current-section)))
      (pi-section-set-hidden build t)
      (pi-section-set-hidden build nil)
      (goto-char (pi-section-beginning build))
      (forward-line 1)
      (should (not (get-text-property (point) 'invisible))))))

(ert-deftest pi-toggle-section-toggles-hidden ()
  (pi-section-tests-with-demo-buffer
    (goto-char 1)
    (let ((build (pi-current-section)))
      (should (not (pi-section-hidden build)))
      (pi-toggle-section)
      (should (pi-section-hidden build))
      (pi-toggle-section)
      (should (not (pi-section-hidden build))))))
