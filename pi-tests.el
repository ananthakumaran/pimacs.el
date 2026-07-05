;;; pi-tests --- This file contains automated tests for pi.el -*- lexical-binding: t; -*-

;;; Code:

;; Test setup:

(require 'ert)

;; development only packages, not declared as a package-dependency
(package-initialize)

(require 'undercover)
(undercover "*.el"
            (:report-format 'codecov)
            (:send-report nil)
            (:exclude "*-tests.el"))

(require 'pi)

(ert-deftest pi--parse-slash-command ()
  (should (equal (pi--parse-slash-command "/model") '(pi-select-model . nil)))
  (should (equal (pi--parse-slash-command "/new") '(pi-new-session . nil)))
  (should (equal (pi--parse-slash-command "/resume") '(pi-resume . nil)))
  (should (equal (pi--parse-slash-command "/compact") '(pi-compact . nil)))
  (should (equal (pi--parse-slash-command "/set-auto-compaction") '(pi-set-auto-compaction . nil)))
  (should (equal (pi--parse-slash-command "/set-auto-retry") '(pi-set-auto-retry . nil)))
  (let ((err (should-error (pi--parse-slash-command "/set-auto-compaction true"))))
    (should (equal "Slash command \"/set-auto-compaction\" does not accept arguments" (error-message-string err))))
  (should (equal (pi--parse-slash-command "/compact custom instructions") '(pi-compact . "custom instructions")))
  (should (equal (pi--parse-slash-command "  /model") '(pi-select-model . nil)))
  (should (equal (pi--parse-slash-command "/model ") '(pi-select-model . nil)))
  (should (null (pi--parse-slash-command "/unknown")))
  (should (null (pi--parse-slash-command "/modelx")))
  (should (null (pi--parse-slash-command "/")))
  (should (null (pi--parse-slash-command "/123")))
  (should (null (pi--parse-slash-command "not-a-slash /model")))
  (should (null (pi--parse-slash-command "")))
  (let ((err (should-error (pi--parse-slash-command "/model arg"))))
    (should (equal "Slash command \"/model\" does not accept arguments" (error-message-string err)))))

(ert-deftest pi--parse-bang-command ()
  (should (equal (pi--parse-bang-command "!ls") "ls"))
  (should (equal (pi--parse-bang-command "!ls -la") "ls -la"))
  (should (equal (pi--parse-bang-command "  !ls") "ls"))
  (should (equal (pi--parse-bang-command "! cat!") " cat!"))
  (should (null (pi--parse-bang-command "!!ls")))
  (should (null (pi--parse-bang-command "!!")))
  (should (null (pi--parse-bang-command "!")))
  (should (null (pi--parse-bang-command "! ")))
  (should (null (pi--parse-bang-command "!!  ")))
  (should (null (pi--parse-bang-command "not-a-bang !ls")))
  (should (null (pi--parse-bang-command ""))))

(ert-deftest pi--parse-double-bang-command ()
  (should (equal (pi--parse-double-bang-command "!!ls") "ls"))
  (should (equal (pi--parse-double-bang-command "!!ls -la") "ls -la"))
  (should (equal (pi--parse-double-bang-command "  !!ls") "ls"))
  (should (null (pi--parse-double-bang-command "!!")))
  (should (null (pi--parse-double-bang-command "!")))
  (should (null (pi--parse-double-bang-command "  !!")))
  (should (null (pi--parse-double-bang-command "!! ")))
  (should (null (pi--parse-double-bang-command "! ")))
  (should (null (pi--parse-double-bang-command "!ls")))
  (should (null (pi--parse-double-bang-command "not-a-bang !!ls")))
  (should (null (pi--parse-double-bang-command ""))))

(ert-deftest pi--extract-truncation-notice-more-lines ()
  (should (equal (pi--extract-truncation-notice
                  "line1\nline2\n[40 more lines in file. Use offset=61 to continue.]")
                 '("line1\nline2" . "[40 more lines in file. Use offset=61 to continue.]"))))

(ert-deftest pi--extract-truncation-notice-showing-lines ()
  (should (equal (pi--extract-truncation-notice
                  "line1\nline2\n[Showing lines 1-1648 of 6218 (50.0KB limit). Use offset=1649 to continue.]")
                 '("line1\nline2" . "[Showing lines 1-1648 of 6218 (50.0KB limit). Use offset=1649 to continue.]"))))

(ert-deftest pi--extract-truncation-notice-no-notice ()
  (should (equal (pi--extract-truncation-notice "line1\nline2\nline3")
                 '("line1\nline2\nline3" . nil))))

(ert-deftest pi--extract-truncation-notice-empty ()
  (should (equal (pi--extract-truncation-notice "")
                 '("" . nil))))

(ert-deftest pi--extract-truncation-notice-showing-lines-no-size ()
  (should (equal (pi--extract-truncation-notice
                  "line1\nline2\n[Showing lines 1-1648 of 6218. Use offset=1649 to continue.]")
                 '("line1\nline2" . "[Showing lines 1-1648 of 6218. Use offset=1649 to continue.]"))))

(ert-deftest pi--extract-truncation-notice-bash-fallback ()
  (should (equal (pi--extract-truncation-notice
                  "line1\nline2\n[Line 1 is 100KB, exceeds 50.0KB limit. Use bash: sed -n '1p' main.go | head -c 51200]")
                 '("line1\nline2" . "[Line 1 is 100KB, exceeds 50.0KB limit. Use bash: sed -n '1p' main.go | head -c 51200]"))))

(ert-deftest pi--join-test ()
  (should (equal (pi--join nil) ""))
  (should (equal (pi--join '()) ""))
  (should (equal (pi--join "hello") "hello"))
  (should (equal (pi--join '("a" "b" "c")) "a\nb\nc"))
  (should (equal (pi--join '("a" "b" "c") ",") "a,b,c"))
  (should (equal (pi--join '("key" . "value")) "value"))
  (should (equal (pi--join '(("k1" . "v1") ("k2" . "v2"))) "v1\nv2"))
  (should (equal (pi--join '(("k1" . "v1") ("k2" . "v2")) ",") "v1,v2"))
  (should (equal (pi--join '(("k1" . "a\nb") ("k2" . "c"))) "a\nb\nc")))

(ert-deftest pi--update-status-widget-joins-statuses-with-space ()
  (with-temp-buffer
    (setq pi--status-widget
          (widget-create 'pi-item :face 'pi-status-face pi--empty-widget-text))
    (setq pi--status-widget-texts (make-hash-table :test 'equal))

    (pi--handle-set-status '(:statusKey "status-b" :statusText "Status B"))
    (pi--handle-set-status '(:statusKey "status-a" :statusText "Status\nA"))

    (should (equal (widget-value pi--status-widget) "Status\nA Status B\n"))))

(ert-deftest pi-clear-ui-keeps-sections-before-prompt-widgets ()
  (with-temp-buffer
    (pi-section--create-root-section)
    (setq pi--tool-calls (make-hash-table :test 'equal))
    (setq pi--prompt-before-widget
          (widget-create 'pi-item :face 'pi-widget-face pi--empty-widget-text))
    (setq pi--prompt-widget
          (widget-create 'editable-field :format "%[user>%] %v" :value ""))
    (setq pi--prompt-after-widget
          (widget-create 'pi-item :face 'pi-widget-face pi--empty-widget-text))
    (setq pi--prompt-widget-lines (make-hash-table :test 'equal))
    (setq pi--status-widget
          (widget-create 'pi-item :face 'pi-status-face pi--empty-widget-text))
    (setq pi--status-widget-texts (make-hash-table :test 'equal))
    (widget-setup)

    (cl-labels ((insert-section ()
                  (let (section)
                    (pi--widget-save-excursion
                      (setq section
                            (pi-section--create-section 'info pi-section--root-section
                              (insert "sections"))))
                    section))
                (set-widgets ()
                  (pi--handle-set-widget '(:widgetKey "before"
                                                      :widgetLines ("before-widget")
                                                      :widgetPlacement "aboveEditor"))
                  (pi--handle-set-widget '(:widgetKey "after"
                                                      :widgetLines ("after-widget")
                                                      :widgetPlacement "belowEditor"))))
      (set-widgets)
      (let ((section (insert-section)))
        (should (< (marker-position (pi-section-beginning section))
                   (marker-position (widget-get pi--prompt-before-widget :from))
                   (marker-position (widget-get pi--prompt-widget :from))
                   (marker-position (widget-get pi--prompt-after-widget :from)))))

      (pi--widget-save-excursion
        (pi--clear-sections)
        (pi--clear-session-widgets))

      (set-widgets)
      (let ((section (insert-section)))
        (should (< (marker-position (pi-section-beginning section))
                   (marker-position (widget-get pi--prompt-before-widget :from))
                   (marker-position (widget-get pi--prompt-widget :from))
                   (marker-position (widget-get pi--prompt-after-widget :from))))))))

;;; pi-tests.el ends here

