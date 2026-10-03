;;; pimacs-tests --- This file contains automated tests for pimacs.el -*- lexical-binding: t; -*-

;;; Code:

;; Test setup:

(require 'ert)

;; development only packages, not declared as a package-dependency
(package-initialize)

(require 'undercover)
(undercover)

(require 'pimacs)

(defconst pimacs-tests--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(ert-deftest pimacs-chat-buffer-name-follows-session-name-or-short-id ()
  (with-temp-buffer
    (setq-local pimacs--project-root "/tmp/pimacs-buffer-name-test/")
    (add-hook 'pimacs--header-line-state-change-hook #'pimacs--update-chat-buffer-name nil t)
    (should (equal (pimacs--chat-buffer-name) "*pimacs-chat:pimacs-buffer-name-test*"))
    (dolist (name '("named" nil ""))
      (pimacs--set-header-line-state (list :sessionName name)
                                     '(:sessionId "00000000-11111111"))
      (let ((label (if (equal name "named") "named" "11111111")))
        (should (equal (buffer-name)
                       (format "*pimacs-chat:pimacs-buffer-name-test:%s*" label)))
        (should (equal (pimacs--chat-label (current-buffer)) label))
        (should (equal (pimacs--format-state-line-session-name pimacs--header-line-state)
                       label))))
    (pimacs--set-header-line-state nil '(:sessionId "00000000-22222222"))
    (should (equal (buffer-name) "*pimacs-chat:pimacs-buffer-name-test:22222222*"))
    (pimacs--handle-set-title '(:title "Extension title"))
    (pimacs--set-header-line-state nil '(:sessionId "00000000-22222222"))
    (should (equal (buffer-name) "*pimacs-chat:pimacs-buffer-name-test:Extension title*"))))

(ert-deftest pimacs-chat--transient-defaults-root-to-project-root ()
  (let ((prefix (transient-prefix :command 'pimacs-test)))
    (cl-letf (((symbol-function 'pimacs--project-root)
               (lambda () "/tmp/project/")))
      (pimacs-chat--transient-init-value prefix))
    (should (equal (oref prefix value) '("--root=/tmp/project/")))))

(ert-deftest pimacs--project-file-completions-use-pimacs-project-root ()
  (let ((pimacs--project-root pimacs-tests--directory)
        (pimacs--project-file-cache nil))
    (let ((files (pimacs--project-file-completions "")))
      (should (member "pimacs-tests.el" files))
      (should-not (member "pimacs.el" files)))))

(ert-deftest pimacs-chat--start-uses-transient-name-and-root ()
  (let (arguments)
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix) '("--name=session" "--root=/tmp/root")))
              ((symbol-function 'pimacs-chat--create)
               (lambda (&rest args) (setq arguments args))))
      (pimacs-chat--start))
    (should (equal arguments '("session" "/tmp/root")))))

(ert-deftest pimacs--start-project-agent-loads-fresh-project-environment ()
  (let* ((root (file-name-as-directory (make-temp-file "pimacs-environment-" t)))
         (bin (expand-file-name "bin" root))
         (formatter (expand-file-name "cljfmt" bin))
         (enable-local-variables :all)
         (launches 0))
    (unwind-protect
        (progn
          (make-directory bin)
          (with-temp-file formatter
            (insert "#!/bin/sh\n[ \"$PIMACS_ENV\" = project ]\n"))
          (set-file-modes formatter #o755)
          (with-temp-file (expand-file-name ".dir-locals.el" root)
            (prin1 `((nil . ((process-environment . (,(concat "PATH=" bin)
                                                     "PIMACS_ENV=project"))
                             (exec-path . (,bin))
                             (pimacs-flags . ("--project-settings")))))
                   (current-buffer)))
          (cl-letf (((symbol-function 'pimacs--start-agent)
                     (lambda (key)
                       (cl-incf launches)
                       (should (equal key "environment-test"))
                       (should (equal pimacs--project-key key))
                       (should (equal (pimacs--project-root) root))
                       (should (equal default-directory root))
                       (should (equal pimacs-flags '("--project-settings")))
                       (should (equal (executable-find "cljfmt") formatter))
                       (should (zerop (call-process "cljfmt" nil nil nil))))))
            (with-temp-buffer
              (pimacs--start-project-agent "environment-test" root)
              (setq-local process-environment '("PATH=/missing" "PIMACS_ENV=stale"))
              (setq-local exec-path '("/missing"))
              (setq-local pimacs-flags '("--stale-settings"))
              (pimacs--start-project-agent "environment-test" root)
              (should (equal (getenv "PIMACS_ENV") "stale"))
              (should (= launches 2)))))
      (delete-directory root t))))

(ert-deftest pimacs--select-chat-appends-id-to-duplicate-names ()
  (let ((first (generate-new-buffer " *pimacs-session-1*"))
        (second (generate-new-buffer " *pimacs-session-2*"))
        (unnamed (generate-new-buffer " *pimacs-session-3*"))
        (unique (generate-new-buffer " *pimacs-session-4*"))
        labels selected)
    (unwind-protect
        (progn
          (with-current-buffer first
            (setq pimacs--header-line-state
                  '(:sessionName "shared" :sessionStats (:sessionId "00000000-11111111"))))
          (with-current-buffer second
            (setq pimacs--header-line-state
                  '(:sessionName "shared" :sessionStats (:sessionId "00000000-22222222"))))
          (with-current-buffer unnamed
            (setq pimacs--header-line-state
                  '(:sessionStats (:sessionId "00000000-33333333"))))
          (with-current-buffer unique
            (setq pimacs--header-line-state
                  '(:sessionName "unique" :sessionStats (:sessionId "00000000-44444444"))))
          (cl-letf (((symbol-function 'pimacs--completing-read)
                     (lambda (_prompt choices &rest _)
                       (setq labels (mapcar #'car choices))
                       "shared 22222222")))
            (setq selected
                  (pimacs--select-chat
                   `(("first" . ,first) ("second" . ,second)
                     ("unnamed" . ,unnamed) ("unique" . ,unique))
                   "Session: ")))
          (should (equal labels '("33333333" "shared 11111111" "shared 22222222" "unique")))
          (should (eq (get-text-property 0 'face (nth 1 labels))
                      'pimacs-session-name-face))
          (should (eq (get-text-property 0 'face (car labels))
                      'pimacs-session-name-face))
          (should (eq (cdr selected) second)))
      (dolist (buffer (list first second unnamed unique))
        (kill-buffer buffer)))))

(ert-deftest pimacs--resume-session-candidates-format-session-records ()
  (let ((record (make-pimacs-session-record
                 :id "12345678-0000-0000-0000-000000000000"
                 :timestamp (encode-time 0 4 3 2 1 2026)
                 :parent-id "87654321-0000-0000-0000-000000000000"
                 :name "named"
                 :cwd "/tmp/project"
                 :preview "preview")))
    (cl-letf (((symbol-function 'pimacs-session-format-timestamp)
               (lambda (_timestamp) "date"))
              ((symbol-function 'pimacs-session-format-relative-time)
               (lambda (_timestamp) "relative")))
      (let* ((candidates (pimacs-session--resume-candidates (list record)))
             (candidate (car candidates))
             (text (car candidate))
             (annotation (funcall
                          (pimacs-session--resume-annotation-function candidates t)
                          text)))
        (should (eq (cdr candidate) record))
        (should (string-match-p "00000000  date  \\[named\\]  preview" text))
        (should-not (string-match-p "/tmp/project" text))
        (should (eq (get-text-property (string-match "00000000" text) 'face text)
                    'pimacs-session-name-face))
        (should (eq (get-text-property (string-match "date" text) 'face text)
                    'shadow))
        (should (eq (get-text-property (string-match "named" text) 'face text)
                    'pimacs-session-name-face))
        (should (equal (substring-no-properties annotation)
                       " /tmp/project  relative"))
        (should (equal (get-text-property 0 'display annotation)
                       `(space :align-to (- right ,(string-width (substring annotation 1))))))
        (should (eq (get-text-property (string-match "/tmp/project" annotation)
                                       'face annotation)
                    'pimacs-session-directory-face))
        (should (eq (get-text-property (string-match "relative" annotation)
                                       'face annotation)
                    'shadow))))))

(ert-deftest pimacs-resume-without-chat-resumes-selected-session ()
  (let* ((cwd (make-temp-file "pimacs-project-" t))
         (session-file (make-temp-file "pimacs-session-" nil ".jsonl"))
         (record (make-pimacs-session-record :path session-file :cwd cwd))
         recent-arguments resumed)
    (unwind-protect
        (cl-letf (((symbol-function 'pimacs--current-chat) (lambda () nil))
                  ((symbol-function 'pimacs--select-relevant-chat) (lambda () nil))
                  ((symbol-function 'pimacs-session-recent-records)
                   (lambda (&rest arguments)
                     (setq recent-arguments arguments)
                     (list record)))
                  ((symbol-function 'pimacs--completing-read)
                   (lambda (_prompt candidates &optional _annotation-function)
                     (caar candidates)))
                  ((symbol-function 'pimacs-resume-session-file)
                   (lambda (path root &optional _new-buffer)
                     (setq resumed (list path root))))
                  (pimacs-session-directory cwd))
          (pimacs-resume 'all)
          (should (equal recent-arguments
                         (list cwd t pimacs-resume-max-sessions)))
          (should (equal resumed (list session-file cwd))))
      (delete-file session-file)
      (delete-directory cwd))))


(ert-deftest pimacs-resume-with-prefix-bypasses-active-chat ()
  (let ((chat (get-buffer-create " *pimacs-test-chat*"))
        standalone-scope)
    (unwind-protect
        (cl-letf (((symbol-function 'pimacs--current-chat) (lambda () chat))
                  ((symbol-function 'pimacs--resume-standalone)
                   (lambda (scope) (setq standalone-scope scope)))
                  ((symbol-function 'pimacs--resume-chat)
                   (lambda () (ert-fail "Should not resume active chat"))))
          (pimacs-resume 'all)
          (should (eq standalone-scope 'all)))
      (kill-buffer chat))))

(ert-deftest pimacs-resume-new-buffer-uses-standalone-scope ()
  (let ((pimacs-resume-default-scope 'current-project)
        arguments)
    (cl-letf (((symbol-function 'pimacs--resume-standalone)
               (lambda (&rest args) (setq arguments args)))
              ((symbol-function 'pimacs--current-chat)
               (lambda () (ert-fail "Should not select an existing chat"))))
      (let ((current-prefix-arg nil))
        (call-interactively #'pimacs-resume-new-buffer))
      (should (equal arguments '(current-project t)))
      (let ((current-prefix-arg '(4)))
        (call-interactively #'pimacs-resume-new-buffer))
      (should (equal arguments '(all t)))
      (let ((pimacs-resume-default-scope 'all))
        (pimacs-resume-new-buffer))
      (should (equal arguments '(all t))))))

(ert-deftest pimacs-resume-new-buffer-resumes-selected-record ()
  (let* ((cwd (make-temp-file "pimacs-project-" t))
         (record (make-pimacs-session-record :path "/tmp/session.jsonl" :cwd cwd))
         (pimacs-session-directory cwd)
         arguments)
    (unwind-protect
        (cl-letf (((symbol-function 'pimacs-session-recent-records)
                   (lambda (&rest _) (list record)))
                  ((symbol-function 'pimacs-session-read-resume-record)
                   (lambda (&rest _) record))
                  ((symbol-function 'pimacs-resume-session-file)
                   (lambda (&rest args) (setq arguments args))))
          (pimacs-resume-new-buffer 'all)
          (should (equal arguments (list "/tmp/session.jsonl" cwd t))))
      (delete-directory cwd))))

(ert-deftest pimacs-resume-session-file-new-buffer-keeps-existing-chat ()
  (let ((pimacs--chats (make-hash-table :test 'equal))
        (pimacs--agents (make-hash-table :test 'equal))
        (cwd (make-temp-file "pimacs-project-" t))
        (session-file (make-temp-file "pimacs-session-" nil ".jsonl"))
        buffers switched)
    (unwind-protect
        (cl-letf (((symbol-function 'pimacs--start-agent)
                   (lambda (key) (puthash key (list key) pimacs--agents)))
                  ((symbol-function 'pimacs-chat-mode) (lambda () nil))
                  ((symbol-function 'pimacs--with-directory-local-variables)
                   (lambda (directory fn)
                     (with-temp-buffer
                       (let ((default-directory directory))
                         (funcall fn)))))
                  ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil))
                  ((symbol-function 'pimacs--switch-session)
                   (lambda (path _message &optional _cb)
                     (push (cons (current-buffer) path) switched))))
          (let* ((original (pimacs-chat--create nil cwd))
                 (original-key (buffer-local-value 'pimacs--project-key original))
                 (original-agent (gethash original-key pimacs--agents)))
            (push original buffers)
            (with-current-buffer original
              (insert "Existing chat")
              (dotimes (_ 2)
                (pimacs-resume-session-file session-file cwd t)
                (push (caar switched) buffers)))
            (should (= (length (delete-dups (copy-sequence buffers))) 3))
            (should (= (hash-table-count pimacs--chats) 3))
            (should (= (hash-table-count pimacs--agents) 3))
            (should (eq (gethash original-key pimacs--chats) original))
            (should (eq (gethash original-key pimacs--agents) original-agent))
            (should (equal (with-current-buffer original (buffer-string))
                           "Existing chat"))
            (dolist (buffer (butlast buffers))
              (with-current-buffer buffer
                (should (equal default-directory (file-name-as-directory cwd)))
                (should (eq (pimacs--current-chat) buffer))
                (should-not (equal pimacs--project-key original-key))
                (should-not (eq (pimacs--current-agent) original-agent))))
            (should (equal (mapcar #'cdr switched) (list session-file session-file)))
            (should (eq (pimacs-chat--create nil cwd) original))
            (pimacs-resume-session-file session-file cwd)
            (should (eq (caar switched) original))))
      (mapc #'kill-buffer buffers)
      (delete-file session-file)
      (delete-directory cwd))))

(ert-deftest pimacs--resume-standalone-directory-respects-scope ()
  (let ((pimacs-session-directory "/tmp/sessions/"))
    (cl-letf (((symbol-function 'pimacs--project-root)
               (lambda () "/tmp/project/")))
      (should (equal (pimacs--resume-standalone-directory 'current-project)
                     "/tmp/sessions/--tmp-project--"))
      (should (equal (pimacs--resume-standalone-directory 'all)
                     "/tmp/sessions/")))))

(ert-deftest pimacs--parse-slash-command ()
  (should (equal (pimacs--parse-slash-command "/model") '(pimacs-select-model . nil)))
  (should (equal (pimacs--parse-slash-command "/new") '(pimacs-new-session . nil)))
  (should (equal (pimacs--parse-slash-command "/resume") '(pimacs-resume . nil)))
  (should (equal (pimacs--parse-slash-command "/compact") '(pimacs-compact . nil)))
  (should (equal (pimacs--parse-slash-command "/clear-queue") '(pimacs-clear-queue . nil)))
  (should (equal (pimacs--parse-slash-command "/edit-queue") '(pimacs-edit-queue . nil)))
  (should (equal (pimacs--parse-slash-command "/set-auto-compaction") '(pimacs-set-auto-compaction . nil)))
  (should (equal (pimacs--parse-slash-command "/set-auto-retry") '(pimacs-set-auto-retry . nil)))
  (let ((err (should-error (pimacs--parse-slash-command "/set-auto-compaction true"))))
    (should (equal "Slash command \"/set-auto-compaction\" does not accept arguments" (error-message-string err))))
  (should (equal (pimacs--parse-slash-command "/compact custom instructions") '(pimacs-compact . "custom instructions")))
  (should (equal (pimacs--parse-slash-command "  /model") '(pimacs-select-model . nil)))
  (should (equal (pimacs--parse-slash-command "/model ") '(pimacs-select-model . nil)))
  (should (null (pimacs--parse-slash-command "/unknown")))
  (should (null (pimacs--parse-slash-command "/modelx")))
  (should (null (pimacs--parse-slash-command "/")))
  (should (null (pimacs--parse-slash-command "/123")))
  (should (null (pimacs--parse-slash-command "not-a-slash /model")))
  (should (null (pimacs--parse-slash-command "")))
  (should (null (pimacs--parse-slash-command "line1\n/model")))
  (should (null (pimacs--parse-slash-command "line1\n  /model")))
  (should (null (pimacs--parse-slash-command "line1\n/unknown")))
  (should (equal (pimacs--parse-slash-command "\n/model") '(pimacs-select-model . nil)))
  (should (equal (pimacs--parse-slash-command "\n\n/model") '(pimacs-select-model . nil)))
  (let ((err (should-error (pimacs--parse-slash-command "/model arg"))))
    (should (equal "Slash command \"/model\" does not accept arguments" (error-message-string err)))))

(ert-deftest pimacs--parse-bang-command ()
  (should (equal (pimacs--parse-bang-command "!ls") "ls"))
  (should (equal (pimacs--parse-bang-command "!ls -la") "ls -la"))
  (should (equal (pimacs--parse-bang-command "  !ls") "ls"))
  (should (equal (pimacs--parse-bang-command "! cat!") " cat!"))
  (should (null (pimacs--parse-bang-command "!!ls")))
  (should (null (pimacs--parse-bang-command "!!")))
  (should (null (pimacs--parse-bang-command "!")))
  (should (null (pimacs--parse-bang-command "! ")))
  (should (null (pimacs--parse-bang-command "!!  ")))
  (should (null (pimacs--parse-bang-command "not-a-bang !ls")))
  (should (null (pimacs--parse-bang-command "")))
  (should (null (pimacs--parse-bang-command "line1\n!ls")))
  (should (null (pimacs--parse-bang-command "line1\n  !ls")))
  (should (null (pimacs--parse-bang-command "line1\n!ls -la")))
  (should (equal (pimacs--parse-bang-command "\n!ls") "ls"))
  (should (equal (pimacs--parse-bang-command "\n\n!ls") "ls")))

(ert-deftest pimacs--parse-double-bang-command ()
  (should (equal (pimacs--parse-double-bang-command "!!ls") "ls"))
  (should (equal (pimacs--parse-double-bang-command "!!ls -la") "ls -la"))
  (should (equal (pimacs--parse-double-bang-command "  !!ls") "ls"))
  (should (null (pimacs--parse-double-bang-command "!!")))
  (should (null (pimacs--parse-double-bang-command "!")))
  (should (null (pimacs--parse-double-bang-command "  !!")))
  (should (null (pimacs--parse-double-bang-command "!! ")))
  (should (null (pimacs--parse-double-bang-command "! ")))
  (should (null (pimacs--parse-double-bang-command "!ls")))
  (should (null (pimacs--parse-double-bang-command "not-a-bang !!ls")))
  (should (null (pimacs--parse-double-bang-command "")))
  (should (null (pimacs--parse-double-bang-command "line1\n!!ls")))
  (should (null (pimacs--parse-double-bang-command "line1\n  !!ls")))
  (should (null (pimacs--parse-double-bang-command "line1\n!!ls -la")))
  (should (equal (pimacs--parse-double-bang-command "\n!!ls") "ls"))
  (should (equal (pimacs--parse-double-bang-command "\n\n!!ls") "ls")))

(ert-deftest pimacs--extract-truncation-notice-more-lines ()
  (should (equal (pimacs--extract-truncation-notice
                  "line1\nline2\n[40 more lines in file. Use offset=61 to continue.]")
                 '("line1\nline2" . "[40 more lines in file. Use offset=61 to continue.]"))))

(ert-deftest pimacs--extract-truncation-notice-showing-lines ()
  (should (equal (pimacs--extract-truncation-notice
                  "line1\nline2\n[Showing lines 1-1648 of 6218 (50.0KB limit). Use offset=1649 to continue.]")
                 '("line1\nline2" . "[Showing lines 1-1648 of 6218 (50.0KB limit). Use offset=1649 to continue.]"))))

(ert-deftest pimacs--extract-truncation-notice-no-notice ()
  (should (equal (pimacs--extract-truncation-notice "line1\nline2\nline3")
                 '("line1\nline2\nline3" . nil))))

(ert-deftest pimacs--extract-truncation-notice-empty ()
  (should (equal (pimacs--extract-truncation-notice "")
                 '("" . nil))))

(ert-deftest pimacs--extract-truncation-notice-showing-lines-no-size ()
  (should (equal (pimacs--extract-truncation-notice
                  "line1\nline2\n[Showing lines 1-1648 of 6218. Use offset=1649 to continue.]")
                 '("line1\nline2" . "[Showing lines 1-1648 of 6218. Use offset=1649 to continue.]"))))

(ert-deftest pimacs--extract-truncation-notice-bash-fallback ()
  (should (equal (pimacs--extract-truncation-notice
                  "line1\nline2\n[Line 1 is 100KB, exceeds 50.0KB limit. Use bash: sed -n '1p' main.go | head -c 51200]")
                 '("line1\nline2" . "[Line 1 is 100KB, exceeds 50.0KB limit. Use bash: sed -n '1p' main.go | head -c 51200]"))))

(ert-deftest pimacs--without-undo-before-preserves-prompt-undo ()
  (with-temp-buffer
    (buffer-enable-undo)
    (insert "transcript\n")
    (let ((input-start (point)))
      ;; The initial transcript is not user undo history.
      (setq buffer-undo-list nil)
      (insert "prompt")
      (put-text-property input-start (point) 'face 'bold)
      (undo-boundary)
      (pimacs--without-undo-before input-start
        (goto-char (point-min))
        (insert "generated\n"))
      (goto-char (point-max))
      (let ((inhibit-message t))
        (undo 1))
      (should (equal (buffer-string) "generated\ntranscript\n")))))

(ert-deftest pimacs--without-undo-before-does-not-capture-body-variables ()
  (with-temp-buffer
    (let ((undo-list :undo-list)
          (input-position :input-position)
          (anchor :anchor)
          (shift :shift))
      (pimacs--without-undo-before (point)
        (should (eq undo-list :undo-list))
        (should (eq input-position :input-position))
        (should (eq anchor :anchor))
        (should (eq shift :shift))))))

(ert-deftest pimacs--discard-and-shift-undo-list-warns-once ()
  (let ((pimacs--apply-undo-entry-warning-issued nil)
        (warnings 0)
        (undo-list '((apply 0 1 2 ignore)
                     (1 . 2)
                     (apply 0 1 2 ignore))))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest _)
                 (cl-incf warnings))))
      (setq undo-list (pimacs--discard-and-shift-undo-list undo-list 1 0))
      (should (equal undo-list '((1 . 2))))
      (should (= warnings 1))
      (pimacs--discard-and-shift-undo-list undo-list 1 0)
      (should (= warnings 1)))))
(ert-deftest pimacs--buffer-string-common-prefix-length ()
  (cl-labels ((common-prefix (buffer-text string)
                (with-temp-buffer
                  (insert buffer-text)
                  (pimacs--buffer-string-common-prefix-length
                   (current-buffer) (point-min) (point-max) string))))
    (should (= (common-prefix "" "") 0))
    (should (= (common-prefix "" "text") 0))
    (should (= (common-prefix "text" "") 0))
    (should (= (common-prefix "matching" "matching") 8))
    (with-temp-buffer
      (insert "ignoredmatching")
      (should (= (pimacs--buffer-string-common-prefix-length
                  (current-buffer) (+ (point-min) 7) (point-max) "matching")
                 8)))
    (should (= (common-prefix "shared" "sharing") 4))
    (should (= (common-prefix "prefix" "prefix-more") 6))
    (should (= (common-prefix "prefix-more" "prefix") 6))
    (let ((buffer-text (propertize "abcdef" 'face 'bold))
          (string (propertize "abcdef" 'face 'bold)))
      (should (= (common-prefix buffer-text string) 6)))
    (let ((buffer-text (copy-sequence "abcdef"))
          (string (copy-sequence "abcdef")))
      (put-text-property 2 6 'face 'bold buffer-text)
      (put-text-property 2 6 'face 'italic string)
      (should (= (common-prefix buffer-text string) 2)))
    (let ((buffer-text (copy-sequence "abcdef"))
          (string (copy-sequence "abcdef")))
      (put-text-property 1 5 'face 'bold buffer-text)
      (put-text-property 1 5 'face 'bold string)
      (put-text-property 3 6 'help-echo "Link" buffer-text)
      (put-text-property 3 6 'help-echo "Link" string)
      (should (= (common-prefix buffer-text string) 6)))
    (let ((buffer-text (copy-sequence "abcdef"))
          (string (copy-sequence "abcdef")))
      (put-text-property 1 5 'face 'bold buffer-text)
      (put-text-property 1 5 'face 'bold string)
      (put-text-property 3 6 'help-echo "First link" buffer-text)
      (put-text-property 3 6 'help-echo "Second link" string)
      (should (= (common-prefix buffer-text string) 3)))
    (let ((buffer-text (copy-sequence "abcdef"))
          (string (copy-sequence "abcdef")))
      (put-text-property 1 4 'face 'bold buffer-text)
      (put-text-property 1 5 'face 'bold string)
      (should (= (common-prefix buffer-text string) 4)))
    (let ((buffer-text (copy-sequence "abcdef"))
          (string (copy-sequence "abcdef")))
      (put-text-property 4 6 'pimacs-test-property 'one buffer-text)
      (put-text-property 4 6 'pimacs-test-property 'two string)
      (should (= (common-prefix buffer-text string) 4)))))

(ert-deftest pimacs--render-apply-operations-replaces-suffix ()
  (with-temp-buffer
    (let* ((context (pimacs--render-create-context))
           (initial (concat (propertize "prefix " 'face 'bold)
                            (propertize "old" 'face 'italic)))
           (replacement (concat (propertize "prefix " 'face 'bold)
                                (propertize "new" 'face 'italic)))
           changes)
      (pimacs--render-apply-operations context (list (list :append initial)))
      (add-hook 'before-change-functions
                (lambda (start end) (push (list start end) changes))
                nil t)
      (pimacs--render-apply-operations
       context (list (list :replace-suffix (length initial) replacement)))
      (should (equal (nreverse changes) '((8 11) (8 8))))
      (should (equal-including-properties
               (buffer-substring (pimacs-render-context-content-begin context)
                                 (pimacs-render-context-content-end context))
               replacement))
      (should (= (pimacs-render-context-rendered-length context)
                 (length replacement)))
      (setq changes nil)
      (pimacs--render-apply-operations
       context (list (list :replace-suffix (length replacement) replacement)))
      (should-not changes)
      (let ((shortened (propertize "prefix" 'face 'bold)))
        (pimacs--render-apply-operations
         context (list (list :replace-suffix (length replacement) shortened)))
        (should (equal (nreverse changes) '((7 11))))
        (should (equal-including-properties
                 (buffer-substring (pimacs-render-context-content-begin context)
                                   (pimacs-render-context-content-end context))
                 shortened))
        (should (= (pimacs-render-context-rendered-length context)
                   (length shortened)))))))

(ert-deftest pimacs--render-point-aligns-only-changed-tail ()
  (with-temp-buffer
    (let* ((context (pimacs--render-create-context))
           (prefix (make-string 12926 ?x))
           (initial (concat prefix "old tail"))
           (align-point (symbol-function 'pimacs--align-point))
           (calls 0))
      (pimacs--render-apply-operations context (list (list :append initial)))
      (goto-char (- (pimacs-render-context-content-end context) 4))
      (cl-letf (((symbol-function 'pimacs--align-point)
                 (lambda (old new offset)
                   (cl-incf calls)
                   (should (equal old "old tail"))
                   (should (equal new "new tail"))
                   (should (= offset 4))
                   (funcall align-point old new offset))))
        (pimacs-section--with-point-restoration
          (save-excursion
            (pimacs--render-apply-operations
             context (list (list :replace-suffix (length initial)
                                 (concat prefix "new tail")))))))
      (should (= calls 1)))))

(ert-deftest pimacs--render-point-unchanged-needs-no-snapshots ()
  (dolist (operations '(nil
                        ((:append "!"))
                        ((:replace-suffix 10 "prefix new"))
                        ((:replace-suffix 10 "prefix old"))))
    (with-temp-buffer
      (let ((context (pimacs--render-create-context)))
        (pimacs--render-apply-operations context '((:append "prefix old")))
        (goto-char (+ (point-min) 2))
        (cl-letf (((symbol-function 'pimacs--align-point)
                   (lambda (&rest _) (ert-fail "Unchanged point was aligned")))
                  ((symbol-function 'buffer-substring-no-properties)
                   (lambda (&rest _) (ert-fail "Unchanged content was copied"))))
          (pimacs-section--with-point-restoration
            (save-excursion
              (pimacs--render-apply-operations context operations))))))))

(defun pimacs-tests--point-fixture-actual (&optional end)
  (let* ((text (buffer-substring-no-properties (point-min) (or end (point-max))))
         (position (- (point) (point-min))))
    (concat (substring text 0 position) "█" (substring text position))))

(defun pimacs-tests--run-point-fixtures (kind runner)
  (let (failures (matched 0))
    (dolist (file (directory-files
                   (expand-file-name "pimacs-point-fixtures" pimacs-tests--directory)
                   t "\\.txt\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (pcase-let ((`(,old ,fixture-text ,expected)
                     (split-string (string-remove-suffix "\n" (buffer-string))
                                   "\n---------\n")))
          (let ((fixture (let ((read-eval nil))
                           (car (read-from-string fixture-text)))))
            (unless (memq (plist-get fixture :kind) '(operations section markdown))
              (ert-fail (list :fixture file :invalid-kind (plist-get fixture :kind))))
            (when (eq kind (plist-get fixture :kind))
              (cl-incf matched)
              (ert-info ((format "Point fixture: %s" file))
                (unless (and (= (cl-count ?█ old) 1)
                             (= (cl-count ?█ expected) 1))
                  (ert-fail (list :fixture file :invalid-cursor-markers t)))
                (let* ((result (funcall runner fixture old))
                       (initial (replace-regexp-in-string "█" "" old))
                       (failure (cond
                                 ((not (equal initial (plist-get result :initial)))
                                  (list :expected-initial initial
                                        :actual-initial (plist-get result :initial)))
                                 ((plist-get result :failure)
                                  (plist-get result :failure))
                                 ((not (equal expected (plist-get result :actual)))
                                  (list :expected expected
                                        :actual (plist-get result :actual))))))
                  (when failure
                    (push (append (list :fixture
                                        (file-relative-name file pimacs-tests--directory)
                                        :operations (plist-get result :operations))
                                  failure)
                          failures)))))))))
    (when (zerop matched)
      (ert-fail (list :kind kind :error :no-point-fixtures)))
    (when failures
      (ert-fail (list :failures (nreverse failures))))))

(defun pimacs-tests--run-operation-point-fixture (fixture old)
  (let ((initial (replace-regexp-in-string "█" "" old))
        (operations (plist-get fixture :operations)))
    (erase-buffer)
    (let ((context (pimacs--render-create-context)))
      (pimacs--render-apply-operations context (list (list :append initial)))
      (let ((actual-initial (buffer-substring-no-properties
                             (pimacs-render-context-content-begin context)
                             (pimacs-render-context-content-end context))))
        (if (not (equal actual-initial initial))
            (list :initial actual-initial :operations operations)
          (setq pimacs--prompt-widget
                (widget-create 'editable-field :format "%v" :value ""))
          (widget-setup)
          (goto-char (+ (point-min) (cl-position ?█ old)))
          (pimacs--widget-save-excursion-preserving-undo
            (pimacs--render-apply-operations context operations))
          (list :initial actual-initial :operations operations
                :actual (pimacs-tests--point-fixture-actual
                         (widget-get pimacs--prompt-widget :from))))))))

(ert-deftest pimacs--streaming-operation-fixtures ()
  (pimacs-tests--run-point-fixtures
   'operations #'pimacs-tests--run-operation-point-fixture))

(defun pimacs-tests--run-section-point-fixture (fixture old)
  (let ((operations (plist-get fixture :operations))
        (pimacs-section-padding "\n\n"))
    (erase-buffer)
    (pimacs-section--create-root-section)
    (let ((result (pimacs-section--new-section
                   'tool-result pimacs-section--root-section)))
      (pimacs-section--insert-section result
        (insert (plist-get fixture :result)))
      (when-let ((adjacent (plist-get fixture :adjacent)))
        (pimacs-section--create-section 'example pimacs-section--root-section
          (insert adjacent)))
      (when-let ((prompt (plist-get fixture :prompt)))
        (setq pimacs--prompt-widget
              (widget-create 'editable-field :format "%v" :value prompt))
        (widget-setup))
      (let ((actual-initial (buffer-substring-no-properties (point-min) (point-max))))
        (if (not (equal actual-initial (replace-regexp-in-string "█" "" old)))
            (list :initial actual-initial :operations operations)
          (goto-char (+ (point-min) (cl-position ?█ old)))
          (cl-labels ((apply-operations ()
                        (dolist (operation operations)
                          (pcase operation
                            (`(:append ,text)
                             (pimacs-section--append-section result
                               (insert text)))
                            (`(:replace ,text)
                             (pimacs-section--replace-section result
                               (insert text)))))))
            (if pimacs--prompt-widget
                (pimacs--widget-save-excursion-preserving-undo
                  (apply-operations))
              (pimacs-section--with-point-restoration
                (save-excursion (apply-operations)))))
          (list :initial actual-initial :operations operations
                :actual (pimacs-tests--point-fixture-actual)))))))

(ert-deftest pimacs--section-point-fixtures ()
  (pimacs-tests--run-point-fixtures
   'section #'pimacs-tests--run-section-point-fixture))

(defun pimacs-tests--run-markdown-point-fixture (fixture old)
  (let ((initial (replace-regexp-in-string "█" "" old))
        applied unexpected)
    (erase-buffer)
    (let ((context (pimacs--render-create-context))
          (state (pimacs--render-markdown :create)))
      (unwind-protect
          (progn
            (pimacs--render-apply-operations
             context (pimacs--render-markdown :stream state
                                              (plist-get fixture :source)))
            (let ((actual-initial
                   (buffer-substring-no-properties
                    (pimacs-render-context-content-begin context)
                    (pimacs-render-context-content-end context))))
              (if (not (equal actual-initial initial))
                  (list :initial actual-initial)
                (setq pimacs--prompt-widget
                      (widget-create 'editable-field :format "%v" :value ""))
                (widget-setup)
                (goto-char (+ (point-min) (cl-position ?█ old)))
                (pimacs--widget-save-excursion-preserving-undo
                  (dolist (delta (plist-get fixture :deltas))
                    (let ((operations (pimacs--render-markdown :stream state delta)))
                      (unless (cl-some (lambda (operation)
                                         (pcase operation
                                           (`(:replace-suffix ,count ,_)
                                            (> count 0))))
                                       operations)
                        (setq unexpected
                              (list :delta delta :actual-operations operations
                                    :expected-operation :replace-suffix)))
                      (push operations applied)
                      (pimacs--render-apply-operations context operations))))
                (list :initial actual-initial :operations (nreverse applied)
                      :failure unexpected
                      :actual (pimacs-tests--point-fixture-actual
                               (widget-get pimacs--prompt-widget :from))))))
        (pimacs--render-markdown :destroy state)))))

(ert-deftest pimacs--markdown-point-fixtures ()
  (require 'pimacs-markdown)
  (unless (pimacs--markdown-available-p)
    (ert-skip "Tree-sitter Markdown grammars are unavailable"))
  (pimacs-tests--run-point-fixtures
   'markdown #'pimacs-tests--run-markdown-point-fixture))

(ert-deftest pimacs--join-test ()
  (should (equal (pimacs--join nil) ""))
  (should (equal (pimacs--join '()) ""))
  (should (equal (pimacs--join "hello") "hello"))
  (should (equal (pimacs--join '("a" "b" "c")) "a\nb\nc"))
  (should (equal (pimacs--join '("a" "b" "c") ",") "a,b,c"))
  (should (equal (pimacs--join '("key" . "value")) "value"))
  (should (equal (pimacs--join '(("k1" . "v1") ("k2" . "v2"))) "v1\nv2"))
  (should (equal (pimacs--join '(("k1" . "v1") ("k2" . "v2")) ",") "v1,v2"))
  (should (equal (pimacs--join '(("k1" . "a\nb") ("k2" . "c"))) "a\nb\nc")))

(ert-deftest pimacs--update-status-widget-joins-statuses-with-space ()
  (with-temp-buffer
    (setq pimacs--status-widget
          (widget-create 'pimacs-item :face 'pimacs-status-face pimacs--empty-widget-text))
    (setq pimacs--status-texts (make-hash-table :test 'equal))

    (pimacs--handle-set-status '(:statusKey "status-b" :statusText "Status B"))
    (pimacs--handle-set-status '(:statusKey "status-a" :statusText "Status\nA"))

    (should (equal (widget-value pimacs--status-widget) "Status\nA Status B\n"))
    (should (equal (get-text-property 0 'help-echo (widget-value pimacs--status-widget))
                   "status-a"))

    (let ((pimacs-status-widget-hidden-keys '("status-a")))
      (pimacs--update-status-widget)
      (should (equal (widget-value pimacs--status-widget) "Status B\n"))
      (should (equal (gethash "status-a" pimacs--status-texts) "Status\nA")))))

(ert-deftest pimacs--handle-bash-execution-update-appends-deltas-by-request-id ()
  (with-temp-buffer
    (pimacs-section--create-root-section)
    (setq pimacs--prompt-widget
          (widget-create 'editable-field :format "%v" :value ""))
    (setq pimacs--bash-executions (make-hash-table :test 'equal))
    (widget-setup)
    (let ((first-call (pimacs-section--new-section 'tool-call pimacs-section--root-section))
          (second-call (pimacs-section--new-section 'tool-call pimacs-section--root-section)))
      (pimacs-section--insert-section first-call
        (insert "first"))
      (pimacs-section--insert-section second-call
        (insert "second"))
      (puthash "req-1" (make-pimacs-tool-call :call-section first-call)
               pimacs--bash-executions)
      (puthash "req-2" (make-pimacs-tool-call :call-section second-call)
               pimacs--bash-executions)
      (pimacs--handle-bash-execution-update '(:id "req-1" :delta "one\n"))
      (pimacs--handle-bash-execution-update '(:id "req-2" :delta "two\n"))
      (pimacs--handle-bash-execution-update '(:id "req-1" :delta "three\n"))
      (dolist (expected '(("req-1" . "one\nthree\n")
                          ("req-2" . "two\n")))
        (let* ((entry (gethash (car expected) pimacs--bash-executions))
               (section (pimacs-tool-call-result-section entry))
               (content (buffer-substring-no-properties
                         (pimacs-section-beginning section)
                         (pimacs-section-end section))))
          (should (equal content (cdr expected))))))))

(ert-deftest pimacs-bash-displays-direct-result-without-updates ()
  (with-temp-buffer
    (pimacs-section--create-root-section)
    (setq pimacs--prompt-widget
          (widget-create 'editable-field :format "%v" :value ""))
    (setq pimacs--spinner (spinner-create 'progress-bar))
    (setq pimacs--bash-executions (make-hash-table :test 'equal))
    (setq-local pimacs--project-key "test")
    (widget-setup)
    (let ((pimacs--chats (make-hash-table :test 'equal))
          callback)
      (puthash pimacs--project-key (current-buffer) pimacs--chats)
      (cl-letf (((symbol-function 'pimacs--send-command)
                 (lambda (_type _args fn)
                   (setq callback fn)
                   "req-1"))
                ((symbol-function 'pimacs--update-header-line)
                 (lambda () nil)))
        (pimacs-bash "printf result")
        (funcall callback '(:id "req-1" :success t :data (:output "result" :exitCode 0))))
      (should (string-match-p "result" (buffer-string)))
      (should (= (hash-table-count pimacs--bash-executions) 0)))))

(ert-deftest pimacs--insert-grep-result-preserves-backslashes-in-matches ()
  (let ((content
         (concat
          "autolink.in.markdown:7: http://one.example\\*literal\n"
          "document.in.markdown:116: [Reference-style link][ref-link]\n"
          "document.in.markdown:124: [ref-link]: https://reference-example.com \"Reference Link Title\"\n"
          "document.in.markdown:130: ![Reference-style link title tooltip\")\n"
          "document.in.markdown:132: ![Reference-style image][ref-image]\n"
          "document.in.markdown:134: [ref-image]: https://via.placeholder.com/200x100 \"Reference Image\"\n"
          "escapes.in.markdown:1: \\*literal\\* \\_literal\\_ \\`literal\\` \\[literal\\](url) \\\\ \\~literal\\~ \\a\n"
          "reference-link.out.txt:1: │ Full reference\n"
          "reference-link.in.markdown:1: [site]: https://example.com \"Pimacs website\"\n"
          "reference-link.in.markdown:3: [Full reference][site]\n"
          "reference-link.in.markdown:4: [site]\n"
          "document.out.txt:211: │ Reference-style link\n"
          "document.out.txt:230: │ ![Reference-style link title tooltip\")\n"
          "document.out.txt:232: │ Reference-style image")))
    (with-temp-buffer
      (pimacs--insert-grep-result
       (list (list :type "text" :text content))
       nil
       '(:pattern "reference|autolink|link title|\\\\\\*literal|site\\]"
                  :path "test/pimacs-markdown-tapes"
                  :glob "*"
                  :ignoreCase t
                  :limit 100))
      (should (equal (buffer-string) content))
      (goto-char (point-min))
      (search-forward "\\*literal")
      (should (eq (get-text-property (- (point) (length "\\*literal")) 'face)
                  'pimacs-grep-match-face)))))

(ert-deftest pimacs--insert-grep-result-does-not-fontify-adjacent-tool-call ()
  (with-temp-buffer
    (pimacs-section--create-root-section)
    (setq pimacs--prompt-widget
          (widget-create 'editable-field :format "%v" :value ""))
    (setq pimacs--tool-calls (make-hash-table :test 'equal))
    (widget-setup)
    (let ((pimacs-section-padding "\n"))
      (cl-letf (((symbol-function 'pimacs--project-root)
                 (lambda () default-directory)))
        (pimacs--insert-message
         '(:role "assistant"
                 :content ((:type "toolCall" :id "grep" :name "grep"
                                  :arguments (:pattern "foo" :path "src"))
                           (:type "toolCall" :id "read" :name "read"
                                  :arguments (:path "pimacs-agent.el" :offset 296 :limit 90)))))
        (pimacs--insert-message
         '(:role "toolResult" :toolCallId "grep" :toolName "grep"
                 :content ((:type "text" :text "reload.md-195- "))))
        (goto-char (point-min))
        (search-forward "read ")
        (should (equal (get-text-property (- (point) (length "read ")) 'face)
                       '(pimacs-tool-name-face pimacs-section-tool-call-face)))))))

(ert-deftest pimacs--insert-grep-args-treats-json-false-as-false ()
  (with-temp-buffer
    (pimacs--insert-grep-args
     '(:pattern "foo" :ignoreCase json-false :literal json-false))
    (should (equal (buffer-string) "/foo/"))))

(ert-deftest pimacs--tool-args-abbreviate-home-paths ()
  (let ((path (expand-file-name "pimacs-tool-path" "~")))
    (with-temp-buffer
      (pimacs--insert-grep-args (list :pattern "foo" :path path))
      (should (equal (buffer-string) "/foo/ in ~/pimacs-tool-path")))
    (with-temp-buffer
      (pimacs--insert-find-args (list :pattern "foo" :path path))
      (should (equal (buffer-string) "/foo/ in ~/pimacs-tool-path")))
    (with-temp-buffer
      (pimacs--insert-ls-args (list :path path))
      (should (equal (buffer-string) "~/pimacs-tool-path")))))

(ert-deftest pimacs--insert-grep-result-fontifies-primary-and-context-lines ()
  (let ((content "dir:name.el:12: foo BAR\ndir:name.el-13-foo BAR"))
    (with-temp-buffer
      (pimacs--insert-grep-result
       (list (list :type "text" :text content)) nil '(:pattern "foo"))
      (should (equal (buffer-string) content))
      (should (eq (get-text-property (point-min) 'face) 'compilation-info))
      (goto-char (point-min))
      (search-forward "12")
      (should (eq (get-text-property (1- (point)) 'face) 'compilation-line-number))
      (search-forward "foo")
      (should (eq (get-text-property (- (point) 3) 'face) 'pimacs-grep-match-face))
      (search-forward "BAR")
      (should-not (get-text-property (- (point) 3) 'face))
      (forward-line 1)
      (should (eq (get-text-property (point) 'face) 'compilation-info))
      (search-forward "13")
      (should (eq (get-text-property (1- (point)) 'face) 'compilation-line-number))
      (search-forward "foo")
      (should-not (get-text-property (- (point) 3) 'face)))))

(ert-deftest pimacs--insert-grep-result-honors-literal-and-ignore-case ()
  (with-temp-buffer
    (pimacs--insert-grep-result
     '((:type "text" :text "a:1: A.B axb"))
     nil '(:pattern "a.b" :literal t :ignoreCase t))
    (goto-char (point-min))
    (search-forward "A.B")
    (should (eq (get-text-property (- (point) 3) 'face) 'pimacs-grep-match-face))
    (search-forward "axb")
    (should-not (get-text-property (- (point) 3) 'face)))
  (with-temp-buffer
    (pimacs--insert-grep-result
     '((:type "text" :text "a:1: FOO foo")) nil '(:pattern "foo"))
    (goto-char (point-min))
    (search-forward "FOO")
    (should-not (get-text-property (- (point) 3) 'face))
    (search-forward "foo")
    (should (eq (get-text-property (- (point) 3) 'face) 'pimacs-grep-match-face))))

(ert-deftest pimacs--insert-grep-result-narrows-match-to-content ()
  (with-temp-buffer
    (pimacs--insert-grep-result
     '((:type "text" :text "a:1: foo bar")) nil '(:pattern "^foo bar$"))
    (goto-char (point-min))
    (search-forward "foo bar")
    (should (eq (get-text-property (- (point) 7) 'face) 'pimacs-grep-match-face))
    (should (eq (get-text-property (1- (point)) 'face) 'pimacs-grep-match-face))
    (should (eq (get-text-property (point-min) 'face) 'compilation-info))))

(ert-deftest pimacs--insert-grep-result-handles-invalid-and-zero-width-patterns ()
  (with-temp-buffer
    (pimacs--insert-grep-result
     '((:type "text" :text "a:1: foo")) nil '(:pattern "["))
    (should (equal (buffer-string) "a:1: foo"))
    (should (eq (get-text-property (point-min) 'face) 'compilation-info))
    (goto-char (point-min))
    (search-forward "foo")
    (should-not (get-text-property (- (point) 3) 'face)))
  (with-temp-buffer
    (insert "foo")
    (pimacs--fontify-grep-matches (point-min) (point-max) "\\_<" nil)
    (should-not (get-text-property (point-min) 'face))))

(ert-deftest pimacs--insert-grep-result-preserves-newlines-and-translates-once ()
  (dolist (content '("" "a:1: foo" "a:1: foo\n" "a:1: foo\n\n"))
    (with-temp-buffer
      (pimacs--insert-grep-result
       (list (list :type "text" :text content)) nil '(:pattern "foo"))
      (should (equal (buffer-string) content))))
  (let ((translations 0)
        (original (symbol-function 'rxt-pcre-to-elisp)))
    (cl-letf (((symbol-function 'rxt-pcre-to-elisp)
               (lambda (pattern)
                 (cl-incf translations)
                 (funcall original pattern))))
      (with-temp-buffer
        (pimacs--insert-grep-result
         '((:type "text" :text "a:1: foo\nb:2: foo")) nil '(:pattern "foo"))))
    (should (= translations 1))))

(ert-deftest pimacs--text-visitors-report-character-offsets-as-columns ()
  (let ((source-line "\tfoo")
        (expected-column (length "\tf")))
    (with-temp-buffer
      (insert source-line)
      (goto-char (point-min))
      (search-forward "f")
      (cl-letf (((symbol-function 'pimacs-section--section-line) (lambda () 1))
                ((symbol-function 'pimacs--project-root) (lambda () "/project/")))
        (let ((read-result (pimacs--visit-read-result nil '(:path "file")))
              (write-result (pimacs--visit-write-call '(:path "file"))))
          (should (= (plist-get read-result :column) expected-column))
          (should (= (plist-get write-result :column) expected-column)))))))

(ert-deftest pimacs--copy-write-call-copies-content ()
  (should (equal (pimacs--copy-write-call
                  '(:path "file.txt" :content "first line\nsecond line\n"))
                 "first line\nsecond line\n"))
  (should (equal (pimacs--copy-write-call '(:path "empty.txt" :content "")) ""))
  (should (eq (pimacs--alist-get-equal "write" pimacs-copy-tool-call-functions)
              'pimacs--copy-write-call)))

(ert-deftest pimacs--visit-grep-result-reports-character-offset-after-tab ()
  (with-temp-buffer
    (insert "example.el:7: \tfoo")
    (goto-char (point-min))
    (search-forward "f")
    (should (equal (pimacs--visit-grep-result nil nil)
                   '(:file "example.el" :line 7 :column 2)))))

(ert-deftest pimacs--visit-file-treats-column-as-character-offset ()
  (let ((source (current-buffer))
        (source-position (point))
        (target (generate-new-buffer " *pimacs-visit-target*"))
        (xref--history (cons nil nil)))
    (unwind-protect
        (progn
          (with-current-buffer target
            (insert "\tfoo"))
          (cl-letf (((symbol-function 'find-file)
                     (lambda (_file) (set-buffer target))))
            (pimacs--visit-file '(:file "unused" :line 1 :column 2))
            (should (eq (marker-buffer (caar xref--history)) source))
            (should (= (marker-position (caar xref--history)) source-position))
            (should (equal (buffer-substring (line-beginning-position) (point))
                           "\tf"))
            (should (looking-at "oo"))))
      (kill-buffer target))))

(ert-deftest pimacs--file-link-pushes-xref-marker ()
  (let ((file (make-temp-file "pimacs-file-link-"))
        (xref--history (cons nil nil)))
    (unwind-protect
        (with-temp-buffer
          (let ((source (current-buffer))
                (source-position nil)
                (widget (pimacs--insert-file-link file default-directory)))
            (setq source-position (point))
            (pimacs--file-link-action widget)
            (should (eq (marker-buffer (caar xref--history)) source))
            (should (= (marker-position (caar xref--history)) source-position))
            (kill-buffer (current-buffer))))
      (delete-file file))))

(ert-deftest pimacs--file-link-displays-project-relative-path ()
  (let* ((root (make-temp-file "pimacs-project-" t))
         (inside (expand-file-name "lib/file.el" root))
         (outside (make-temp-file "pimacs-file-link-"))
         (home-file (expand-file-name "pimacs-file-link-test" "~")))
    (unwind-protect
        (with-temp-buffer
          (let ((inside-widget (pimacs--insert-file-link "lib/file.el" root)))
            (insert " ")
            (let ((outside-widget (pimacs--insert-file-link outside root)))
              (insert " ")
              (let ((home-widget (pimacs--insert-file-link home-file root)))
                (should (equal (buffer-string)
                               (concat "lib/file.el " (abbreviate-file-name outside) " ~/pimacs-file-link-test")))
                (should (equal (widget-value inside-widget) inside))
                (should (equal (widget-value outside-widget) outside))
                (should (equal (widget-value home-widget) home-file))))))
      (delete-file outside)
      (delete-directory root t))))

(ert-deftest pimacs--render-diff-strips-file-headers ()
  (let* ((header "--- a/file.el\n+++ b/file.el\n")
         (body "@@ -1 +1 @@\n-old\n+new\n")
         (rendered (pimacs--render-diff (concat header body))))
    (should (equal (substring-no-properties rendered) body))))

(ert-deftest pimacs--diff-hunk-location-counts-lines ()
  (with-temp-buffer
    (insert "@@ -10,2 +20,3 @@\n context\n-old\n+new\n new-context\n")
    (goto-char (point-min))
    (search-forward "-old")
    (beginning-of-line)
    (should (equal (pimacs--diff-hunk-location) '(:line 21 :column 0)))
    (search-forward "+new")
    (beginning-of-line)
    (should (equal (pimacs--diff-hunk-location) '(:line 21 :column 0)))
    (forward-char 3)
    (should (equal (pimacs--diff-hunk-location) '(:line 21 :column 2)))))

(ert-deftest pimacs--diff-hunk-location-uses-character-columns ()
  (with-temp-buffer
    (insert "@@ -1 +1 @@\n+\tfoo\n")
    (goto-char (point-min))
    (search-forward "f")
    (should (equal (pimacs--diff-hunk-location) '(:line 1 :column 2)))))

(ert-deftest pimacs--handle-agent-state-formats-parallel-tools ()
  (with-temp-buffer
    (setq pimacs--spinner (spinner-create 'progress-bar))
    (pimacs-section--create-root-section)

    (pimacs--handle-agent-state
     '(:type "message_update"
             :assistantMessageEvent (:type "toolcall_start" :id "read-1" :toolName "read")))
    (should (equal (pimacs--format-state) "tool(read)"))
    (should (spinner--active-p pimacs--spinner))

    (pimacs--handle-agent-state
     '(:type "tool_execution_start" :toolCallId "read-1" :toolName "read"))
    (should (equal (pimacs--format-state) "tool(read)"))
    (should (spinner--active-p pimacs--spinner))

    (pimacs--handle-agent-state
     '(:type "message_update"
             :assistantMessageEvent (:type "toolcall_start" :id "grep-1" :toolName "grep")))
    (should (equal (pimacs--format-state) "tool(grep, read)"))
    (should (spinner--active-p pimacs--spinner))

    (pimacs--handle-agent-state
     '(:type "message_update"
             :assistantMessageEvent (:type "toolcall_start" :id "bash-1" :toolName "bash")))
    (should (equal (pimacs--format-state) "tool(bash, grep + 1 more)"))
    (should (spinner--active-p pimacs--spinner))

    (pimacs--handle-agent-state '(:type "tool_execution_end" :toolCallId "bash-1" :toolName "bash"))
    (should (equal (pimacs--format-state) "tool(grep, read)"))
    (should (spinner--active-p pimacs--spinner))

    (pimacs--handle-agent-state '(:type "tool_execution_end" :toolCallId "grep-1" :toolName "grep"))
    (should (equal (pimacs--format-state) "tool(read)"))
    (should (spinner--active-p pimacs--spinner))

    (pimacs--handle-agent-state '(:type "tool_execution_end" :toolCallId "read-1" :toolName "read"))
    (should (equal (pimacs--format-state) "thinking"))
    (should (spinner--active-p pimacs--spinner))

    (pimacs--handle-agent-state '(:type "agent_settled"))
    (should (equal (pimacs--format-state) "idle"))
    (should-not (spinner--active-p pimacs--spinner))))

(ert-deftest pimacs--handle-message-update-renders-toolcall-start-early ()
  (with-temp-buffer
    (pimacs-section--create-root-section)
    (setq pimacs--tool-calls (make-hash-table :test 'equal))
    (setq pimacs--prompt-widget
          (widget-create 'editable-field :format "%v" :value ""))
    (widget-setup)

    (pimacs--handle-message-update
     '(:assistantMessageEvent (:type "toolcall_start" :id "read-1" :toolName "read")))

    (let* ((entry (gethash "read-1" pimacs--tool-calls))
           (call-section (pimacs-tool-call-call-section entry)))
      (should entry)
      (should (equal (pimacs-tool-call-tool-name entry) "read"))
      (should-not (pimacs-tool-call-args entry))
      (should-not (pimacs-tool-call-result-section entry))
      (should (string-match-p "…"
                              (buffer-substring-no-properties
                               (pimacs-section-beginning call-section)
                               (pimacs-section-end call-section))))

      (pimacs--handle-message-update
       '(:assistantMessageEvent
         (:type "toolcall_end"
                :toolCall (:id "read-1" :name "read" :arguments (:path "file.el")))))

      (should (eq (pimacs-tool-call-call-section entry) call-section))
      (should (equal (pimacs-tool-call-args entry) '(:path "file.el")))
      (should-not (string-match-p "…"
                                  (buffer-substring-no-properties
                                   (pimacs-section-beginning call-section)
                                   (pimacs-section-end call-section))))
      (should (pimacs-tool-call-result-section entry))
      (should (= (length (pimacs-section-children pimacs-section--root-section)) 1))
      (should (= (length (pimacs-section-children call-section)) 1)))))

(ert-deftest pimacs--handle-message-end-creates-section-without-deltas ()
  (with-temp-buffer
    (pimacs-section--create-root-section)
    (setq pimacs--content-sections (make-hash-table :test 'eql))
    (setq pimacs--prompt-widget
          (widget-create 'editable-field :format "%v" :value ""))
    (widget-setup)

    (pimacs--handle-message-end
     '(:message (:role "assistant"
                       :content ((:type "text" :text "Hello")))))

    (let ((section (car (pimacs-section-children pimacs-section--root-section))))
      (should (eq (pimacs-section-type section) 'assistant))
      (should (equal (pimacs-section-assistant-info-content
                      (pimacs-section-info section))
                     '((:type "text" :text "Hello"))))
      (should (string-match-p "assistant> Hello"
                              (buffer-substring-no-properties
                               (pimacs-section-beginning section)
                               (pimacs-section-end section)))))
    (should (= (hash-table-count pimacs--content-sections) 0))))

(ert-deftest pimacs--handle-message-update-batch-merges-compatible-deltas ()
  (let* ((first '(:assistantMessageEvent (:type "text_delta" :delta "a" :contentIndex 0)))
         (events (list first
                       '(:assistantMessageEvent (:type "text_delta" :delta "b" :contentIndex 0))
                       '(:assistantMessageEvent (:type "thinking_delta" :delta "c" :contentIndex 0))
                       '(:assistantMessageEvent (:type "thinking_delta" :delta "d" :contentIndex 0))
                       '(:assistantMessageEvent (:type "text_delta" :delta "e" :contentIndex 1))
                       '(:assistantMessageEvent (:type "text_delta" :delta "f" :contentIndex 1))))
         handled)
    (cl-letf (((symbol-function 'pimacs--handle-message-update)
               (lambda (event) (push event handled))))
      (pimacs--handle-message-update-batch events))
    (should (equal (mapcar (lambda (event)
                             (let ((assistant-message-event
                                    (plist-get event :assistantMessageEvent)))
                               (list (plist-get assistant-message-event :type)
                                     (plist-get assistant-message-event :contentIndex)
                                     (plist-get assistant-message-event :delta)
                                     "assistant")))
                           (nreverse handled))
                   '(("text_delta" 0 "ab" "assistant")
                     ("thinking_delta" 0 "cd" "assistant")
                     ("text_delta" 1 "ef" "assistant"))))
    (should (equal (plist-get (plist-get first :assistantMessageEvent) :delta) "a"))))

(ert-deftest pimacs--markdown-renderer-lifecycle ()
  (with-temp-buffer
    (pimacs-section--create-root-section)
    (setq pimacs--content-sections (make-hash-table :test 'eql))
    (setq pimacs--prompt-widget
          (widget-create 'editable-field :format "%v" :value ""))
    (widget-setup)

    (let* ((operations nil)
           (pimacs-markdown-renderer
            (lambda (operation &optional _state text)
              (push operation operations)
              (pcase operation
                (:create (list :renderer-state))
                (:stream (list (list :append (concat "stream: " text))))
                (:final (list (list :append (concat "full: " text))))
                (:destroy nil)))))
      (pimacs--handle-message-update
       '(:assistantMessageEvent (:type "text_delta" :delta "Hello" :contentIndex 0)
                                :message (:role "assistant")))
      (should (string-match-p "assistant> stream: Hello" (buffer-string)))
      (pimacs--handle-message-update
       '(:assistantMessageEvent (:type "text_delta" :delta " world" :contentIndex 0)
                                :message (:role "assistant")))
      (let ((section (pimacs-content-section-section
                      (gethash 0 pimacs--content-sections))))
        (should (string-match-p
                 "stream: Hello.*stream:  world"
                 (buffer-substring-no-properties
                  (pimacs-section-beginning section)
                  (pimacs-section-end section)))))

      (pimacs--handle-message-end
       '(:message (:role "assistant"
                         :content ((:type "text" :text "Hello world")))))
      (should (string-match-p "assistant> full: Hello world" (buffer-string)))
      (should-not (string-match-p "stream: Hello" (buffer-string)))
      (should (equal (nreverse operations)
                     '(:create :stream :stream :final :destroy))))))

(ert-deftest pimacs--handle-message-end-preserves-reading-point ()
  (dolist (type '("text" "thinking"))
    (dolist (location '(inside prompt))
      (with-temp-buffer
        (pimacs-section--create-root-section)
        (setq pimacs--content-sections (make-hash-table :test 'eql))
        (setq pimacs--prompt-widget
              (widget-create 'editable-field :format "%v" :value ""))
        (widget-setup)
        (let* ((renderer (lambda (operation &optional _state text)
                           (pcase operation
                             (:create nil)
                             ((or :stream :final) (list (list :append text))))))
               (pimacs-markdown-renderer renderer)
               (pimacs-thinking-renderer renderer))
          (pimacs--handle-message-update
           `(:assistantMessageEvent
             (:type ,(if (equal type "text") "text_delta" "thinking_delta")
                    :delta "Hello world" :contentIndex 0)))
          (let* ((section (pimacs-content-section-section
                           (gethash 0 pimacs--content-sections)))
                 (position (if (eq location 'inside)
                               (save-excursion
                                 (goto-char (pimacs-section-beginning section))
                                 (search-forward "world")
                                 (- (point) 3))
                             (widget-field-start pimacs--prompt-widget)))
                 (offset (- position (pimacs-section-beginning section))))
            (goto-char position)
            (pimacs--handle-message-end
             `(:message (:role "assistant"
                               :content ((:type ,type
                                                ,(if (equal type "text") :text :thinking)
                                                "Hello world")))))
            (if (eq location 'inside)
                (should (= (- (point) (pimacs-section-beginning section)) offset))
              (should (= (point) (widget-field-start pimacs--prompt-widget))))))))))

(ert-deftest pimacs-section-applies-configured-face ()
  (with-temp-buffer
    (let ((pimacs-section-type-faces '((info . bold))))
      (pimacs-section--create-root-section)
      (pimacs-section--create-section 'info pimacs-section--root-section
        (pimacs-section--insert-chrome "chrome " 'italic)
        (insert (propertize "content" 'face 'underline)))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     (concat "chrome content" pimacs-section-padding)))
      (should (equal (get-text-property (point-min) 'face)
                     '(italic bold)))
      (should (equal (get-text-property (+ (point-min) (length "chrome ")) 'face)
                     '(bold underline)))
      (let ((face (get-text-property (1- (point-max)) 'face)))
        (should (eq (if (listp face) (car face) face) 'bold))))))

(ert-deftest pimacs-section-styles-appended-and-replaced-content ()
  (with-temp-buffer
    (let ((pimacs-section-type-faces '((info . bold))))
      (pimacs-section--create-root-section)
      (let ((section (pimacs-section--create-section 'info pimacs-section--root-section
                       (insert "one"))))
        (pimacs-section--append-section section
          (insert (propertize " two" 'face 'italic)))
        (save-excursion
          (goto-char (point-min))
          (search-forward "two")
          (should (equal (get-text-property (- (point) 3) 'face)
                         '(bold italic))))
        (pimacs-section--append-section section
          (delete-region (- (point) 2) (point))
          (insert (propertize "wo" 'face 'underline)))
        (save-excursion
          (goto-char (point-min))
          (search-forward "wo")
          (should (equal (get-text-property (- (point) 2) 'face)
                         '(bold underline))))
        (pimacs-section--replace-section section
          (insert (propertize "three" 'face 'underline)))
        (should (equal (get-text-property (point-min) 'face)
                       '(bold underline)))))))

(ert-deftest pimacs-section-child-face-does-not-inherit-parent-face ()
  (with-temp-buffer
    (let ((pimacs-section-type-faces
           '((assistant . bold) (tool-result . italic))))
      (pimacs-section--create-root-section)
      (let ((parent (pimacs-section--create-section 'assistant pimacs-section--root-section
                      (insert "parent"))))
        (pimacs-section--create-section 'tool-result parent
          (insert (propertize "child" 'face 'underline)))
        (save-excursion
          (goto-char (point-min))
          (search-forward "child")
          (should (equal (get-text-property (- (point) 5) 'face)
                         '(italic underline))))))))

(ert-deftest pimacs--thinking-renderer-applies-section-face ()
  (with-temp-buffer
    (pimacs-section--create-root-section)
    (pimacs-section--create-section 'thinking pimacs-section--root-section
      (pimacs--thinking-insert
       "# Heading\n**bold** and *italic* ~~strike~~ `code` [link](https://example.com)" nil))
    (cl-labels ((property-at (text property)
                  (save-excursion
                    (goto-char (point-min))
                    (search-forward text)
                    (get-text-property (- (point) (length text)) property))))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     (concat "Heading\nbold and italic strike code link"
                             pimacs-section-padding)))
      (should (equal (property-at "Heading" 'face)
                     '(pimacs-section-thinking-face pimacs-markdown-heading-face)))
      (should (equal (property-at "bold" 'face)
                     '(pimacs-section-thinking-face pimacs-markdown-bold-face)))
      (should (eq (property-at "and" 'face) 'pimacs-section-thinking-face))
      (should (equal (property-at "italic" 'face)
                     '(pimacs-section-thinking-face pimacs-markdown-italic-face)))
      (should (equal (property-at "strike" 'face)
                     '(pimacs-section-thinking-face pimacs-markdown-strike-through-face)))
      (should (equal (property-at "code" 'face)
                     '(pimacs-section-thinking-face pimacs-markdown-inline-code-face)))
      (should (equal (property-at "link" 'face)
                     '(pimacs-section-thinking-face pimacs-markdown-link-face)))
      (should (equal (property-at "link" 'help-echo) "https://example.com")))))

(ert-deftest pimacs--insert-content-renders-image ()
  (unless (image-type-available-p 'png)
    (message "Skipping image rendering test: PNG support is unavailable")
    (ert-skip "PNG support is unavailable"))
  (let ((data (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert-file-contents-literally
                 (expand-file-name "../integration/project/green-triangle.png"
                                   pimacs-tests--directory))
                (buffer-string))))
    (with-temp-buffer
      (cl-letf (((symbol-function 'display-images-p)
                 (lambda () t)))
        (pimacs--insert-content
         (list (list :type "image"
                     :mimeType "image/png"
                     :data (base64-encode-string data t)))))
      (should (equal (buffer-string) "\n \n"))
      (let ((image (get-text-property (1+ (point-min)) 'display)))
        (should (eq (image-property image :type) 'png))
        (should (equal (image-property image :data) data))))))
(ert-deftest pimacs-clear-ui-keeps-sections-before-prompt-widgets ()
  (with-temp-buffer
    (pimacs-section--create-root-section)
    (setq pimacs--tool-calls (make-hash-table :test 'equal))
    (setq pimacs--bash-executions (make-hash-table :test 'equal))
    (setq pimacs--content-sections (make-hash-table :test 'eql))
    (setq pimacs--prompt-before-widget
          (widget-create 'pimacs-item :face 'pimacs-widget-face pimacs--empty-widget-text))
    (setq pimacs--prompt-widget
          (widget-create 'editable-field :format "%[user>%] %v" :value ""))
    (setq pimacs--prompt-after-widget
          (widget-create 'pimacs-item :face 'pimacs-widget-face pimacs--empty-widget-text))
    (setq pimacs--prompt-widget-lines (make-hash-table :test 'equal))
    (setq pimacs--status-widget
          (widget-create 'pimacs-item :face 'pimacs-status-face pimacs--empty-widget-text))
    (setq pimacs--status-texts (make-hash-table :test 'equal))
    (widget-setup)

    (cl-labels ((insert-section ()
                  (let (section)
                    (pimacs--widget-save-excursion
                      (setq section
                            (pimacs-section--create-section 'info pimacs-section--root-section
                              (insert "sections"))))
                    section))
                (set-widgets ()
                  (pimacs--handle-set-widget '(:widgetKey "before"
                                                          :widgetLines ("before-widget")
                                                          :widgetPlacement "aboveEditor"))
                  (pimacs--handle-set-widget '(:widgetKey "after"
                                                          :widgetLines ("after-widget")
                                                          :widgetPlacement "belowEditor"))))
      (set-widgets)
      (let ((section (insert-section)))
        (should (< (marker-position (pimacs-section-beginning section))
                   (marker-position (widget-get pimacs--prompt-before-widget :from))
                   (marker-position (widget-get pimacs--prompt-widget :from))
                   (marker-position (widget-get pimacs--prompt-after-widget :from)))))

      (pimacs--widget-save-excursion
        (pimacs--clear-sections)
        (pimacs--clear-session-widgets))

      (set-widgets)
      (let ((section (insert-section)))
        (should (< (marker-position (pimacs-section-beginning section))
                   (marker-position (widget-get pimacs--prompt-before-widget :from))
                   (marker-position (widget-get pimacs--prompt-widget :from))
                   (marker-position (widget-get pimacs--prompt-after-widget :from))))))))

(defun pimacs-tests--setup-chat-widgets ()
  (setq pimacs--prompt-widget
        (widget-create 'editable-field :format "%[user>%] %v" :value ""))
  (setq pimacs--prompt-after-widget
        (widget-create 'pimacs-item :face 'pimacs-widget-face pimacs--empty-widget-text))
  (setq pimacs--status-widget
        (widget-create 'pimacs-item :face 'pimacs-status-face pimacs--empty-widget-text))
  (setq pimacs--prompt-widget-lines (make-hash-table :test 'equal))
  (setq pimacs--status-texts (make-hash-table :test 'equal))
  (widget-setup))

(ert-deftest pimacs--widget-save-excursion-checks-tail-once-for-nested-inserts ()
  (with-temp-buffer
    (pimacs-section--create-root-section)
    (pimacs-tests--setup-chat-widgets)
    (pimacs-focus-prompt)
    (setq pimacs--prompt-detached t)
    (let ((tail-checks 0)
          (recenters 0)
          (window 'pimacs-test-window))
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) window))
                ((symbol-function 'window-point) (lambda (&rest _) (point)))
                ((symbol-function 'pimacs--window-at-tail-p)
                 (lambda (&optional _)
                   (cl-incf tail-checks)
                   (should (= tail-checks 1))
                   t))
                ((symbol-function 'pimacs--recenter-chat)
                 (lambda ()
                   (cl-incf recenters)
                   (should (string-match-p "first" (buffer-string)))
                   (should (string-match-p "second" (buffer-string))))))
        (pimacs--widget-save-excursion
          (pimacs--widget-save-excursion
            (pimacs-section--create-section 'info pimacs-section--root-section
              (insert "first\n")))
          (should (= recenters 0))
          (pimacs--widget-save-excursion
            (pimacs-section--create-section 'info pimacs-section--root-section
              (insert "second\n")))
          (should (= recenters 0)))
        (should (= tail-checks 1))
        (should (= recenters 1))
        (should-not pimacs--prompt-detached)
        (should-not pimacs--view-update-buffer)
        (should (= (point) (widget-field-text-end pimacs--prompt-widget)))))))

(ert-deftest pimacs--widget-save-excursion-anchors-view-when-scrolled-away ()
  "Inserting output while scrolled away must not move the view when point stays in the prompt."
  (with-temp-buffer
    (pimacs-section--create-root-section)
    (pimacs-tests--setup-chat-widgets)
    (pimacs--widget-save-excursion
      (pimacs-section--create-section 'info pimacs-section--root-section
        (insert "first\nsecond\nthird\nfourth\nfifth\n")))
    (let ((window 'pimacs-test-window)
          (visible-end (save-excursion
                         (goto-char (point-min))
                         (forward-line 2)
                         (point))))
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) window))
                ((symbol-function 'selected-window) (lambda () window))
                ((symbol-function 'window-point) (lambda (&rest _) (point)))
                ((symbol-function 'pimacs--window-at-tail-p) (lambda (&optional _) nil))
                ((symbol-function 'window-end) (lambda (&rest _) visible-end))
                ((symbol-function 'window-start) (lambda (&rest _) (point-min)))
                ((symbol-function 'pimacs--recenter-chat)
                 (lambda () (ert-fail "recentered while scrolled away"))))
        (should-not pimacs--prompt-detached)
        (pimacs--widget-save-excursion
          (pimacs--widget-save-excursion
            (pimacs-section--create-section 'info pimacs-section--root-section
              (insert "new streaming output\n")))
          (should-not pimacs--prompt-detached))
        (should pimacs--prompt-detached)
        (should (= (point) (1- visible-end)))))))

(ert-deftest pimacs--widget-save-excursion-returns-to-prompt-at-tail ()
  "Following resumes once the tail is visible again."
  (with-temp-buffer
    (pimacs-section--create-root-section)
    (pimacs-tests--setup-chat-widgets)
    (pimacs--widget-save-excursion
      (pimacs-section--create-section 'info pimacs-section--root-section
        (insert "content\n")))
    (setq pimacs--prompt-detached t)
    (let (recentered)
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'pimacs-test-window))
                ((symbol-function 'window-point) (lambda (&rest _) (point)))
                ((symbol-function 'pimacs--point-in-prompt-p) (lambda () t))
                ((symbol-function 'pimacs--window-at-tail-p) (lambda (&optional _) t))
                ((symbol-function 'pimacs--recenter-chat) (lambda () (setq recentered t))))
        (pimacs--widget-save-excursion
          (pimacs-section--create-section 'info pimacs-section--root-section
            (insert "more output\n")))
        (should-not pimacs--prompt-detached)
        (should recentered)
        (should (= (point) (widget-field-text-end pimacs--prompt-widget)))))))

(ert-deftest pimacs--widget-save-excursion-preserves-restored-reading-point ()
  "Re-rendering the section under point must not pin to the old window end."
  (with-temp-buffer
    (pimacs-section--create-root-section)
    (pimacs-tests--setup-chat-widgets)
    (let (section)
      (pimacs--widget-save-excursion
        (setq section
              (pimacs-section--create-section 'info pimacs-section--root-section
                (insert "aaa\nbbb\nccc\n"))))
      (goto-char (pimacs-section-beginning section))
      (search-forward "ccc")
      (backward-char)
      (let ((window 'pimacs-test-window)
            (visible-end (save-excursion
                           (goto-char (pimacs-section-beginning section))
                           (forward-line 1)
                           (point)))
            (reading-offset (- (point) (pimacs-section-beginning section))))
        (should (> (point) visible-end))
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) window))
                  ((symbol-function 'selected-window) (lambda () window))
                  ((symbol-function 'window-point) (lambda (&rest _) (point)))
                  ((symbol-function 'pimacs--window-at-tail-p)
                   (lambda (&optional _) (ert-fail "checked tail while reading")))
                  ((symbol-function 'window-end) (lambda (&rest _) visible-end))
                  ((symbol-function 'window-start) (lambda (&rest _) (point-min)))
                  ((symbol-function 'pimacs--recenter-chat)
                   (lambda () (ert-fail "recentered while reading"))))
          (pimacs--widget-save-excursion
            (pimacs--widget-save-excursion
              (pimacs-section--replace-section section
                (insert "prefix\naaa\nbbb\nccc\n"))))
          (should-not pimacs--prompt-detached)
          (should (< (point) (widget-get pimacs--prompt-widget :from)))
          (should-not (= (point) (1- visible-end)))
          (should (= (- (point) (pimacs-section-beginning section))
                     (+ (length "prefix\n") reading-offset))))))))

(ert-deftest pimacs--history-split-entries-keeps-tool-call-with-result ()
  (let* ((prefix (cl-loop for index below 9
                          collect (list :type "session_info"
                                        :name (format "name-%d" index))))
         (call '(:type "message"
                       :message (:role "assistant"
                                       :content ((:type "toolCall" :id "call-1"
                                                        :name "read" :arguments (:path "README.md"))))))
         (result '(:type "message"
                         :message (:role "toolResult" :toolCallId "call-1"
                                         :toolName "read"
                                         :content ((:type "text" :text "result")))))
         (last '(:type "message" :message (:role "user" :content "last")))
         (chunks (pimacs--history-split-entries
                  (append prefix (list call result last)))))
    (should (equal (mapcar #'length chunks) '(5 6 1)))
    (should (eq (nth 4 (nth 1 chunks)) call))
    (should (eq (nth 5 (nth 1 chunks)) result))))

(ert-deftest pimacs--history-split-entries-closes-orphaned-tool-call-at-next-message ()
  (let* ((prefix (cl-loop for index below 9
                          collect (list :type "session_info"
                                        :name (format "name-%d" index))))
         (orphan '(:type "message"
                         :message (:role "assistant"
                                         :content ((:type "toolCall" :id "call-1"
                                                          :name "read" :arguments (:path "README.md"))))))
         (next-turn '(:type "message"
                            :message (:role "assistant"
                                            :content ((:type "text" :text "continuing")))))
         (suffix (cl-loop for index below 9
                          collect (list :type "session_info"
                                        :name (format "later-%d" index))))
         (chunks (pimacs--history-split-entries
                  (append prefix (list orphan next-turn) suffix))))
    ;; The missing result cannot occur after NEXT-TURN, so it must not keep
    ;; the remaining history in one unsplittable chunk.
    (should (equal (mapcar #'length chunks) '(5 6 5 4)))
    (should (eq (nth 4 (nth 1 chunks)) orphan))
    (should (eq (nth 5 (nth 1 chunks)) next-turn))))

(ert-deftest pimacs--render-session-history-progressively-inserts-history ()
  (with-temp-buffer
    (pimacs-section--create-root-section)
    (setq pimacs--tool-calls (make-hash-table :test 'equal)
          pimacs--bash-executions (make-hash-table :test 'equal)
          pimacs--content-sections (make-hash-table :test 'eql)
          pimacs--history-render-generation 0)
    (setq pimacs--prompt-widget
          (widget-create 'editable-field :format "%v" :value ""))
    (widget-setup)
    (let ((entries (cl-loop for index below 12
                            collect (list :type "message"
                                          :message (list :role "user"
                                                         :content (format "message-%d" index)))))
          (pimacs-section-padding "|")
          (pimacs-section-autohide-count 5)
          scheduled)
      (cl-letf (((symbol-function 'pimacs--history-schedule-idle-render)
                 (lambda (buffer generation)
                   (setq scheduled (list buffer generation)))))
        (pimacs--widget-save-excursion
          (pimacs--render-session-history entries))
        (should (= (length (pimacs-section-children pimacs-section--root-section)) 3))
        (should (= (pimacs--history-pending-entry-count) 10))
        (should (string-match-p "Loading history: 10 entries pending"
                                (buffer-string)))
        (pimacs--history-render-idle (current-buffer) pimacs--history-render-generation)
        (should (= (length (pimacs-section-children pimacs-section--root-section)) 8))
        (should (= (pimacs--history-pending-entry-count) 5))
        (pimacs--history-render-idle (current-buffer) pimacs--history-render-generation))
      (let ((roots (pimacs-section-children pimacs-section--root-section)))
        (should (= (length roots) 12))
        (should-not pimacs--history-loading-section)
        (should-not pimacs--history-render-pending)
        (should (cl-every #'pimacs-section--hidden-p (seq-take roots 7)))
        (should (cl-every #'pimacs-section--visible-p (seq-drop roots 7)))
        (should (equal (buffer-string)
                       (concat "user> message-0|user> message-1|user> message-2|"
                               "user> message-3|user> message-4|user> message-5|"
                               "user> message-6|user> message-7|user> message-8|"
                               "user> message-9|user> message-10|user> message-11|\n")))
        (should (equal scheduled
                       (list (current-buffer) pimacs--history-render-generation)))))))

(ert-deftest pimacs--render-session-history-disables-lazy-autohide-with-nil-count ()
  (with-temp-buffer
    (pimacs-section--create-root-section)
    (setq pimacs--tool-calls (make-hash-table :test 'equal)
          pimacs--bash-executions (make-hash-table :test 'equal)
          pimacs--content-sections (make-hash-table :test 'eql)
          pimacs--history-render-generation 0)
    (setq pimacs--prompt-widget
          (widget-create 'editable-field :format "%v" :value ""))
    (widget-setup)
    (let ((entries (cl-loop for index below 12
                            collect (list :type "message"
                                          :message (list :role "user"
                                                         :content (format "message-%d" index)))))
          (pimacs-section-autohide-count nil))
      (cl-letf (((symbol-function 'pimacs--history-schedule-idle-render)
                 (lambda (_buffer _generation))))
        (pimacs--widget-save-excursion
          (pimacs--render-session-history entries))
        (pimacs--history-render-idle (current-buffer) pimacs--history-render-generation)
        (pimacs--history-render-idle (current-buffer) pimacs--history-render-generation))
      (let ((roots (pimacs-section-children pimacs-section--root-section)))
        (should (= (length roots) 12))
        (should (cl-every #'pimacs-section--visible-p roots))))))


;;; pimacs-tests.el ends here

