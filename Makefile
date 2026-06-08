export EMACS ?= $(shell command -v emacs 2>/dev/null)
CASK_DIR := $(shell cask package-directory)

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: compile
compile: cask
	cask emacs -batch -L . -L test \
	  -f batch-byte-compile $$(cask files); \
	  (ret=$$? ; cask clean-elc && exit $$ret)

.PHONY: test
test: compile
	cask emacs --batch -L . -L test -l pi-tests.el -l pi-section-tests.el -f ert-run-tests-batch-and-exit

.PHONY: integration
integration: compile
	cask emacs --batch -L . -L test -l integration/pi-integration-tests.el -f ert-run-tests-batch-and-exit

.PHONY: format
format:
	cask emacs --batch -L . -l pi.el -l pi-section.el -l pi-edit.el -l pi-tests.el -l pi-section-tests.el \
	  --eval " \
	  (progn \
            (setq-default indent-tabs-mode nil) \
	    (dolist (f command-line-args-left) \
	      (with-current-buffer (find-file-noselect f) \
                (message \"formatting %s\" f) \
	        (indent-region (point-min) (point-max)) \
	        (save-buffer))))" \
          pi.el pi-section.el pi-edit.el pi-tests.el pi-section-tests.el


.PHONY: sandbox
sandbox:
	rm -rf sandbox
	mkdir sandbox
	emacs -Q --init-directory=./sandbox --debug \
	        --eval '(setq user-emacs-directory (file-truename "sandbox"))' \
	        -l package \
	        --eval "(add-to-list 'package-archives '(\"gnu\" . \"http://elpa.gnu.org/packages/\") t)" \
	        --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
	        --eval "(package-refresh-contents)" \
	        --eval "(package-initialize)" \
	        --eval "(use-package pi :ensure t :vc (:url \"git@github.com:ananthakumaran/pi.el.git\" :rev :newest) :commands (pi-chat))" \
                --eval "(when (eq system-type 'darwin) (setq mac-option-key-is-meta nil mac-command-key-is-meta t mac-command-modifier 'meta mac-option-modifier 'none))"


define ESCRIPT
(with-temp-buffer
  (require 'pp)
  (require 'subr-x)
  (insert-file-contents "pi.el")
  (while
      (ignore-errors
        (let ((sexp (read (current-buffer))))
          (when sexp
            (when (eq (car sexp) 'defcustom)
              (unless (cadr (cddr sexp))
                (princ (format "Documentation missing for defcustom %S\n" (cadr sexp)))
                (kill-emacs 1))
              (let* ((name (cadr sexp))
                     (default-raw (pp-to-string (eval (car (cddr sexp)) t)))
                     (default-str (string-trim default-raw))
                     (doc (replace-regexp-in-string "`\\([^ ]*\\)'" "`\\1`" (cadr (cddr sexp)))))
                (if (string-match-p "\n" default-str)
                    (princ (format "#### %s\n\n<details><summary>Default Value</summary>\n\n```elisp\n%s\n```\n\n</details>\n\n%s\n\n" name default-str doc))
                  (princ (format "#### %s `%s`\n\n%s\n\n" name default-str doc)))))
            t)))))
endef
export ESCRIPT


readme:
	ruby -e 'puts IO.read("README.md").split("## Custom Variables")[0] + "## Custom Variables\n\n" + `emacs --batch --eval "$$ESCRIPT"`' | sponge README.md
