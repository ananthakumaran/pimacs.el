export EMACS ?= $(shell command -v emacs 2>/dev/null)
CASK_DIR := $(shell cask package-directory)

MATCH ?=

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: setup
setup: cask
	npm install -g --ignore-scripts @earendil-works/pi-coding-agent
	cd integration/fixture && npm install

.PHONY: compile
compile: cask
	@cask emacs -batch -L . -L test \
	  -f batch-byte-compile pi-utils.el pi-core.el pi-section.el pi-edit.el pi-agent.el pi.el; \
	  (ret=$$? ; cask clean-elc && exit $$ret)

.PHONY: package-lint
package-lint: cask
	@cask emacs -Q --batch \
	  --eval "(setq package-lint-main-file \"pi.el\")" \
	  -f package-lint-batch-and-exit \
	  pi-utils.el pi-core.el pi-section.el pi-edit.el pi-agent.el pi.el

.PHONY: test
test: compile
	@cask emacs --batch -L . -L test -l pi-tests.el -l pi-section-tests.el --eval '(let ((ert-quiet (equal (getenv "PI_CODING_AGENT") "true"))) (ert-run-tests-batch-and-exit "$(MATCH)"))'

.PHONY: integration
integration: compile
	@cask emacs --batch -L . -L test -l integration/pi-integration-tests.el --eval '(let ((ert-quiet (equal (getenv "PI_CODING_AGENT") "true"))) (ert-run-tests-batch-and-exit "$(MATCH)"))'

.PHONY: format
format:
	@cask emacs --batch -L . -l pi-utils.el -l pi-core.el -l pi.el -l pi-section.el -l pi-edit.el -l pi-agent.el -l pi-tests.el -l pi-section-tests.el -l integration/pi-integration-tests.el \
	  --eval " \
	  (let ((inhibit-message t) \
                (message-log-max nil)) \
            (setq-default indent-tabs-mode nil) \
	    (dolist (f command-line-args-left) \
	      (with-current-buffer (find-file-noselect f) \
	        (indent-region (point-min) (point-max)) \
	        (save-buffer))))" \
          pi-utils.el pi-core.el pi-section.el pi-edit.el pi-agent.el pi.el pi-tests.el pi-section-tests.el integration/pi-integration-tests.el


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
  (require 'subr-x)
  (insert-file-contents "pi-section.el")
  (insert-file-contents "pi-utils.el")
  (insert-file-contents "pi-core.el")
  (insert-file-contents "pi-agent.el")
  (insert-file-contents "pi.el")
  (while
      (ignore-errors
        (let ((form-start (point))
              (sexp (read (current-buffer))))
          (when sexp
            (when (eq (car sexp) 'defcustom)
              (unless (cadr (cddr sexp))
                (princ (format "Documentation missing for defcustom %S\n" (cadr sexp)))
                (kill-emacs 1))
              (let* ((name (cadr sexp))
                     (default-raw
                      (save-excursion
                        (goto-char form-start)
                        (forward-comment (point-max))
                        (forward-char 1)
                        (forward-sexp 1)
                        (forward-comment (point-max))
                        (forward-sexp 1)
                        (forward-comment (point-max))
                        (let ((default-start (point)))
                          (forward-sexp 1)
                          (buffer-substring-no-properties default-start (point)))))
                     (default-str
                      (with-temp-buffer
                        (emacs-lisp-mode)
                        (insert default-raw)
                        (let ((inhibit-message t))
                          (indent-region (point-min) (point-max)))
                        (string-trim (buffer-string))))
                     (doc (replace-regexp-in-string "`\\([^']*\\)'" "@code{\\1}" (cadr (cddr sexp)))))
                (if (string-match-p "\n" default-str)
                    (princ (format "@defopt %s\n\n@lisp\n%s\n@end lisp\n\n%s\n@end defopt\n\n" name default-str doc))
                  (princ (format "@defopt %s @code{%s}\n\n%s\n@end defopt\n\n" name default-str doc)))))
            t)))))
endef
export ESCRIPT


.PHONY: docs-lint
docs-lint:
	@cask emacs --batch -L . \
	  --eval "(require 'checkdoc)" \
	  --eval "(checkdoc-file \"pi.el\")" \
	  --eval "(checkdoc-file \"pi-section.el\")" \
	  --eval "(checkdoc-file \"pi-utils.el\")" \
	  --eval "(checkdoc-file \"pi-edit.el\")" \
	  --eval "(checkdoc-file \"pi-agent.el\")" \
	  --eval "(checkdoc-file \"pi-core.el\")" 2>&1 | grep '^pi[.-]' | grep -v 'All variables and subroutines might as well have a documentation string' || true

.PHONY: docs
docs: pi.info
	@ruby -e 'txt = IO.read("pi.texi").split("@c custom-variables-start")[0] + "@c custom-variables-start\n\n" + `$(EMACS) -Q --batch --eval "$$ESCRIPT"` + "@c custom-variables-end" + IO.read("pi.texi").split("@c custom-variables-end")[1]; File.write("pi.texi", txt)'
	@makeinfo pi.texi
	@makeinfo --no-number-sections --html --no-split -o ./docs/index.html pi.texi
