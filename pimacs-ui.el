;;; pimacs-ui.el --- Shared Pimacs UI primitives -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Shared presentation primitives for Pimacs UI buffers.

;;; Code:

(require 'pimacs-section)
(require 'pcre2el)

(defface pimacs-chat-role-face
  '((t :inherit font-lock-builtin-face))
  "Face used for generic chat role labels."
  :group 'pimacs)

(defface pimacs-chat-user-role-face
  '((t :inherit font-lock-keyword-face))
  "Face used for user chat message role labels."
  :group 'pimacs)

(defface pimacs-chat-assistant-role-face
  '((t :inherit font-lock-constant-face))
  "Face used for assistant chat message role labels."
  :group 'pimacs)

(defface pimacs-tool-name-face
  '((t :inherit font-lock-function-name-face))
  "Face used for tool names in tool execution events."
  :group 'pimacs)

(defface pimacs-grep-match-face
  '((t :inherit match))
  "Face used to highlight matching text in grep tool results."
  :group 'pimacs)

(defun pimacs-ui--role-face (role)
  (pcase role
    ("user" 'pimacs-chat-user-role-face)
    ("assistant" 'pimacs-chat-assistant-role-face)
    (_ 'pimacs-chat-role-face)))

(defun pimacs-ui--insert-role-prefix (role)
  (pimacs-section--insert-chrome (format "%s> " role)
                                 (pimacs-ui--role-face role)))

(defun pimacs-ui--insert-tool-name (tool-name)
  (pimacs-section--insert-chrome (format "%s " tool-name)
                                 'pimacs-tool-name-face))


(defun pimacs--grep-pattern-regexp (pattern literal)
  (if literal
      (regexp-quote pattern)
    (condition-case nil
        (rxt-pcre-to-elisp pattern)
      (error nil))))

(defun pimacs--fontify-grep-matches (begin end regexp ignore-case)
  (when regexp
    (save-excursion
      (let ((case-fold-search (if ignore-case t nil))
            (searching t))
        (save-restriction
          (narrow-to-region begin end)
          (goto-char (point-min))
          (while (and searching (re-search-forward regexp nil t))
            (if (= (match-beginning 0) (match-end 0))
                (if (< (point) (point-max))
                    (forward-char 1)
                  (setq searching nil))
              (add-text-properties (match-beginning 0) (match-end 0)
                                   '(face pimacs-grep-match-face)))))))))

(provide 'pimacs-ui)

;;; pimacs-ui.el ends here
