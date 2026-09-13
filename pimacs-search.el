;;; pimacs-search.el --- Historical session search -*- lexical-binding: t; -*-

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

;; Historical Pi session search.

;;; Code:

(require 'cl-lib)
(require 'pimacs-core)

(defcustom pimacs-search-rg-executable "rg"
  "Ripgrep executable used for historical session searches."
  :type 'string
  :group 'pimacs)

(defcustom pimacs-search-jq-executable "jq"
  "Jq executable used for historical session searches."
  :type 'string
  :group 'pimacs)

(defcustom pimacs-search-default-folder
  (expand-file-name "sessions/" "~/.pi/agent/")
  "Default directory containing Pi session directories."
  :type 'directory
  :group 'pimacs)

(defconst pimacs-search--filter-types
  '(user assistant thinking tool-call tool-result bash compact))

(defconst pimacs-search--default-filters
  '(user assistant))

(cl-defstruct pimacs-search-request
  folder scope query filters project-root)

(defun pimacs-search--project-session-directory (folder project-root)
  (let* ((project-root (directory-file-name (expand-file-name project-root)))
         (path (replace-regexp-in-string "\\`[/\\\\]+" "" project-root))
         (path (replace-regexp-in-string "[:/\\\\]" "-" path)))
    (expand-file-name (format "--%s--" path) folder)))

(defun pimacs-search--default-request ()
  (make-pimacs-search-request
   :folder (expand-file-name pimacs-search-default-folder)
   :scope 'current-project
   :query ""
   :filters (copy-sequence pimacs-search--default-filters)
   :project-root (pimacs--project-root)))

(defun pimacs-search--request-directory (request)
  (pcase (pimacs-search-request-scope request)
    ('current-project
     (pimacs-search--project-session-directory
      (pimacs-search-request-folder request)
      (pimacs-search-request-project-root request)))
    ('all-projects (pimacs-search-request-folder request))
    (_ (error "Unknown search scope: %S"
              (pimacs-search-request-scope request)))))

(provide 'pimacs-search)

;;; pimacs-search.el ends here
