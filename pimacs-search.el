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

(require 'pimacs-core)

(defcustom pimacs-search-rg-executable "rg"
  "Ripgrep executable used for historical session searches."
  :type 'string
  :group 'pimacs)

(defcustom pimacs-search-jq-executable "jq"
  "Jq executable used for historical session searches."
  :type 'string
  :group 'pimacs)

(provide 'pimacs-search)

;;; pimacs-search.el ends here
