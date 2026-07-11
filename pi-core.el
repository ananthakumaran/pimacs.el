;;; pi-core.el --- Core shared definitions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Anantha Kumaran.

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by

;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Core shared definitions used by both pi.el and pi-agent.el.

;;; Code:

(require 'pi-utils)
(require 'project)

(pi--def-permanent-buffer-local pi--project-root nil)
(pi--def-permanent-buffer-local pi--project-key nil)

(defun pi--project-root ()
  (or
   pi--project-root
   (let ((project (project-current))
         (path default-directory))
     (if project
         (setq path (project-root project))
       (message "Couldn't find project root folder. Using '%s' as project root." default-directory))
     (let ((full-path (expand-file-name path)))
       (setq pi--project-root full-path)
       full-path))))

(defun pi--project-name ()
  (file-name-nondirectory (directory-file-name (pi--project-root))))

(defun pi--agent-buffer-name ()
  (format "*pi-agent:%s*" (pi--project-key)))

(defun pi--project-key ()
  "Unique key for the current project, used for internal hash tables."
  (or
   pi--project-key
   (md5 (pi--project-root))))

(provide 'pi-core)

;;; pi-core.el ends here
