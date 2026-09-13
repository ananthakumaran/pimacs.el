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
(require 'pimacs-section)

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

(defconst pimacs-search--jq-filter
  "def selected($kind): ($filters | index($kind)) != null;

def has_content_type($kind):
  (.message.content? // []) as $content
  | if ($content | type) == \"array\"
    then any($content[]; .type == $kind)
    else false
    end;

def accepted:
  if .type == \"message\" then
    if .message.role == \"user\" then selected(\"user\")
    elif .message.role == \"assistant\" then
      (selected(\"assistant\") and
       ((.message.content | type) == \"string\" or has_content_type(\"text\"))) or
      (selected(\"thinking\") and has_content_type(\"thinking\")) or
      (selected(\"tool-call\") and has_content_type(\"toolCall\"))
    elif .message.role == \"toolResult\" then selected(\"tool-result\")
    elif .message.role == \"bashExecution\" then selected(\"bash\")
    else false
    end
  elif .type == \"compaction\" then selected(\"compact\")
  else false
  end;

select(.type == \"match\")
| .data as $match
| ($match.lines.text | fromjson) as $entry
| select($entry | accepted)
| {path: $match.path.text, offset: $match.absolute_offset, entry: $entry}
")

(defconst pimacs-search--buffer-name "*Pimacs Session Search*")

(defvar-local pimacs-search--controls-section nil)
(defvar-local pimacs-search--status-section nil)
(defvar-local pimacs-search--results-section nil)

(define-derived-mode pimacs-search-mode special-mode "Pimacs Search"
  "Major mode for browsing historical Pi session search results."
  (setq-local truncate-lines t))

(defun pimacs-search--initialize-buffer ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq pimacs-section--root-section nil)
    (let ((root (pimacs-section--create-root-section)))
      (setq pimacs-search--controls-section
            (pimacs-section--create-section 'custom root
              (insert (propertize "Session Search" 'face 'bold))))
      (setq pimacs-search--status-section
            (pimacs-section--create-section 'info root
              (insert "No search started.")))
      (setq pimacs-search--results-section
            (pimacs-section--create-section 'custom root)))))

(defun pimacs-search--buffer ()
  (let ((buffer (get-buffer-create pimacs-search--buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'pimacs-search-mode)
        (pimacs-search-mode)
        (pimacs-search--initialize-buffer)))
    buffer))

;;;###autoload
(defun pimacs-search-sessions ()
  "Display the persistent buffer for searching historical Pi sessions."
  (interactive)
  (pop-to-buffer (pimacs-search--buffer)))

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
