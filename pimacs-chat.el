;;; pimacs-chat.el --- Active chat management -*- lexical-binding: t; -*-

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

;; Active chat management for Pimacs.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'pimacs-core)
(require 'pimacs-agent)
(require 'pimacs-state-line)
(require 'pimacs-utils)

(defcustom pimacs-list-chats-table
  '(("Chat" . (:session_name face pimacs-session-name-face))
    ("Provider" . :provider)
    ("Model" . :model)
    ("State" . (:agent_state face font-lock-constant-face))
    ("Context" . (:context_usage face shadow))
    ("Messages" . :total_messages)
    ("Cost" . (:cost face shadow))
    ("Project" . (:project_root face pimacs-session-directory-face)))
  "Columns displayed by `pimacs-list-chats'.

Each entry is (HEADER . COMPONENT).  COMPONENT uses the same format as an
entry in `pimacs-header-line-format'."
  :type `(repeat
          (cons (string :tag "Header")
                ,(cadr pimacs--state-line-format-type)))
  :group 'pimacs)

(defcustom pimacs-list-chats-sort-key '("Chat" . nil)
  "Initial sort order for `pimacs-list-chats'.

The car is a header from `pimacs-list-chats-table'.  A non-nil cdr sorts
in descending order."
  :type '(cons (string :tag "Column")
               (boolean :tag "Descending"))
  :group 'pimacs)

(defvar pimacs--chats (make-hash-table :test 'equal))

(defun pimacs--current-chat ()
  (gethash pimacs--project-key pimacs--chats))

(defun pimacs--active-chat-candidates ()
  (let (candidates)
    (maphash
     (lambda (key agent)
       (when-let ((chat (gethash key pimacs--chats)))
         (when (and (process-live-p agent)
                    (buffer-live-p chat))
           (push (cons key chat) candidates))))
     pimacs--agents)
    candidates))

(defun pimacs--relevant-chat-candidates ()
  (let ((path (expand-file-name (or buffer-file-name default-directory))))
    (seq-filter
     (lambda (candidate)
       (when-let* ((agent (gethash (car candidate) pimacs--agents))
                   (root (process-get agent 'project-root)))
         (file-in-directory-p path root)))
     (pimacs--active-chat-candidates))))

(defun pimacs--chat-label (chat &optional include-id)
  (with-current-buffer chat
    (let* ((name (plist-get pimacs--header-line-state :sessionName))
           (session-id (pimacs--plist-get pimacs--header-line-state :sessionStats :sessionId))
           (short-id (pimacs--short-uuid session-id)))
      (if (and (stringp name) (not (string-empty-p name)))
          (let ((display-name (propertize name 'face 'pimacs-session-name-face)))
            (if (and include-id short-id)
                (concat display-name " " short-id)
              display-name))
        (propertize (or short-id "unknown") 'face 'pimacs-session-name-face)))))

(defun pimacs--select-chat (candidates prompt)
  (cond
   ((null candidates) nil)
   ((null (cdr candidates)) (car candidates))
   (t
    (let* ((labels
            (mapcar (lambda (candidate)
                      (cons (pimacs--chat-label (cdr candidate)) candidate))
                    candidates))
           (choices
            (sort
             (mapcar
              (lambda (label)
                (if (> (cl-count (car label) labels :key #'car :test #'equal) 1)
                    (cons (pimacs--chat-label (cdr (cdr label)) t) (cdr label))
                  label))
              labels)
             (lambda (a b) (string< (car a) (car b)))))
           (annotation-function
            (lambda (label)
              (when-let* ((candidate (cdr (assoc label choices)))
                          (agent (gethash (car candidate) pimacs--agents))
                          (root (process-get agent 'project-root)))
                (concat "  " (propertize (abbreviate-file-name (expand-file-name root)) 'face 'pimacs-session-directory-face)))))
           (completion-extra-properties
            `(:annotation-function ,annotation-function))
           (selected (completing-read prompt choices nil t)))
      (cdr (assoc selected choices))))))

(defun pimacs--select-relevant-chat ()
  (when-let ((candidate (pimacs--select-chat (pimacs--relevant-chat-candidates)
                                             "Pimacs chat: ")))
    (setq-local pimacs--project-key (car candidate))
    (cdr candidate)))

(defvar-keymap pimacs-list-chats-mode-map
  :doc "Keymap for `pimacs-list-chats-mode'."
  :parent tabulated-list-mode-map
  "RET" #'pimacs-list-chats-visit
  "g" #'pimacs-list-chats-refresh)

(define-derived-mode pimacs-list-chats-mode tabulated-list-mode "Pimacs Chats"
  "Major mode for listing active Pimacs chats."
  (setq tabulated-list-padding 0
        tabulated-list-sort-key (copy-tree pimacs-list-chats-sort-key)))

(defun pimacs--list-chats-entries ()
  (mapcar
   (lambda (candidate)
     (with-current-buffer (cdr candidate)
       (list candidate
             (vconcat
              (mapcar
               (lambda (column)
                 (pimacs--format-state-line-component
                  (pimacs--state-line-state) (cdr column)))
               pimacs-list-chats-table)))))
   (pimacs--active-chat-candidates)))

(defun pimacs--list-chats-format (entries)
  (vconcat
   (cl-loop for column in pimacs-list-chats-table
            for index from 0
            collect
            (list (car column)
                  (max (+ 2 (string-width (car column)))
                       (or (cl-loop for entry in entries
                                    maximize (string-width
                                              (aref (cadr entry) index)))
                           0))
                  t))))

(defun pimacs-list-chats-refresh ()
  "Refresh the Pimacs chats list."
  (interactive)
  (let ((entries (pimacs--list-chats-entries)))
    (setq tabulated-list-format (pimacs--list-chats-format entries)
          tabulated-list-entries entries)
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(defun pimacs-list-chats-visit ()
  "Visit the Pimacs chat on the current line."
  (interactive)
  (when-let ((candidate (tabulated-list-get-id)))
    (pop-to-buffer (cdr candidate))))

(defun pimacs-list-chats ()
  "List active Pimacs chats in a tabulated buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Pimacs Chats*")))
    (with-current-buffer buffer
      (pimacs-list-chats-mode)
      (pimacs-list-chats-refresh))
    (pop-to-buffer buffer)))

(defun pimacs-switch-chat ()
  "Switch to another active Pimacs chat."
  (interactive)
  (if-let ((candidate (pimacs--select-chat (pimacs--active-chat-candidates)
                                           "Switch to Pimacs chat: ")))
      (pop-to-buffer (cdr candidate))
    (user-error "No active Pimacs chats")))

(provide 'pimacs-chat)

;;; pimacs-chat.el ends here
