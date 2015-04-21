;;; emacs-git-grep.el --- interface of git grep with helm

;; Copyright (C) 2015 ganmacs

;; Author: ganmacs <ganmacs_at_gmail.com>
;; Maintainer: ganmacs <ganmacs_at_gmail.com>
;; URL: https://github.com/ganmacs/emacs-git-grep
;; Version: 0.0.1
;; Keywords: helm, git
;; Package-Requires: ((helm "1.6.9"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-utils)

(defgroup emacs-git-grep-helm nil
  "git grep  with helm interface"
  :group 'helm)

(defcustom emacs-git-grep-optional-command ""
  "git grep optional command"
  :type 'string
  :group 'git-grep-helm)

(defcustom emacs-git-grep-candidates-limits 200
  "emacs git grep prompt"
  :type 'integer
  :group 'git-grep-helm)

(defcustom emacs-git-grep-helm-ignore-case t
  "git grep ignore case"
  :type 'boolean
  :group 'git-grep-helm)

(defvar emacs-git-grep-base-command "git grep -n -I --no-color ")
(defvar emacs-git-grep-buffer "emacs git grep")
(defvar emacs-git-grep-prompt "git grep: ")
(defvar emacs-git-grep-mark-point "")
(defvar emacs-git-grep-point "")

(defun emacs-git-grep-chomp (str)
  (replace-regexp-in-string "[\n\r]" "" str))

(defun emacs-git-project? ()
  (let* ((cmd "git rev-parse --is-inside-work-tree")
         (project (emacs-git-grep-chomp (shell-command-to-string cmd))))
    (string= project "true")))

(defun emacs-git-grep-directory ()
  (let ((cmd "git rev-parse --show-toplevel"))
    (if (emacs-git-project?)
        (concat (emacs-git-grep-chomp (shell-command-to-string cmd)) "/"))))

(defun emacs-git-grep-command ()
  (format "%s%s%s"
          emacs-git-grep-base-command
          (if emacs-git-grep-helm-ignore-case "-i " "")
          (if (string= "" emacs-git-grep-mark-point) "" emacs-git-grep-mark-point)))

(defun emacs-git-grep-init ()
  (let* ((cmd-format (emacs-git-grep-command))
         (cmd (read-shell-command emacs-git-grep-prompt cmd-format))
         (buffer (helm-candidate-buffer 'global)))
    (with-current-buffer buffer
      (let ((default-directory  (emacs-git-grep-directory)))
        (unless (zerop (call-process-shell-command cmd nil t))
          (error "Failed: '%s'" cmd))
        (when (zerop (length (buffer-string)))
          (error "No output: '%s'" cmd))))))

(defun emacs-git-grep-candidates-style (candidate)
  (let* ((elements (split-string candidate ":"))
         (file-path (propertize (car elements) 'face 'helm-moccur-buffer))
         (line (propertize (cadr elements) 'face 'helm-grep-lineno))
         (contents (mapconcat 'identity (cddr elements) "")))
    (format "%s:%s:%s" file-path line contents)))

(defun emacs-git-grep-find-file-action (candidate)
  (let* ((elements (split-string candidate ":"))
         (file-path (concat (emacs-git-grep-directory) (car elements)))
         (line (string-to-number (cadr elements))))
    (find-file file-path)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun emacs-git-grep-persistent-action (candidate)
  (let* ((elements (split-string candidate ":"))
         (file-path (concat (emacs-git-grep-directory) (car elements)))
         (line (string-to-number (cadr elements))))
    (find-file file-path)
    (goto-char (point-min))
    (forward-line (1- line))
    (helm-highlight-current-line)))

(defvar emacs-git-grep-source
  '((name . emacs-git-grep-buffer)
    (init . emacs-git-grep-init)
    (candidates-in-buffer)
    (persistent-action . emacs-git-grep-persistent-action)
    (real-to-display . emacs-git-grep-candidates-style)
    (action . (("Open File" . emacs-git-grep-find-file-action)))))

(defun emacs-git-grep-mark-string ()
  (if (and mark-active transient-mark-mode)
      (buffer-substring-no-properties (region-beginning) (region-end))
    ""))

;;;###autoload
(defun emacs-git-grep ()
  (interactive)
  (unless (emacs-git-project?) (error "here is not git repogitory"))
  (let ((emacs-git-grep-mark-point (emacs-git-grep-mark-string))
        (header-name (format "Search at %s" (emacs-git-grep-directory))))
    (set-mark nil)
    (helm-attrset 'name header-name emacs-git-grep-source)
    (helm :buffer "*git-grep-helm*"
          :sources emacs-git-grep-source
          :candidate-number-limit emacs-git-grep-candidates-limits)))

(provide 'emacs-git-grep)

;; emacs-git-grep.el ends here
