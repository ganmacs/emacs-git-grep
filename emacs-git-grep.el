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

(defcustom emacs-git-grep-condidates-limits 200
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

(defun chomp (str)
  (replace-regexp-in-string "[\n\r]" "" str))

(defun git-project? ()
  (let* ((cmd "git rev-parse --is-inside-work-tree")
         (project (chomp (shell-command-to-string cmd))))
    (string= project "true")))

(defun git-project-root-directory ()
  (let ((cmd "git rev-parse --show-toplevel"))
    (if (git-project?)
        (chomp (shell-command-to-string cmd)))))

(defun emacs-git-grep-init ()
  (let ((cmd (read-shell-command "git grep: " git-grep-helm-base-command))
        (buffer (helm-candidate-buffer 'global)))
    (with-current-buffer buffer
      (let ((default-directory (concat (git-project-root-directory) "/")))
        (unless (zerop (call-process-shell-command cmd nil t))
          (error "Failed: '%s'" cmd))
        (when (zerop (length (buffer-string)))
          (error "No output: '%s'" cmd))))))

(defun emacs-git-grep-condidates-style (candidate)
  (let* ((elements (split-string candidate ":"))
         (file-path (propertize (car elements) 'face 'helm-moccur-buffer))
         (line (propertize (cadr elements) 'face 'helm-grep-lineno))
         (contents (mapconcat 'identity (cddr elements) "")))
    (format "%s:%s:%s" file-path line contents)))

(defun emacs-git-grep-find-file-action (candidate)
  (let* ((elements (split-string candidate ":"))
         (file-path (concat (git-project-root-directory) "/" (car elements)))
         (line (string-to-number (cadr elements))))
    (find-file file-path)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun emacs-git-grep-persistent-action (candidate)
  (let* ((elements (split-string candidate ":"))
         (file-path (concat (git-project-root-directory) "/" (car elements)))
         (line (string-to-number (cadr elements))))
    (find-file file-path)
    (goto-char (point-min))
    (forward-line (1- line))
    (helm-highlight-current-line)))

(defvar emacs-git-grep-source
  '((name . "emacs git grep")
    (init . emacs-git-grep-init)
    (candidates-in-buffer)
    (persistent-action . emacs-git-grep-persistent-action)
    (real-to-display . emacs-git-grep-condidates-style)
    (action . (("Open File" . emacs-git-grep-find-file-action)))
    (candidate-number-limit . 9999)))

;;;###autoload
(defun emacs-git-grep ()
  (interactive)
  (helm-attrset 'git-project-root-directory nil emacs-git-grep-source)
  (helm :sources emacs-git-grep-source
        :buffer "*git-grep-helm*"))

(provide 'emacs-git-grep)

;; emacs-git-grep.el ends here
