
(defvar my/name "Ashish R Vidyarthi")
(defvar my/mailaddress "ashish.vid@gmail.com")

(defvar my/org-path-name
  (expand-file-name "~/org/")
  "Root path-name for org-mode files")

(defun my/org-file-name (file-name)
  "Create file-name relative to my/org-path-name"
  (concat my/org-path-name file-name))

(defvar my/notes-file-name
  (my/org-file-name "notes.org")
  "Main notes file-name")

(setq-default my/template-directory (concat my/org-path-name "templates/"))

(defun my/template-file-name (file-name)
  "Create file-name relative to my/template-directory"
  (concat my/template-directory file-name))

(setq-default my/project-root (expand-file-name "~/src/"))

(defun my/project-directory (name)
  (concat my/project-root name))

(defun my/system-is-mac ()
  (eq system-type 'darwin))

(defvar conf:cache-dir (concat user-emacs-directory "cache/"))
  (unless (file-exists-p conf:cache-dir)
    (make-directory conf:cache-dir))

(defvar conf:yatemplates-dir (concat user-emacs-directory "templates/"))
  (unless (file-exists-p conf:yatemplates-dir)
    (make-directory conf:yatemplates-dir))

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))
