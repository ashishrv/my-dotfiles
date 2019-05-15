(defvar my/name "Ashish R Vidyarthi")
(defvar my/mailaddress "ashish.vid@gmail.com")

(defvar my/org-path-name
  (expand-file-name "~/org/")
  "Root path-name for org-mode files")

(setq-default my/template-directory (concat my/org-path-name "templates/"))

(setq-default my/project-root (expand-file-name "~/src/"))

(defvar conf:cache-dir (concat user-emacs-directory "cache/"))
  (unless (file-exists-p conf:cache-dir)
    (make-directory conf:cache-dir))

(defvar conf:yatemplates-dir (concat user-emacs-directory "templates/"))
  (unless (file-exists-p conf:yatemplates-dir)
    (make-directory conf:yatemplates-dir))

(defun my/template-file-name (file-name)
  "Create file-name relative to my/template-directory"
  (concat my/template-directory file-name))

(defun my/org-file-name (file-name)
  "Create file-name relative to my/org-path-name"
  (concat my/org-path-name file-name))

(defvar my/notes-file-name
  (my/org-file-name "notes.org")
  "Main notes file-name")

(defun my/project-directory (name)
  (concat my/project-root name))

(defun my/system-is-mac ()
  (eq system-type 'darwin))


(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;; https://superuser.com/questions/131538/can-i-create-directories-that-dont-exist-while-creating-a-new-file-in-emacs
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir :parents)))))
