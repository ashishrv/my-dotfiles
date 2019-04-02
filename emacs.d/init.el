;; This fixed garbage collection, makes emacs start up faster
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(defun startup/reset-gc ()
  (setq gc-cons-threshold 16777216
	gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc)

;; load rest of emacs configuration as org files

(when (file-readable-p "~/.emacs.d/standard.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/standard.org")))

(when (file-readable-p "~/.emacs.d/variables.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/variables.org")))

(when (file-readable-p "~/.emacs.d/package_manager.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/package_manager.org")))

(when (file-readable-p "~/.emacs.d/packages.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/packages.org")))

(when (file-readable-p "~/.emacs.d/languages.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/languages.org")))

(when (file-readable-p "~/.emacs.d/tools.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/tools.org")))

(when (file-readable-p "~/.emacs.d/keychords.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/keychords.org")))

(when (file-readable-p "~/.emacs.d/theme.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/theme.org")))
