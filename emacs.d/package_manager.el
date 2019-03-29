

;; We will be using straight.el
;; https://github.com/raxod502/straight.el

;; Bootstrap package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Effectively replace use-package with straight-use-package
;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; ensure we can install from git sources
(use-package git
  :straight t)

;; Packages required by many other packages
;; lists
(use-package dash
  :straight t
  :config (require 'dash))
;; hash-tables
(use-package ht
  :straight t
  :config (require 'ht))
;; strings
(use-package s
  :straight t
  :config (require 's))
;; association lists
(use-package a
  :straight t
  :config (require 'a))
