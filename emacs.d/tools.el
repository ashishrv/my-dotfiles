

;; File explorer
;; https://github.com/Alexander-Miller/treemacs
(straight-use-package 'treemacs)
(straight-use-package 'treemacs-magit)
(require 'treemacs)
(setq treemacs-follow-after-init t
        treemacs-width 35
        treemacs-indentation 1
        treemacs-recenter-after-file-follow nil
        treemacs-collapse-dirs (if (executable-find "python") 3 0)
        treemacs-silent-refresh t
        treemacs-silent-filewatch t
        treemacs-change-root-without-asking t
        treemacs-sorting 'alphabetic-desc
        treemacs-show-hidden-files t
        treemacs-never-persist nil
        treemacs-is-never-other-window t
        treemacs-indentation-string (propertize " ǀ " 'face 'font-lock-comment-face))

(global-set-key [f8] 'treemacs)
(global-set-key (kbd "C-c f") 'treemacs-select-window)
