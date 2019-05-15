;; More editing configurations
;; Use spaces
(setq-default indent-tabs-mode nil)
;; visual fill-column
(use-package visual-fill-column
  :config (global-visual-fill-column-mode))
;; fill at 85
(setq-default fill-column 85)
;; autofill text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Install doom themes
;; https://github.com/hlissner/emacs-doom-themes
(straight-use-package 'doom-themes)
(require 'doom-themes)
;; Selecting a doom theme
(load-theme 'doom-one t)
;; (load-theme 'doom-one-light t)
;;(load-theme 'doom-vibrant t)


(doom-themes-visual-bell-config)
(doom-themes-treemacs-config)
(doom-themes-org-config)
