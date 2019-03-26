
;; Use Menlo as default font
(set-default-font "Menlo 10")

;; Install doom themes
;; https://github.com/hlissner/emacs-doom-themes
(straight-use-package 'doom-themes)
(require 'doom-themes)
;; Selecting a doom theme
;; (load-theme 'doom-one t)
;; (load-theme 'doom-one-light t)
(load-theme 'doom-vibrant t)

(doom-themes-visual-bell-config)
(doom-themes-treemacs-config)
(doom-themes-org-config)
