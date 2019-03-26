
;; Setup minor packages and their configuration

;; https://framagit.org/steckerhalter/discover-my-major
(straight-use-package 'discover-my-major)

;; https://github.com/justbur/emacs-which-key
(straight-use-package 'which-key)
(setq which-key-popup-type 'side-window)
(setq which-key-side-window-location 'right)
(setq which-key-separator " ")
(setq which-key-prefix-prefix "+")
(which-key-mode)
