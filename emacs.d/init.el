
;; refactor every functionality into sepearte files
;; Use: https://github.com/emacs-tw/awesome-emacs

;; Do not display the splash screen
(setq inhibit-startup-screen t)
;; Remove the menu bar
(customize-set-variable 'menu-bar-mode nil)
;; Remove the tool bar
(if window-system
    (customize-set-variable 'tool-bar-mode nil))
;; Remove the scroll bar
(customize-set-variable 'scroll-bar-mode nil)
;; maximize my emacs frame on start-up
(toggle-frame-maximized)


;; package manager
(load "~/.emacs.d/package_manager.el")
(load "~/.emacs.d/minor_packages.el")
(load "~/.emacs.d/languages.el")
(load "~/.emacs.d/tools.el")
(load "~/.emacs.d/theme.el")
