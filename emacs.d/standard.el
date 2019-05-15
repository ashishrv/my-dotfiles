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
;; no error bell
(setq ring-bell-function 'ignore)
;; Make the yes or no prompts shorter.
(defalias 'yes-or-no-p 'y-or-n-p)
;; stops emacs adding customised settings to init.el
(setq custom-file (make-temp-file "emacs-custom"))
;; Highlight the current line.
(global-hl-line-mode 1)
(global-prettify-symbols-mode t)
;; Make minibuffer larger
(setq max-mini-window-height 1.5)

;; Create a central repository for backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
    backup-by-copying t    ; Don't delink hardlinks
    version-control t      ; Use version numbers on backups
    delete-old-versions t  ; Automatically delete excess backups
    kept-new-versions 20   ; how many of the newest versions to keep
    kept-old-versions 5    ; and how many of the old
    )

;; Make sure that UTF-8 is used everywhere
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)

;; Enable winner mode
;; allows you to “undo” (and “redo”) changes with C-Left and C-Right
(when (fboundp 'winner-mode)
  (winner-mode 1))
