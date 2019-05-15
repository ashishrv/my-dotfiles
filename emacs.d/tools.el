(use-package smartparens
  :straight t
  :init
  (smartparens-global-mode))

;; https://github.com/larstvei/Focus
;; M-x focus-mode
;; q quit
(use-package focus
  :straight t)

(use-package crux
    :straight t
    :bind (("C-a" . crux-move-beginning-of-line)))
;; Remove whitespace at the end of lines on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package use-package-chords
    :straight t
    :config
    (key-chord-mode 1))

(use-package undo-tree
    :straight t
    :diminish undo-tree-mode:
    :config
    (global-undo-tree-mode 1))

(use-package avy
    :straight t
    :chords (("jj" . avy-goto-char-timer)
             ("gg" . avy-goto-line)))

;; avy-goto-line, it will switch to goto-line with that digit already entered
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "C-c C-j") 'avy-resume)

(use-package ace-window
    :straight t
    :chords ("WW" . ace-window)
    :config
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package expand-region
    :straight t
    :bind ("C-=" . er/expand-region))

(use-package rainbow-delimiters
    :straight t
    :config
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Highlight strings which represent colours
;; don't want colour names to be highlighted
(use-package rainbow-mode
    :straight t
    :config
    (setq rainbow-x-colors nil)
    (add-hook 'prog-mode-hook 'rainbow-mode))

;; An Emacs front-end for fzf.
;; fzf is a fuzzy file finder which is very quick.
;; https://github.com/bling/fzf.el
(use-package fzf
    :straight t)

;; Individual language packages often support IDE features like jump to source,
;; but dumb-jump attempts to support many languages by simple searching.
;; It's quite effective even with dynamic libraries like JS and Python.

(use-package dumb-jump
    :straight t
    :diminish dumb-jump-mode
    :bind (("C-M-g" . dumb-jump-go)
           ("C-M-p" . dumb-jump-back)
           ("C-M-q" . dumb-jump-quick-look)))

(use-package git-gutter
    :straight t
    :config
    (global-git-gutter-mode 't)
    :diminish git-gutter-mode)

(use-package git-timemachine
    :straight t)

;; https://github.com/cyrus-and/zoom
(use-package zoom
  :straight t
  :delight
  :config
  (zoom-mode t)
  (defun size-callback ()
    (cond ((> (frame-pixel-width) 1280) '(90 . 0.75))
          (t                            '(0.618 . 0.618))))
  (setf zoom-ignored-major-modes '(dired-mode markdown-mode ediff-mode magit-popup-mode treemacs-mode ranger-mode))
  (setf zoom-size 'size-callback))

;; Easy workspaces creation and switching
;;
(use-package eyebrowse
  :straight t
  :config
  (setq eyebrowse-mode-line-separator " "
                 eyebrowse-new-workspace t)
  (eyebrowse-mode t))
