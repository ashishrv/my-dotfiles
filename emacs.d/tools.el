

;; provides intelligent editing of anything that you normally have to type twice
;; keeping parentheses balanced
(use-package smartparens
  :straight t
  :init
  (smartparens-global-mode))

;; https://github.com/larstvei/Focus
;; M-x focus-mode
;; q quit
(use-package focus
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


;; crux has useful functions extracted from Emacs Prelude
;; https://github.com/bbatsov/crux
(use-package crux
    :straight t
    :bind (("C-a" . crux-move-beginning-of-line)))
;; Remove whitespace at the end of lines on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Key chords bind functions to sequential key presses like jj.
;; It makes evil mode being turned off much more palatable.
;; https://github.com/jwiegley/use-package

(use-package use-package-chords
    :straight t
    :config
    (key-chord-mode 1))

;; undo-tree visualises undo history as a tree for easy navigation
(use-package undo-tree
    :straight t
    :diminish undo-tree-mode:
    :config
    (global-undo-tree-mode 1))

;; avy let's us jump to any character or line quickly
;; https://github.com/abo-abo/avy
(use-package avy
    :straight t
    :chords (("jj" . avy-goto-char-timer)
             ("gg" . avy-goto-line)))

;; avy-goto-line, it will switch to goto-line with that digit already entered
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "C-c C-j") 'avy-resume)

;; ace-window lets us navigate between windows in the same way as avy
;; https://github.com/abo-abo/ace-window

(use-package ace-window
    :straight t
    :chords ("WW" . ace-window)
    :config
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


;; semantic selection
;; https://github.com/magnars/expand-region.el
(use-package expand-region
    :straight t
    :bind ("C-=" . er/expand-region))

;; Highlight parens etc. for improved readability.
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

;; Git helpers
(use-package git-gutter
    :straight t
    :config
    (global-git-gutter-mode 't)
    :diminish git-gutter-mode)

(use-package git-timemachine
    :straight t)



;; https://github.com/magnars/multiple-cursors.el
;; https://github.com/phillord/lentic
;; https://github.com/emacsfodder/move-text
;; https://github.com/Malabarba/aggressive-indent-mode
;; https://github.com/zk-phi/indent-guide
;; https://github.com/mrkkrp/vimish-fold
;; https://github.com/gregsexton/origami.el


;; https://github.com/Wilfred/deadgrep
;; https://github.com/politza/pdf-tools
;; https://github.com/rudolfochrist/interleave


;; https://github.com/corpix/ob-blockdiag.el
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-ditaa.html
;; https://github.com/fniessen/org-macros
;; https://github.com/fniessen/org-html-themes
;; https://github.com/fniessen/refcard-org-mode
