
;; https://github.com/emacs-lsp/dap-mode



;; Python development environment
;; https://github.com/jorgenschaefer/elpy
;; https://elpy.readthedocs.io/en/latest/index.html
;; Requires pip install jedi flake8 autopep8
(straight-use-package 'elpy)
(elpy-enable)
;; https://github.com/proofit404/anaconda-mode
(straight-use-package 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-mode)

(use-package company-anaconda
      :straight t
      :config
      (add-to-list 'company-backends 'company-anaconda))

;; Smart dash guesses _ vs - depending on context.
(use-package smart-dash
    :straight t
    :config
    (add-hook 'python-mode-hook 'smart-dash-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))


;; Markdown Mode
;; https://jblevins.org/projects/markdown-mode/
(straight-use-package 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
;; Add a live preview
;; https://github.com/shime/emacs-livedown
(load "~/.emacs.d/external/emacs-livedown/livedown.el")


;; Saltstack
(straight-use-package 'salt-mode)
(add-hook 'salt-mode-hook
   (lambda ()
      (flyspell-mode 1)))

;; restructured text
;; http://docutils.sourceforge.net/docs/user/emacs.html


;; Go environment
;; Go-mode provides basic language support
;; call gofmt on each save to keep code
(use-package go-mode
    :straight t
    :config
    (add-hook 'before-save-hook 'gofmt-before-save)
   ;; use eldoc to display documentation
    (use-package go-eldoc
      :straight t
      :config
      (add-hook 'go-mode-hook 'go-eldoc-setup))
   ;; guru / doctor for IDE functionality
    (use-package godoctor
      :straight t)
    (use-package go-guru
      :straight t))

;; company integration
(use-package company-go
      :straight t
      :config
      (add-to-list 'company-backends 'company-go))

;; Go guru needs a scope to look at, this function sets it to the current package.
(defun jc/go-guru-set-current-package-as-main ()
    "GoGuru requires the scope to be set to a go package which
     contains a main, this function will make the current package the
     active go guru scope, assuming it contains a main"
    (interactive)
    (let* ((filename (buffer-file-name))
           (gopath-src-path (concat (file-name-as-directory (go-guess-gopath)) "src"))
           (relative-package-path (directory-file-name (file-name-directory (file-relative-name filename gopath-src-path)))))
      (setq go-guru-scope relative-package-path)))



;; Elxir
(use-package elixir-mode
    :straight t
    :config
    (use-package alchemist
      :straight t))


;; Haskell
(use-package haskell-mode
    :straight t)
;; Code formatting is easier with hindent
(use-package hindent
    :straight t)
;; Completion is via ghc-mod / company.
;; Install the former separately with cabal install ghc-mod.
(use-package ghc
    :straight t
    :config
    (add-hook 'haskell-mode-hook (lambda () (ghc-init))))
;; company integration
(use-package company-ghc
    :straight t
    :config
    (add-to-list 'company-backends 'company-ghc))


;; C
;; Emacs has a great built in C/C++ mode
(use-package irony
    :straight t
    :hook (c-mode . irony-mode))
(use-package company-irony
    :straight t
    :config
    (add-to-list 'company-backends 'company-irony))
(use-package flycheck-irony
    :straight t
    :hook (flycheck-mode . flycheck-irony-setup))
