
;; Python development environment
;; https://github.com/jorgenschaefer/elpy
;; https://elpy.readthedocs.io/en/latest/index.html
;; Requires pip install jedi flake8 autopep8
(straight-use-package 'elpy)
(elpy-enable)
;; https://github.com/proofit404/anaconda-mode
(straight-use-package 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-mode)


;; Markdown Mode
;; https://jblevins.org/projects/markdown-mode/
(straight-use-package 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
