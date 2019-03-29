;; Now to install a package permanently, place a call to straight-use-package
;; Example: (straight-use-package 'el-patch)
;; All packages that I like are listed below
;; These may be required by other packages so installed here


;; generic completion frontend for Emacs
;; https://github.com/abo-abo/swiper
;; Installing Counsel will bring in Ivy and Swiper as dependencies.
;;;; swiper and ivy
(use-package swiper
  :straight t
  :diminish ivy-mode
  :init
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    ;; number of result lines to display
    (setq ivy-height 12)
    ;; intentional space before end of string
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-display-style 'fancy)
    (setq enable-recursive-minibuffers t)
    (setq ivy-initial-inputs-alist nil)
    (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
    (setq ivy-switch-buffer-faces-alist
        '((emacs-lisp-mode . outline-1)
          (dired-mode . outline-2)
          (js2-mode . outline-4)
          (clojure-mode . outline-5)
          (org-mode . outline-3)))
    (setq ivy-magic-slash-non-match-action nil)
  :bind
    (("M-x". counsel-M-x)
    ("C-x C-f". counsel-find-file)
    ("C-s". swiper)
    ("C-c b". ivy-switch-buffer)
    ("C-c 4 b". ivy-switch-buffer-other-window)
    ("C-c r". counsel-recentf)
    ("C-c s". counsel-ag)
    ("C-x l". counsel-locate)
    ("C-c C-r". ivy-resume)
    ("<f1> f". counsel-describe-function)
    ("<f1> v". counsel-describe-variable)

    ;; change these bindings so that they do not look random
    ("C-c o". counsel-imenu)
    ("C-c y" . counsel-yank-pop)
    ("C-x r l" . counsel-bookmark)
    ("C-c g". counsel-git)
    ("C-c j". counsel-git-grep)

   :map ivy-mode-map
    ("S-SPC" . toggle-input-method)))

;; Additional bindings for counsel
(use-package counsel
  :straight t
  :bind
    (("M-y" . counsel-yank-pop)
  :map ivy-minibuffer-map
    ("M-y" . ivy-next-line)))

;;hydra presents menus for ivy commands
(use-package ivy-hydra
  :straight t)

;; Git
(use-package magit
  :straight t
  :bind
    ("C-x g" . magit-status)
  :config
    (setq magit-completing-read-function 'ivy-completing-read))


;; Company for command completion
(straight-use-package 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)



;; https://framagit.org/steckerhalter/discover-my-major
(straight-use-package 'discover-my-major)

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :straight t
  :diminish which-key-mode
  :init
    (which-key-mode)
    (which-key-setup-side-window-bottom)
  :config
    (setq which-key-sort-order 'which-key-prefix-then-key-order)

    (setq which-key-popup-type 'side-window
          which-key-side-window-location 'right
          which-key-side-window-max-height 0.5
          which-key-side-window-max-width 0.33
          which-key-idle-delay 0.5
          which-key-separator " "
          which-key-min-display-lines 7
          which-key-prefix-prefix "+"))




;; projectile
;; https://github.com/bbatsov/projectile
(use-package projectile
    :straight t
    :diminish projectile-mode
    :config
    (setq projectile-known-projects-file (concat conf:cache-dir "projectile-bookmarks.eld")
          projectile-cache-file (concat conf:cache-dir "projectile.cache")
          projectile-file-exists-remote-cache-expire (* 10 60)
          projectile-indexing-method 'alien
          projectile-enable-caching t
          projectile-project-search-path '("~/personal/workspace/" "~/works/workspace/")
          projectile-find-dir-include-top-level t
          projectile-completion-system 'ivy))


(use-package counsel-projectile
    :straight t
    :init
      (counsel-projectile-mode)
    :config
      (setq counsel-projectile-rg-options-history (list "-uuu"))
      (add-hook 'text-mode-hook 'counsel-projectile-mode)
      (add-hook 'prog-mode-hook 'counsel-projectile-mode)
      (setq counsel-projectile-switch-project-action 'deer)
      (projectile-global-mode)
    :bind
      ("C-c g". counsel-projectile-rg)
      ("C-c p". projectile-command-map))

;; Ranger
;; https://github.com/ralesi/ranger.el
(use-package ranger
    :straight t
    :config
        (setq ranger-cleanup-eagerly t
              ranger-show-hidden t
              ranger-footer-delay 0.2
              ranger-preview-delay 0.040
              ranger-parent-depth 2
              ranger-width-parents 0.2
              ranger-max-parent-width 0.2
              ranger-preview-file t
              ranger-show-literal t
              ranger-width-preview 0.55
              ranger-excluded-extensions '("mkv" "iso" "mp4" "zip*")
              ranger-max-preview-size 2
              ranger-override-dired-mode t
              ranger-cleanup-eagerly t
              ranger-dont-show-binary t))


;; Yasnippets templates
;; https://github.com/joaotavora/yasnippet
;; Use yasnippet. The after-save-hook causes all snippets to be reloaded after saving a snippet file.

(use-package yasnippet
  :straight t
  :init
    (progn
      (add-hook 'after-save-hook
                (lambda ()
                  (when (eql major-mode 'snippet-mode)
                    (yas-reload-all)))))
    (yas-global-mode 1)
  :config
    ;; yasnippet for git commit messages
    (add-hook 'git-commit-mode-hook
          (lambda ()
            (when (derived-mode-p 'text-mode)
              (yas-activate-extra-mode 'text-mode+git-commit-mode))))
  :mode ("\\.yas" . snippet-mode))

;; https://github.com/mineo/yatemplate
(use-package yatemplate
  :config
    ;; Define template directory
    (setq yatemplate-dir conf:yatemplates-dir)
    ;; Coupling with auto-insert
    (setq auto-insert-alist nil)
    (setq auto-insert-query nil)
    (yatemplate-fill-alist)
    (add-hook 'find-file-hook 'auto-insert)
)

(use-package ivy-yasnippet
  :straight t
  :bind
    ("C-x y" . ivy-yasnippet)
)
