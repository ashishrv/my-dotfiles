;; Now to install a package permanently, place a call to straight-use-package
;; Example: (straight-use-package 'el-patch)
;; All packages that I like are listed below
;; These may be required by other packages so installed here

;; Git
(straight-use-package 'magit)

;; generic completion frontend for Emacs
;; https://github.com/abo-abo/swiper
;; Installing Counsel will bring in Ivy and Swiper as dependencies.
(straight-use-package 'counsel)
(require 'counsel)
(ivy-mode 1)
(setq ivy-display-style 'fancy)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-height 12)
;; intentional space before end of string
(setq ivy-count-format "(%d/%d) ")
(setq ivy-initial-inputs-alist nil)
(setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
(setq ivy-magic-slash-non-match-action nil)

;;hydra presents menus for ivy commands
(straight-use-package 'ivy-hydra)
(require 'ivy-hydra)

;; Use counsel for the two important functions
;; Running commands and opening files
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)


;; Company for command completion
(straight-use-package 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)


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

;; Ranger
;; https://github.com/ralesi/ranger.el
(straight-use-package 'ranger)
(setq ranger-cleanup-eagerly t)
(setq ranger-show-hidden t)
(setq ranger-footer-delay 0.2)
(setq ranger-preview-delay 0.040)
(setq ranger-parent-depth 2)
(setq ranger-width-parents 0.2)
(setq ranger-max-parent-width 0.2)
(setq ranger-preview-file t)
(setq ranger-show-literal t)
(setq ranger-width-preview 0.55)
(setq ranger-excluded-extensions '("mkv" "iso" "mp4" "zip*"))
(setq ranger-max-preview-size 2)
(setq ranger-dont-show-binary t)
