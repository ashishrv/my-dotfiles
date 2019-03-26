

;; We will be using straight.el
;; https://github.com/raxod502/straight.el

;; Bootstrap package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))



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
(setq ivy-use-virtual-buffers t)
;; intentional space before end of string
(setq ivy-count-format "(%d/%d) ")
(setq ivy-initial-inputs-alist nil)
(setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
(setq ivy-magic-slash-non-match-action nil)

(global-set-key (kbd "M-j") #'counsel-M-x)
(global-set-key (kbd "C-o") #'counsel-find-file)
