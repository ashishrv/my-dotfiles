;; Add package archive links
;; These are the links to Elpa, Marmelade and Melpa

(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))



;; Various themes installed and for use
;; (load-theme 'anti-zenburn t)

;; Theme
;; https://github.com/bbatsov/zenburn-emacs
(load-theme 'zenburn t)
(set-cursor-color "firebrick")

