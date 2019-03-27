

;; Projectile
;; https://github.com/bbatsov/projectile
(straight-use-package 'projectile)
(straight-use-package 'counsel-projectile)
(counsel-projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
