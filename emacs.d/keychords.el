;; My key chords
(defun my/switch-to-previous-buffer ()
    "Switch to previously open buffer.
     Repeated invocations toggle between the two most recently open buffers."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))

(key-chord-define-global "BB" 'my/switch-to-previous-buffer)
(key-chord-define-global "PP" 'counsel-projectile-switch-project)
(key-chord-define-global "SS" 'counsel-projectile-ag)
(key-chord-define-global "UU" 'undo-tree-visualize)
(key-chord-define-global "WS" 'ace-swap-window)
