; Unbind all insert mode keys
(setq evil-insert-state-map (make-sparse-keymap))
(setq evil-default-state 'insert)

(evil-mode)

; Remap jk to Esc
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
