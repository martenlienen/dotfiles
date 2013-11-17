(setq evil-default-state 'insert)

(evil-mode)

; Unbind all insert mode keys
(setq evil-insert-state-map (make-sparse-keymap))

; Remap jk to Esc
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
