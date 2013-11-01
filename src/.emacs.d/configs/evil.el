(evil-mode)

; Unbind all insert mode keys
(setq evil-insert-state-map (make-sparse-keymap))

(defconfig evil
  :global-evil-keys ((normal "," 'evil-ex)
                     (visual "c" 'comment-or-uncomment-region)))

; Remap jk to Esc
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

; Toggle evil mode
(define-key evil-normal-state-map (kbd "M-^") 'evil-mode)
(global-set-key (kbd "M-^") 'evil-mode)

; Window management
(define-key evil-normal-state-map (kbd "M-c") 'split-window-horizontally)
(define-key evil-normal-state-map (kbd "M-s") 'split-window-vertically)
