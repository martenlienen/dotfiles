(evil-mode)

(defconfig evil
  :global-evil-keys ((insert "RET" nil)
                     (insert "SPC" nil)
                     (normal "," 'evil-ex)
                     (visual "c" 'comment-or-uncomment-region)))

; Remap jk to Esc
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

; Toggle evil mode
(define-key evil-normal-state-map (kbd "M-^") 'evil-mode)
(global-set-key (kbd "M-^") 'evil-mode)

; Window management
(define-key evil-normal-state-map (kbd "M-c") 'split-window-horizontally)
(define-key evil-normal-state-map (kbd "M-s") 'split-window-vertically)
