(evil-mode)

; Toggle evil mode
(define-key evil-normal-state-map (kbd "M-^") 'evil-mode)
(global-set-key (kbd "M-^") 'evil-mode)

; Remap jk to ESC
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

; Do not overload RET and SPC
(define-key evil-insert-state-map (kbd "RET") nil)
(define-key evil-insert-state-map (kbd "SPC") nil)

; , is much easier to type when your fingers are on the motion keys
(define-key evil-normal-state-map (kbd ",") 'evil-ex)

; Open files
(define-key evil-normal-state-map (kbd "SPC t") 'projectile-find-file)

; Ace jump
(define-key evil-normal-state-map (kbd "SPC w") 'ace-jump-word-mode)
(define-key evil-normal-state-map (kbd "SPC h") 'ace-jump-char-mode)

; Toggle comments
(define-key evil-visual-state-map (kbd "c") 'comment-or-uncomment-region)

; Searching with ag
(require 'ag)
(add-to-list 'ag-arguments "--hidden")
(define-key evil-normal-state-map (kbd "SPC \\") 'ag-project)

; Magit
(require 'magit)
(defun cqql/magit-commit-all ()
  (interactive)
  (magit-commit-internal "commit" '("--all")))

(define-key evil-normal-state-map (kbd "SPC g g") 'magit-status)
(define-key evil-normal-state-map (kbd "SPC g c") 'cqql/magit-commit-all)

; Window management
(define-key evil-normal-state-map (kbd "C-M-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-M-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-M-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-M-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "M-c") 'split-window-horizontally)
(define-key evil-normal-state-map (kbd "M-s") 'split-window-vertically)
