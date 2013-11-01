(require 'rspec-mode)

(defun cqql/body-to-hash (body)
  (let ((parts (-partition 2 body))
        (settings (make-hash-table)))
    (-map
     (lambda (tuple) (puthash (car tuple) (cadr tuple) settings))
     parts)
    settings))

(defmacro defconfig (mode &rest body)
  "A macro to configure modes"
  (let* ((evil-mode-name (intern (concat "evil-" (symbol-name mode) "-mode")))
         (evil-keymap (intern (concat (symbol-name evil-mode-name) "-map")))
         (evil-hook-name (intern (concat (symbol-name evil-mode-name) "-hook")))
         (hook-name (intern (concat (symbol-name mode) "-mode-hook")))
         (hash (cqql/body-to-hash body))
         (settings (gethash :settings hash))
         (evil-keys (gethash :evil-keys hash))
         (hooks (gethash :hooks hash ())))
    `(progn
       ,@(-map (lambda (setting) `(setq ,(car setting) ,(cadr setting))) settings)
       (define-minor-mode ,evil-mode-name ,(concat "Evil mode bindings for " (symbol-name mode)) :keymap (make-sparse-keymap))
       ,@(-map (lambda (key) `(evil-define-key ',(car key) ,evil-keymap (kbd ,(cadr key)) ',(caddr key))) evil-keys)
       (add-hook ',hook-name ',evil-mode-name))))

(defconfig rspec
  :settings ((rspec-use-rake-when-possible nil))
  :evil-keys ((normal "SPC f" rspec-verify-single)
              (normal "SPC r r" rspec-rerun)
              (normal "SPC r f" rspec-verify)
              (normal "SPC r g" rspec-verify-all)))
