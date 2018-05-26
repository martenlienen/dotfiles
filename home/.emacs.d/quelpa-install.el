(package-initialize)

(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

;; Package configuration
(quelpa 'use-package)

;; My own package
(quelpa '(cqql :fetcher file :path "home/.emacs.d/lisp/cqql.el"))

;; Key bindings
(quelpa 'free-keys)

;; Libaries
(quelpa 'dash)
(quelpa 's)

;; Planning
(quelpa 'org-plus-contrib)
(quelpa 'org-bullets)

;; Load PATH from shell, even when emacs is not started from one
(quelpa 'exec-path-from-shell)

;; General editing
(quelpa 'avy)
(quelpa 'ace-window)
(quelpa 'multiple-cursors)
(quelpa 'smartparens)
(quelpa 'expand-region)
(quelpa 'wrap-region)
(quelpa 'centimacro)
(quelpa 'visual-regexp)
(quelpa 'key-chord)
(quelpa 'hydra)
(quelpa 'beginend)

;; Convenience
(quelpa 'dictcc)

;; UI
(quelpa 'color-theme-sanityinc-tomorrow)
(quelpa 'smart-mode-line)
(quelpa 'rainbow-delimiters)
(quelpa 'shackle)
(quelpa 'highlight-symbol)
(quelpa 'discover-my-major)

;; Snippets
(quelpa 'yasnippet)

;; Autocomplete
(quelpa 'company)

;; VCS
(quelpa 'magit)
(quelpa 'git-timemachine)

;; Syntax checking
(quelpa 'flycheck)

;; Searching
(quelpa 'swiper)
(quelpa 'ivy)
(quelpa 'counsel)
(quelpa 'projectile)
(quelpa 'flx)

;; Markdown
(quelpa 'markdown-mode)

;; Emacs Lisp
(quelpa 'macrostep)

;; C/C++
(quelpa 'cmake-mode)
(quelpa 'google-c-style)
(quelpa 'irony)
(quelpa 'irony-eldoc)
(quelpa 'company-irony)
(quelpa 'company-irony-c-headers)
(quelpa 'clang-format)

;; Python
(quelpa 'pyenv-mode)
(quelpa 'anaconda-mode)
(quelpa 'company-anaconda)

;; Anything web related
(quelpa 'web-mode)

;; Javascript
(quelpa 'js2-mode)
(quelpa 'js2-refactor)

;; Lua
(quelpa 'lua-mode)

;; Rust
(quelpa 'rust-mode)
(quelpa 'racer)
(quelpa 'cargo)
(quelpa 'flycheck-rust)

;; LaTeX
(quelpa 'auctex)

;; Markup Languages
(quelpa 'yaml-mode)
