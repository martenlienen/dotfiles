(require 'package)
(package-initialize)

;; Install packages from ELPA first because quelpa only knows MELPA
(require 'cl-lib)
(let ((elpa-packages '(auctex spinner)))
  (unless package-archive-contents
    (package-refresh-contents))

  (cl-loop for pkg in elpa-packages
           unless (package-installed-p pkg) do (package-install pkg)))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; Package configuration
(quelpa 'use-package)
(quelpa 'use-package-chords)

;; My own package
(quelpa '(cqql :fetcher file :path "home/.emacs.d/lisp/cqql.el"))
(quelpa '(cqql-deadgrep :fetcher file :path "home/.emacs.d/lisp/cqql-deadgrep.el"))

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
(quelpa 'hydra)
(quelpa 'beginend)
(quelpa 'smartscan)

;; Convenience
(quelpa 'dictcc)

;; UI
(quelpa 'darktooth-theme)
(quelpa 'smart-mode-line)
(quelpa 'delight)
(quelpa 'rainbow-delimiters)
(quelpa 'highlight-symbol)
(quelpa 'discover-my-major)
(quelpa 'helpful)
(quelpa 'beacon)

;; Debugging
(quelpa 'realgud)

;; Snippets
(quelpa 'yasnippet)

;; Autocomplete
(quelpa 'lsp-mode)
(quelpa 'lsp-ui)
(quelpa '(lsp-julia :fetcher github :repo "non-Jedi/lsp-julia" :files (:defaults "languageserver")))
(quelpa 'company)
(quelpa 'company-lsp)

;; VCS
(quelpa 'magit)
(quelpa 'git-timemachine)

;; Syntax checking
(quelpa 'flycheck)

;; Searching
(quelpa 'swiper)
(quelpa 'ivy)
(quelpa 'ivy-rich)
(quelpa 'ivy-prescient)
(quelpa 'counsel)
(quelpa 'projectile)
(quelpa 'flx)
(quelpa 'deadgrep)

;; Markdown
(quelpa 'markdown-mode)

;; Emacs Lisp
(quelpa 'macrostep)

;; C/C++
;; (quelpa 'cquery)
;; (quelpa 'cmake-mode)
;; (quelpa 'google-c-style)
;; (quelpa 'clang-format)

;; Python
(quelpa 'pyenv-mode)
(quelpa 'python-pytest)
(quelpa 'isortify)
(quelpa 'pip-requirements)
(quelpa 'blacken)
(quelpa 'python-docstring)
(quelpa 'anaconda-mode)
(quelpa 'company-anaconda)
(quelpa 'ein)

;; Julia
(quelpa 'julia-mode)
(quelpa 'julia-repl)

;; Anything web related
;; (quelpa 'web-mode)

;; Javascript
;; (quelpa 'js2-mode)
;; (quelpa 'js2-refactor)

;; Rust
(quelpa 'rust-mode)
(quelpa 'racer)
(quelpa 'cargo)
(quelpa 'flycheck-rust)

;; Writing
(quelpa '(darkroom :repo "joaotavora/darkroom" :fetcher "github"))

;; LaTeX
(quelpa 'auctex)

;; Markup Languages
(quelpa 'yaml-mode)
