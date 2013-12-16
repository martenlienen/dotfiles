(defvar cqql/packages nil)

(defmacro cqql/add-packages (&rest packages)
  `(progn
     ,@(mapcar (lambda (package) `(push ',package cqql/packages))
	       packages)))

;; Libaries
(cqql/add-packages dash
                   s)

;; General editing
(cqql/add-packages evil
                   ace-jump-mode
                   key-chord
                   multiple-cursors
                   smartparens
                   expand-region)

;; Syntax checking
(cqql/add-packages flycheck)

;; Snippets
(cqql/add-packages yasnippet)

;; Autocomplete
(cqql/add-packages auto-complete
                   ac-nrepl
                   ac-math
                   company)

;; VCS
(cqql/add-packages magit)

;; Searching
(cqql/add-packages ag
                   icicles
                   projectile
                   flx-ido)

;; Markdown
(cqql/add-packages markdown-mode)

;; Clojure
(cqql/add-packages clojure-mode
                   clojure-test-mode
                   cider
                   rainbow-delimiters)

;; Ruby
(cqql/add-packages ruby-additional
                   ruby-electric
                   robe
                   company-inf-ruby
                   rspec-mode)

;; HTML replacements
(cqql/add-packages haml-mode
                   jade-mode)

;; CSS replacements
(cqql/add-packages scss-mode
                   less-css-mode)

;; Javascript replacements
(cqql/add-packages coffee-mode)

;; LaTeX
(cqql/add-packages auctex)

(require 'cl-lib)
(require 'package)

(defun cqql/missing-packages (packages)
  (cl-reduce
   (lambda (acc package)
     (if (package-installed-p package)
         acc
       (cons package acc)))
   packages
   :initial-value '()))

(defun cqql/install-packages (packages)
  (let ((missing (cqql/missing-packages packages)))
    (when missing
      (package-refresh-contents)
      (mapc 'package-install missing))))

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(setq package-enable-at-startup nil)

(cqql/install-packages cqql/packages)

(provide 'packages)
