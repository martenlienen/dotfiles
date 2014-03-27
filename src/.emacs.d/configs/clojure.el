(require 'cider-eldoc)

(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'clojure-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'smartparens-strict-mode)
