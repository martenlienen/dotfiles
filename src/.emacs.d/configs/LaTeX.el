(require 'tex-site)
(require 'ac-math)

(defconfig LaTeX
  :hooks (auto-complete-mode ac-latex-mode-setup TeX-source-correlate-mode LaTeX-math-mode)
  :settings ((TeX-electric-sub-and-superscript t)
             (TeX-PDF-mode t)
             (TeX-save-query nil))
  :files ("\\.tex\\'"))

(setq ac-sources
      (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
              ac-sources))
                
(add-to-list 'ac-modes 'LaTeX-mode)
