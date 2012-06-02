(require 'tex-site)
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(add-hook 'text-mode-hook
            '(lambda ()
               (auto-fill-mode)))

(add-hook 'LaTeX-mode-hook
          'turn-on-auto-fill)
(add-hook 'tex-mode-hook
          'turn-on-auto-fill)
(add-hook 'text-mode-hook
          (lambda () (abbrev-mode 1)))
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

; make PDF by default (can toggle with C-c C-t C-p
(add-hook 'TeX-mode-hook '(lambda () (TeX-PDF-mode 1)))

; these math abbrevs (` as prefix char) are also useful in TeX/ConTeXt files
;(require 'latex)                        ; defines LaTeX-math-mode
(add-hook 'TeX-mode-hook 'LaTeX-math-mode)
; Emacs help for \label, \ref, \cite.  Normally used only with
; LaTeX-mode but also useful with plain TeX + eplain and with ConTeXt, so:
(setq reftex-plug-into-AUCTeX t)
(add-hook 'TeX-mode-hook 'reftex-mode)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-method 'synctex)
