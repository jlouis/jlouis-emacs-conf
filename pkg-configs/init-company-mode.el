(require 'company)

(setq company-idle-delay 0.3)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-lenght 2)

(setq company-tooltip-flip-when-above t)
(global-company-mode 1)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-w") 'backward-kill-word))

(provide 'init-company)
