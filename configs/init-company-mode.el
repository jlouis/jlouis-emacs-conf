(require 'company)

(setq company-idle-delay 0.5)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-lenght 2)

(setq company-tooltip-flip-when-above t)
(global-company-mode 1)

(provide 'init-company)
