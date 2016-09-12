;; Tuareg
(require 'merlin)

(autoload 'merlin-mode "merlin" "Merlin Mode" t)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)

;;; caml-code.el ends here

