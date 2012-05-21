(require 'erlang-start)
(require 'distel)
(distel-setup)

;; Wrangler

(add-hook 'erlang-mode-hook 'esk-prog-mode-hook)

;; Align (thanks @eproxus)
(add-hook 'align-load-hook
          (lambda ()
            (add-to-list 'align-rules-list
                         '(erlang-align
                           (regexp . ",\\(\\s-+\\)")
                           (repeat . t)
                           (modes quote (erlang-mode))))))


