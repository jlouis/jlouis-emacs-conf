(require 'erlang-start)
(require 'distel)
(distel-setup)

;; Wrangler
(add-to-list 'load-path
          "/usr/local/lib/erlang/lib/wrangler-1.0/elisp")
(require 'wrangler)

(add-hook 'erlang-mode-hook 'esk-prog-mode-hook)

;; Align (thanks @eproxus)
(add-hook 'align-load-hook
          (lambda ()
            (add-to-list 'align-rules-list
                         '(erlang-align
                           (regexp . ",\\(\\s-+\\)")
                           (repeat . t)
                           (modes quote (erlang-mode))))))


