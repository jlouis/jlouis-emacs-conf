(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang" exec-path))
(require 'erlang-start)

(add-hook 'erlang-mode-hook 'esk-prog-mode-hook)

;; Align (thanks @eproxus)
(add-hook 'align-load-hook
          (lambda ()
            (add-to-list 'align-rules-list
                         '(erlang-align
                           (regexp . ",\\(\\s-+\\)")
                           (repeat . t)
                           (modes quote (erlang-mode))))))


