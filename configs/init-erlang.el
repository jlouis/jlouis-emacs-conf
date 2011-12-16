(setq erlang-root-dir "/usr/local/lib/erlang")
(setq tools-ver "2.6.6.6")
(setq load-path (cons (concat erlang-root-dir "/lib/tools-" tools-ver "/emacs")
                      load-path))
(setq exec-path (cons (concat erlang-root-dir "/bin")
                      exec-path))
(require 'erlang-start)

;; Wrangler

(add-to-list 'load-path "/usr/local/share/wrangler/elisp")
;(require 'wrangler)

;(load-file "/usr/local/share/wrangler/elisp/graphviz-dot-mode.el")
(add-hook 'erlang-mode-hook 'esk-prog-mode-hook)




