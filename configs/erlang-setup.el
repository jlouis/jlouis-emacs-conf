

(setq erlang-root-dir "/usr/local/lib/erlang")
(setq tools-ver "2.6.6.2")
(setq load-path (cons (concat erlang-root-dir "/lib/tools-" tools-ver "/emacs")
		      load-path))
(setq exec-path (cons (concat erlang-root-dir "/bin")
		      exec-path))
(require 'erlang-start)
