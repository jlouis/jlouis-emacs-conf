(require 'anything-config)
(require 'anything)
(setq anything-idle-delay 0.3)
(setq anything-input-idle-delay 0)
(setq anything-candidate-number-limit 100)
(setq anything-persistent-action-use-special-display t)
(require 'anything-etags)
(require 'anything-grep)

(defun anything-etags-maybe-at-point ()
  (interactive)
  (anything-etags-select-from-here))


(setq anything-sources
      '(anything-c-source-buffers
	anything-c-source-buffer-not-found
	anything-c-source-etags-select
	anything-c-source-files-in-current-dir+
	anything-c-source-find-files
	anything-c-source-ffap-guesser
        anything-c-source-file-name-history
	anything-c-source-kill-ring
	anything-c-source-fixme
	anything-c-source-man-pages
	anything-c-source-locate
        anything-c-source-emacs-commands
	anything-c-source-complex-command-history
	))



