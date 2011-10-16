(require 'anything-config)
(require 'anything)

(defun my-anything ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-etags-select
     anything-c-source-fixme
     anything-c-source-kill-ring     
     anything-c-source-buffers
     anything-c-source-recentf
     anything-c-source-ffap-guesser
     anything-c-source-info-pages
     anything-c-source-info-elisp
     anything-c-source-man-pages
     anything-c-source-locate
     anything-c-source-emacs-commands)
   " *my-anything*"))

(global-set-key (kbd "M-o") 'my-anything)

