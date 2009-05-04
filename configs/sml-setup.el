;; SML catcher
(require 'compile)
(add-to-list 'compilation-error-regexp-alist
             '("^Error: \\([^\t\n]*\\) \\([0-9]+\\)\\.\\([0-9]+\\)\\.$"
               1 2 3))

(load "sml-mode-startup")