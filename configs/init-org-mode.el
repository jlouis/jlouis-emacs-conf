(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-default-notes-file "~/org/notes.org")
(setq org-capture-templates
      '(("t" "Task" entry (file+headline "~/org/notes.org" "Tasks")
         "* TODO %?\n  %i\n  %a")))
