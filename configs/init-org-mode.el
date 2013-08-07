(require 'org-install)
(require 'org)
(require 'remember)

;; Mouse control
(require 'org-mouse)

;; Extension handling
(add-to-list 'auto-mode-alist '("\\.org$'" . org-mode))

(defvar org-notes-file "~/Dropbox/notes.org")

(defun jl/notes ()
  "Open the Notes organization file"
  (interactive)
  (find-file org-notes-file))

(setq org-todo-keywords '((type "TODO" "NEXT" "WAITING" "DONE")))

(defvar org-other-notes)
(setf org-other-notes
      (list "~/Dropbox/issuu.org"))

(setf org-agenda-files (cons org-notes-file org-other-notes))

;; #+SEQ_TODO: TODO | DONE
;; #+SEQ_TODO: REPORT BUG KNOWNCAUSE | FIXED 
;; #+SEQ_TODO: | CANCELLED

(setq org-agenda-custom-commands
      '(("w" todo "WAITING" nil)
        ("n" todo "NEXT" nil)
        ("d" "Agenda + Next Actions" ((agenda) (todo "NEXT")))))

(setq org-tag-alist
      '(("STUDIO" . ?s)
        ("COMPUTER" . ?c)
        ("MAIL" . ?m)
        ("HOME" . ?h)
        ("WORK" . ?w)
        ("READING" . ?r)))

(setq org-archive-location "%s_archive::")

(setq org-reverse-note-order t)         ; Dump notes at the beginning by
                                        ; default
(setq org-default-notes-file "~/Dropbox/remember.org")
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

(setq org-remember-templates
      '((?t "* TODO %?\n  %i\n  %a" "~/Dropbox/todo.org")
        (?j "* %U %?\n\n  %i\n  %a" "~/Dropbox/journal.org")
        (?i "* %^{Title}\n  %i\n  %a" "~/Dropbox/remember.org" "New Ideas")))

(setq org-return-follows-link t)
(setq org-hide-leading-stars t) 
(setf org-tags-column -65)
(setf org-special-ctrl-a/e t)

(setq org-log-done t)
(setq org-deadline-warning-days 14)
(setq org-fontify-emphasized-text t)
(setq org-fontify-done-headline t)
(setq org-agenda-include-all-todo nil)
(setq org-directory "~/Dropbox")
(setq org-export-with-section-numbers nil)
(setq org-export-with-toc nil)
(setq org-adapt-indentation nil)

;; widen category field a little
(setq org-agenda-prefix-format "  %-17:c%?-12t% s") 

(setq org-capture-templates
      '(("t" "Task" entry (file+headline "~/org/notes.org" "Tasks")
         "* TODO %?\n  %i\n  %a")))
