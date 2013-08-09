(require 'org-install)

;; Mouse control
(require 'org-mouse)

;; Extension handling
(add-to-list 'auto-mode-alist '("\\.org$'" . org-mode))

(setq org-directory "~/Dropbox/org")

(setq org-default-notes-file (concat org-directory "/todo.org"))

(setq org-todo-keywords '((type "TODO" "NEXT" "WAITING" "DONE")))

(setq org-agenda-files '("~/Dropbox/org"))


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
        ("BUY" . ?b)
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

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline
                           (concat org-directory "/todo.org") "Tasks") 
	 "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry
         (file+datetree (concat org-directory
                                "/journal.org"))
	 "* %?\nEntered on %U\n  %i\n  %a")
	("k" "Kwotes" item
         (file+headline (concat org-directory "/quotes.org")
                        "Unsorted Quotes"))))

(setq org-return-follows-link t)
(setq org-hide-leading-stars t) 
(setf org-tags-column -65)
(setf org-special-ctrl-a/e t)

(setq org-log-done t)
(setq org-deadline-warning-days 14)
(setq org-fontify-emphasized-text t)
(setq org-fontify-done-headline t)
(setq org-agenda-include-all-todo nil)
(setq org-export-with-section-numbers nil)
(setq org-export-with-toc nil)
(setq org-adapt-indentation nil)

;; widen category field a little
(setq org-agenda-prefix-format "  %-17:c%?-12t% s") 
