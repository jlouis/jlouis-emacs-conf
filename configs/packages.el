;; Set up use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package company
  :ensure t

  :config
  (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  
  (setq company-tooltip-flip-when-above t)
  (global-company-mode 1)
  (define-key company-active-map (kbd "C-w") 'backward-kill-word) )

(use-package elm-mode
  :ensure t)

(use-package expand-region
  :ensure t)

(use-package graphql-mode
  :ensure t)

(use-package iedit
  :ensure t)

(use-package idris-mode
  :ensure t)

(use-package json
  :ensure t)

(use-package js2-mode
  :ensure t

  :config
  (autoload 'js2-mode "js2" nil t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)) )

(use-package go-mode
  :ensure t
  ;; go get -u github.com/x/tools/cmd/...
  ;; go get -u github.com/rogpeppe/godef/...
  ;; go get -u github.com/nsf/gocode
  ;; go get -u golang.org/x/tools/cmd/goimports
  ;; go get -u golang.org/x/tools/cmd/guru
  ;; go get -u github.com/dougm/goflymake

  :config
  (setq-local compile-command "go build -v && go test -v && go vet")
  (set
   (make-local-variable 'company-backends)
   '(company-go))
  (subword-mode +1)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  :bind
  ("C-c C-c" . compile)
  ("M-." . godef-jump))

(use-package go-eldoc
  :ensure t

  :config
  (go-eldoc-setup))

(use-package magit
  :ensure t

  :bind
  ("C-c g" . magit-status))

(use-package magit-popup
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package material-theme
  :ensure t)

(use-package org
  :ensure t

  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c n" . org-iswitchb)
  ("C-c s" . org-capture)
  
  :config
  (add-to-list 'auto-mode-alist '("\\.org$'" . org-mode))
  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org"))
  (setq org-default-notes-file (concat org-directory "/todo.org"))

  (setq org-todo-keywords '((type "TODO" "NEXT" "WAITING" "DONE")))

  ;; #+SEQ_TODO: TODO | DONE
  ;; #+SEQ_TODO: REPORT BUG KNOWNCAUSE | FIXED 
  ;; #+SEQ_TODO: | CANCELLED

  (setq org-agenda-custom-commands
        '(("w" todo "WAITING" nil)
          ("n" todo "NEXT" nil)
          ("d" "Agenda + Next Actions" ((agenda) (todo "NEXT")))))

  (setq org-archive-location "%s_archive::")

  (setq org-reverse-note-order t)         ; Dump notes at the beginning by def.
  (setq org-default-notes-file "~/org/remember.org")
  (setq remember-annotation-functions '(org-remember-annotation))
  (setq remember-handler-functions '(org-remember-handler))

  (setq org-refile-use-outline-path 'file)
  (setq org-refile-targets '((org-agenda-files :level . 1)))

  (setq org-capture-templates
        '(("i" "Inbox" entry (file+headline "inbox.org" "Tasks") 
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("k" "Kwotes" item (file+headline "quotes.org")
           "Unsorted Quotes")))

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
  (setq org-agenda-prefix-format "  %-17:c%?-12t% s") )

(use-package powerline
  :ensure t

  :config
  (powerline-default-theme) )

(use-package smex
  :ensure t

  :config
  (setq smex-save-file
      (concat user-emacs-directory ".smex-items"))
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "C-x C-m") 'smex)
  (global-set-key (kbd "C-c C-m") 'smex) )

(use-package tuareg
  :ensure t)

(use-package merlin
  :ensure t
  :after (tuareg)

  :config
  (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      ;; Register Merlin
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
      (autoload 'merlin-mode "merlin" nil t nil)
      ;; Automatically start it in OCaml buffers
      (add-hook 'tuareg-mode-hook 'merlin-mode t)
      (add-hook 'caml-mode-hook 'merlin-mode t)
      ;; Use opam switch to lookup ocamlmerlin binary
      (setq merlin-command 'opam))) )

(provide 'packages)

