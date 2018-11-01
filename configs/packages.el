;;; package --- Installation of packages via use-package
;;; Commentary:
;;;  Kept separate as to not clutter the main init files
;;; Code:
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Things we want to install in the future:
;; nix-mode nix-sandbox nix-buffer
;; restclient
;; edts (Erlang)
;;

(eval-when-compile
  (require 'use-package))

(use-package emacs
  :delight
  (visual-line-mode))

(use-package server
  :unless (or noninteractive)
  :no-require
  :hook (after-init . server-start))

(use-package material-theme
  :ensure t)

;; ------------------------------------------------------------
;; Window Navigation

;; (use-package flx-ido
;;   :ensure t

;;   :config
;;   (ido-mode 1)
;;   (ido-everywhere 1)
;;   (flx-ido-mode 1)
;;   ;; disable ido faces to see flx highlights.
;;   (setq ido-enable-flex-matching t)
;;   (setq ido-use-faces nil))

;; (use-package ido-completing-read+
;;   :ensure t
;;   :after (flx-ido)

;;   :config
;;   (ido-ubiquitous-mode 1))

;; This whole thing can be configured way more:
;; https://oremacs.com/swiper/
;;

(use-package recentf
  :config
  (recentf-mode 1))

(use-package tramp
  :config
  (setq tramp-default-method "sshx")

  (add-to-list 'tramp-default-method-alist
	       '("\\`localhost\\'" "\\`root\\'" "su"))
  (add-to-list 'tramp-default-method-alist '("" "jlouis" "ssh"))
  (add-to-list 'tramp-default-method-alist '("" "jla" "ssh"))
  (add-to-list 'tramp-default-method-alist '("" "root" "ssh"))

  (tramp-set-completion-function "ssh"
			         '((tramp-parse-sconfig "/etc/ssh_config")
				   (tramp-parse-sconfig "~/.ssh/config"))))

(use-package ffap
  :bind ("C-c v" . ffap))

(use-package smartparens-config
  :ensure smartparens
  :delight

  :commands smartparens-mode)

(use-package flx
  :ensure t)

(use-package avy
  :ensure t
  :bind* ("C-." . avy-goto-char-timer)
  :config
  (avy-setup-default))

(use-package avy-zap
  :ensure t
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package ivy
  :ensure t
  :delight

  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package swiper
  :ensure t
  :after (ivy)

  :bind
  ("C-s" . swiper))

(use-package counsel
  :ensure t
  :after (ivy)

  :bind
  ("C-c r" . counsel-git-grep)
  ("C-c f" . counsel-git)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x C-r" . counsel-recentf))

(use-package perspective
  :ensure t
  :config
  (persp-mode 1))

;; ------------------------------------------------------------
;; Generally useful modes

(use-package eshell
  :commands (eshell eshell-command)
  )

(use-package powerline
  :ensure t

  :config
  (powerline-default-theme) )

(use-package abbrev
  :delight)

(use-package autorevert
  :delight auto-revert-mode)

(use-package smex
  :ensure t

  :config
  (setq smex-save-file
      (concat user-emacs-directory ".smex-items")))

(use-package hippie-expand
  :init
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill))
  :bind
  ("M-/" . hippie-expand))

(use-package fill-column-indicator
  :ensure t)

(use-package whitespace-cleanup-mode
  :ensure t
  :delight

  :hook (after-init-hook . global-whitespace-cleanup-mode))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

(use-package delight
  :ensure t

  :config
  (delight '((auto-fill-function " AF" t))))

(use-package eldoc
  :delight eldoc-mode)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

(use-package yasnippet
  :ensure t
  :delight yas-minor-mode

  :config
  (progn
    (yas-global-mode 1)
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (defun yas/goto-end-of-active-field ()
      (interactive)
      (let* ((snippet (car (yas-active-snippets)))
             (position (yas--field-end (yas--snippet-active-field snippet))))
        (if (= (point) position)
            (move-end-of-line 1)
          (goto-char position))))

    (defun yas/goto-start-of-active-field ()
      (interactive)
      (let* ((snippet (car (yas-active-snippets)))
             (position (yas--field-start (yas--snippet-active-field snippet))))
        (if (= (point) position)
            (move-beginning-of-line 1)
          (goto-char position))))

    (define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
    (define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)
    ;;(setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))
    ;;(setq yas-verbosity 1)
    (setq yas-wrap-around-region t)))

(use-package company
  :ensure t
  :delight

  :config
  (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)

  (setq company-tooltip-flip-when-above t)
  (global-company-mode 1)
  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "TAB") nil)
  (define-key company-active-map (kbd "C-w") 'backward-kill-word) )

(use-package expand-region
  :ensure t)

(use-package iedit
  :ensure t)

(use-package flycheck
  :ensure t
  :delight

  :init (global-flycheck-mode)
  :config

  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(use-package flycheck-color-mode-line
  :ensure t
  :delight

  :after (flycheck)
  :hook (flycheck-mode-hook . flycheck-color-mode-line-mode))

(use-package magit
  :ensure t
  :after (diff-hl)

  :bind
  ("C-c g" . magit-status)

  :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :config
  (setq-default magit-diff-refine-hunk 1))

(use-package magit-todos
  :ensure t
  :after (magit)

  :config
  (magit-todos-mode))

(use-package magit-popup
  :ensure t)

(use-package multi-line
  :ensure t

  :bind
  ("C-c d" . multi-line))

(use-package restclient
  :ensure t)

;; ------------------------------------------------------------
;; File Modes

(use-package dockerfile-mode
  :ensure t)

(use-package elm-mode
  :ensure t)

(use-package ess
  :ensure t)

(use-package graphql-mode
  :ensure t

  :config
  (subword-mode 1))

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

  :hook (before-save-hook . gofmt-before-save)
  :config
  (setq-local compile-command "go build -v && go test -v && go vet")
  (set
   (make-local-variable 'company-backends)
   '(company-go))
  (subword-mode 1)
  (setq gofmt-command "goimports"))

(use-package go-eldoc
  :ensure t

  :config
  (go-eldoc-setup))

(use-package markdown-mode
  :ensure t)

(use-package tuareg
  :ensure t)

(use-package erlang-start
  :config
  (add-hook 'erlang-mode-hook #'smartparens-mode)
  )

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

;; ------------------------------------------------------------
;; Org
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

(provide 'packages)
;;; packages.el ends here
