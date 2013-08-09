;;; global.el --- Global configuration

;; Disable lots and lots of fluff from the terminal. I don't want it.
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)
  (blink-cursor-mode -1)
  (scroll-bar-mode -1))

(mouse-wheel-mode t)
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq visible-bell t)
(setq-default echo-keystrokes 0.1)
(setq-default font-lock-maximum-decoration t)
(setq-default inhibit-startup-message t)
(setq-default global-font-lock-mode t)
(setq-default delete-by-moving-to-trash t)
(setq-default shift-select-mode nil)
(setq-default default-tab-width 8)
(setq-default indent-tabs-mode nil)
(setq-default require-final-newline t) 
(setq tab-always-indent 'complete)
(setq-default line-number-mode t)
(setq-default column-number-mode t)
(setq-default size-indication-mode t)
(setq-default mouse-yank-at-point t)
(setq-default ediff-merge-split-window-functon 'split-window-vertically)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
(setq-default mode-compile-always-save-buffer-p t)
(setq-default compilation-window-height 12)
(setq-default x-select-enable-clipboard t)
(setq-default oddmuse-directory (concat emacs-config-dir "oddmuse"))
(setq-default xterm-mouse-mode t)

(setf split-height-threshold 40
      split-width-threshold 160)

(setq scroll-preserve-screen-position 1)

(electric-indent-mode t)
(electric-layout-mode t)

(defun dont-kill-emacs ()
      (interactive)
      (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))
(global-set-key "\C-x\C-c" 'dont-kill-emacs)

(set-language-environment "UTF-8")
(auto-compression-mode t)
(auto-insert-mode t)
(defvar next-line-add-newlines nil)
(quietly-read-abbrev-file)

(autoload 'goto-last-change
  "goto-last-change" "Set point to the position of the last change." t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(defalias 'yes-or-no-p 'y-or-n-p)
(random t)

(setq diff-switches "-u")

;; Marks
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order. This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; Keybindings
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-z" 'undo)
(global-set-key (kbd "M-/") 'hippie-expand)
;; These two is because the M-` is now taken by Unity, and there
;; were no intelligent binding on ', so I just took that one.
(global-set-key (kbd "M-'") 'jump-to-mark)
(global-set-key (kbd "C-'") 'push-mark-no-activate)
(global-set-key [f12] 'compile)
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key [f11] 'fullscreen-toggle)
(global-set-key [f10] 'magit-status)
(global-set-key (kbd "C-x C-i") 'imenu)
(global-set-key (kbd "C-c e") 'esk-eval-and-replace)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c r") 'vc-git-grep)

(global-set-key (kbd "C-c |") 'align)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c n") 'org-iswitchb)
(global-set-key (kbd "C-c s") 'org-capture)

(global-set-key (kbd "C-=") 'er/expand-region)

;;; global.el ends here

(setq sentence-end-double-space nil
      uniquify-buffer-name-style 'forward
      ediff-window-setup-function 'ediff-setup-windows-plain
      oddmuse-directory "~/.emacs.d/oddmuse"
      save-place-file "~/.emacs.d/places"
      backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups")))
      diff-switches "-u")

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(set-default 'indicate-buffer-boundaries 'left)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

(random t) ;; Seed the random-number generator

;; Get around the emacswiki spam protection
(eval-after-load 'oddmuse
  (add-hook 'oddmuse-mode-hook
            (lambda ()
              (unless (string-match "question" oddmuse-post)
                (setq oddmuse-post (concat "uihnscuskc=1;" oddmuse-post))))))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

