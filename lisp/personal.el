;;; global.el --- Global configuration

(require 'thingatpt)
(require 'imenu)

;;; Function definitions
(defun esk-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun esk-insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun indent-whole-buffer ()
  "Indent the whole buffer and make it nice to work on"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun replace-latex-danish-characters ()
  "Replace Latex chars with danish chars"
  (interactive)
  (let ((start (point-min))
        (end (point-max)))
    (query-replace "\\aa{}" "å" nil start end)
    (query-replace "\\AA{}" "Å" nil start end)
    (query-replace "\\o{}"  "ø" nil start end)
    (query-replace "\\O{}"  "Ø" nil start end)
    (query-replace "\\ae{}" "æ" nil start end)
    (query-replace "\\AE{}" "Æ" nil start end)))

(defun view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory emacs-config-dir 0)
  (byte-recompile-directory (concat emacs-config-dir "configs/" 0))
  (byte-recompile-directory (concat emacs-config-dir "elpa-to-submit/" 0)))



;; Backups
(defconst use-backup-dir t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs.local/backups/")))
      version-control t
      kept-new-versions 16
      kept-old-versions 2
      delete-old-versions t
      backup-by-copying-when-linked t)

;; Disable lots and lots of fluff from the terminal. I don't want it.
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)
  (blink-cursor-mode -1)
  (scroll-bar-mode -1))

;; Do not really pop up dialog boxes which doesn't really work
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

(mouse-wheel-mode t)
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(setq-default echo-keystrokes 0.1)
(setq-default inhibit-startup-message t)
(setq-default delete-by-moving-to-trash t)
(setq-default shift-select-mode nil)
(setq-default default-tab-width 4)
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
(setq-default compilation-auto-jump-to-first-error t)
(setq-default compilation-window-height 12)
(setq-default x-select-enable-clipboard t)
(setq-default xterm-mouse-mode t)

(setf split-height-threshold 40
      split-width-threshold 160)

(setq scroll-preserve-screen-position 1)

(electric-indent-mode t)
(electric-layout-mode t)

(defun dont-kill-emacs ()
  "I always accidenally hit the exit Emacs keybinding. This function handles that problem."
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

(add-hook 'text-mode-hook
          'turn-on-auto-fill)
(add-hook 'text-mode-hook
          'turn-on-flyspell)

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
;; These two is because the M-` is now taken by Unity, and there
;; were no intelligent binding on ', so I just took that one.
(global-set-key (kbd "M-'") 'jump-to-mark)
(global-set-key (kbd "C-'") 'push-mark-no-activate)
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key (kbd "C-x C-i") 'imenu)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c j") 'join-line)

(global-set-key (kbd "C-c |") 'align)
(global-set-key (kbd "C-c m") (lambda () (interactive)
                                (erlang-man-function (current-word))))
(global-set-key (kbd "C-=") 'er/expand-region)

(setq sentence-end-double-space nil
      uniquify-buffer-name-style 'forward
      ediff-window-setup-function 'ediff-setup-windows-plain
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

(defalias 'auto-tail-revert-mode 'tail-mode)

(provide 'personal)
