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
