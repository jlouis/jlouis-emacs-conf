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

(defconst use-backup-dir t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs.local/backups/")))
      version-control t
      kept-new-versions 16
      kept-old-versions 2
      delete-old-versions t
      backup-by-copying-when-linked t)

(defun w-keys ()
  (interactive)
  (find-file "/home/jlouis/Dropbox/TODO/work-keys.org.gpg"))

(defun w-gtd ()
  (interactive)
  (find-file "/home/jlouis/Dropbox/TODO/work-todo.org"))

(defun keys ()
  (interactive)
  (find-file "/home/jlouis/Dropbox/TODO/keys.org.gpg"))

(defun gtd ()
  (interactive)
  (find-file "/home/jlouis/Dropbox/TODO/todo.org"))

