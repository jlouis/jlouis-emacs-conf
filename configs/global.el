;;; global.el --- Global configuration

;; Copyright (C) 2009  Jesper Louis Andersen

;; Author: Jesper Louis Andersen <jesper.louis.andersen@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(setq inhibit-startup-message t)
(setq default-tab-width 8)
(setq indent-tabs-mode nil)
(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(line-number-mode t)
(setq mouse-yank-at-point t)

(setq scroll-step 1)
(setq ediff-merge-split-window-functon 'split-window-vertically)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq mode-compile-always-save-buffer-p t)
(setq compilation-window-height 12)

(auto-compression-mode t)
(auto-insert-mode t)
(defvar next-line-add-newlines nil)

(quietly-read-abbrev-file)
(server-start)

(autoload 'goto-last-change
  "goto-last-change" "Set point to the position of the last change." t)

(defun fullscreen-toggle ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

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

;; Keybindings
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-z" 'undo)
(global-set-key [f12] 'compile)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key [f11] 'fullscreen-toggle)

;;; global.el ends here
