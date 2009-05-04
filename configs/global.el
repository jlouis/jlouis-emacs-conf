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

;; Keybindings
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-z" 'undo)
(global-set-key [f12] 'compile)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)


;;; global.el ends here
