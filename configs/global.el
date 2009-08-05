;;; global.el --- Global configuration

; Copyright (c) 2009, Jesper Louis Andersen <jesper.louis.andersen@gmail.com>
; All rights reserved.

; Redistribution and use in source and binary forms, with or
; without modification, are permitted provided that the following
; conditions are met:

; * Redistributions of source code must retain the above
;   copyright notice, this list of conditions and the following
;   disclaimer.

; * Redistributions in binary form must reproduce the above
;   copyright notice, this list of conditions and the following
;   disclaimer in the documentation and/or other materials
;   provided with the distribution.

; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (blink-cursor-mode -1))

;(add-hook 'before-make-frame-hook 'turn-off-tool-bar)

(mouse-wheel-mode t)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq visible-bell t
      echo-keystrokes 0.1
      font-lock-maximum-decoration t
      inhibit-startup-message t
      global-font-lock-mode t
      delete-by-moving-to-trash t
      shift-select-mode nil
      default-tab-width 8
      indent-tabs-mode nil
      line-number-mode t
      mouse-yank-at-point t
      scroll-step 1
      ediff-merge-split-window-functon 'split-window-vertically
      ediff-window-setup-function 'ediff-setup-windows-plain
      mode-compile-always-save-buffer-p t
      compilation-window-height 12
      whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100
      oddmuse-directory (concat dotfiles-dir "oddmuse")
      xterm-mouse-mode t
      save-place-file (concat dotfiles-dir "places"))

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


;; Keybindings
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-z" 'undo)
(global-set-key [f12] 'compile)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key [f11] 'fullscreen-toggle)
(global-set-key [f10] 'magit-status)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;;; global.el ends here
