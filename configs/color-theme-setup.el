; Color Theme

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

(require 'color-theme)
(color-theme-initialize)
(eval-after-load "color-theme"
  '(progn
     (color-theme-greiner)))

(defun color-theme-inkpot ()
      "Color theme based on the Inkpot theme. Ported and tweaked by Per Vognsen."
      (interactive)
      (color-theme-install
       '(color-theme-inkpot
         ((foreground-color . "#cfbfad")
          (background-color . "#1e1e27")
          (border-color . "#3e3e5e")
          (cursor-color . "#404040")
          (background-mode . dark))
         (region ((t (:background "#404040"))))
         (highlight ((t (:background "#404040"))))
         (fringe ((t (:background "#16161b"))))
         (show-paren-match-face ((t (:background "#606060"))))
         (isearch ((t (:bold t :foreground "#303030" :background "#cd8b60"))))
         (modeline ((t (:bold t :foreground "#b9b9b9" :background "#3e3e5e"))))
         (modeline-inactive ((t (:foreground "#708090" :background "#3e3e5e"))))
         (modeline-buffer-id ((t (:bold t :foreground "#b9b9b9" :background "#3e3e5e"))))
         (minibuffer-prompt ((t (:bold t :foreground "#708090"))))
         (font-lock-builtin-face ((t (:foreground "#c080d0"))))
         (font-lock-comment-face ((t (:foreground "#708090")))) ; original inkpot: #cd8b00
         (font-lock-constant-face ((t (:foreground "#506dbd"))))
         (font-lock-doc-face ((t (:foreground "#cd8b00"))))
         (font-lock-function-name-face ((t (:foreground "#87cefa"))))
         (font-lock-keyword-face ((t (:bold t :foreground "#c080d0"))))
         (font-lock-preprocessor-face ((t (:foreground "309090"))))
         (font-lock-reference-face ((t (:bold t :foreground "#808bed"))))
         (font-lock-string-face ((t (:foreground "#ffcd8b" :background "#404040"))))
         (font-lock-type-face ((t (:foreground "#ff8bff"))))
         (font-lock-variable-name-face ((t nil)))
         (font-lock-warning-face ((t (:foreground "#ffffff" :background "#ff0000")))))))
