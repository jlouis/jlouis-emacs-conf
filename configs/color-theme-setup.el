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




;;; Author: Ethan Schoonover, Solarized; Greg Pfeil, Emacs adaptation
;;; URL: http://ethanschoonover.com/solarized

;;; This file is not (YET) part of GNU Emacs.

;;; # Usage

;;; 1. Install the color-theme package
;;;   (http://www.emacswiki.org/cgi-bin/wiki/ColorTheme)
;;; 2. Load this file
;;; 3. M-x color-theme-solarized-[dark|light]

(eval-when-compile
  (require 'color-theme))

;; `((normal (:foreground ,base0 :background ,base03))
;;   (comment (:foreground ,base01 :italic t))
;;   ((constant string character number boolean float) (:foreground ,cyan))
;;   ((identifier function directory) (:foreground ,blue))
;;   ((statement conditional repeat label operator keyword exception)
;;    (:foreground ,green))
;;   ((pre-proc include define macro pre-condit) (:foreground orange))
;;   ((type storage-class structure typedef (:foreground yellow)))
;;   ((special special-char tag delimiter special-comment debug)
;;    (:foreground ,red))
;;   (underlined (:foreground ,violet))
;;   (error (:foreground ,red :bold t))
;;   (todo (:foreground ,magenta :bold t))
;;   (special-key (:foreground ,base02))
;;   (non-text (:foreground ,base02 :bold t))
;;   ())

(defun color-theme-solarized (mode)
  "Color theme by Ethan Schoonover, created 2011-03-24.
Ported to Emacs by Greg Pfeil, http://ethanschoonover.com/solarized."
  (interactive "Slight or dark? ")
  (let ((base03  "#002b36")
        (base02  "#073642")
        (base01  "#586e75")
        (base00  "#657b83")
        (base0   "#839496")
        (base1   "#93a1a1")
        (base2   "#eee8d5")
        (base3   "#fdf6e3")
        (yellow  "#b58900")
        (orange  "#cb4b16")
        (red     "#dc322f")
        (magenta "#d33682")
        (violet  "#6c71c4")
        (blue    "#268bd2")
        (cyan    "#2aa198")
        (green   "#859900"))
    (when (eq 'light mode)
      (rotatef base03 base3)
      (rotatef base02 base2)
      (rotatef base01 base1)
      (rotatef base00 base0))
    (color-theme-install
     `(color-theme-solarized
       ((foreground-color . ,base0)
        (background-color . ,base03)
        (background-mode . ,mode)
        (cursor-color . ,base0))
       ;; basic
       (default ((t (:foreground ,base0))))
       (cursor ((t (:foreground ,base0 :background ,base03 :inverse-video t))))
       (escape-glyph-face ((t (:foreground ,red))))
       (fringe ((t (:foreground ,base01 :background ,base02))))
       (header-line ((t (:foreground ,base0 :background ,base2))))
       (highlight ((t (:background ,base02))))
       (isearch ((t (:foreground ,yellow :inverse-video t))))
       (menu ((t (:foreground ,base0 :background ,base02))))
       (minibuffer-prompt ((t (:foreground ,blue))))
       (mode-line
        ((t (:foreground ,base1 :background ,base02
                         :box (:line-width 1 :color ,base1)))))
       (mode-line-buffer-id ((t (:foreground ,base1))))
       (mode-line-inactive
        ((t (:foreground ,base0  :background ,base02
                         :box (:line-width 1 :color ,base02)))))
       (region ((t (:background ,base01))))
       (secondary-selection ((t (:background ,base02))))
       (trailing-whitespace ((t (:foreground ,red :inverse-video t))))
       (vertical-border ((t (:foreground ,base0))))
       ;; compilation
       (compilation-info ((t (:forground ,green :bold t))))
       (compilation-warning ((t (:foreground ,orange :bold t))))
       ;; customize
       (custom-button
        ((t (:background ,base02 :box (:line-width 2 :style released-button)))))
       (custom-button-mouse ((t (:inherit custom-button :foreground ,base1))))
       (custom-button-pressed
        ((t (:inherit custom-button-mouse
                      :box (:line-width 2 :style pressed-button)))))
       (custom-comment-tag ((t (:background ,base02))))
       (custom-comment-tag ((t (:background ,base02))))
       (custom-documentation ((t (:inherit default))))
       (custom-group-tag ((t (:foreground ,orange :bold t))))
       (custom-link ((t (:foreground ,violet))))
       (custom-state ((t (:foreground ,green))))
       (custom-variable-tag ((t (:foreground ,orange :bold t))))
       ;; diff
       (diff-added ((t (:foreground ,green :inverse-video t))))
       (diff-changed ((t (:foreground ,yellow :inverse-video t))))
       (diff-removed ((t (:foreground ,red :inverse-video t))))
       ;; emacs-wiki
       (emacs-wiki-bad-link-face ((t (:foreground ,red :underline t))))
       (emacs-wiki-link-face ((t (:foreground ,blue :underline t))))
       (emacs-wiki-verbatim-face ((t (:foreground ,base00 :underline t))))
       ;; font-lock
       (font-lock-builtin-face ((t (:foreground ,green))))
       (font-lock-comment-face ((t (:foreground ,base01 :italic t))))
       (font-lock-constant-face ((t (:foreground ,cyan))))
       (font-lock-function-name-face ((t (:foreground ,blue))))
       (font-lock-keyword-face ((t (:foreground ,green))))
       (font-lock-string-face ((t (:foreground ,cyan))))
       (font-lock-type-face ((t (:foregound ,yellow))))
       (font-lock-variable-name-face ((t (:foregound ,blue))))
       (font-lock-warning-face ((t (:foreground ,red :bold t))))
       ;; info
       (info-xref ((t (:foreground ,blue :underline t))))
       (info-xref-visited ((t (:inherit info-xref :foreground ,magenta))))
       ;; org
       (org-hide ((t (:foreground ,base03))))
       (org-todo ((t (:foreground ,red :bold t))))
       (org-done ((t (:foreground ,green :bold t))))
       ;; show-paren
       (show-paren-match-face ((t (:background ,cyan :foreground ,base3))))
       (show-paren-mismatch-face ((t (:background ,red :foreground ,base3))))))))

(defun color-theme-solarized-dark ()
  (interactive)
  (color-theme-solarized 'dark))

(defun color-theme-solarized-light ()
  (interactive)
  (color-theme-solarized 'light))

(provide 'color-theme-solarized)

(eval-after-load "color-theme"
  '(progn
     (color-theme-solarized-light)))
