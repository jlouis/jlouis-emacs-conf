;; init.el --- Initialization file for my Emacs setup

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

;; Get up and running, set custom to something else, and set up load paths
;; for the rest of the system. Then proceed to load each configuration file
;; for each module installed in emacs via load-cfg-files.
;;
;; TODO:
;;  ffap support
;;
;;; Code:

;;; Load path setup

(setq dotfiles-dir (file-name-directory
		    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/site/"))
(add-to-list 'load-path (concat dotfiles-dir "/auto-install/"))

;;; TODO: More to add here.

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file (concat dotfiles-dir "custom.el"))
(setq abbrev-file-name (concat dotfiles-dir "abbrev_defs"))
(defconst *emacs-config-dir* (concat dotfiles-dir "/configs/" ""))

(defun load-config-files (files)
  (dolist (f files)
    (load (expand-file-name
	   (concat *emacs-config-dir* f)))
    (message "Loaded config file: %s" file)))

;; From the emacs starter kit.

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'auto-install)

(load-config-files  '("defuns"
		      "anything-setup"
		      "color-theme-setup"
		      "c-mode-setup"
		      "diredx-setup"
		      "epa-setup"
		      "eshell-setup"
		      ;"gist-setup"
		      "global"
		      "hippie-expand-setup"
		      "ido-setup"
		      ;"js2-setup"
		      ;"kill-ring-search-setup"
		      ;"magit-setup"
		      "midnight-setup"
		      "nxml-setup"
		      "org-setup"
		      "proof-general-setup"
		      ;"sml-setup"
		      "tex-code"
		      "tramp-setup"
		      "tuareg-setup"
		      ;"twelf-setup"
		      "twit-setup"
		      "haskell-mode-setup"
		      "go-mode-setup"
		      "uniquify-setup"))

(load custom-file 'noerror)
(server-start)
;;; init.el ends here
