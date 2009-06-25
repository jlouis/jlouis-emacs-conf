;;; init.el --- Initialization file for my Emacs setup

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
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(defconst *emacs-config-dir* "~/.emacs.d/configs/" "")
(defun load-cfg-files (files)
  (dolist (f files)
    (load (expand-file-name
	   (concat *emacs-config-dir* f)))
    (message "Loaded config file: %s" file)))

(defun extend-load-path (path)
  (setq load-path (cons (expand-file-name path) load-path)))

(extend-load-path "~/.emacs.d/vendor")
(extend-load-path "~/.emacs.d")
;; Get vendor stuff loaded as well
(progn (cd "~/.emacs.d/vendor")
       (normal-top-level-add-subdirs-to-load-path))

(load-cfg-files '("global"
		  "c-mode-setup"
		  ;"color-theme-setup"
		  "uniquify-setup"
		  "midnight-setup"
		  "diredx-setup"
		  "epa-setup"
		  "hippie-expand-setup"
		  "ido-setup"
		  "js2-setup"
		  "nxml-setup"
		  "org-setup"
		  "proof-general-setup"
		  "sml-setup"
		  "tex-code"
		  "tramp-setup"
		  "tuareg-setup"
		  "twelf-setup"
		  "dpaste-setup"
		  "gist-setup"
		  "magit-setup"
		  "browse-kill-ring-setup"))

;; Get back to homedir
(cd "~")

;;; init.el ends here
