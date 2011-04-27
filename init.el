;; init.el --- Initialization file for my Emacs setup
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

(setq emacs-config-dir (file-name-directory
		    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path emacs-config-dir)
(add-to-list 'load-path (concat emacs-config-dir "/site/"))

;;; TODO: More to add here.

(setq autoload-file (concat emacs-config-dir "loaddefs.el"))
(setq site-root (concat emacs-config-dir "/site/"))
(setq custom-file (concat emacs-config-dir "custom.el"))
(setq package-user-dir (concat emacs-config-dir "elpa"))
(setq abbrev-file-name (concat emacs-config-dir "abbrev_defs"))
(defconst *emacs-config-dir* (concat emacs-config-dir "/configs/" ""))

;;; package.el configuration
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))

(defun load-config-files (files)
  (dolist (f files)
    (load (expand-file-name
	   (concat *emacs-config-dir* f)))
    (message "Loaded config file: %s" file)))

(load (concat emacs-config-dir "elpa/package.el"))
(package-initialize)

;; From the emacs starter kit.

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)

(load-config-files  '("defuns" ;; Has to go first
		      "global" ;; Has to go second
		      "color-theme-setup"
		      "c-mode-setup"
		      "diredx-setup"
		      "erlang-setup"
		      "agda2-setup"
		      "epa-setup"
		      "eshell-setup"
		      "graphviz-mode-setup"
		      "magit-setup"
		      "hippie-expand-setup"
		      "ido-setup"
		      "midnight-setup"
		      "nxml-setup"
		      "org-setup"
		      "proof-general-setup"
		      "recentf-setup"
		      "tex-code"
		      "tramp-setup"
		      "tuareg-setup"
		      "haskell-mode-setup"
		      "uniquify-setup"))

(load custom-file 'noerror)
;;; init.el ends here
(put 'downcase-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
