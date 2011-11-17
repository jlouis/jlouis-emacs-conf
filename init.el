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
(setq disabled-command-function nil)

(setq emacs-config-dir (file-name-directory
                        (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path emacs-config-dir)
(add-to-list 'load-path (concat emacs-config-dir "/site/"))

(setq autoload-file (concat emacs-config-dir "loaddefs.el"))
(setq site-root (concat emacs-config-dir "/site/"))
(setq custom-file (concat emacs-config-dir "custom.el"))
(setq package-user-dir (concat emacs-config-dir "elpa"))
(setq abbrev-file-name (concat emacs-config-dir "abbrev_defs"))
(defconst *emacs-config-dir* (concat emacs-config-dir "/configs/" ""))

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'ansi-color)

;;; package.el configuration
(load (concat emacs-config-dir "package.el"))
(package-initialize)

(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

;;; el-get configuration
(add-to-list 'load-path (concat emacs-config-dir "/el-get/el-get"))

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

(setq el-get-user-package-directory
      (concat user-emacs-directory "/config"))

(setq el-get-sources
 '((:name smex
          :after (lambda ()
                   (progn
                     (setq smex-save-file
                           (concat user-emacs-directory ".smex-items"))
                     (global-set-key (kbd "M-x") 'smex))))
   (:name idle-highlight-mode
          :type elpa)
   (:name ido-ubiquitous
          :type elpa)
   (:name magit
          :after (lambda ()
                   (global-set-key (kbd "C-c g") 'magit-status)))))

(setq my-packages
      (append
       '(el-get auctex haskell-mode graphviz-dot-mode
         gist)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)
(el-get 'wait)

(add-to-list 'custom-theme-load-path (concat emacs-config-dir "/themes"))
(setq custom-safe-themes t)
(load-theme 'solarized)

(defun load-config-files (files)
  (dolist (f files)
    (load (expand-file-name
           (concat *emacs-config-dir* f)))
    (message "Loaded config file: %s" file)))

(load-config-files  '("defuns" ;; Has to go first
                      "global" ;; Has to go second
                      "init-auctex"
                      "init-ido"
                      "init-c-mode"
                      "init-erlang"
                      "init-epa"
                      "init-eshell"
                      "init-recentf"
                      "init-tramp"
                      "init-flymake"
                      "init-hippie-expand"
                      ;;"nxml-setup"
                      ;;"org-setup"
                      ;;"proof-general-setup"
                      ;;"tuareg-setup"
                      "init-uniquify"))

;; Awfully simple initializations
(require 'go-mode-load)
(require 'midnight)
(require 'inf-haskell)

(load custom-file 'noerror)
;;; init.el ends here

