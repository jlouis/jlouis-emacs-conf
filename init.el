;; init.el --- Initialization file for my Emacs setup
;;; Commentary:

;; Set up system-specific stuff first
(cond
 ((eq system-type 'gnu/linux)
  (progn
    ((lambda (font)
       (set-frame-font font)
       (set-face-attribute 'default nil
                           :font font
                           :height 120
                           :weight 'normal)
       (set-face-font 'default font))
     "Droid Sans Mono")))
 ((eq system-type 'darwin)
  (progn
    (push "/usr/local/bin" exec-path)
    (push "/usr/local/sbin" exec-path)
    (push "/usr/texbin" exec-path)
    (push "/usr/bin" exec-path)
    (push "/usr/sbin" exec-path)
    (push "~/.cabal/bin" exec-path)
    (setenv "PATH"
            (concat "/usr/local/bin:/usr/local/sbin:"
                    "~/.cabal/bin:"
                    "/Users/jlouis/bin:"
                    "/usr/texbin:" (getenv "PATH")))
    (setenv "ERL_LIBS"
            (concat "/Users/jlouis/lib/erlang"))
    ((lambda (font)
       (set-frame-font font)
       (set-face-attribute 'default nil
                           :font font
                           :height 150
                           :weight 'normal)
       (set-face-font 'default font))
     "Source Code Pro"))))
 
(setq disabled-command-function nil)
(put 'set-goal-column           'disabled nil)
(put 'erase-buffer              'disabled nil)
(put 'downcase-region           'disabled nil)
(put 'upcase-region             'disabled nil)
(put 'narrow-to-region          'disabled nil)
(put 'narrow-to-page            'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Paths, sir, paths!
(setq emacs-config-dir (file-name-directory
                        (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path (concat emacs-config-dir "/site/"))

(setq autoload-file (concat emacs-config-dir "loaddefs.el"))
(setq site-root (concat emacs-config-dir "/site/"))
(setq custom-file (concat emacs-config-dir "custom.el"))
(setq package-user-dir (concat emacs-config-dir "elpa"))
(setq abbrev-file-name (concat emacs-config-dir "abbrev_defs"))
(defconst *emacs-config-dir* (concat emacs-config-dir "/configs/" ""))

;; Basic stuff we really need all the time
(require 'saveplace)
(require 'ffap)
(require 'ansi-color)

;;; package.el configuration
(require 'package)
(dolist (arch '(("gnu" . "http://elpa.gnu.org/packages/")
                ("melpa" . "https://melpa.org/packages/")
                ("tromey" . "http://tromey.com/elpa/")
                ("marmalade" . "http://marmalade-repo.org/packages/")
                ))
  (add-to-list 'package-archives arch))
(package-initialize)

;;; el-get configuration
(add-to-list 'load-path (concat emacs-config-dir "/el-get/el-get"))

(setq erlang-root-dir "/usr/local/lib/erlang")
(setq tools-ver "2.8.5")
(setq load-path (cons (concat erlang-root-dir "/lib/tools-" tools-ver "/emacs")
                      load-path))
(setq exec-path (cons (concat erlang-root-dir "/bin")
                      exec-path))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path (concat emacs-config-dir "/el-get-user/recipes"))


(setq el-get-user-package-directory
      (concat user-emacs-directory "/configs"))

;; Now, set up some el-get-sources overrides for our programs
(setq el-get-sources
 '((:name smex
          :after (progn
                     (setq smex-save-file
                           (concat user-emacs-directory ".smex-items"))
                     (global-set-key (kbd "M-x") 'smex)
                     (global-set-key (kbd "C-x C-m") 'smex)
                     (global-set-key (kbd "C-c C-m") 'smex)))
   (:name http-twiddle
          :type elpa)
   (:name material-theme
	  :type elpa)
   (:name magit
          :after (progn
                   (global-set-key (kbd "C-c g") 'magit-status)))))

;; Set up the packages that we are using
(setq my-packages
      (append
       '(el-get
         csv-mode
         expand-region
	 tuareg-mode
         go-mode
         graphviz-dot-mode
         htmlize
         json js2-mode
         markdown-mode)
       (mapcar 'el-get-source-name el-get-sources)))

;; Install all the packages
(el-get 'sync my-packages)
;; This is worth setting the first time you run, to wait on
;; the sync to complete
;; (el-get 'wait)

;; Setup a theme, it is a solarized variant
(add-to-list 'custom-theme-load-path
	     (concat emacs-config-dir "themes/"))
(setq custom-safe-themes t)

;; A function to load config files
(defun load-config-files (files)
  (dolist (f files)
    (load (expand-file-name
           (concat *emacs-config-dir* f)))
    (message "Loaded config file: %s" f)))

;; Now, load the config files one at a time
(load-config-files  '("defuns" ;; Has to go first
                      "global" ;; Has to go second
                      "init-ido"
                      "init-c-mode"
                      "init-erlang"
                      "init-epa"
                      "init-eshell"
                      "init-recentf"
                      "init-tramp"
                      ;"init-agda2"
                      "init-hippie-expand"
                      ;;"init-proofgeneral"
                      ;"init-twelf"
                      "init-uniquify" ))

;; Awfully simple initializations
(require 'midnight)

;; Get our custom configuration loaded
(load custom-file 'noerror)
;;; init.el ends here
(server-start)
(load-theme 'material)
