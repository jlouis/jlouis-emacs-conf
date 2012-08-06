;; init.el --- Initialization file for my Emacs setup
;;; Commentary:

;; Get up and running, set custom to something else, and set up load paths
;; for the rest of the system. Then proceed to load each configuration file
;; for each module installed in emacs via load-cfg-files.
;;

(if (string-equal "darwin" (symbol-name system-type))
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
                      "/usr/texbin:" (getenv "PATH")))
      (setenv "ERL_LIBS"
              (concat "/Users/jlouis/lib/erlang"))
      (set-fontset-font "fontset-default"
                        'unicode
                        '("Menlo" . "iso10646-1"))
      (set-frame-font "Menlo-12")))

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

;; Basic stuff we really need all the time
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'ansi-color)

;;; package.el configuration
(package-initialize)

(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

;;; el-get configuration
(add-to-list 'load-path (concat emacs-config-dir "/el-get/el-get"))

;;; Erlang mode load path needs to go here. Otherwise distel will not like us
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq tools-ver "2.6.7")
(setq load-path (cons (concat erlang-root-dir "/lib/tools-" tools-ver "/emacs")
                      load-path))
(setq exec-path (cons (concat erlang-root-dir "/bin")
                      exec-path))

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

(setq el-get-user-package-directory
      (concat user-emacs-directory "/configs"))

;; Now, set up some el-get-sources overrides for our programs
(setq el-get-sources
 '((:name smex
          :after (lambda ()
                   (progn
                     (setq smex-save-file
                           (concat user-emacs-directory ".smex-items"))
                     (global-set-key (kbd "M-x") 'smex))))
   (:name haskell-mode
          :after (lambda ()
                   (progn
                     (require 'inf-haskell))))
   (:name pastels-on-dark-theme
          :type elpa)
   (:name solarized-theme
          :type elpa)
   (:name zenburn-theme
          :type elpa)
   (:name idle-highlight-mode
          :type elpa)
   (:name ido-ubiquitous
          :type elpa)
   (:name yasnippet
          :website "http://code.google.com/p/yasnippet/"
          :description "YASnippet is a template system for Emacs."
          :type git
          :url "https://github.com/capitaomorte/yasnippet.git"
          :features "yasnippet"
          :prepare (lambda ()
                     ;; Set up the default snippets directory
                     ;;
                     ;; Principle: don't override any user settings
                     ;; for yas/snippet-dirs, whether those were made
                     ;; with setq or customize.  If the user doesn't
                     ;; want the default snippets, she shouldn't get
                     ;; them!
                     (unless (or (boundp 'yas/snippet-dirs)
                                 (get 'yas/snippet-dirs 'customized-value))
                       (setq yas/snippet-dirs 
                             (list (concat el-get-dir
                                           (file-name-as-directory "yasnippet")
                                           "snippets")))))

       :post-init (lambda ()
                      ;; Trick customize into believing the standard
                      ;; value includes the default snippets.
                      ;; yasnippet would probably do this itself,
                      ;; except that it doesn't include an
                      ;; installation procedure that sets up the
                      ;; snippets directory, and thus doesn't know
                      ;; where those snippets will be installed.  See
                      ;; http://code.google.com/p/yasnippet/issues/detail?id=179
                      (put 'yas/snippet-dirs 'standard-value 
                           ;; as cus-edit.el specifies, "a cons-cell
                           ;; whose car evaluates to the standard
                           ;; value"
                           (list (list
                                  'quote
                                  (list (concat el-get-dir
                                                (file-name-as-directory
                                                     "yasnippet")
                                                "snippets"))))))
       ;; byte-compile load vc-svn and that fails
       ;; see https://github.com/dimitri/el-get/issues/200
       :compile nil)
   (:name magit
          :after (lambda ()
                   (global-set-key (kbd "C-c g") 'magit-status)))))

;; Set up the packages that we are using
(setq my-packages
      (append
       '(
         el-get
         ;;ProofGeneral
         csv-mode
         dig
	 distel
         ess ess-edit ess-smart-underscore
         ace-jump-mode
	 tuareg-mode
         go-mode
         graphviz-dot-mode
         haskell-mode
         htmlize
         json js2-mode
         markdown-mode
         magithub
         nxhtml
         org-mode
         ;;sml-mode
         ssh-config
         )
       (if (string-equal "darwin" (symbol-name system-type))
         '(growl)
         '())
       (mapcar 'el-get-source-name el-get-sources)))

;; Install all the packages
(el-get 'sync my-packages)
;; This is worth setting the first time you run, to wait on
;; the sync to complete
;(el-get 'wait)

;; Setup a theme, it is a solarized variant
(setq custom-safe-themes t)

(defun dark ()
  (interactive)
  (load-theme 'solarized-dark))
(defun light ()
  (interactive)
  (load-theme 'solarized-light))

(dark)
(if (string-equal "darwin" (symbol-name system-type))
  (progn
    (set-frame-font "Menlo-12")))

;; A function to load config files
(defun load-config-files (files)
  (dolist (f files)
    (load (expand-file-name
           (concat *emacs-config-dir* f)))
    (message "Loaded config file: %s" file)))

;; Now, load the config files one at a time
(load-config-files  '("defuns" ;; Has to go first
                      "global" ;; Has to go second
                      ;"init-auctex"
                      "init-ido"
                      "init-c-mode"
                      "init-erlang"
                      "init-epa"
                      "init-eshell"
                      "init-recentf"
                      "init-tramp"
                      "init-flymake"
                      ;;"init-agda2"
                      "init-hippie-expand"
                      "init-proofgeneral"
                      "init-uniquify"))

;; Awfully simple initializations
(require 'midnight)

;; Get our custom configuration loaded
(load custom-file 'noerror)
;;; init.el ends here
(server-start)

