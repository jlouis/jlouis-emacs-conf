;; init.el --- Initialization file for my Emacs setup
;;; Commentary:
;; Set up system-specific stuff first

;;; package.el configuration
(require 'package)
(dolist (arch '(("gnu" . "http://elpa.gnu.org/packages/")
                ("melpa" . "https://melpa.org/packages/")
                ("tromey" . "http://tromey.com/elpa/")
                ))
  (add-to-list 'package-archives arch))

(package-initialize)

(cond
 ((eq system-type 'gnu/linux)
  (progn
    (setq erlang-root-dir "/usr/lib/erlang")
    (setq tools-ver "2.10.1")
    ((lambda (font)
       (set-frame-font font)
       (set-face-attribute 'default nil
                           :font font
                           :height 110
                           :weight 'normal)
       (set-face-font 'default font))
     "Source Code Pro")))
 ((eq system-type 'darwin)
  (progn
    (setq erlang-root-dir "/usr/local/lib/erlang")
    (setq tools-ver "2.11.1")
    (setq ocaml-ver "4.05.0")
    (push "/usr/local/bin" exec-path)
    (push "/usr/local/sbin" exec-path)
    (push "/Library/TeX/texbin" exec-path)
    (push "/usr/bin" exec-path)
    (push "/usr/sbin" exec-path)
    (push (concat "/Users/jesperlouisandersen/.opam/" ocaml-ver "/bin") exec-path)
    (push "/Users/jesperlouisandersen/.cabal/bin" exec-path)
    (push "/Users/jesperlouisandersen/bin" exec-path)
    (push "/Users/jesperlouisandersen/go/bin" exec-path)
    (setenv "PATH"
            (concat "/usr/local/bin:/usr/local/sbin:"
                    "/Users/jesperlouisandersen/go/bin:"
                    "/Users/jesperlouisandersen/.cabal/bin:"
                    (concat "/Users/jesperlouisandersen/.opam/" ocaml-ver "/bin:")
                    "/Users/jesperlouisandersen/.opam/4.05.0/bin:"
                    "/Users/jesperlouisandersen/bin:"
                    "/usr/texbin:" (getenv "PATH")))
    (setenv "ERL_LIBS"
            (concat "/Users/jesperlouisandersen/lib/erlang"))
    ((lambda (font)
       (set-frame-font font)
       (set-face-attribute 'default nil
                           :font font
                           :height 160
                           :weight 'normal)
       (set-face-font 'default font))
     "Go Mono"))))

;; Disable lots of stuff which we don't want to have in the setup
(setq disabled-command-function nil)
(put 'set-goal-column           'disabled nil)
(put 'erase-buffer              'disabled nil)
(put 'downcase-region           'disabled nil)
(put 'upcase-region             'disabled nil)
(put 'narrow-to-region          'disabled nil)
(put 'narrow-to-page            'disabled nil)
(put 'narrow-to-defun           'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Paths, sir, paths!
(setq emacs-config-dir (file-name-directory
                        (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path (concat emacs-config-dir "/site/"))
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

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

(setq load-path (cons (concat erlang-root-dir "/lib/tools-" tools-ver "/emacs")
                      load-path))
(setq exec-path (cons (concat erlang-root-dir "/bin")
                      exec-path))

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
                      "packages" ;; General package configurations
                      ;;"init-auctex"
                      "init-ido"
                      "init-c-mode"
                      "init-erlang"
                      "init-epa"
                      "init-eshell"
                      "init-recentf"
                      "init-tramp"
                      "init-agda2"
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

