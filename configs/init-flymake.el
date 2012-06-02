(require 'flymake)
(require 'erlang-flymake)

;; Erlang
;; (defun flymake-erlang-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                      'flymake-create-temp-inplace))
;;          (local-file
;;           (file-relative-name temp-file
;;                               (file-name-directory buffer-file-name))))
;;     (list (concat emacs-config-dir
;;                   "/scripts/"
;;                   "flymake_erlang_script") (list local-file))))

;; (add-to-list 'flymake-allowed-file-name-masks
;;              '("\\.erl\\'" flymake-erlang-init))

(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Disable flymake for html and xml files as this is handled by nxml mode.
(delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)
(delete '("\\.xml?\\'" flymake-xml-init) flymake-allowed-file-name-masks)

(defun flymake-get-tex-args (file-name)
  (list "latex" (list "-file-line-error-style" file-name)))

