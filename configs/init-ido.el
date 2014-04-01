;; IDO mode
(setq ido-max-directory-size 1000000)
(setq ido-enable-flex-matching t)
(require 'ido)
(ido-mode t)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

