;;Custom Compile Command

(defun go-mode-setup ()
  (setq-local compile-command "go build -v && go test -v && go vet && golint")
  (define-key (current-local-map) "\C-c\C-c" 'compile)
  (set
   (make-local-variable 'company-backends) '(company-go))
  
  (go-eldoc-setup)
  (subword-mode +1)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump))

(add-hook 'go-mode-hook 'go-mode-setup)
