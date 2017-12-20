;;Custom Compile Command

; go get -u github.com/x/tools/cmd/...
; go get -u github.com/rogpeppe/godef/...
; go get -u github.com/nsf/gocode
; go get -u golang.org/x/tools/cmd/goimports
; go get -u golang.org/x/tools/cmd/guru
; go get -u github.com/dougm/goflymake

(defun go-mode-setup ()
  (setq-local compile-command "go build -v && go test -v && go vet")
  (define-key (current-local-map) "\C-c\C-c" 'compile)
  (set
   (make-local-variable 'company-backends) '(company-go))
  
  (go-eldoc-setup)
  (subword-mode +1)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump))

(add-hook 'go-mode-hook 'go-mode-setup)
