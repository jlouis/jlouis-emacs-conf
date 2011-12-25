(add-to-list 'auto-mode-alist
             (cons (concat "\\."
                           (regexp-opt '("xml" "xsd" "sch" "rng"
                                         "xslt" "svg" "rss") t) "\\'")
                   'nxml-mode))
(unify-8859-on-decoding-mode)
(fset 'xml-mode 'nxml-mode)
;(delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)
;(delete '("\\.xml?\\'" flymake-xml-init) flymake-allowed-file-name-masks)
