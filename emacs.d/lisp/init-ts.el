(evil-define-key 'insert typescript-ts-mode-map (kbd "M-RET") 'jtt-extra-newline)
(evil-define-key 'insert tsx-ts-mode-map (kbd "M-RET") 'jtt-extra-newline)

(sp-local-pair '(tsx-ts-mode typescript-ts-mode) "<" ">")
(sp-local-tag  '(tsx-ts-mode typescript-ts-mode)
							 "<" "<_>" "</_>"
							 :transform 'sp-match-sgml-tags
							 :post-handlers '(sp-html-post-handler))
(sp-local-pair '(tsx-ts-mode typescript-ts-mode) "<" ">")
(sp-local-tag  '(tsx-ts-mode typescript-ts-mode)
							 "<" "<_>" "</_>"
							 :transform 'sp-match-sgml-tags
							 :post-handlers '(sp-html-post-handler))

(provide 'init-ts)
