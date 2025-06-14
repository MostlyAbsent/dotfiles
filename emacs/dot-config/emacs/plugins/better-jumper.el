(straight-use-package 'better-jumper)

; init
(global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
(global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
(global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
(global-set-key [remap xref-go-back] #'better-jumper-jump-backward)
(global-set-key [remap xref-go-forward] #'better-jumper-jump-forward)

(better-jumper-mode)

(provide 'better-jumper)
