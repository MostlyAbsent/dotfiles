(package-initialize)

(load (expand-file-name "config/package-managers" user-emacs-directory))
;(load (expand-file-name "config/emacs" user-emacs-directory))
(load (expand-file-name "config/rebinder" user-emacs-directory))
(load (expand-file-name "config/evil" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(quelpa-use-package quelpa)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
