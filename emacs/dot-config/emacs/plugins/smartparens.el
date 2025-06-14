(straight-use-package 'smartparens)

(smartparens-global-mode)

;; The default is 100, because smartparen's scans are relatively expensive
;; (especially with large pair lists for some modes), we reduce it, as a
;; better compromise between performance and accuracy.
(setq sp-max-prefix-length 25)

;; Silence some harmless but annoying echo-area spam
(dolist (key '(:unmatched-expression :no-matching-tag))
  (setf (alist-get key sp-message-alist) nil))

;; Smartparens breaks evil-mode's replace state
(defvar doom-buffer-smartparens-mode nil)
(add-hook 'evil-replace-state-exit-hook
  (defun doom-enable-smartparens-mode-maybe-h ()
    (when doom-buffer-smartparens-mode
      (turn-on-smartparens-mode)
      (kill-local-variable 'doom-buffer-smartparens-mode))))
(add-hook 'evil-replace-state-entry-hook
  (defun doom-disable-smartparens-mode-maybe-h ()
    (when smartparens-mode
      (setq-local doom-buffer-smartparens-mode t)
      (turn-off-smartparens-mode))))

(provide 'smartparens)
