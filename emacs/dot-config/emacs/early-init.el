;; defer gc until after init
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold 800000)))

;; disable the inbuilt package manager in favour of straight
(setq package-enable-at-startup nil)
