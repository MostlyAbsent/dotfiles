(require 'rebinder)
(define-key global-map (kbd "C-u") (rebinder-dynamic-binding "C-x"))
