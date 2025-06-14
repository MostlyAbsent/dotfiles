;; extend load path to use the plugin folder for my custom configs
(add-to-list 'load-path (expand-file-name "plugins/" user-emacs-directory))

;; bootstrap straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; defaults
(require 'defaults)
;; TODO: gmch mode for better gc

;; features to init
(require 'session-exit)
(require 'keybinds-conf)
(require 'editor-conf)
(require 'file-extensions)
