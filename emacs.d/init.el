(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(let ((files (directory-files-and-attributes (expand-file-name "lisp" user-emacs-directory) t)))
  (dolist (file files)
    (let ((filename (car file))
	  (dir (nth 1 file)))
      (when (and dir
		 (not (string-suffix-p "." filename)))
	(add-to-list 'load-path (car file))))))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'init-utils)
(require 'init-elpa)

;; Emacs Configurations

(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)

(show-paren-mode 1)
(setq show-paren-delay 0)
(global-hl-line-mode 1)

(setq indicate-empty-lines t)
(setq show-trailing-whitespace t)
(setq indent-tabs-mode nil)

(setq visible-bell t)
(setq custom-safe-themes t)
(column-number-mode t)
(setq tab-width 4)

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(set-face-attribute 'default nil
		    :font "Source Code Pro"
		    :slant 'normal
		    :height 120)

(load-theme 'cyberpunk t)

(add-hook 'display-line-numbers-mode-hook
	  (lambda () (setq display-line-numbers-type 'relative)))
(global-display-line-numbers-mode)

(setq ispell-program-name "/usr/local/bin/hunspell")

(put 'narrow-to-region 'disabled nil)

(add-hook 'text-mode-hook 'flyspell-mode)

;; Make it so keyboard-escape-quit doesn't delete-other-windows
(require 'cl-lib)
(defadvice keyboard-escape-quit
    (around keyboard-escape-quit-dont-delete-other-windows activate)
  (cl-letf (((symbol-function 'delete-other-windows)
             (lambda () nil)))
    ad-do-it))

;; General Keybindings
(define-key global-map "\M-Q" 'unfill-paragraph)

;; Language Specific Configurations
(add-hook 'sh-mode-hook (lambda ()
			  (setq sh-basic-offset 2)
			  (setq sh-indentation 2)))

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'html-mode-hook 'subword-mode)
(setq js-indent-level 2)

;; Install Packages
;;(require-package 'fullframe)
(require-package 'cl-lib)
(require 'cl-lib)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'init-evil)
(evil-mode 1)

(use-package paredit
  :ensure t
  :config
  (add-hook 'eldoc-mode-hook 'enable-paredit-mode))

(use-package org
  :ensure t
  :bind (("C-c l" .   org-store-link)
	 ("C-c s" .   org-set-property)
	 ("C-c M-l" . org-insert-last-stored-link))
  :config
  (setq org-blank-before-new-entry '((heading . t)
				     (plan-list-item . t)))
  (setq org-insert-heading-respect-content t)
  (setq org-pretty-entities t)
  (setq org-startup-truncated t)
  (add-hook 'org-mode-hook 'org-indent-mode))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode))

(use-package smart-mode-line
	     :ensure t
	     :config
	     (setq sml/theme 'dark)
	     (sml/setup))

(use-package s
  :ensure t
  :defer t)

(use-package dash
  :ensure t
  :defer t)

(use-package visual-fill-column
  :ensure t
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  (setq-default visual-fill-column-width 85)
  (setq-default visual-fill-column-center-text t)
  (setq-default fill-column 80)
  (global-visual-line-mode)
  (global-visual-fill-column-mode))

(use-package company
  :ensure t
  :defer 2
  :config
  (setq company-idle-delay 0.4)
  (setq company-selection-wrap-around t)
  (define-key company-active-map (kbd "ESC") 'company-abort)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)

  (global-company-mode))

(use-package rainbow-delimiters
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook
	    (lambda ()
	      (enable-paredit-mode)
	      (subword-mode)
	      (rainbow-delimiters-mode))))

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (setq cider-repl-pop-to-buffer-on-connect t)
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  (setq cider-repl-wrap-history t)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)

  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode)))

(use-package projectile
  :ensure t
  :config (projectile-global-mode))

(use-package tagedit
  :ensure t
  :config (add-hook 'html-mode-hook 'tagedit-mode))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-branch-arguments nil)
  (setq magit-push-always-verify nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (add-hook 'git-commit-mode-hook 'evil-insert-state))

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package org-contrib
  :ensure t)

(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; (use-package flycheck
;;   :ensure t
;;   :config
;;   (add-hook 'after-init-hook 'global-flycheck-mode))

(provide 'init)
