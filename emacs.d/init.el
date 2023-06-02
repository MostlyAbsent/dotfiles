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
(setq-default show-trailing-whitespace t)
(setq indent-tabs-mode nil)

(add-hook 'display-line-numbers-mode-hook
	  (lambda () (setq display-line-numbers-type 'relative)))
(global-display-line-numbers-mode)

(setq visible-bell t)
(setq custom-safe-themes t)
(column-number-mode t)
(setq tab-width 4)

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

(set-face-attribute 'default nil
		    :font "Fira Code"
		    :slant 'Normal
		    :height 130)

(if (file-directory-p "/opt/homebrew")
    (setq ispell-program-name "/opt/homebrew/bin/hunspell")
  (setq ispell-program-name "/usr/local/bin/hunspell"))

(add-hook 'text-mode-hook 'flyspell-mode)

;; Allow using a space in the minibuffer without triggering completion
(define-key minibuffer-local-completion-map "\M- "
    (lambda () (interactive) (insert " ")))

(setq completion-ignore-case t)

(require 'rebinder)
(define-key global-map (kbd "C-u") (rebinder-dynamic-binding "C-x"))

;;;; Enabled Commands
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; General Keybindings
(define-key global-map (kbd "M-Q") 'unfill-paragraph)
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(global-set-key (kbd "s-n") 'make-frame)
(global-set-key (kbd "s-w") 'delete-frame)

;;;; Make it so keyboard-escape-quit doesn't delete-other-windows
(require 'cl-lib)
(defadvice keyboard-escape-quit
    (around keyboard-escape-quit-dont-delete-other-windows activate)
  (cl-letf (((symbol-function 'delete-other-windows)
             (lambda () nil)))
    ad-do-it))

;; Language Specific Configurations
(add-hook 'sh-mode-hook (lambda ()
			  (setq sh-basic-offset 2)
			  (setq sh-indentation 2)))

(add-hook 'html-mode-hook 'subword-mode)
(setq js-indent-level 2)

;; Install Packages
(require-package 'cl-lib)
(require 'cl-lib)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'init-evil)
(evil-mode 1)

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
  (setq org-tags-column -50)
  (setq org-startup-truncated t)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (require 'org-collector)
  (customize-set-value 'org-latex-hyperref-template "
\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},
 pdfsubject={%d},\n pdfcreator={%c},\n pdflang={%L},\n hidelinks=true}\n")
  (setq org-agenda-files '("~/Documents/job_notes/daily.org")))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode))

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
  (setq-default visual-fill-column-width 95)
  (add-hook 'text-mode-hook 'visual-line-mode)
  (add-hook 'prog-mode-hook 'visual-line-mode)
  (setq-default visual-fill-column-center-text t)
  (setq-default fill-column 90))

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

(use-package lsp-mode
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook
	    (lambda ()
	      (subword-mode)
	      (rainbow-delimiters-mode)
	      (lsp))))

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (setq cider-repl-pop-to-buffer-on-connect t)
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  (setq cider-repl-wrap-history t)

  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojurescript-mode))
  (add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode)))

(use-package projectile
  :ensure t
  :pin melpa-stable
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

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

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (add-hook 'eldoc-mode-hook 'smartparens-mode)
  (add-hook 'yaml-mode-hook 'smartparens-mode)
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (add-hook 'shell-mode-hook 'smartparens-mode)
  (sp-pair "'" "'" :actions nil)
  (sp-pair "<" ">")
  (sp-local-pair 'mhtml-mode "{%" "%}")
  (define-key global-map (kbd "M-(") 'sp-wrap-round)
  (define-key global-map (kbd "M-[") 'sp-wrap-square)
  (define-key global-map (kbd "M-q") 'sp-indent-defun)
  (defun my-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  (sp-local-pair 'css-mode
		 "{"
		 nil
		 :post-handlers '((my-create-newline-and-enter-sexp "RET"))))

(use-package ivy
   :ensure t
   :config (ivy-mode 1))

(use-package impatient-mode
  :ensure t
  :config
  (defun my-html-mode-hook ()
  "Starts the `simple-httpd' server if it is not already running, and turns
on `impatient-mode' for the current buffer."
  (unless (get-process "httpd")
    (message "starting httpd server...")
    (httpd-start))
  (impatient-mode))
  (add-hook 'html-mode-hook 'my-html-mode-hook))

(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures
   'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::" ":::" ":="
		"!!" "!=" "!==" "-}" "----" "-->" "->" "->>" "-<" "-<<" "-~" "#{" "#["
		"##" "###" "####" "#(" "#?" "#_" "#_(" ".-" ".=" ".." "..<" "..." "?="
		"??" ";;" "/*" "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>"
		"^=" "$>" "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<=" "=<<"
		"=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>" "<$"
		"<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<"
		"<<-" "<<=" "<<<" "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode 't))

(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

(use-package lua-mode
  :ensure t)

(use-package vterm
  :ensure t)

(use-package js2-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
(provide 'init)
