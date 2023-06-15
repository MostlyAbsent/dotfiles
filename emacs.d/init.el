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
(global-set-key (kbd "M-u") nil)
(global-set-key (kbd "M-Q") 'unfill-paragraph)
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(global-set-key (kbd "s-n") 'make-frame)
(global-set-key (kbd "s-w") 'delete-frame)
(global-set-key (kbd "s-v") 'evil-paste-after)

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
(require 'init-org)
(evil-mode 1)

(use-package s
  :ensure t
  :defer t)

(use-package dash
  :ensure t
  :defer t)

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

(use-package flycheck
  :ensure t)

(use-package lsp-mode
  :ensure t
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp))
  :config
  ;; add paths to your local installation of project mgmt tools, like lein
  (setenv "PATH" (concat
                   "/usr/local/bin" path-separator
                   (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-diagnostics t
	lsp-ui-sideline-delay 0))

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
  (setq cider-repl-pop-to-buffer-on-connect nil)
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
  :config
  (setq projectile-create-missing-test-files t)
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-branch-arguments nil)
  (setq magit-push-always-verify nil)
  (setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer
         (cond ((and (derived-mode-p 'magit-mode)
                     (eq (with-current-buffer buffer major-mode)
                         'magit-status-mode))
                nil)
               ((memq (with-current-buffer buffer major-mode)
                      '(magit-process-mode
                        magit-revision-mode
                        magit-diff-mode
                        magit-stash-mode))
                nil)
               (t
                '(display-buffer-same-window))))))
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
  (sp-local-pair 'mhtml-mode "{%" "%}")
  (define-key global-map (kbd "M-(") 'sp-wrap-round)
  (define-key global-map (kbd "M-[") 'sp-wrap-square)
  (define-key global-map (kbd "M-{") 'sp-wrap-curly)
  (define-key global-map (kbd "M-q") 'sp-indent-defun)
  (define-key global-map (kbd "C-<left>") 'sp-backward-sexp)
  (define-key global-map (kbd "C-<right>") 'sp-forward-sexp)
  (define-key global-map (kbd "C-<down>") 'sp-down-sexp)
  (define-key global-map (kbd "C-<up>") 'sp-backward-up-sexp)
  (define-key global-map (kbd "M-<down>") 'sp-forward-slurp-sexp)
  (define-key global-map (kbd "M-<up>") 'sp-forward-barf-sexp)
  (defun my-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  (sp-local-pair 'html-mode "<" ">")
  (sp-local-pair 'css-mode
		 "{"
		 nil
		 :post-handlers '((my-create-newline-and-enter-sexp "RET"))))

(use-package vertico
  :ensure t
  :config
  (vertico-mode)
  (setq vertico-resize nil
	vertico-count 17
	vertico-cycle t))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((files (styles partial-completion)))))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :ensure t
  :init
  (setq doom-modeline-bar-width 3
	doom-modeline-github t
	doom-modeline-major-mode-icon t
	doom-modeline-buffer-file-name-style 'relative-from-project)
  :config
  (add-hook 'after-setting-font-hook #'+modeline-resize-for-font-h)
  (custom-set-faces
   `(mode-line ((t (:background "#303030"))))
   `(mode-line-emphasis ((t (:foreground "#AAAAAA"))))))

(use-package impatient-mode
  :ensure t
  :config
  (defun my-html-mode-hook ()
  "Starts the `simple-httpd' server if it is not already running,
and turns on `impatient-mode' for the current buffer."
  (unless (get-process "httpd")
    (message "starting httpd server...")
    (httpd-start))
  (impatient-mode))
  (add-hook 'web-mode-hook 'my-html-mode-hook))

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
  :ensure t
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

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (global-set-key (kbd "s-z") 'undo-tree-undo)
  (global-set-key (kbd "s-y") 'undo-tree-redo))

(use-package treemacs
  :ensure t
  :defer t)

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package restclient
  :ensure t)

(use-package js2-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package dockerfile-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  :config
  (defun my-web-mode-hook ()
    (setq web-mode-enable-auto-pairing nil))

  (add-hook 'web-mode-hook  'my-web-mode-hook)

  (defun sp-web-mode-is-code-context (id action context)
    (and (eq action 'insert)
	 (not (or (get-text-property (point) 'part-side)
                  (get-text-property (point) 'block-side)))))

  (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))
  :bind
  (("C-c C-g +" . web-mode-element-extract)
   ("C-c C-g -" . web-mode-element-contract)
   ("C-c C-g /" . web-mode-element-close)
   ("C-c C-g I" . web-mode-element-insert-at-point)
   ("C-c C-g a" . web-mode-element-content-select)
   ("C-c C-g b" . web-mode-element-beginning)
   ("C-c C-g c" . web-mode-element-clone)
   ("C-c C-g d" . web-mode-element-child)
   ("C-c C-g e" . web-mode-element-end)
   ("C-c C-g f" . web-mode-element-children-fold-or-unfold)
   ("C-c C-g i" . web-mode-element-insert)
   ("C-c C-g k" . web-mode-element-kill)
   ("C-c C-g m" . web-mode-element-mute-blanks)
   ("C-c C-g n" . web-mode-element-next)
   ("C-c C-g p" . web-mode-element-previous)
   ("C-c C-g r" . web-mode-element-rename)
   ("C-c C-g s" . web-mode-element-select)
   ("C-c C-g t" . web-mode-element-transpose)
   ("C-c C-g u" . web-mode-element-parent)
   ("C-c C-g v" . web-mode-element-vanish)
   ("C-c C-g w" . web-mode-element-wrap)))

(provide 'init)
