(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(weyland-yutani-theme undo-fu-session company-lsp vertico-buffer yaml-mode
                          web-mode lsp-ui lsp-mode json-mode js2-mode
                          rainbow-mode elisp-slime-nav rainbow-delimiters
                          company consult orderless vertico exec-path-from-shell
                          zop-to-char which-key volatile-highlights super-save
                          smartrep smartparens operate-on-number nlinum
                          move-text magit projectile imenu-anywhere hl-todo
                          guru-mode git-modes git-timemachine gist flycheck
                          expand-region epl editorconfig easy-kill diminish
                          diff-hl discover-my-major crux browse-kill-ring anzu
                          ag ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI Pretties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(disable-theme 'zenburn)

(use-package weyland-yutani-theme
  :ensure t
  :config
  (setq hl-paren-colors '("#000000"))
  (setq hl-paren-background-colors '("#B376D2"))
  :init
  (load-theme 'weyland-yutani t))

(add-to-list 'default-frame-alist '(font . "Mononoki Nerd Font Propo-20"))

(use-package olivetti
  :ensure t
  :hook ((prog-mode . olivetti-mode)
         (text-mode . olivetti-mode)
         (lsp-ui-flycheck-list-mode . olivetti-mode)
         (org-agenda-mode . olivetti-mode)
         (org-roam-mode . olivetti-mode)
         (magit-mode . olivetti-mode)
         (gitignore-mode . olivetti-mode)
         (harpoon-mode . olivetti-mode))
  :init
  (setq olivetti-body-width 90))

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(setq scroll-margin 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(add-hook 'org-mode-hook 'auto-fill-mode)

(setq projectile-globally-ignored-files
      '(".#*" ".DS_Store" "node_modules" "dist" "build"))

(recentf-mode 1)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((clojure-mode . lsp)
         (python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-capf))

(use-package cider
  :ensure t
  :config
  (setq cider-preferred-build-tool 'clojure-cli))

;; Force splitting to bottom half
(setq split-width-threshold nil)
(setq split-height-threshold 0)

(setq fill-column 78)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(defun jtt/yas-insert-by-key (key)
  "Find a yasnippet by its KEY in the current mode's table and insert it."
  (yas-expand-snippet (yas-lookup-snippet key)))

(defun jtt/scroll-up-and-recenter ()
  "Scroll up half a screen and then center the cursor vertically."
  (interactive)
  (call-interactively 'evil-scroll-up)
  (recenter))

(defun jtt/scroll-down-and-recenter ()
  "Scroll down half a screen and then center the cursor vertically."
  (interactive)
  (call-interactively 'evil-scroll-down)
  (recenter))

(use-package undo-fu
  :ensure t
  :after evil
  :config
  (setq undo-limit 67108864)
  (setq undo-strong-limit 100663296)
  (setq undo-outer-limit 1006632960)
  (setq evil-undo-system 'undo-fu))

(use-package undo-fu-session
  :after undo-fu
  :ensure t
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode))

(use-package harpoon
  :straight t
  :config
  (setq harpoon-project-package 'projectile))

(defun jtt/save-word ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct
       'save
       nil
       (car word)
       current-location
       (cadr word)
       (caddr word)
       current-location))))

(setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'org-agenda-files (expand-file-name "~/Documents/org/todo.org"))

(setq org-capture-templates
      '(("t"
         "Global Todo"
         entry
         (file "~/Documents/org/todo.org")
         "* TODO %?\n  - Created on %U")))

(setq org-agenda-window-setup 'current-window)

(defun jtt/org-move-element-up ()
  "Move the current Org element (subtree or list item) up.
If not on a recognized element, do nothing."
  (interactive)
  (cond
   ((org-at-heading-p) (call-interactively 'org-move-subtree-up))
   ((org-at-item-p)    (call-interactively 'org-move-item-up))))

(defun jtt/org-move-element-down ()
  "Move the current Org element (subtree or list item) down.
If not on a recognized element, do nothing."
  (interactive)
  (cond
   ((org-at-heading-p) (call-interactively 'org-move-subtree-down))
   ((org-at-item-p)    (call-interactively 'org-move-item-down))))

(defun jtt/find-global-todo-file ()
  "Open the global ~/Documents/org/todo.org file."
  (interactive)
  (find-file "~/Documents/org/todo.org"))

(use-package org-roam
  :ensure t
  :init
  (setq org-directory "~/Documents/org/")
  (setq org-roam-directory (file-truename "~/Documents/org/roam"))
  :config
  (org-roam-db-autosync-mode))

(defun jtt/set-fill-column ()
  (setq fill-column 78))

(add-hook 'org-mode-hook #'jtt/set-fill-column)

(defun jtt/org-mode-evil-collection-overrides ()
  "Override evil-collection keybindings in org-mode for harpoon."
  (general-define-key
   :keymaps 'org-mode-map
   :states '(normal visual motion)
   "M-h" 'harpoon-go-to-1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq select-enable-clipboard t)

(setq mac-option-key-is-meta t
      mac-command-modifier 'super
      mac-option-modifier 'meta)

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.15))

(use-package general
  :ensure t
  :after evil-collection
  :config
  (general-define-key
   :keymaps '(normal insert)
   "s-v" 'yank
   "C-0" 'universal-argument
   "C-u" 'jtt/scroll-up-and-recenter
   "C-d" 'jtt/scroll-down-and-recenter
   "C-r" 'undo-fu-only-redo
   "C-<left>" 'sp-beginning-of-sexp
   "C-<right>" 'sp-end-of-sexp
   "C-<up>" 'sp-up-sexp
   "C-<down>" 'sp-down-sexp)

  (general-define-key
   :keymaps '(normal motion visual)
   "C-e" 'harpoon-quick-menu-hydra
   "M-h" 'harpoon-go-to-1
   "M-t" 'harpoon-go-to-2
   "M-n" 'harpoon-go-to-3
   "M-s" 'harpoon-go-to-4)

  (general-define-key
   :keymaps '(normal motion visual)
   "zg" 'jtt/save-word)

  (general-define-key
   :keymaps 'insert
   "<left>"  'left-char
   "<right>" 'right-char
   "<up>"    'previous-line
   "<down>"  'next-line)

  (general-define-key
   :keymaps 'evil-motion-state-map
   "SPC" nil)

  (general-create-definer jtt-leader-map
    :keymaps '(normal visual motion)
    :prefix "SPC")

  (general-define-key
   :keymaps '(normal visual motion)
   :prefix "SPC"

    ;; files
    "f"  '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find file")
    "fr" '(consult-recent-file :which-key "find recent")

    "qq" '(evil-save-and-quit :which-key "quit")

    ;; buffers
    "b" '(:ignore t :which-key "buffers")
    "bb" '(switch-to-buffer :which-key "switch")

    ;; ;; projectile
    "p" '(:ignore t :which-key "project")
    "pp" '(projectile-switch-project :which-key "switch project")
    "pf" '(projectile-find-file-dwim :which-key "find file")
    "/" '(projectile-grep :which-key "grep")

    ;; harpoon
    "j" '(:ignore t :which-key "harpoon")
    "jc" 'harpoon-clear
    "jf" 'harpoon-toggle-file
    "ja" 'harpoon-add-file

    ;; code
    "c" '(:ignore t :which-key "code")
    "cc" '(comment-dwim :which-key "comment")
    "cr" '(lsp-rename :which-key "rename")
    "cR" '(cider-restart :which-key "repl restart")
    "ce" '(cider-eval-last-sexp :which-key "eval")
    "cE" '(cider-eval-defun-at-point :which-key "eval at point")
    "cb" '(cider-load-buffer :which-key "load buffer")
    "ca" '(lsp-code-action :which-key "action")
    "cj" '(cider-jack-in :which-key "repl jack")
    "cq" '(cider-quit :which-key "repl quit")

    ;; go
    "g" '(:ignore t :which-key "go")
    "gg" '(magit :which-key "magit")
    "gd" '(lsp-find-definition :which-key "defininition")
    "gi" '(lsp-find-implementation :which-key "implementation")
    "gr" '(lsp-find-references :which-key "references")
    "gh" '(lsp-ui-doc-show :which-key "hover docs")
    "ge" '(lsp-ui-flycheck-list :which-key "list errors")

    ;; notes
    "n" '(:ignore t :which-key "notes")
    "nf" '(org-roam-node-find :which-key "find")
    "ni" '(org-roam-node-insert :which-key "insert")
    "nb" '(org-roam-buffer-toggle :which-key "backlinks")
    "<return>" '((lambda ()
                   (interactive)
                   (let ((org-link-frame-setup '((file . find-file))))
                     (org-open-at-point)))
                 :which-key "follow link")

    ;; todos
    "t" '(:ignore t :which-key "todos")
    "tg" '(org-set-tags-command :which-key "tags")
    "tt" '(jtt/find-global-todo-file :which-key "todos")
    "tn" '((lambda ()
             (interactive)
             (org-capture nil "t"))
           :which-key "new")
    "ta" '(org-todo-list :which-key "agenda"))

  (general-define-key
   :keymaps 'vertico-map
   "s-v" 'yank
   "C-y" 'vertico-exit
   "C-j" 'vertico-exit-input)

  (general-define-key
   :keymaps 'org-mode-map
   "M-<up>" 'jtt/org-move-element-up
   "M-<down>" 'jtt/org-move-element-down)

  (add-hook 'org-mode-hook #'jtt/org-mode-evil-collection-overrides)

  (defun jtt/org-link-abbreviator ()
    "Transforms an Org-mode ID link alias like
  '[[id:ID-STRING][Long Alias]]' to '[[id:ID-STRING][Short Alias]]'."
    (interactive)
    (let (position (point))
      ;;try anzu
      ))

  (general-define-key
   :keymaps 'org-mode-map
   :states '(normal visual motion)
   :prefix "SPC"

   "o" '(:ignore t :which-key "org mode")
   "oq" '((lambda () (interactive) (jtt/yas-insert-by-key "quote"))
          :which-key "quote block")
   "os" '((lambda () (interactive) (jtt/yas-insert-by-key "src"))
          :which-key "source block")
   "oe" '((lambda () (interactive) (jtt/yas-insert-by-key "example"))
          :which-key "example block")
   "ot" '((lambda () (interactive) (jtt/yas-insert-by-key "table"))
          :which-key "table")
   "op" '((lambda () (interactive) (jtt/yas-insert-by-key "property"))
          :which-key "properties")
   "oh" '((lambda () (interactive) (jtt/yas-insert-by-key "todo"))
          :which-key "headline (todo)")
   "of" '(org-footnote-action :which-key "footnote")
   "oa" '(jtt/org-link-abbreviator :which-key "abbreviate"))

  (general-define-key
   :keymaps 'company-active-map
   "C-y" 'company-complete-selection
   "<return>" nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hack to center minibuffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom jtt-centered-minibuffer-width 100
  "The maximum width for content in the centered minibuffer."
  :type 'integer
  :group 'ui)

(defun jtt/set-centered-minibuffer-margins ()
  "Calculate and apply margins to the minibuffer window to center its content."
  (let* (
         (frame-width (frame-width))
         (margin-space (- frame-width jtt-centered-minibuffer-width))
         (left-margin (max 0 (floor (/ margin-space 2.0)))))
    (set-window-margins (minibuffer-window) left-margin left-margin)))

(defun jtt/remove-centered-minibuffer-margins ()
  "Remove margins from the minibuffer window."
  (set-window-margins (minibuffer-window) nil nil))

(add-hook 'minibuffer-setup-hook #'jtt/set-centered-minibuffer-margins)
(add-hook 'minibuffer-exit-hook #'jtt/remove-centered-minibuffer-margins)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
