(defun jdc--config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key
   "'" 'toggle-comment-on-line
   "," 'other-window
   ":" 'eval-expression
   "a" 'org-agenda
   "C" 'jtt-capitalize-word-at-point
   "d" 'jtt-downcase-word-at-point
   "e k" 'cider-load-buffer
   "e s" 'cider-eval-sexp-at-point
   "i" 'org-clock-in
   "j c" 'cider-jack-in-clj
   "j s" 'jtt-cider-jack-in-shadow
   "l" 'evil-execute-macro
   "m" 'magit
   "o" 'org-clock-out
   "p f" 'projectile-find-file
   "p t" 'projectile-toggle-between-implementation-and-test
   "p w" 'projectile-switch-project
   "q q" 'cider-quit
   "s s" 'sort-lines
   "s a" 'lsp-execute-code-action
   "t n" 'cider-test-run-ns-tests
   "t t" 'cider-test-run-test
   "u" 'jtt-upcase-word-at-point
   "w" 'save-buffer
   "y" 'yank-to-x-clipboard
	 "e c" 'jtt-copy-item
   ))

(defun jdc--config-evil ()
  "Configure evil mode."

  (dolist (mode '(dired-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/") 'evil-search-forward
    (kbd "n") 'evil-search-next
    (kbd "N") 'evil-search-previous)

  (evil-define-key 'normal global-map (kbd "<down>") 'evil-next-visual-line)
	(evil-define-key 'normal global-map (kbd "s") 'evil-goto-mark)
  (evil-define-key '(normal insert) global-map (kbd "C-M-d") 'evil-scroll-up)
  (evil-define-key 'normal global-map (kbd "<up>") 'evil-previous-visual-line)
  (evil-define-key '(normal insert) global-map (kbd "C-e") 'end-of-line)
  (define-key evil-normal-state-map "u" nil)
  (evil-define-key 'insert global-map (kbd "C-v") 'yank)
  (evil-define-key 'insert global-map (kbd "C-t C-c") 'jtt-close-tag)
  (evil-define-key '(normal insert) global-map (kbd "M-v") 'yank-from-kill-ring)

  (define-key evil-normal-state-map [escape] 'keyboard-escape-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit))

(defmacro define-evil-or-global-key (key def &optional state)
  "Defina a key KEY with DEF in an Evil map, or in the global map.

If the Evil map for STATE is defined (or `normal' if STATE is not
provided) the key will be defined in that map. Failing that, it will
be defined globally.

Note that STATE should be provided as an unquoted symbol.

This macro provides a way to override Evil mappings in the appropriate
Evil map in a manner that is compatible with environments where Evil
is not used."
  (let* ((evil-map-name (if state
			    (concat "evil-" (symbol-name state) "-state-map")
			  "evil-normal-state-map"))
	 (map (if (boundp (intern evil-map-name))
		  (intern evil-map-name)
		global-map)))
    `(define-key ,map ,key ,def)))

(use-package evil
  :ensure t
  :commands (evil-mode evil-define-key)
  :config
  (add-hook 'evil-mode-hook 'jdc--config-evil)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (jdc--config-evil-leader))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :ensure t))

(provide 'init-evil)
