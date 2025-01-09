(defun jtt-org-agenda-redo ()
  "Inspired by nebucatnetzer, (Nebucatnetzer)."
  (with-current-buffer "*Org Agenda*"
    (org-agenda-maybe-redo)))

(defun jtt-copy-item ()
	(interactive)
	(end-of-line)
	(org-mark-element)
	(deactivate-mark)
	(call-process-region (point) (mark) "pbcopy"))

(defun jtt-get-id ()
	(interactive)
	(let* ((id_ (org-entry-get (point) "id"))
				 (id (if (not id_)
								 (number-to-string (random 99999))
							 id_)))
		(org-set-property "id" id)
		id))

(defun jtt-export-thought ()
	(interactive)
	(let ((id (jtt-get-id)))
			(org-mark-subtree)
	 (let ((thought (buffer-substring (region-beginning) (region-end))))
		 (with-temp-file (concat id "-" (org-get-heading) ".org")
			 (insert thought)))
	 (deactivate-mark)))

(use-package org
  :ensure t
  :bind (("C-c a" . org-agenda)
				 ("C-c e" . jtt-export-thought))
  :config
  (setq org-default-notes-file
				(expand-file-name "~/Documents/org/agenda.org"))
  (setq org-agenda-files '("~/Documents/org"))
  (setq org-pretty-entities t)
  (setq org-todo-keywords
				'((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "INTR(i)" "DONE(d)")))
  (setq org-agenda-span 'day)
  (setq org-agenda-todo-ignore-scheduled 'future)
	(add-hook 'org-mode-hook (lambda () (add-to-list 'org-latex-log "tex")))
  (setq org-agenda-todo-ignore-time-comparison-use-seconds t)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  (setq org-agenda-custom-commands
				'(("n" "Agenda / INTR / PROG / NEXT"
					 ((agenda "" nil)
						(todo "INTR" nil)
						(todo "PROG" nil)
						(todo "NEXT" nil))
					 nil)))

  (add-hook 'org-mode-hook 'org-indent-mode)
  ;;(add-hook 'org-mode-hook 'lsp-org)
  (add-hook 'org-capture-after-finalize-hook 'jtt-org-agenda-redo))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode))

(provide 'init-org)
