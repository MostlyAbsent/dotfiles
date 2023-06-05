(use-package org
  :ensure t
  :bind (("C-c a" . org-agenda))
  :config
  (setq org-agenda-files '("~/Documents/org/agenda.org"))
  (setq org-pretty-entities t)
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "INTR(i)" "DONE(d)")))
  (setq org-agenda-span 'day)
  (setq org-agenda-todo-ignore-scheduled 'future)
  (setq org-agenda-todo-ignore-time-comparison-use-seconds t)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  (setq org-agenda-custom-commands
	'(("n" "Agenda / INTR / PROG / NEXT"
	   ((agenda "" nil)
	    (todo "INTR" nil)
	    (todo "PROG" nil)
	    (todo "NEXT" nil))
	   nil)))

  (add-hook 'org-mode-hook 'org-indent-mode))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode))

(provide 'init-org)
