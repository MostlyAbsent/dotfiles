(setq org-roam-directory (file-truename "~/Documents/org-roam"))
(setq org-cite-global-bibliography (list (file-truename "~/Documents/org-cite/global.bib")))
(org-roam-db-autosync-mode)
(setq org-roam-completion-everywhere t)
(setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))

(provide 'init-roam)
