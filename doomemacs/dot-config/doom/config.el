;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'weyland-yutani)

(setq doom-font (font-spec :family "Mononoki Nerd Font Propo" :size 20))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! olivetti
  :hook ((prog-mode . olivetti-mode)
         (text-mode . olivetti-mode)
         ;; (bibtex-mode . olivetti-mode)
         ;; (lsp-ui-flycheck-list-mode . olivetti-mode)
         (org-agenda-mode . olivetti-mode)
         ;; (org-roam-mode . olivetti-mode)
         ;; (magit-mode . olivetti-mode)
         ;; (gitignore-mode . olivetti-mode)
         ;; (harpoon-mode . olivetti-mode)
         ;; (typescript-ts-base-mode . olivetti-mode)
         ;; (conf-mode . olivetti-mode)
         )
  :init
  (setq olivetti-body-width 90))

(setq confirm-kill-emacs nil)

(setq ns-right-alternate-modifier 'meta)

(setq scroll-margin 8)

(add-hook 'org-mode-hook (lambda () (org-indent-mode -1)) 100)
(add-hook 'org-mode-hook (lambda () (auto-fill-mode)))

(after! recentf
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name
                "~/Documents/dotfiles/doomemacs/dot-config/emacs/.local")))

(defun jtt/org-link-abbreviator ()
  "Transforms an Org-mode ID link alias like
  '[[id:ID-STRING][Long Alias]]' to '[[id:ID-STRING][Short Alias]]'."
  (interactive)
  (save-excursion
    (let ((line-start (line-beginning-position))
          (line-end (line-end-position)))
      (goto-char line-start)
      (while (re-search-forward
              "\\(\\[\\[.*\\]\\[\\)\\(.*(\\)\\(.*\\))\\(\\]\\]\\)"
              line-end
              t)
        (replace-match "\\1\\3\\4" nil nil)))))

(after! org
  (setq org-log-done t)
  (setq org-agenda-custom-commands
        '(("u" "Semester 1" agenda ""
           ((org-agenda-start-day "-3d")
            (org-agenda-span
             (- 169                     ; June 15 + 3d behind buffer
                (string-to-number (format-time-string "%j" (current-time))))))))))

(map! :leader "j c" #'harpoon-clear)
(map! :leader "j e" #'harpoon-quick-menu-hydra)
(map! :leader "j a" #'harpoon-add-file)
(map! :leader "M-h" #'harpoon-go-to-1)
(map! :leader "M-t" #'harpoon-go-to-2)
(map! :leader "M-n" #'harpoon-go-to-3)
(map! :leader "M-s" #'harpoon-go-to-4)

(use-package! org-ql)

(defun jtt-org-clock-export-with-prefix ()
  "Call `org-clock-export` with a universal prefix argument (C-u)."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (org-clock-export)))

(use-package! org-clock-export
  :config
  (setq org-clock-export-export-file-name
        (concat (getenv "HOME") "/Documents/toggl/output.csv"))
  (setq org-clock-export-data-format
        '("start"
          (concat start-year "-" start-month "-" start-day "T"
                  start-hour ":" start-minute ":00")
          "duration-hours" total-hours
          "duration-min" total-minutes
          "description" (concat "\"" (org-entry-get (point) "ITEM") "\"")
          "tags" (or (org-entry-get (point) "CATEGORY") ""))))

(after! citar
  (setq! citar-bibliography '("~/Documents/Zotero/My Library.bib")))
