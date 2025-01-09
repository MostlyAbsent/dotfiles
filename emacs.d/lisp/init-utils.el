(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(defun jtt-extra-newline ()
	(interactive)
	(align-newline-and-indent)
	(align-newline-and-indent)
	(previous-line))

(defun jtt-close-tag ()
  "Close the previously defined XML tag

Originally from https://www.emacswiki.org/emacs/CloseXmlTag; it used a do function not present in standard Emacs. Made a few changes to use while instead.

Emacs function documentation is trash."
  (interactive)
  (let ((tag nil)
        (quote nil))
    (save-excursion
			(let ((skip 1))
				(while
						(< 0 skip)
					(re-search-backward "</?[a-zA-Z0-9_-]+")
					(cond ((looking-at "</")
								 (setq skip (+ skip 1)))
								((not (looking-at "<[a-zA-Z0-9_-]+[^>]*?/>"))
								 (setq skip (- skip 1))))))
      (when (looking-at "<\\([a-zA-Z0-9_-]+\\)")
        (setq tag (match-string 1)))
      (if (eq (get-text-property (point) 'face)
              'font-lock-string-face)
          (setq quote t)))
    (save-excursion
			(when tag
				(setq quote (and quote
												 (not (eq (get-text-property (- (point) 1) 'face)
																	'font-lock-string-face))))
				(if quote
						(insert "\""))
				(insert "</" tag ">")
				(if quote
						(insert "\""))))))

(defun jtt-cider-jack-in-shadow ()
  (interactive)
  (start-process "shadow-cljs" "shadow-cljs" "pnpm" "run" "dev")
  (switch-to-buffer "shadow-cljs")
  (sleep-for 7)
  (cider-connect-cljs))

(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun jtt-next-right-par ()
  "Move the cursor to the next right paren or simplar"
  (interactive)
  (re-search-forward (regexp-opt '(")" "]" "}")) nil t)
  (backward-char 1))

(defun jtt-word-boundary-at-point-or-region (&optional callback)
  "Return teh boundary of the word under the cursor.

Forwards the points to `callback' as (`callback' p1 p2), if present."
  (let ((deactivate-mark nil)
	$p1 $p2)
    (if (use-region-p)
	(setq $p1 (region-beginning)
	      $p2 (region-end))
      (save-excursion
	(skip-chars-backward "[:alpha:]")
	(setq $p1 (point))
	(skip-chars-forward "[:alpha:]")
	(setq $p2 (point))))
    (when callback
      (funcall callback $p1 $p2))
    (list $p1 $p2)))

(defun jtt-capitalize-region (p1 p2)
  (downcase-region p1 p2)
  (upcase-initials-region p1 p2))

(defun jtt-capitalize-word-at-point ()
  (interactive)
  (jtt-word-boundary-at-point-or-region #'jtt-capitalize-region))

(defun jtt-upcase-word-at-point ()
  (interactive)
  (jtt-word-boundary-at-point-or-region #'upcase-region))

(defun jtt-downcase-word-at-point ()
  (interactive)
  (jtt-word-boundary-at-point-or-region #'downcase-region))

(defun xah-insert-random-uuid ()
  "Insert a UUID.
This commands calls “uuidgen” on MacOS, Linux, and calls PowelShell on Microsoft Windows.
URL `http://xahlee.info/emacs/emacs/elisp_generate_uuid.html'
Version: 2020-06-04 2023-05-13"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (shell-command "pwsh.exe -Command [guid]::NewGuid().toString()" t))
   ((string-equal system-type "darwin") ; Mac
    (shell-command "uuidgen" t))
   ((string-equal system-type "gnu/linux")
    (shell-command "uuidgen" t))
   (t
    ;; code here by Christopher Wellons, 2011-11-18.
    ;; and editted Hideki Saito further to generate all valid variants for "N" in xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx format.
    (let ((xstr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                              (user-uid)
                              (emacs-pid)
                              (system-name)
                              (user-full-name)
                              (current-time)
                              (emacs-uptime)
                              (garbage-collect)
                              (buffer-string)
                              (random)
                              (recent-keys)))))
      (insert (format "%s-%s-4%s-%s%s-%s"
                      (substring xstr 0 8)
                      (substring xstr 8 12)
                      (substring xstr 13 16)
                      (format "%x" (+ 8 (random 4)))
                      (substring xstr 17 20)
                      (substring xstr 20 32)))))))

(provide 'init-utils)
