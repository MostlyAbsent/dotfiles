(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

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

(provide 'init-utils)
