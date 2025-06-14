;; This function will be called by our external Babashka script.
(defun jtt/interactive-kill-emacs-server ()
  "Interactively save buffers and then kill the Emacs server.
If the user cancels the save prompt, the server is NOT killed.
This function is designed to be called from `emacsclient -c`."
  (interactive)
  ;; Bring this frame to the front so the user sees the prompts.
  (select-frame-set-input-focus (selected-frame))

  ;; This is the key: `save-some-buffers` is interactive. It will prompt
  ;; the user for each unsaved file. It returns `t` if the user cancels.
  (if (save-some-buffers)
      ;; If the user did NOT cancel the save prompts...
      (progn
        ;; Double-check that no modified buffers remain.
        (if (not (modified-buffers-exist-p))
            ;; If all buffers are clean, it's safe to kill the server.
            (kill-emacs)
          ;; If some buffers are still modified (e.g., read-only),
          ;; inform the user and abort the shutdown.
          (message "Shutdown aborted: Some buffers are still modified.")))
    ;; If the user explicitly cancelled the save prompt, abort.
    (message "Shutdown cancelled by user.")))

(provide 'session-exit)
