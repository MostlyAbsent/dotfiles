(require 'cl-lib)

(defun jtt/interactive-kill-emacs-server ()
  "Interactively save buffers and then kill the Emacs server.
If the user cancels the save prompt, the server is NOT killed.
This function is designed to be called from `emacsclient -c`."
  (interactive)
  ;; Bring this frame to the front so the user sees the prompts.
  (select-frame-set-input-focus (selected-frame))

  (save-buffers-kill-emacs))

(provide 'session-exit)
