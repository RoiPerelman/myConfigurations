(defun roip/inspekto-rsync ()
  "Rsync inspekto project to dupspekto, async.
Also prints shell output and completion message to *Messages*."
  (interactive)
  (message "[roip/inspekto-rsync] Save hook triggered")
  (let* ((project (project-current))
         (project-root (when project (expand-file-name (project-root project))))
         (target-root (expand-file-name "~/sinspekto/inspekto/")))
    (when (and project-root (string= project-root target-root))
      (let* ((command "rsync -avz --delete ~/sinspekto/inspekto/ ~/sinspekto/dupspekto/")
             (proc (start-process-shell-command
                    "inspekto-proc" nil command)))
	(message "[roip/inspekto-rsync] Starting async command: %s" command)
	(message "[roip/inspekto-rsync] %s" proc)
	(set-process-filter
	 proc
	 (lambda (_proc output)
	   (message "[roip/inspekto-rsync] %s" output)))
        (set-process-sentinel
         proc
         (lambda (_proc event)
           (when (string= event "finished\n")
             (message "[roip/inspekto-rsync] Async command finished."))))))))

;;; the following adds a buffer local minor mode that start a buffer local after-save-hook
;;;###autoload
(define-minor-mode roip/inspekto-sync-mode
  "Minor mode to auto-rsync after saving in the Inspekto project."
  :lighter " sinspekto"
  (if roip/inspekto-sync-mode
      (progn
        (add-hook 'after-save-hook #'roip/inspekto-rsync nil t)
        (message "[roip/inspekto-sync-mode] Enabled"))
    (remove-hook 'after-save-hook #'roip/inspekto-rsync t) ;; The t means buffer-local
    (message "[roip/inspekto-sync-mode] Disabled")))

(defun roip/enable-inspekto-sync-if-in-project ()
  "Enable `roip/inspekto-sync-mode` if in the Inspekto project."
  (when-let ((project (project-current))
             (project-root (expand-file-name (project-root project)))
             (target-root (expand-file-name "~/sinspekto/inspekto/")))
    (when (string= project-root target-root)
      (message "[roip/maybe-enable-inspekto-sync] Enabling inspekto sync mode"
      (roip/inspekto-sync-mode 1)))))

;; (add-hook 'find-file-hook #'roip/maybe-enable-inspekto-sync)

(provide 'roip-inspekto-sync)


;;; inspekto-sync.el ends here
