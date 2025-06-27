;; respects full and relative folder paths but must end with /
(defvar roip/inspekto-sync-project-root
  (let ((env (getenv "INSPEKTO_SYNC_PROJECT_ROOT")))
    (when env (expand-file-name env)))
  "Root directory of the Inspekto project, or nil if not set in the INSPEKTO_SYNC_PROJECT_ROOT environment variable.")

;; respects full and ssh and but must end with /
(defvar roip/inspekto-sync-target-root
  (let ((env (getenv "INSPEKTO_SYNC_TARGET_ROOT")))
    (when env (expand-file-name env)))
  "Target directory for rsync operations, or nil if not set in the INSPEKTO_SYNC_TARGET_ROOT environment variable.")

(defun roip/inspekto-sync-config-valid-p ()
  (cond
   ((not roip/inspekto-sync-project-root)
    (message "[roip/inspekto-sync] Error Missing `roip/inspekto-sync-project-root` or `INSPEKTO_SYNC_PROJECT_ROOT`.")
    nil)
   ((not roip/inspekto-sync-target-root)
    (message "[roip/inspekto-sync] Error Missing `roip/inspekto-sync-target-root` or `INSPEKTO_SYNC_TARGET_ROOT`.")
    nil)
   (t t)))

(defun roip/inspekto-rsync ()
  "Rsync inspekto project to dupspekto, async.
Also prints shell output and completion message to *Messages*."
  (interactive)
  (when (roip/inspekto-sync-config-valid-p)
    (message "[roip/inspekto-rsync] Save hook triggered")
    (let* ((project (project-current))
           (project-root (when project (expand-file-name (project-root project))))
           (sync-project-root roip/inspekto-sync-project-root)
	   (sync-target-root roip/inspekto-sync-target-root))
      (message "[roip/inspekto-rsync] Project root: %s" project-root)
      (message "[roip/inspekto-rsync] Sync project root: %s" sync-project-root)
      (when (and project-root (string= project-root sync-project-root))
	(let* ((command (format "rsync -avz --delete %s --exclude=\"*/__pycache__\" --exclude=\"*.pyc\" --exclude=\"build/\" %s"
				sync-project-root
				sync-target-root))
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
               (message "[roip/inspekto-rsync] Async command finished.")))))))))

;;; the following adds a buffer local minor mode that start a buffer local after-save-hook
;;;###autoload
(define-minor-mode roip/inspekto-sync-mode
  "Minor mode to auto-rsync after saving in the Inspekto project."
  :lighter " sinspekto"
  (when (roip/inspekto-sync-config-valid-p)
    (if roip/inspekto-sync-mode
	(progn
          (add-hook 'after-save-hook #'roip/inspekto-rsync nil t)
          (message "[roip/inspekto-sync-mode] Enabled"))
      (remove-hook 'after-save-hook #'roip/inspekto-rsync t) ;; The t means buffer-local
      (message "[roip/inspekto-sync-mode] Disabled"))))

(defun roip/enable-inspekto-sync-if-in-project ()
  "Enable `roip/inspekto-sync-mode` if in the Inspekto project."
  (when (roip/inspekto-sync-config-valid-p)
    (when-let ((project (project-current))
               (project-root (expand-file-name (project-root project)))
               (sync-project-root (expand-file-name roip/inspekto-sync-project-root)))
      (when (string= project-root sync-project-root)
	(message "[roip/maybe-enable-inspekto-sync] Enabling inspekto sync mode"
		 (roip/inspekto-sync-mode 1))))))

(provide 'roip-inspekto-sync)

;;; inspekto-sync.el ends here
