;;; -*- lexical-binding: t; -*-

(provide 'mongodb-database)

(require 'mongodb-shell)

(defun mongodb-view-database (mongo-shell db-name)
  (message "%S" mongo-shell)
  (switch-to-buffer
   (get-buffer-create (format "mongodb-database: %s/%s" (mongodb-shell-uri mongo-shell) db-name)))
  (mongodb-database-mode)
  (read-only-mode -1)
  (setq display-line-numbers nil)
  (erase-buffer)
  (message "%S %S" mongo-shell db-name)
  (setq-local mongodb-shell-process mongo-shell)
  (magit-insert-section (mongodb-database-buffer-section)
    (magit-insert-section (mongodb-database-info-section)
      (mongodb--insert-header-line "Database Name" (propertize db-name 'face 'magit-branch-local))
      (mongodb--insert-header-line "Connection String" (mongodb-shell-uri mongodb-shell-process))
      (mongodb--insert-header-line "MongoDB Server Version" (mongodb-shell-server-version mongodb-shell-process))
      (mongodb--insert-header-line "MongoDB Shell Version" (mongodb-shell-shell-version mongodb-shell-process))
      (mongodb--insert-header-line "Topology" (mongodb-shell-topology-type mongodb-shell-process)))
    (newline)
    (magit-insert-section (mongodb-database-collections)
      (let ((colls (mongodb-shell-list-collections mongo-shell db-name)))
        (magit-insert-heading
          (propertize "Collections" 'face 'magit-section-heading)
          (propertize (format " (%d)" (length colls))))
        (seq-do
         (lambda (coll)
           (insert (propertize coll 'face 'magit-branch-remote) "\n"))
         colls))))
  (read-only-mode))

(defvar mongodb-database-mode-map nil "Keymap for MongoDB database buffers")

(progn
  (setq mongodb-database-mode-map (make-sparse-keymap))

  ;; (when (require 'evil nil t)
  ;;   (evil-define-key 'normal evg-view-patch-mode-map
  ;;     (kbd "<RET>") 'evg-view-task-at-point
  ;;     "r" 'evg-view-patch-refresh
  ;;     "d" 'evg-switch-task-format
  ;;     (kbd "M-j") 'evg-goto-next-task-failure
  ;;     (kbd "M-k") 'evg-goto-previous-task-failure
  ;;     evg-back-key 'evg-back))
  ;; (define-key evg-view-patch-mode-map (kbd "<RET>") 'evg-view-task-at-point)
  ;; (define-key evg-view-patch-mode-map (kbd "r") 'evg-view-patch-refresh)
  ;; (define-key evg-view-patch-mode-map (kbd "d") 'evg-switch-task-format)
  ;; (define-key evg-view-patch-mode-map (kbd "M-n") 'evg-goto-next-task-failure)
  ;; (define-key evg-view-patch-mode-map (kbd "M-p") 'evg-goto-previous-task-failure)
  ;; (define-key evg-view-task-mode-map evg-back-key 'evg-back)
  )

(define-derived-mode
  mongodb-database-mode
  mongodb-base-mode
  "MongoDB Database"
  "Major mode for MongoDB database operations")
