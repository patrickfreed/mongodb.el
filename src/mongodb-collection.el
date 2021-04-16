;;; -*- lexical-binding: t; -*-

(provide 'mongodb-collection)

(require 'mongodb-shell)

(defvar-local mongodb-namespace-current nil)

(defun mongodb-view-collection (mongo-shell db-name coll-name)
  (message "in here")
  (switch-to-buffer
   (get-buffer-create (format "mongodb-collection: %s/%s.%s" (mongodb-shell-uri mongo-shell) db-name coll-name)))
  (mongodb-collection-mode)
  (message "here1")
  (read-only-mode -1)
  (setq display-line-numbers nil)
  (erase-buffer)
  (setq-local mongodb-shell-process mongo-shell)
  (setq-local mongodb-namespace-current (format "%s.%s" db-name coll-name))
  (message "here2")
  (magit-insert-section (mongodb-collection-buffer-section)
    (magit-insert-section (mongodb-collection-info-section)
      (mongodb--insert-header-line "Namespace" (propertize mongodb-namespace-current 'face 'magit-branch-local))
      (mongodb--insert-header-line "Connection String" (mongodb-shell-uri mongodb-shell-process))
      (mongodb--insert-header-line "MongoDB Server Version" (mongodb-shell-server-version mongodb-shell-process))
      (mongodb--insert-header-line "MongoDB Shell Version" (mongodb-shell-shell-version mongodb-shell-process))
      (mongodb--insert-header-line "Topology" (mongodb-shell-topology-type mongodb-shell-process)))
    (newline)
    (message "here3")
    (magit-insert-section (mongodb-collection-documents)
      (magit-insert-heading
        (propertize "Documents" 'face 'magit-section-heading)
        (propertize (format " (%d)" (mongodb-shell-collection-count mongodb-shell-process db-name coll-name))))
      (let* ((result (mongodb-shell-find mongodb-shell-process db-name coll-name "{}"))
             (first-batch (car result))
             (cursor-id (cdr result)))
        (seq-do
         (lambda (doc)
           (magit-insert-section (mongodb-collection-document doc t)
             (if (> (length doc) (window-width))
                 (progn
                   (magit-insert-heading
                     (propertize "Large documents hidden by default, press <TAB> to expand." 'face 'shadow))
                   (magit-insert-section (mongodb-collection-document-more doc t)
                     (insert (mongodb-document-string doc) "\n")))
               (insert (mongodb-document-string doc) "\n"))))
         first-batch)
        (when cursor-id
          (insert-text-button "Type + to show more results")))))
  (read-only-mode))

(defun mongodb-document-string (doc)
  (with-temp-buffer
    (javascript-mode)
    (insert doc)
    (font-lock-fontify-region (point-min) (point-max))
    (buffer-string)))

;; (define-transient-command mongodb-collection-dispatch ()
;;   "Database operations"
;;   ["Database operations"
;;    ("r" "Run a database command" mongodb-database--run-command)
;;    ("d" "View another database" mongodb-database--use-database)
;;    ("D" "Drop this database" mongodb-database--drop)])

(defvar mongodb-collection-mode-map nil "Keymap for MongoDB collection buffers")

(progn
  (setq mongodb-collection-mode-map (make-sparse-keymap))

  ;; (when (require 'evil nil t)
    ;; (evil-define-key 'normal mongodb-collection-mode-map
    ;;   ;; "?" 'mongodb-collection-dispatch
    ;;   ;; "c" 'mongodb-collection--use-collection
    ;;   ;; "D" 'mongodb-collection--drop
    ;;   ))
  ;; (define-key mongodb-collection-mode-map (kbd "c") 'mongodb-collection--use-collection)
  ;; (define-key mongodb-collection-mode-map (kbd "D") 'mongodb-collection--drop)
  ;; (define-key mongodb-collection-mode-map (kbd "?") 'mongodb-collection-dispatch)
  )

(define-derived-mode
  mongodb-collection-mode
  mongodb-base-mode
  "MongoDB Collection"
  "Major mode for MongoDB collection operations")
