;;; -*- lexical-binding: t; -*-

(provide 'mongodb-database)

(require 'mongodb-shell)
(require 'mongodb-collection)

(defvar-local mongodb-database-current nil)
(defvar-local mongodb-database-connect-buffer nil)

(defun mongodb-database--use-database (db-name)
  (interactive
   (list (completing-read "View database: " (mongodb-shell-list-database-names mongodb-shell-process))))
  (mongodb-view-database mongodb-shell-process db-name))

(defun mongodb-view-database (mongo-shell db-name)
  (let ((prev-buffer (current-buffer)))
    (switch-to-buffer
     (get-buffer-create (format "mongodb-database: %s/%s" (mongodb-shell-uri mongo-shell) db-name)))
    (mongodb-database-mode)
    (read-only-mode -1)
    (setq display-line-numbers nil)
    (erase-buffer)
    (setq-local mongodb-database-connect-buffer prev-buffer)
    (setq-local mongodb-shell-process mongo-shell)
    (setq-local mongodb-database-current db-name)
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
             (magit-insert-section (mongodb-collection-section coll)
               (insert (propertize coll 'face 'magit-branch-remote) "\n")))
           colls))))
    (read-only-mode)))

(defun mongodb-database--drop ()
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to drop the %S database?" mongodb-database-current))
      (progn
        (mongodb-shell-command
         mongodb-shell-process
         (format "db.getSiblingDB(%S).dropDatabase()" mongodb-database-current))
        (message "Database %S dropped." mongodb-database-current)
        (mongodb-database-quit))
    (message "Drop cancelled.")))

(defun mongodb-database--run-command ()
  (interactive)
  (mongodb-shell-command mongodb-shell-process (format "use %s" mongodb-database-current))

  (mongodb-query-input
   "database command"
   mongodb-shell-process
   (lambda (command)
     (mongodb-shell-command mongodb-shell-process (format "db.runCommand(%s)" command)))
   :no-cursor t))

(defun mongodb-database--view-collection-at-point ()
  (interactive)
  (when-let ((coll (magit-section-value-if 'mongodb-collection-section)))
    (mongodb-view-collection mongodb-shell-process mongodb-database-current coll)))

(defun mongodb-database--drop-collection-at-point ()
  (interactive)
  (when-let ((coll (magit-section-value-if 'mongodb-collection-section)))
    (when (y-or-n-p (format "Are you sure you want to drop the %S collection?" coll))
      (mongodb-shell-drop-collection mongodb-shell-process mongodb-database-current coll)
      (mongodb-database-refresh t))))

(defun mongodb-database--create-collection (&optional args)
  (interactive (list (transient-args 'mongodb-database-create-collection-transient)))
  (let ((name (read-string "Collection name: ")))
    (mongodb-shell-create-collection
     mongodb-shell-process
     mongodb-database-current
     name
     (mongodb-args-to-document args))
    (mongodb-view-collection mongodb-shell-process mongodb-database-current name)))

(defun mongodb-database-refresh (&optional silent)
  (interactive)
  (when (not silent)
    (message "refreshing..."))
  (mongodb-view-database mongodb-shell-process mongodb-database-current)
  (when (not silent)
    (message "refreshing...done")))

(defun mongodb-database-quit ()
  (interactive)
  (if mongodb-database-connect-buffer
      (let ((prev-buffer (current-buffer)))
        (switch-to-buffer mongodb-database-connect-buffer)
        (kill-buffer prev-buffer))
    (quit-window t)))

(define-transient-command mongodb-database-dispatch ()
  "Database operations"
  ["Database operations"
   ("c" "View a collection" mongodb-collection--use-collection)
   ("C" "Create a collection" mongodb-database-create-collection-transient)
   ("r" "Run a database command" mongodb-database--run-command)
   ("d" "View another database" mongodb-database--use-database)
   ("X" "Drop this database" mongodb-database--drop)
   ("gr" "refresh" mongodb-database-refresh)])

(define-transient-command mongodb-database-create-collection-transient ()
  "createCollection command"
  ["Command Options"
   ("w" "Write concern" "writeConcern=")]
  ["Collection Options"
   ("c" mongodb-database-create-collection-capped)
   ("e" "Expire after seconds" "expireAfterSeconds=")
   ("s" "Size" "size=")
   ("M" "Max" "max=")
   ("V" "View on" "viewOn=")
   ("p" "Aggregation pipeline used to create the view" "pipeline=")]
  ["Validation options"
   ("vd" "Validator document" "validator=")
   ("va" mongodb-database-create-collection-validation-action)
   ("vl" mongodb-database-create-collection-validation-level)]
  ["Create Collection"
   ("C" "Create a collection with the provided options" mongodb-database--create-collection)])

(transient-define-argument mongodb-database-create-collection-timeseries-granularity ()
  :description "Granularity (default \"seconds\")"
  :class 'transient-switches
  :key "g"
  :argument-format "timeseries.granularity=%s"
  :argument-regexp "timeseries.granularity=\\(\"seconds\"\\|\"minutes\"\\|\"hours\"\\)"
  :choices '("seconds" "minutes" "hours"))

(transient-define-argument mongodb-database-create-collection-validation-action ()
  :description "Validation action (default \"error\")"
  :class 'transient-switches
  :key "va"
  :argument-format "validationAction=%s"
  :argument-regexp "validationAction=\\(\"error\"\\|\"warn\"\\)"
  :choices '("\"error\"" "\"warn\""))

(transient-define-argument mongodb-database-create-collection-validation-level ()
  :description "Validation level (default \"strict\")"
  :class 'transient-switches
  :key "vl"
  :argument-format "validationLevel=%s"
  :argument-regexp "validationLevel=\\(\"off\"\\|\"strict\"\\|\"moderate\")"
  :choices '("\"off\"" "\"moderate\"" "\"strict\""))

(transient-define-argument mongodb-database-create-collection-capped ()
  :description "Capped (default false)"
  :class 'transient-switches
  :key "c"
  :argument-format "capped=%s"
  :argument-regexp "capped=\\(true\\|false\\)"
  :choices '("true" "false"))

(defvar mongodb-database-mode-map nil "Keymap for MongoDB database buffers")

(progn
  (setq mongodb-database-mode-map (make-sparse-keymap))

  (when (require 'evil nil t)
    (evil-define-key 'normal mongodb-database-mode-map
      "?" 'mongodb-database-dispatch
      "r" 'mongodb-database--run-command
      "c" 'mongodb-collection--use-collection
      "C" 'mongodb-database-create-collection-transient
      "d" 'mongodb-database--use-database
      "x" 'mongodb-database--drop-collection-at-point
      "X" 'mongodb-database--drop
      "q" 'mongodb-database-quit
      "gr" 'mongodb-database-refresh
      (kbd "<RET>") 'mongodb-database--view-collection-at-point))
  (define-key mongodb-database-mode-map (kbd "r") 'mongodb-database--run-command)
  (define-key mongodb-database-mode-map (kbd "c") 'mongodb-collection--use-collection)
  (define-key mongodb-database-mode-map (kbd "C") 'mongodb-database-create-collection-transient)
  (define-key mongodb-database-mode-map (kbd "d") 'mongodb-database--use-database)
  (define-key mongodb-database-mode-map (kbd "x") 'mongodb-database--drop-collection-at-point)
  (define-key mongodb-database-mode-map (kbd "X") 'mongodb-database--drop)
  (define-key mongodb-database-mode-map (kbd "q") 'mongodb-database-quit)
  (define-key mongodb-database-mode-map (kbd "gr") 'mongodb-database-refresh)
  (define-key mongodb-database-mode-map (kbd "<RET>") 'mongodb-database--view-collection-at-point)
  (define-key mongodb-database-mode-map (kbd "?") 'mongodb-database-dispatch)
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
