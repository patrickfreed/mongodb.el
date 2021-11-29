;;; -*- lexical-binding: t; -*-

(provide 'mongodb-collection)

(require 'mongodb-shell)
(require 'mongodb-query)
(require 'mongodb-cursor)

(require 'seq)

(defconst mongodb-collection-preview-batch-size 20)

(defvar-local mongodb-collection-current nil)
(defvar-local mongodb-database-current nil)
(defvar-local mongodb-collection-prev-buffer nil)
(defvar-local mongodb-collection-info nil)
(defvar-local mongodb-collection-documents '())
(defvar-local mongodb-collection-cursor nil)
(defvar-local mongodb-collection-preview-count mongodb-collection-preview-batch-size)
(defvar-local mongodb-collection-point-start nil)


(cl-defstruct mongodb-collection-doc
  doc
  collapsed)

(defun mongodb-collection-doc-new (doc)
  (make-mongodb-collection-doc
   :doc doc
   :collapsed (with-temp-buffer
                 (insert doc)
                 (> (count-lines (point-min) (point-max)) 25))))

(defun mongodb-view-collection (mongo-shell db-name coll-name &optional documents)
  (let ((starting-point mongodb-collection-point-start))
    (let ((buffer-name (format "mongodb-collection: %s/%s.%s" (mongodb-shell-uri mongo-shell) db-name coll-name)))
      (if (not (string= buffer-name (buffer-name)))
          (let ((prev-buffer (current-buffer)))
            (switch-to-buffer
             (get-buffer-create buffer-name))
            (mongodb-collection-mode)
            (setq display-line-numbers nil)
            (setq-local mongodb-collection-prev-buffer prev-buffer)))
      (setq starting-point (point)))
    (read-only-mode -1)
    (erase-buffer)
    (setq-local mongodb-shell-process mongo-shell)
    (setq-local mongodb-database-current db-name)
    (setq-local mongodb-collection-current coll-name)
    (setq-local mongodb-collection-info (mongodb-collection--get-info))
    (if (eq documents 'refresh)
        (progn
          (when mongodb-collection-cursor
            (mongodb-cursor-close mongodb-collection-cursor))
          (setq-local mongodb-collection-cursor nil)
          (setq-local mongodb-collection-documents '()))
      (setq-local mongodb-collection-documents (or documents '())))
    (when (< (length mongodb-collection-documents) mongodb-collection-preview-count)
      (when (not mongodb-collection-cursor)
        (setq-local mongodb-collection-cursor (mongodb-shell-find-cursor mongodb-shell-process db-name coll-name "{}")))
      (let ((i (length mongodb-collection-documents)))
        (while (and (mongodb-cursor-has-next-p mongodb-collection-cursor) (< i mongodb-collection-preview-count))
          (setq mongodb-collection-documents
                (append mongodb-collection-documents
                        (list (mongodb-collection-doc-new (mongodb-cursor-next mongodb-collection-cursor)))))
          (setq i (1+ i)))))

    (magit-insert-section (mongodb-collection-buffer-section)
      (magit-insert-section (mongodb-collection-info-section)
        (mongodb--insert-header-line "Database Name" (propertize mongodb-database-current 'face 'magit-branch-local))
        (mongodb--insert-header-line "Collection Name"
                                     (propertize mongodb-collection-current 'face 'magit-branch-remote))
        (mongodb--insert-header-line "Type" (alist-get 'type mongodb-collection-info))
        (mongodb--insert-header-line "Connection String" (mongodb-shell-uri mongodb-shell-process))
        (mongodb--insert-header-line "MongoDB Server Version" (mongodb-shell-server-version mongodb-shell-process))
        (mongodb--insert-header-line "MongoDB Shell Version" (mongodb-shell-shell-version mongodb-shell-process))
        (mongodb--insert-header-line "Topology" (mongodb-shell-topology-type mongodb-shell-process)))
      (newline)

      (setq-local mongodb-collection-point-start (point))

      ;; insert options information
      (magit-insert-section (mongodb-collection-options)
        (magit-insert-heading
          (propertize "Options" 'face 'magit-section-heading)
          (propertize (format " (%d)" (hash-table-count (alist-get 'options mongodb-collection-info)))))
        (maphash
         (lambda (k v) (mongodb--insert-property k (mongodb-document-string (json-encode v)) 1))
         (alist-get 'options mongodb-collection-info)))
      (newline)

      ;; insert index information
      (when (not (string= (alist-get 'type mongodb-collection-info) "view"))
        (let ((indexes (mongodb-shell-list-indexes mongodb-shell-process db-name coll-name)))
          (magit-insert-section (mongodb-collection-indexes)
            (magit-insert-heading
              (propertize "Indexes" 'face 'magit-section-heading)
              (propertize (format " (%d)" (length indexes))))
            (seq-do
             (lambda (index)
               (magit-insert-section (mongodb-collection-index index t)
                 (insert (mongodb-document-string index) "\n")))
             indexes)))
        (newline))

      ;; insert document preview
      (magit-insert-section (mongodb-collection-documents)
        (let ((document-count (mongodb-shell-collection-count mongodb-shell-process db-name coll-name)))
          (magit-insert-heading
            (propertize "Documents" 'face 'magit-section-heading)
            (propertize (format " (%d)" document-count)))
          (seq-do
           (lambda (doc)
             (magit-insert-section (mongodb-collection-document doc t)
               (insert
                (with-temp-buffer
                  (if (not (mongodb-collection-doc-collapsed doc))
                      (insert (mongodb-document-string (mongodb-collection-doc-doc doc)) "\n")
                    (insert "{ " (propertize "...Document contents collapsed, press <TAB> to expand" 'face 'shadow) " }\n"))
                  (buffer-string)))))
           mongodb-collection-documents)
          (when (> document-count mongodb-collection-preview-count)
            (insert-text-button "Type + to preview more documents")))))
    (newline 2)
    (goto-char starting-point)
    (read-only-mode)))

(defun mongodb-collection--get-info ()
  (mongodb-shell-command mongodb-shell-process (concat "use " mongodb-database-current))
  (let* ((info-json (mongodb-shell-command
                     mongodb-shell-process
                     (format "JSON.stringify(db.getCollectionInfos({ \"name\": %S })[0]);" mongodb-collection-current)))
         (info-map (json-parse-string info-json)))
    (list (cons 'type (gethash "type" info-map))
          (cons 'options (gethash "options" info-map)))))

(defun mongodb-collection-toggle-at-point ()
  (interactive)
  (let* ((section (magit-current-section))
         (section-type (car (car (magit-section-ident section)))))
    (if (eq 'mongodb-collection-document section-type)
        (mongodb-collection-toggle-doc-at-point)
      (magit-section-toggle section))))

(defun mongodb-collection--set-expanded-at-point (expanded)
  (let* ((section (magit-current-section)))
    (when-let (((magit-section-match 'mongodb-collection-document))
               (doc (cdr (car (magit-section-ident section)))))
      (setf (mongodb-collection-doc-abbreviated doc) (not expanded))
      (when expanded
        (message "document contents expanded, press '-' to collapse"))
      (let ((p (point)))
        (mongodb-view-collection mongodb-shell-process mongodb-database-current mongodb-collection-current mongodb-collection-documents)
        (goto-char p)))))

(defun mongodb-collection--preview-more ()
  (interactive)
  (setq-local mongodb-collection-preview-count (+ mongodb-collection-preview-count mongodb-collection-preview-batch-size))
  (message "previewing %s more documents..." mongodb-collection-preview-count)
  (mongodb-view-collection mongodb-shell-process mongodb-database-current mongodb-collection-current mongodb-collection-documents)
  (message "previewing %s more documents...done" mongodb-collection-preview-batch-size))

(defun mongodb-collection-expand-at-point ()
  (interactive)
  (mongodb-collection--set-expanded-at-point t))

(defun mongodb-collection-hide-at-point ()
  (interactive)
  (mongodb-collection--set-expanded-at-point nil))

(defun mongodb-collection-toggle-doc-at-point ()
  (interactive)
  (let* ((section (magit-current-section))
         (doc (cdr (car (magit-section-ident section)))))
    (setf (mongodb-collection-doc-collapsed doc) (not (mongodb-collection-doc-collapsed doc)))
    (let ((p (point)))
      (mongodb-view-collection mongodb-shell-process mongodb-database-current mongodb-collection-current mongodb-collection-documents)
      (goto-char p))))

(defun mongodb-collection-refresh (&optional silent)
  (interactive)
  (when (not silent)
    (message "refreshing..."))
  (mongodb-view-collection mongodb-shell-process mongodb-database-current mongodb-collection-current 'refresh)
  (when (not silent)
    (message "refreshing...done")))

(defun mongodb-document-string (doc)
  (with-temp-buffer
    (javascript-mode)
    (insert doc)
    (font-lock-fontify-region (point-min) (point-max))
    (buffer-string)))

(defun mongodb-collection--use-collection (coll-name)
  (interactive
   (list (completing-read "View collection: "
                          (mongodb-shell-list-collections mongodb-shell-process mongodb-database-current))))
  (mongodb-view-collection mongodb-shell-process mongodb-database-current coll-name))

(defun mongodb-collection--find (&optional args)
  (interactive (list (transient-args 'mongodb-collection-find-transient)))
  (let ((shell-process mongodb-shell-process)
        (db mongodb-database-current)
        (coll mongodb-collection-current))
    (let ((pairs (seq-map (lambda (kvp) (split-string kvp "=")) args)))
      (mongodb-query-input
       "find filter"
       shell-process
       db
       coll
       (lambda (filter)
         (mongodb-shell-find-build coll filter pairs))
       (lambda (filter)
         (mongodb-shell-find-cursor shell-process db coll filter pairs))))))

(defun mongodb-collection--aggregate (&optional args)
  (interactive (list (transient-args 'mongodb-collection-aggregate-transient)))
  (let ((shell-process mongodb-shell-process)
        (db mongodb-database-current)
        (coll mongodb-collection-current))
    (let ((pairs (seq-map (lambda (kvp) (split-string kvp "=")) args)))
      (mongodb-query-input
       "aggregation pipeline"
       shell-process
       db
       coll
       (lambda (pipeline)
         (mongodb-shell-aggregate-build coll pipeline pairs))
       (lambda (pipeline)
         (mongodb-shell-aggregate-cursor shell-process db coll pipeline pairs))
       :input-type 'array))))

(defun mongodb-args-to-document (args)
  (concat
   "{"
   (string-join
    (seq-map (lambda (kvp)
               (let ((parts (split-string kvp "=")))
                 (format "%S: %s" (car parts) (cadr parts))))
             args)
    ",")
   "}"))

(defun mongodb-collection--insert-one (&optional args)
  (interactive (list (transient-args 'mongodb-collection-insert-one-transient)))
  (let ((shell-process mongodb-shell-process)
        (db mongodb-database-current)
        (coll mongodb-collection-current)
        (buf (current-buffer))
        (args (mongodb-args-to-document args)))
    (message "inserting into collection %s" mongodb-collection-current)
    (mongodb-query-input
     "document to insert"
     shell-process
     db
     coll
     (lambda (doc) (mongodb-shell-insert-one-build coll doc args))
     (lambda (doc)
       (let ((result (mongodb-shell-insert-one shell-process db coll doc args)))
         (with-current-buffer buf
           (mongodb-collection-refresh t))
         result))
     :no-cursor t)))

(defun mongodb-collection--insert-many (&optional args)
  (interactive (list (transient-args 'mongodb-collection-insert-many-transient)))
  (let ((shell-process mongodb-shell-process)
        (db mongodb-database-current)
        (coll mongodb-collection-current)
        (buf (current-buffer))
        (args (mongodb-args-to-document args)))
    (mongodb-query-input
     "documents to insert"
     shell-process
     db
     coll
     (lambda (docs) (mongodb-shell-insert-many-build coll docs args))
     (lambda (docs)
       (let ((result
              (mongodb-shell-insert-many shell-process db coll docs args)))
         (with-current-buffer buf
           (mongodb-collection-refresh t))
         result))
     :no-cursor t
     :input-type 'array)))

(defun mongodb-collection--update (update-fun update-build-fun &optional args)
  (let ((shell-process mongodb-shell-process)
        (db mongodb-database-current)
        (coll mongodb-collection-current)
        (buf (current-buffer))
        (args (mongodb-args-to-document args)))
    (message "updating collection %s" mongodb-collection-current)
    (mongodb-query-input
     "filter and update documents"
     shell-process
     db
     coll
     (lambda (filter-update) (funcall update-build-fun coll filter-update args))
     (lambda (filter-update)
       (let ((result (funcall update-fun shell-process db coll filter-update args)))
         (with-current-buffer buf
           (mongodb-collection-refresh t))
         result))
     :no-cursor t
     :num-inputs 2
     :headings '("Filter document" "Update document"))))


(defun mongodb-collection--update-one (&optional args)
  (interactive (list (transient-args 'mongodb-collection-update-one-transient)))
  (mongodb-collection--update 'mongodb-shell-update-one 'mongodb-shell-update-one-build args))

(defun mongodb-collection--update-many (&optional args)
  (interactive (list (transient-args 'mongodb-collection-update-many-transient)))
  (mongodb-collection--update 'mongodb-shell-update-many 'mongodb-shell-update-many-build args))

(defun mongodb-collection--replace-one (&optional args)
  (interactive (list (transient-args 'mongodb-collection-replace-one-transient)))
  (let ((shell-process mongodb-shell-process)
        (db mongodb-database-current)
        (coll mongodb-collection-current)
        (buf (current-buffer))
        (args (mongodb-args-to-document args)))
    (mongodb-query-input
     "filter and replacement documents"
     shell-process
     db
     coll
     (lambda (filter-replacement) (mongodb-shell-replace-one-build coll filter-replacement args))
     (lambda (filter-replacement)
       (let ((result
              (mongodb-shell-replace-one shell-process db coll filter-replacement args)))
         (with-current-buffer buf
           (mongodb-collection-refresh t))
         result))
     :no-cursor t
     :num-inputs 2
     :headings '("Filter document" "Replacement document"))))

(defun mongodb-collection--delete-one (&optional args)
  (interactive (list (transient-args 'mongodb-collection-delete-one-transient)))
  (let ((shell-process mongodb-shell-process)
        (db mongodb-database-current)
        (coll mongodb-collection-current)
        (buf (current-buffer))
        (args (mongodb-args-to-document args)))
    (mongodb-query-input
     "Enter a filter document"
     shell-process
     db
     coll
     (lambda (filter) (mongodb-shell-delete-one-build coll filter args))
     (lambda (filter)
       (let ((result (mongodb-shell-delete-one shell-process db coll filter args)))
         (with-current-buffer buf
           (mongodb-collection-refresh t))
         result))
     :no-cursor t)))

(defun mongodb-collection--delete-many (&optional args)
  (interactive (list (transient-args 'mongodb-collection-delete-one-transient)))
  (let ((shell-process mongodb-shell-process)
        (db mongodb-database-current)
        (coll mongodb-collection-current)
        (buf (current-buffer))
        (args (mongodb-args-to-document args)))
    (mongodb-query-input
     "Enter a filter document"
     shell-process
     db
     coll
     (lambda (filter) (mongodb-shell-delete-many-build coll filter args))
     (lambda (filter)
       (let ((result (mongodb-shell-delete-many shell-process db coll filter args)))
         (with-current-buffer buf
           (mongodb-collection-refresh t))
         result))
     :no-cursor t)))

(defun mongodb-collection--create-index (&optional args)
  (interactive (list (transient-args 'mongodb-collection-create-index-transient)))
  (let ((shell-process mongodb-shell-process)
        (db mongodb-database-current)
        (coll mongodb-collection-current)
        (buf (current-buffer))
        (args (mongodb-args-to-document args)))
    (mongodb-query-input
     "Enter the index keys document"
     shell-process
     db
     coll
     (lambda (keys) (mongodb-shell-create-index-build coll keys args))
     (lambda (keys)
       (let ((result (mongodb-shell-create-index shell-process db coll keys args)))
         (with-current-buffer buf
           (mongodb-collection-refresh t))
         result))
     :no-cursor t)))

(defun mongodb-collection--drop (&optional args)
  (interactive (list (transient-args 'mongodb-collection-drop-transient)))
  (let ((shell-process mongodb-shell-process)
        (db mongodb-database-current)
        (coll mongodb-collection-current)
        (buf (current-buffer)))
    (mongodb-shell-drop-collection shell-process db coll (mongodb-args-to-document args))
    (mongodb-collection-quit)))

(defun mongodb-collection--back ()
  (interactive)
  (let ((old-buf (current-buffer)))
    (mongodb-database--use-database mongodb-database-current)
    (kill-buffer old-buf)))

(defun mongodb-collection-quit ()
  (interactive)
  (if mongodb-collection-prev-buffer
      (let ((prev-buffer (current-buffer)))
        (switch-to-buffer mongodb-collection-prev-buffer)
        (kill-buffer prev-buffer))
    (quit-window t)))

(define-transient-command mongodb-collection-dispatch ()
  "Collection operations"
  ["General"
   ("C" "View another collection" mongodb-collection--use-collection)
   ("<" "Go back to database" mongodb-collection--back)
   ("gr" "Refresh" mongodb-collection-refresh)]
  ["Write operations"
   ("i" "insertOne" mongodb-collection-insert-one-transient)
   ("I" "insertMany" mongodb-collection-insert-many-transient)
   ("u" "updateOne" mongodb-collection-update-one-transient)
   ("U" "updateMany" mongodb-collection-update-many-transient)
   ("r" "replaceOne" mongodb-collection-replace-one-transient)
   ("d" "deleteOne" mongodb-collection-delete-one-transient)
   ("D" "deleteMany" mongodb-collection-delete-many-transient)
   ("c" "createIndex" mongodb-collection-create-index-transient)
   ("X" "drop" mongodb-collection-drop-transient)]
  ["Read Operations"
   ("f" "find" mongodb-collection-find-transient)
   ("a" "aggregate" mongodb-collection-aggregate-transient)])

(define-transient-command mongodb-collection-find-transient ()
  "Find command"
  ["Options"
   ("l" "Limit number of documents" "limit=")
   ("s" "Skip number of documents" "skip=")
   ("o" "Order the documents by" "sort=")
   ("p" "Projection" "projection=")
   ("rp" "Read preference" "readPref=")
   ("rc" "Read concern" "readConcern=")
   ("m" "Limit the cumulative processing time" "maxTimeMS=")
   ("c" "Attach a comment" "comment=")
   ("h" "The index hint" "hint=")]
  ["Find"
   ("f" "Prompt for filter and execute the find" mongodb-collection--find)])

(define-transient-command mongodb-collection-aggregate-transient ()
  "Aggregate command"
  ["Options"
   ("l" "Limit number of documents" "limit=")
   ("s" "Skip number of documents" "skip=")
   ("o" "Order the documents by" "sort=")
   ("rp" "Read preference" "readPref=")
   ("rc" "Read concern" "readConcern=")
   ("m" "Limit the cumulative processing time" "maxTimeMS=")
   ("c" "Attach a comment" "comment=")
   ("h" "The index hint" "hint=")]
  ["Aggregate"
   ("a" "Prompt for a pipeline and execute the aggregate" mongodb-collection--aggregate)])

(define-transient-command mongodb-collection-insert-one-transient ()
  "insertOne command"
  ["Options"
   ("w" "Write concern" "writeConcern=")]
  ["Insert One"
   ("i" "Prompt for a document and insert it" mongodb-collection--insert-one)])

(define-transient-command mongodb-collection-insert-many-transient ()
  "insertMany command"
  ["Options"
   ("w" "Write concern" "writeConcern=")
   ("o" mongodb-insert-many-ordered)]
  ["Insert Many"
   ("I" "Prompt for an array of documents and insert them" mongodb-collection--insert-many)])

(define-transient-command mongodb-collection-update-one-transient ()
  "updateOne command"
  ["Options"
   ("s" mongodb-update-upsert)
   ("w" "Write concern" "writeConcern=")]
  ["Update One"
   ("u" "Prompt for a filter document and an update document" mongodb-collection--update-one)])

(define-transient-command mongodb-collection-update-many-transient ()
  "updateMany command"
  ["Options"
   ("s" mongodb-update-upsert)
   ("w" "Write concern" "writeConcern=")]
  ["Update Many"
   ("U" "Prompt for a filter document and an update document" mongodb-collection--update-many)])

(define-transient-command mongodb-collection-replace-one-transient ()
  "replaceOne command"
  ["Options"
   ("s" mongodb-update-upsert)
   ("w" "Write concern" "writeConcern=")]
  ["Replace One"
   ("r" "Prompt for a filter document and a replacement document" mongodb-collection--replace-one)])

(define-transient-command mongodb-collection-delete-one-transient ()
  "deleteOne command"
  ["Options"
   ("w" "Write concern" "writeConcern=")]
  ["Delete One"
   ("d" "Prompt for a filter document and execute the delete" mongodb-collection--delete-one)])

(define-transient-command mongodb-collection-delete-many-transient ()
  "deleteMany command"
  ["Options"
   ("w" "Write concern" "writeConcern=")]
  ["Delete Many"
   ("D" "Prompt for a filter document and execute the delete" mongodb-collection--delete-many)])

(define-transient-command mongodb-collection-create-index-transient ()
  "createIndex command"
  ["Command Options"
   ("w" "Write concern" "writeConcern=")]
  ["Index Options"
   ("u" mongodb-create-index-unique)
   ("n" "Name" "name=")
   ("p" "Partial filter expression" "partialFilterExpression=")
   ("s" mongodb-create-index-sparse)
   ("e" "Expire after seconds" "expireAfterSeconds=")
   ("h" mongodb-create-index-hidden)]
  ["Text Index Options"
   ("w" "Weights" "weights=")
   ("d" "Default language" "default_language=")
   ("l" "Language override" "language_override=")]
  ["2d Index Options"
   ("b" "Bits" "bits=")
   ("m" "Min" "min=")
   ("M" "Max" "max=")]
  ["geoHaystack Index Options"
   ("k" "Bucket size" "bucketSize=")]
  ["Wildcard Index Options"
   ("p" "Wildcard projection" "wildcardProjection=")]
  ["Create Index"
   ("c" "Enter the keys document and create the index" mongodb-collection--create-index)])

(define-transient-command mongodb-collection-drop-transient ()
  "drop command"
  ["Options"
   ("w" "Write concern" "writeConcern=")]
  ["Drop Collection"
   ("X" "Drop this collection" mongodb-collection--drop)])

(transient-define-argument mongodb-create-index-hidden ()
  :description "Hidden (default false)"
  :class 'transient-switches
  :key "h"
  :argument-format "hidden=%s"
  :argument-regexp "hidden=\\(true\\|false\\)"
  :choices '("true" "false"))

(transient-define-argument mongodb-create-index-unique ()
  :description "Unique (default false)"
  :class 'transient-switches
  :key "u"
  :argument-format "unique=%s"
  :argument-regexp "unique=\\(true\\|false\\)"
  :choices '("true" "false"))

(transient-define-argument mongodb-create-index-sparse ()
  :description "Sparse (default false)"
  :class 'transient-switches
  :key "s"
  :argument-format "sparse=%s"
  :argument-regexp "sparse=\\(true\\|false\\)"
  :choices '("true" "false"))

(transient-define-argument mongodb-update-upsert ()
  :description "Upsert (default false)"
  :class 'transient-switches
  :key "s"
  :argument-format "upsert=%s"
  :argument-regexp "upsert=\\(true\\|false\\)"
  :choices '("true" "false"))

(transient-define-argument mongodb-insert-many-ordered ()
  :description "Ordered (default true)"
  :class 'transient-switches
  :key "o"
  :argument-format "ordered=%s"
  :argument-regexp "ordered=\\(true\\|false\\)"
  :choices '("true" "false"))

(defvar mongodb-collection-mode-map nil "Keymap for MongoDB collection buffers")

(progn
  (setq mongodb-collection-mode-map (make-sparse-keymap))

  (when (require 'evil nil t)
    (evil-define-key 'normal mongodb-collection-mode-map
      "?" 'mongodb-collection-dispatch
      "C" 'mongodb-collection--use-collection
      "i" 'mongodb-collection-insert-one-transient
      "I" 'mongodb-collection-insert-many-transient
      "u" 'mongodb-collection-update-one-transient
      "U" 'mongodb-collection-update-many-transient
      "r" 'mongodb-collection-replace-one-transient
      "d" 'mongodb-collection-delete-one-transient
      "D" 'mongodb-collection-delete-many-transient
      "f" 'mongodb-collection-find-transient
      "a" 'mongodb-collection-aggregate-transient
      "c" 'mongodb-collection-create-index-transient
      "X" 'mongodb-collection-drop-transient
      "gr" 'mongodb-collection-refresh
      "<" 'mongodb-collection--back
      "q" 'mongodb-collection-quit
      "+" 'mongodb-collection--preview-more
      (kbd "<tab>") 'mongodb-collection-toggle-at-point
      ;; "D" 'mongodb-collection--drop
      ))
  (define-key mongodb-collection-mode-map (kbd "C") 'mongodb-collection--use-collection)
  (define-key mongodb-collection-mode-map (kbd "i") 'mongodb-collection-insert-one-transient)
  (define-key mongodb-collection-mode-map (kbd "I") 'mongodb-collection-insert-many-transient)
  (define-key mongodb-collection-mode-map (kbd "u") 'mongodb-collection-update-one-transient)
  (define-key mongodb-collection-mode-map (kbd "U") 'mongodb-collection-update-many-transient)
  (define-key mongodb-collection-mode-map (kbd "r") 'mongodb-collection-replace-one-transient)
  (define-key mongodb-collection-mode-map (kbd "d") 'mongodb-collection-delete-one-transient)
  (define-key mongodb-collection-mode-map (kbd "D") 'mongodb-collection-delete-many-transient)
  (define-key mongodb-collection-mode-map (kbd "c") 'mongodb-collection-create-index-transient)
  (define-key mongodb-collection-mode-map (kbd "q") 'mongodb-collection-quit)
  (define-key mongodb-collection-mode-map (kbd "f") 'mongodb-collection-find-transient)
  (define-key mongodb-collection-mode-map (kbd "a") 'mongodb-collection-aggregate-transient)
  (define-key mongodb-collection-mode-map (kbd "X") 'mongodb-collection-drop-transient)
  (define-key mongodb-collection-mode-map (kbd "?") 'mongodb-collection-dispatch)
  (define-key mongodb-collection-mode-map (kbd "<") 'mongodb-collection-back)
  (define-key mongodb-collection-mode-map (kbd "gr") 'mongodb-collection-refresh)
  (define-key mongodb-collection-mode-map (kbd "<tab>") 'mongodb-collection-toggle-at-point)
  (define-key mongodb-collection-mode-map (kbd "+") 'mongodb-collection--preview-more))

(define-derived-mode
  mongodb-collection-mode
  mongodb-base-mode
  "MongoDB Collection"
  "Major mode for MongoDB collection operations")
