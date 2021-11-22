;;; -*- lexical-binding: t; -*-

(provide 'mongodb-collection)

(require 'mongodb-shell)
(require 'mongodb-query)

(require 'seq)

(defvar-local mongodb-collection-current nil)
(defvar-local mongodb-database-current nil)
(defvar-local mongodb-collection-prev-buffer nil)
(defvar-local mongodb-collection-info nil)

(defun mongodb-view-collection (mongo-shell db-name coll-name)
  (let ((prev-buffer (current-buffer)))
    (switch-to-buffer
     (get-buffer-create (format "mongodb-collection: %s/%s.%s" (mongodb-shell-uri mongo-shell) db-name coll-name)))
    (mongodb-collection-mode)
    (read-only-mode -1)
    (setq display-line-numbers nil)
    (erase-buffer)
    (setq-local mongodb-collection-prev-buffer prev-buffer)
    (setq-local mongodb-shell-process mongo-shell)
    (setq-local mongodb-database-current db-name)
    (setq-local mongodb-collection-current coll-name)
    (setq-local mongodb-collection-info (mongodb-collection--get-info))
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

      ;; insert coument preview
      (magit-insert-section (mongodb-collection-documents)
        (let* ((result (mongodb-shell-find mongodb-shell-process db-name coll-name "{}"))
               (first-batch (car result))
               (cursor-id (cdr result))
               (count (mongodb-shell-collection-count mongodb-shell-process db-name coll-name)))
          (magit-insert-heading
            (propertize "Documents" 'face 'magit-section-heading)
            (propertize (format " (%d)" count)))
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
            (insert (propertize (format "...omitting %s document(s) from preview..." (- count 20)) 'face 'shadow)))))

      ;; insert index information
      (when (not (string= (alist-get 'type mongodb-collection-info) "view"))
        (newline 2)
        (let ((indexes (mongodb-shell-list-indexes mongodb-shell-process db-name coll-name)))
          (magit-insert-section (mongodb-collection-indexes)
            (magit-insert-heading
              (propertize "Indexes" 'face 'magit-section-heading)
              (propertize (format " (%d)" (length indexes))))
            (seq-do
             (lambda (index)
               (magit-insert-section (mongodb-collection-index index t)
                 (insert (mongodb-document-string index) "\n")))
             indexes))))

      ;; insert options information
      (newline 2)
      (magit-insert-section (mongodb-collection-options)
        (magit-insert-heading
          (propertize "Options" 'face 'magit-section-heading)
          (propertize (format " (%d)" (hash-table-count (alist-get 'options mongodb-collection-info)))))
        (maphash
         (lambda (k v) (mongodb--insert-property k (mongodb-document-string (json-encode v)) 1))
         (alist-get 'options mongodb-collection-info))))
    (read-only-mode)))

(defun mongodb-collection--get-info ()
  (mongodb-shell-command mongodb-shell-process (concat "use " mongodb-database-current))
  (let* ((info-json (mongodb-shell-command
                     mongodb-shell-process
                     (format "JSON.stringify(db.getCollectionInfos({ \"name\": %S })[0]);" mongodb-collection-current)))
         (info-map (json-parse-string info-json)))
    (list (cons 'type (gethash "type" info-map))
          (cons 'options (gethash "options" info-map)))))

(defun mongodb-collection-refresh (&optional silent)
  (interactive)
  (when (not silent)
    (message "refreshing..."))
  (mongodb-view-collection mongodb-shell-process mongodb-database-current mongodb-collection-current)
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
       (lambda (filter)
         (mongodb-shell-find-pretty shell-process db coll filter pairs))))))

(defun mongodb-collection--aggregate (&optional args)
  (interactive (list (transient-args 'mongodb-collection-aggregate-transient)))
  (let ((shell-process mongodb-shell-process)
        (db mongodb-database-current)
        (coll mongodb-collection-current))
    (let ((pairs (seq-map (lambda (kvp) (split-string kvp "=")) args)))
      (mongodb-query-input
       "aggregation pipeline"
       shell-process
       (lambda (pipeline)
         (mongodb-shell-aggregate-pretty shell-process db coll pipeline pairs))
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
        (buf (current-buffer)))
    (message "inserting into collection %s" mongodb-collection-current)
    (mongodb-query-input
     "document to insert"
     shell-process
     (lambda (doc)
       (let ((result (mongodb-shell-insert-one shell-process db coll doc (mongodb-args-to-document args))))
         (with-current-buffer buf
           (mongodb-collection-refresh t))
         result))
     :no-cursor t)))

(defun mongodb-collection--insert-many (&optional args)
  (interactive (list (transient-args 'mongodb-collection-insert-many-transient)))
  (let ((shell-process mongodb-shell-process)
        (db mongodb-database-current)
        (coll mongodb-collection-current)
        (buf (current-buffer)))
    (mongodb-query-input
     "documents to insert"
     shell-process
     (lambda (docs)
       (let ((result
              (mongodb-shell-insert-many shell-process db coll docs (mongodb-args-to-document args))))
         (with-current-buffer buf
           (mongodb-collection-refresh t))
         result))
     :no-cursor t
     :input-type 'array)))

(defun mongodb-collection--update (update-fun &optional args)
  (let ((shell-process mongodb-shell-process)
        (db mongodb-database-current)
        (coll mongodb-collection-current)
        (buf (current-buffer)))
    (message "updating collection %s" mongodb-collection-current)
    (mongodb-query-input
     "filter and update documents"
     shell-process
     (lambda (filter-update)
       (let ((result (funcall update-fun shell-process db coll filter-update (mongodb-args-to-document args))))
         (with-current-buffer buf
           (mongodb-collection-refresh t))
         result))
     :no-cursor t
     :num-inputs 2
     :headings '("Filter document" "Update document"))))


(defun mongodb-collection--update-one (&optional args)
  (interactive (list (transient-args 'mongodb-collection-update-one-transient)))
  (mongodb-collection--update 'mongodb-shell-update-one args))

(defun mongodb-collection--update-many (&optional args)
  (interactive (list (transient-args 'mongodb-collection-update-many-transient)))
  (mongodb-collection--update 'mongodb-shell-update-many args))

(defun mongodb-collection--replace-one (&optional args)
  (interactive (list (transient-args 'mongodb-collection-replace-one-transient)))
  (let ((shell-process mongodb-shell-process)
        (db mongodb-database-current)
        (coll mongodb-collection-current)
        (buf (current-buffer)))
    (mongodb-query-input
     "filter and replacement documents"
     shell-process
     (lambda (filter-replacement)
       (let ((result
              (mongodb-shell-replace-one shell-process db coll filter-replacement (mongodb-args-to-document args))))
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
        (buf (current-buffer)))
    (mongodb-query-input
     "Enter a filter document"
     shell-process
     (lambda (filter)
       (let ((result (mongodb-shell-delete-one shell-process db coll filter (mongodb-args-to-document args))))
         (with-current-buffer buf
           (mongodb-collection-refresh t))
         result))
     :no-cursor t)))

(defun mongodb-collection--delete-many (&optional args)
  (interactive (list (transient-args 'mongodb-collection-delete-one-transient)))
  (let ((shell-process mongodb-shell-process)
        (db mongodb-database-current)
        (coll mongodb-collection-current)
        (buf (current-buffer)))
    (mongodb-query-input
     "Enter a filter document"
     shell-process
     (lambda (filter)
       (let ((result (mongodb-shell-delete-many shell-process db coll filter (mongodb-args-to-document args))))
         (with-current-buffer buf
           (mongodb-collection-refresh t))
         result))
     :no-cursor t)))

(defun mongodb-collection--create-index (&optional args)
  (interactive (list (transient-args 'mongodb-collection-create-index-transient)))
  (let ((shell-process mongodb-shell-process)
        (db mongodb-database-current)
        (coll mongodb-collection-current)
        (buf (current-buffer)))
    (mongodb-query-input
     "Enter the index keys document"
     shell-process
     (lambda (keys)
       (let ((result (mongodb-shell-create-index shell-process db coll keys (mongodb-args-to-document args))))
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
   ("<" "Go back to database" 'mongodb-collection--back)
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
   ("u" "Prompt for a filter document and an update document" mongodb-collection--update-many)])

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
   ("d" "Prompt for a filter document and execute the delete" mongodb-collection--delete-many)])

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
  (define-key mongodb-collection-mode-map (kbd "gr") 'mongodb-collection-refresh))

(define-derived-mode
  mongodb-collection-mode
  mongodb-base-mode
  "MongoDB Collection"
  "Major mode for MongoDB collection operations")
