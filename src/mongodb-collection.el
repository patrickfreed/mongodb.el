;;; -*- lexical-binding: t; -*-

(provide 'mongodb-collection)

(require 'mongodb-shell)
(require 'mongodb-query)

(require 'seq)

(defvar-local mongodb-namespace-current nil)

(defun mongodb-view-collection (mongo-shell db-name coll-name)
  (switch-to-buffer
   (get-buffer-create (format "mongodb-collection: %s/%s.%s" (mongodb-shell-uri mongo-shell) db-name coll-name)))
  (mongodb-collection-mode)
  (read-only-mode -1)
  (setq display-line-numbers nil)
  (erase-buffer)
  (setq-local mongodb-shell-process mongo-shell)
  (setq-local mongodb-namespace-current (cons db-name coll-name))
  (magit-insert-section (mongodb-collection-buffer-section)
    (magit-insert-section (mongodb-collection-info-section)
      (mongodb--insert-header-line
       "Namespace"
       (propertize (format "%s.%s" (car mongodb-namespace-current) (cdr mongodb-namespace-current))
                   'face 'magit-branch-local))
      (mongodb--insert-header-line "Connection String" (mongodb-shell-uri mongodb-shell-process))
      (mongodb--insert-header-line "MongoDB Server Version" (mongodb-shell-server-version mongodb-shell-process))
      (mongodb--insert-header-line "MongoDB Shell Version" (mongodb-shell-shell-version mongodb-shell-process))
      (mongodb--insert-header-line "Topology" (mongodb-shell-topology-type mongodb-shell-process)))
    (newline)
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

(defun mongodb-collection--use-collection (coll-name)
  (interactive
   (list (completing-read "View collection: "
                          (mongodb-shell-list-collections mongodb-shell-process (car mongodb-namespace-current)))))
  (mongodb-view-collection mongodb-shell-process (car mongodb-namespace-current) coll-name))

(defun mongodb-collection--find (&optional args)
  (interactive (list (transient-args 'mongodb-collection-find-transient)))
  (let ((shell-process mongodb-shell-process)
        (db (car mongodb-namespace-current))
        (coll (cdr mongodb-namespace-current)))
    (let ((pairs (seq-map (lambda (kvp) (split-string kvp "=")) args)))
      (mongodb-query-input
       "find filter"
       shell-process
       (lambda (filter)
         (mongodb-shell-find-pretty shell-process db coll filter pairs))))))

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
        (db (car mongodb-namespace-current))
        (coll (cdr mongodb-namespace-current)))
    (mongodb-query-input
     "document to insert"
     shell-process
     (lambda (doc)
       (mongodb-shell-insert-one shell-process db coll doc (mongodb-args-to-document args)))
     t)))

(define-transient-command mongodb-collection-dispatch ()
  "Collection operations"
  ["Collection operations"
   ("c" "View another collection" mongodb-collection--use-collection)
   ("f" "Execute a find query on this collection" mongodb-collection-find-transient)
   ("i" "Insert one document into the collection" mongodb-collection-insert-one-transient)
   ;; ("D" "Drop this collection" mongodb-collection--drop)
   ])

(define-transient-command mongodb-collection-find-transient ()
  "Find command"
  ["Options"
   ("l" "Limit the number of documents returned" "limit=")
   ("s" "Skip this number of documents from the query" "skip=")
   ("o" "Order the returned documents" "sort=")
   ("p" "Specify the fields to return in the documents that match the query filter" "projection=")
   ("P" "Specify the read preference to use for the find" "readPref=")
   ("r" "Specify the read concern to use for the find" "readConcern=")
   ("m" "Limit the cumulative time for processing operations for the find" "maxTimeMS=")
   ("c" "Attach a comment to the query" "comment=")
   ("h" "Specify the index to use for the find" "hint=")]
  ["Commands"
   ("f" "Prompt for filter and execute the find" mongodb-collection--find)])

(define-transient-command mongodb-collection-insert-one-transient ()
  "Find command"
  ["Options"
   ("w" "Specify a document expressing the write concern" "writeConcern=")]
  ["Commands"
   ("i" "Prompt for a document and insert it" mongodb-collection--insert-one)])

(defvar mongodb-collection-mode-map nil "Keymap for MongoDB collection buffers")

(progn
  (setq mongodb-collection-mode-map (make-sparse-keymap))

  (when (require 'evil nil t)
    (evil-define-key 'normal mongodb-collection-mode-map
      "?" 'mongodb-collection-dispatch
      "c" 'mongodb-collection--use-collection
      ;; "D" 'mongodb-collection--drop
      ))
  (define-key mongodb-collection-mode-map (kbd "c") 'mongodb-collection--use-collection)
  ;; (define-key mongodb-collection-mode-map (kbd "D") 'mongodb-collection--drop)
  (define-key mongodb-collection-mode-map (kbd "?") 'mongodb-collection-dispatch)
  )

(define-derived-mode
  mongodb-collection-mode
  mongodb-base-mode
  "MongoDB Collection"
  "Major mode for MongoDB collection operations")
