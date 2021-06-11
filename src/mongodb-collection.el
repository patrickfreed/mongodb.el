;;; -*- lexical-binding: t; -*-

(provide 'mongodb-collection)

(require 'mongodb-shell)
(require 'mongodb-query)
(require 'mongodb-cursor)

(require 'seq)

(defvar-local mongodb-collection-current nil)
(defvar-local mongodb-database-current nil)
(defvar-local mongodb-collection-documents '())
(defvar-local mongodb-collection-cursor nil)

(cl-defstruct mongodb-collection-doc
  doc
  collapsed
  abbreviated)

(defun mongodb-view-collection (mongo-shell db-name coll-name &optional is-refresh documents)
  (let ((buf (get-buffer-create (format "mongodb-collection: %s/%s.%s" (mongodb-shell-uri mongo-shell) db-name coll-name))))
    (if is-refresh
        (set-buffer buf)
      (switch-to-buffer buf)))
  (mongodb-collection-mode)
  (read-only-mode -1)
  (setq display-line-numbers nil)
  (erase-buffer)
  (setq-local mongodb-shell-process mongo-shell)
  (setq-local mongodb-database-current db-name)
  (setq-local mongodb-collection-current coll-name)
  (setq-local mongodb-collection-documents
              (or documents
                  (let ((cursor (mongodb-shell-find-cursor mongodb-shell-process db-name coll-name "{}"))
                        (docs '()))
                    (while (mongodb-cursor-has-next cursor)
                      (setq docs (append docs (list (make-mongodb-collection-doc
                                                     :doc (mongodb-cursor-next cursor)
                                                     :collapsed nil
                                                     :abbreviated t)))))
                    docs)))
  (message "num docs: %s" (length mongodb-collection-documents))
  (magit-insert-section (mongodb-collection-buffer-section)
    (magit-insert-section (mongodb-collection-info-section)
      (mongodb--insert-header-line "Database Name" (propertize mongodb-database-current 'face 'magit-branch-local))
      (mongodb--insert-header-line "Collection Name" (propertize mongodb-collection-current 'face 'magit-branch-remote))
      (mongodb--insert-header-line "Connection String" (mongodb-shell-uri mongodb-shell-process))
      (mongodb--insert-header-line "MongoDB Server Version" (mongodb-shell-server-version mongodb-shell-process))
      (mongodb--insert-header-line "MongoDB Shell Version" (mongodb-shell-shell-version mongodb-shell-process))
      (mongodb--insert-header-line "Topology" (mongodb-shell-topology-type mongodb-shell-process)))
    (newline)
    (magit-insert-section (mongodb-collection-documents)
      (magit-insert-heading
        (propertize "Documents" 'face 'magit-section-heading)
        (propertize (format " (%d)" (mongodb-shell-collection-count mongodb-shell-process db-name coll-name))))
      (seq-do
       (lambda (doc)
         (magit-insert-section (mongodb-collection-document doc t)
           (if (mongodb-collection-doc-collapsed doc)
               (insert "{ ... }\n")
             (let ((first) (more) (n-lines))
               (with-temp-buffer
                 (insert (mongodb-document-string (mongodb-collection-doc-doc doc)) "\n")
                 (goto-char (point-min))
                 (setq first
                       (buffer-substring
                        (point-min)
                        (progn (forward-line 25) (point))))
                 (when (> (count-lines (point-min) (point-max)) 25)
                   (forward-line 1)
                   (setq more (buffer-substring (point) (point-max)))
                   (setq n-lines (- (count-lines (point-min) (point-max)) 25))))
               (insert first)
               (when more
                 (if (mongodb-collection-doc-abbreviated doc)
                     (progn
                       (insert (propertize (format "\t...%s more lines collapsed\n" n-lines) 'face '('shadow italic)))
                       (insert "}\n"))
                   (insert more)))))))
       mongodb-collection-documents)
      ;; (let ((cursor (mongodb-shell-find-cursor mongodb-shell-process db-name coll-name "{}")))
      ;;   (while (mongodb-cursor-has-next cursor)
      ;;     (let ((doc (mongodb-cursor-next cursor)))
      ;;       (magit-insert-section (mongodb-collection-document doc t)
      ;;         (let ((first) (more) (n-lines))
      ;;           (with-temp-buffer
      ;;             (insert (mongodb-document-string doc) "\n")
      ;;             (goto-char (point-min))
      ;;             (setq first
      ;;                   (buffer-substring
      ;;                    (point-min)
      ;;                    (progn (forward-line 25) (point))))
      ;;             (when (> (count-lines (point-min) (point-max)) 25)
      ;;               (forward-line 1)
      ;;               (setq more (buffer-substring (point) (point-max)))
      ;;               (setq n-lines (- (count-lines (point-min) (point-max)) 25))))
      ;;           (insert first)
      ;;           (when more
      ;;             (insert (propertize (format "\t...%s more lines collapsed\n" n-lines) 'face '('shadow italic)))
      ;;             (insert "}\n")
      ;;             ))))))
      ;; (let* ((result (mongodb-shell-find mongodb-shell-process db-name coll-name "{}"))
      ;;        (first-batch (car result))
      ;;        (cursor-id (cdr result)))
      ;;   (seq-do
      ;;    (lambda (doc)
      ;;      (magit-insert-section (mongodb-collection-document doc t)
      ;;        (if (> (length doc) (window-width))
      ;;            (progn
      ;;              (magit-insert-heading
      ;;                (propertize "Large documents hidden by default, press <TAB> to expand." 'face 'shadow))
      ;;              (magit-insert-section (mongodb-collection-document-more doc t)
      ;;                (insert (mongodb-document-string doc) "\n")))
      ;;          (insert (mongodb-document-string doc) "\n"))))
      ;;    first-batch)
      ;;   (when cursor-id
      ;;     (insert-text-button "Type + to show more results")))
      ))
  (read-only-mode))

(defun mongodb-collection-toggle-at-point ()
  (interactive)
  (let* ((section (magit-current-section))
         (section-type (car (car (magit-section-ident section)))))
    (if (eq 'mongodb-collection-document section-type)
        (mongodb-collection-toggle-doc-at-point)
      (magit-section-toggle section))))

(defun mongodb-collection-expand-at-point ()
  (interactive)
  (let* ((section (magit-current-section))
         (doc (cdr (car (magit-section-ident section)))))
    (setf (mongodb-collection-doc-abbreviated doc) (not (mongodb-collection-doc-abbreviated doc)))
    (let ((p (point)))
      (mongodb-view-collection mongodb-shell-process mongodb-database-current mongodb-collection-current nil mongodb-collection-documents)
      (goto-char p))))

(defun mongodb-collection-toggle-doc-at-point ()
  (interactive)
  (let* ((section (magit-current-section))
         (doc (cdr (car (magit-section-ident section)))))
    (setf (mongodb-collection-doc-collapsed doc) (not (mongodb-collection-doc-collapsed doc)))
    (let ((p (point)))
      (mongodb-view-collection mongodb-shell-process mongodb-database-current mongodb-collection-current nil mongodb-collection-documents)
      (goto-char p))))

(defun mongodb-collection-refresh (&optional silent)
  (interactive)
  (save-excursion
    (when (not silent)
      (message "refreshing..."))
    (mongodb-view-collection mongodb-shell-process mongodb-database-current mongodb-collection-current t)
    (when (not silent)
      (message "refreshing...done"))))

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
    (mongodb-command-input
     "document to insert"
     shell-process
     (lambda (doc)
       (mongodb-shell-insert-one shell-process db coll doc (mongodb-args-to-document args)))
     'document
     (lambda ()
       (with-current-buffer buf
         (mongodb-collection-refresh t))))))

(defun mongodb-collection--insert-many (&optional args)
  (interactive (list (transient-args 'mongodb-collection-insert-many-transient)))
  (let ((shell-process mongodb-shell-process)
        (db mongodb-database-current)
        (coll mongodb-collection-current)
        (buf (current-buffer)))
    (mongodb-command-input
     "documents to insert"
     shell-process
     (lambda (docs)
       (mongodb-shell-insert-many shell-process db coll docs (mongodb-args-to-document args)))
     'array
     (lambda ()
       (with-current-buffer buf
         (mongodb-collection-refresh t))))))

(define-transient-command mongodb-collection-dispatch ()
  "Collection operations"
  ["Collection operations"
   ("c" "View another collection" mongodb-collection--use-collection)
   ("f" "find" mongodb-collection-find-transient)
   ("io" "insertOne" mongodb-collection-insert-one-transient)
   ("im" "insertMany" mongodb-collection-insert-many-transient)
   ("r" "Refresh" mongodb-collection-refresh)
   ;; ("D" "Drop this collection" mongodb-collection--drop)
   ])

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
   ("i" "Prompt for an array of documents and insert them" mongodb-collection--insert-many)])

(transient-define-argument mongodb-insert-many-ordered ()
  :description "Whether to insert the documents in order or not (default true)"
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
      "c" 'mongodb-collection--use-collection
      "io" 'mongodb-collection-insert-one-transient
      "im" 'mongodb-collection-insert-many-transient
      "f" 'mongodb-collection-find-transient
      "r" 'mongodb-collection-refresh
      "z" 'mongodb-collection-expand-at-point
      "<tab>" 'mongodb-collection-toggle-at-point
      "TAB" 'mongodb-collection-toggle-at-point
      ;; "D" 'mongodb-collection--drop
      ))
  (define-key mongodb-collection-mode-map (kbd "c") 'mongodb-collection--use-collection)
  (define-key mongodb-collection-mode-map (kbd "im") 'mongodb-collection-insert-one-transient)
  (define-key mongodb-collection-mode-map (kbd "io") 'mongodb-collection-insert-one-transient)
  (define-key mongodb-collection-mode-map (kbd "f") 'mongodb-collection-find-transient)
  ;; (define-key mongodb-collection-mode-map (kbd "D") 'mongodb-collection--drop)
  (define-key mongodb-collection-mode-map (kbd "?") 'mongodb-collection-dispatch)
  (define-key mongodb-collection-mode-map (kbd "r") 'mongodb-collection-refresh))
  (define-key mongodb-collection-mode-map (kbd "<tab>") 'mongodb-collection-toggle-at-point)
  (define-key mongodb-collection-mode-map (kbd "TAB") 'mongodb-collection-toggle-at-point)

(define-derived-mode
  mongodb-collection-mode
  mongodb-base-mode
  "MongoDB Collection"
  "Major mode for MongoDB collection operations")
