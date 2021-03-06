;;; -*- lexical-binding: t; -*-

(provide 'mongodb-shell)

(require 'cl-lib)

(cl-defstruct mongodb-shell
  uri
  process
  shell-version
  server-version
  topology-type)

(defconst mongodb-shell-prompt-regex "^[\\. ]*\\([^\n]*:\\)?\\(PRIMARY\\|SECONDARY\\|ARBITER\\|mongos\\)?> $")

(defun mongodb-shell-start (uri)
  (with-current-buffer (get-buffer-create "*mongodb-shell-process*")
    (erase-buffer)
    (let ((process (start-process "mongo-shell" (current-buffer) "mongo" uri)))
      (while (not (re-search-backward mongodb-shell-prompt-regex nil t))
        (accept-process-output process))
      (let ((topology-type
             (let ((server-type (or (match-string 2) "")))
               (message "stuff %S" (string= server-type "mongos"))
               (cond
                ((string-match-p "\\(PRIMARY\\|SECONDARY\\|ARBITER\\)" server-type) "Replica Set")
                ((string= server-type "mongos") "Sharded")
                ("Standalone")))))
        (goto-char (point-min))
        (let ((shell-version
               (when (re-search-forward "MongoDB shell version v\\([0-9.]+\\)$" nil t)
                 (match-string 1)))
              (server-version
               (when (re-search-forward "MongoDB server version: \\([0-9.]+\\)$" nil t)
                 (match-string 1))))
          (let ((shell (make-mongodb-shell
                        :uri uri
                        :process process
                        :shell-version shell-version
                        :server-version server-version
                        :topology-type topology-type)))
            (mongodb-shell-command shell "let cursors = {};")
            shell))))))

(defun mongodb-shell-command (shell command &optional db)
  (when db
    (mongodb-shell-command shell (format "use %s" db)))
  (let ((process (mongodb-shell-process shell)))
    (with-current-buffer (process-buffer process)
      (let ((output-start (point-max)))
        (process-send-string process (concat command "\n"))
        (goto-char output-start)
        (while
            (and
             (process-live-p process)
             (save-restriction
               (narrow-to-region output-start (point-max))
               (goto-char (point-min))
               (not (re-search-forward mongodb-shell-prompt-regex nil t))))
          (accept-process-output process))
        (when (process-live-p process)
          (save-restriction
            (narrow-to-region output-start (point-max))
            (goto-char (point-min))
            (re-search-forward mongodb-shell-prompt-regex)
            (let ((output-end (match-beginning 0)))
              (string-trim (buffer-substring output-start output-end) "[ \t\n\r\\.]+"))))))))

(defun mongodb--parse-databases ()
  (if (re-search-forward "^\\([^ ]+\\)[ ]+\\([0-9.]+\\)GB$" nil t)
      (cons (cons (match-string 1) (string-to-number (match-string 2))) (mongodb--parse-databases))
    '()))

(defun mongodb-shell-list-databases (shell)
  (with-temp-buffer
    (message "listing databases...")
    (insert (mongodb-shell-command shell "show dbs"))
    (goto-char (point-min))
    (mongodb--parse-databases)))

(defun mongodb-shell-list-database-names (shell)
  (seq-map 'car (mongodb-shell-list-databases shell)))

(defun mongodb--parse-collections ()
  (if (not (eobp))
      (cons
       (string-trim (buffer-substring (point) (progn (forward-line 1) (point))))
       (mongodb--parse-collections))
    '()))
(defun mongodb-shell-list-collections (shell db)
  (with-temp-buffer
    (mongodb-shell-command shell (concat "use " db))
    (insert (mongodb-shell-command shell "show collections"))
    (goto-char (point-min))
    (mongodb--parse-collections)))

(defun mongodb-shell-collection-count (shell db coll)
  (mongodb-shell-command shell (concat "use " db))
  (string-to-number (mongodb-shell-command shell (format "db[%S].countDocuments({})" coll))))

(defun mongodb-shell-find (shell db coll filter)
  (mongodb-shell-command shell (concat "use " db))
  (let ((uuid (mongodb-shell-command shell "UUID().hex()"))
        (has-more))
    (with-temp-buffer
      (insert (mongodb-shell-command shell (format "cursors[%S] = db[%S].find(%s)" uuid coll filter)))
      (goto-char (point-min))
      (when (re-search-forward "^Type \"it\" for more" nil t)
        (setq has-more t)
        (narrow-to-region (point-min) (match-beginning 0)))
      (goto-char (point-min))
      (cons
       (cl-loop
        until (eobp)
        collect (prog1 (buffer-substring (line-beginning-position) (line-end-position))
                  (forward-line)))
       (and has-more uuid)))))

(defun mongodb-shell-find-build (coll filter &optional args)
  (let* ((command (format "db.%s.find(%s)" coll (string-trim-right filter))))
    ;; TODO: write a function for this
    (when-let ((limit (cadr (assoc "limit" args))))
      (setq command (concat command (format ".limit(%s)" limit))))
    (when-let ((skip (cadr (assoc "skip" args))))
      (setq command (concat command (format ".skip(%s)" skip))))
    (when-let ((comment (cadr (assoc "comment" args))))
      (setq command (concat command (format ".comment(%s)" comment))))
    (when-let ((sort (cadr (assoc "sort" args))))
      (setq command (concat command (format ".sort(%s)" sort))))
    (when-let ((read-concern (cadr (assoc "readConcern" args))))
      (setq command (concat command (format ".readConcern(%s)" read-concern))))
    (when-let ((read-pref (cadr (assoc "readPref" args))))
      (setq command (concat command (format ".readPref(%s)" read-pref))))
    (when-let ((max-time-ms (cadr (assoc "maxTimeMS" args))))
      (setq command (concat command (format ".maxTimeMS(%s)" max-time-ms))))
    (when-let ((hint (cadr (assoc "hint" args))))
      (setq command (concat command (format ".hint(%s)" hint))))
    command))

(cl-defun mongodb-shell-find-cursor (shell db coll filter &optional args)
  (let* ((cursor-name (mongodb-cursor-generate-name shell))
         (command (format "let %s = %s" cursor-name (mongodb-shell-find-build coll filter))))
    (mongodb-shell-command shell command db)
    (make-mongodb-cursor :shell shell :name cursor-name)))

(defun mongodb-shell-aggregate-build (coll pipeline &optional args)
  (let ((command (format "db[%S].aggregate(%s).pretty()" coll pipeline)))
    ;; TODO: write a function for this
    (when-let ((limit (cadr (assoc "limit" args))))
      (setq command (concat command (format ".limit(%s)" limit))))
    (when-let ((skip (cadr (assoc "skip" args))))
      (setq command (concat command (format ".skip(%s)" skip))))
    (when-let ((comment (cadr (assoc "comment" args))))
      (setq command (concat command (format ".comment(%s)" comment))))
    (when-let ((sort (cadr (assoc "sort" args))))
      (setq command (concat command (format ".sort(%s)" sort))))
    (when-let ((read-concern (cadr (assoc "readConcern" args))))
      (setq command (concat command (format ".readConcern(%s)" read-concern))))
    (when-let ((read-pref (cadr (assoc "readPref" args))))
      (setq command (concat command (format ".readPref(%s)" read-pref))))
    (when-let ((max-time-ms (cadr (assoc "maxTimeMS" args))))
      (setq command (concat command (format ".maxTimeMS(%s)" max-time-ms))))
    (when-let ((hint (cadr (assoc "hint" args))))
      (setq command (concat command (format ".hint(%s)" hint))))
    command))

(defun mongodb-shell-aggregate-cursor (shell db coll pipeline &optional args)
  (let* ((cursor-name (mongodb-cursor-generate-name shell))
         (command (format "const %s = %s" cursor-name (mongodb-shell-aggregate-build coll pipeline args))))
    (mongodb-shell-command shell command)
    (make-mongodb-cursor :shell shell :name cursor-name)))

(defun mongodb-shell-insert-one-build (coll document &optional args)
  (format "db[%S].insertOne(%s, %s)" coll document (or args "{}")))

(defun mongodb-shell-insert-one (shell db coll document &optional args)
  (mongodb-shell-command shell (mongodb-shell-insert-one-build coll document args) db))

(defun mongodb-shell-insert-many-build (coll documents &optional args)
  (format "db[%S].insertMany(%s, %s)" coll documents (or args "{}")))

(defun mongodb-shell-insert-many (shell db coll documents &optional args)
  (mongodb-shell-command shell (mongodb-shell-insert-many-build coll documents args) db))

(defun mongodb-shell-update-one-build (coll filter-update &optional args)
  (format "db[%S].updateOne(%s, %s)" coll filter-update (or args "{}")))

(defun mongodb-shell-update-one (shell db coll filter-update &optional args)
  (mongodb-shell-command shell (mongodb-shell-update-one-build coll filter-update args) db))

(defun mongodb-shell-update-many-build (coll filter-update &optional args)
  (format "db[%S].updateMany(%s, %s)" coll filter-update (or args "{}")))

(defun mongodb-shell-update-many (shell db coll filter-update &optional args)
  (mongodb-shell-command shell (concat "use " db))
  (mongodb-shell-command shell (mongodb-shell-update-many-build coll filter-update args)))

(defun mongodb-shell-replace-one-build (coll filter-replacement &optional args)
  (format "db[%S].replaceOne(%s, %s)" coll filter-replacement (or args "{}")))

(defun mongodb-shell-replace-one (shell db coll filter-replacement &optional args)
  (mongodb-shell-command shell (mongodb-shell-replace-one-build coll filter-replacement args) db))

(defun mongodb-shell-delete-one-build (coll filter &optional args)
  (format "db[%S].deleteOne(%s, %s)" coll filter (or args "{}")))

(defun mongodb-shell-delete-one (shell db coll filter &optional args)
  (mongodb-shell-command shell (mongodb-shell-delete-one-build coll filter args) db))

(defun mongodb-shell-delete-many-build (coll filter &optional args)
  (format "db[%S].deleteMany(%s, %s)" coll filter (or args "{}")))

(defun mongodb-shell-delete-many (shell db coll filter &optional args)
  (mongodb-shell-command shell (mongodb-shell-delete-many-build coll filter args) db))

(defun mongodb-shell-drop-collection (shell db coll &optional args)
  (mongodb-shell-command shell (concat "use " db))
  (mongodb-shell-command shell (format "db[%S].drop(%s)" coll (or args "{}"))))

(defun mongodb-shell-list-indexes (shell db coll &optional args)
  (mongodb-shell-command shell (concat "use " db))
  (let* ((cursor-name (mongodb-cursor-generate-name shell))
         (command (format
                   "let %s = new DBCommandCursor(db, db.runCommand({ \"listIndexes\": %S }))"
                   cursor-name
                   coll)))
    (mongodb-shell-command shell (concat command ";"))
    (mongodb-cursor-to-list (make-mongodb-cursor :name cursor-name :shell shell))))

(defun mongodb-shell-create-index-build (coll keys &optional args)
  (format "db[%S].createIndex(%s, %s)" coll keys (or args "{}")))

(defun mongodb-shell-create-index (shell db coll keys &optional args)
  (mongodb-shell-command shell (mongodb-shell-create-index-build coll keys args) db))

(defun mongodb-shell-create-collection (shell db coll-name &optional args)
  (mongodb-shell-command shell (concat "use " db))
  (mongodb-shell-command shell (format "db.createCollection(%S, %s)" coll-name (or args "{}"))))

(defun mongodb-shell-generate-uuid (shell)
  (mongodb-shell-command shell "UUID().hex()"))

(defun mongodb-shell-pretty-print (shell obj)
  (mongodb-shell-command shell (format "printjson(%s)" obj)))
