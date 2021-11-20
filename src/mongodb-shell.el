;;; -*- lexical-binding: t; -*-

(provide 'mongodb-shell)

(cl-defstruct mongodb-shell
  uri
  process
  shell-version
  server-version
  topology-type)

(defconst mongodb-shell-prompt-regex "^\\([^\n]*:\\)?\\(PRIMARY\\|SECONDARY\\|ARBITER\\|mongos\\)?> $")

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

(defun mongodb-shell-command (shell command)
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
              (string-trim (buffer-substring output-start output-end)))))))))

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
  (string-to-number (mongodb-shell-command shell (format "db.%s.countDocuments({})" coll))))

(defun mongodb-shell-find (shell db coll filter)
  (mongodb-shell-command shell (concat "use " db))
  (let ((uuid (mongodb-shell-command shell "UUID().hex()"))
        (has-more))
    (with-temp-buffer
      (insert (mongodb-shell-command shell (format "cursors[%S] = db.%s.find(%s)" uuid coll filter)))
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

(defun mongodb-shell-find-pretty (shell db coll filter &optional args)
  (mongodb-shell-command shell (concat "use " db))
  (let* ((uuid (mongodb-shell-command shell "UUID().hex()"))
         (projection (or (cadr (assoc "projection" args)) "{}"))
         (command (format "const cursor%s = db.%s.find(%s, %s).pretty()" uuid coll filter projection)))
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
    (mongodb-shell-command shell (concat command ";"))
    uuid))

(defun mongodb-shell-aggregate-pretty (shell db coll pipeline &optional args)
  (mongodb-shell-command shell (concat "use " db))
  (let* ((uuid (mongodb-shell-command shell "UUID().hex()"))
         (command (format "const cursor%s = db.%s.aggregate(%s).pretty()" uuid coll pipeline)))
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
    (mongodb-shell-command shell (concat command ";"))
    uuid))

(defun mongodb-shell-insert-one (shell db coll document &optional args)
  (mongodb-shell-command shell (concat "use " db))
  (mongodb-shell-command shell (format "db.%s.insertOne(%s, %s)" coll document (or args "{}"))))

(defun mongodb-shell-insert-many (shell db coll documents &optional args)
  (mongodb-shell-command shell (concat "use " db))
  (mongodb-shell-command shell (format "db.%s.insertMany(%s, %s)" coll documents (or args "{}"))))

(defun mongodb-shell-cursor-live-pretty-p (shell cursor-id)
  (string= (mongodb-shell-command shell (format "cursor%s.isExhausted()" cursor-id)) "false"))

(defun mongodb-shell-cursor-pretty-next (shell cursor-id)
  (with-temp-buffer
    (insert (mongodb-shell-command shell (format "cursor%s" cursor-id)))
    (goto-char (point-min))
    (if (re-search-forward "^Type \"it\" for more" nil t)
        (buffer-substring (point-min) (match-beginning 0))
      (buffer-string))))

(defun mongodb-shell-cursor-live-p (shell cursor-id)
  (string= (mongodb-shell-command shell (format "cursors[%S].isExhausted()" cursor-id)) "false"))

(defun mongodb-shell-cursor-next (shell cursor-id)
  (with-temp-buffer
    (insert (mongodb-shell-command shell (format "cursors[%S]" cursor-id)))
    (goto-char (point-min))
    (if (re-search-forward "^Type \"it\" for more" nil t)
        (buffer-substring (point-min) (match-beginning 0))
      (buffer-string))))
