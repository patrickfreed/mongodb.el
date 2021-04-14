;;; -*- lexical-binding: t; -*-

(provide 'mongodb-shell)

(cl-defstruct mongodb-shell
  uri
  process
  shell-version
  server-version
  topology-type)

(defconst mongodb-shell-prompt-regex "^\\([^:]*:\\)?\\(PRIMARY\\|SECONDARY\\|ARBITER\\|mongos\\)?> $")

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
          (make-mongodb-shell
           :uri uri
           :process process
           :shell-version shell-version
           :server-version server-version
           :topology-type topology-type))))))

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
