;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'magit-section)

(cl-defstruct mongodb-shell
  uri
  process)

(defvar output-start 0)

(defun mongodb-shell-start (uri)
  (with-current-buffer (get-buffer-create "*mongodb-shell-process*")
    (erase-buffer)
    (let ((process (start-process "mongo-shell" (current-buffer) "mongo" uri)))
      (while (not (re-search-backward "^>" nil t))
        (accept-process-output process))
      (make-mongodb-shell
       :uri uri
       :process process))))

(defun mongodb-shell-command (shell command)
  (let ((process (mongodb-shell-process shell)))
    (with-current-buffer (process-buffer process)
      (let ((output-start (point-max)))
        (process-send-string process (concat command "\n"))
        (goto-char output-start)
        (while
            (and
             (process-live-p process)
             (not (re-search-backward "^>" output-start t))
             (or
              (= (point) output-start)
              (not (eq (char-before (1- (point))) ?\>))))
          (accept-process-output process))
        (let ((output-end (- (point-max) 2)))
          (string-trim (buffer-substring output-start output-end)))))))

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

(let ((mongo-process (mongodb-shell-start "mongodb://localhost:27017/admin")))
  (message "==============")
  (with-current-buffer (get-buffer-create "*mongo-scratch*")
    (erase-buffer)
    ;; (insert (format "%S" (mongodb-shell-list-collections mongo-process "admin")))
    (insert (mongodb-shell-command mongo-process "show collections"))
    (mongodb-shell-command mongo-process "exit")))

(defvar-local mongo-shell nil)

(defun mongodb--insert-header-line (key value)
  (magit-insert-section (mongodb-header)
    (insert (propertize (format "%-30s" (concat key ": ")) 'face 'bold))
    (insert value)
    (newline)))

(defun mongodb--make-indentation (level)
  (make-string (* level 2) ? ))

(defun mongodb--insert-property (key value indentation)
  (magit-insert-section (mongodb-property key)
    (magit-insert-heading (propertize (concat (mongodb--make-indentation indentation) key) 'face 'bold))
    (magit-insert-section (mongodb-property-value value)
      (insert (mongodb--make-indentation (1+ indentation)) value))
    (newline)))

(defun mongodb-connect (uri)
  (interactive "sConnection String: ")
  (message "connecting")
  (switch-to-buffer (get-buffer-create (format "mongodb: %S" uri)))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (mongodb-mode)
    (when (not mongo-shell)
      (setq-local mongo-shell (mongodb-shell-start uri)))
    (magit-insert-section (mongodb-buffer)
      (magit-insert-section (mongodb-deployment-info)
        (magit-insert-heading "foo")
        (mongodb--insert-header-line "Connection String" uri)
        (mongodb--insert-header-line "MongoDB Server Version" "4.4.0")
        (mongodb--insert-header-line "MongoDB Shell Version" "4.4.0")
        (mongodb--insert-header-line "Topology" "Standalone"))
      (newline)
      (let ((dbs (mongodb-shell-list-databases mongo-shell)))
        (magit-insert-section (mongodb-databases)
          (magit-insert-heading
            (propertize "Databases" 'face 'magit-section-heading)
            (propertize (format " (%d)\n" (length dbs)) 'face 'default))
          (seq-do
           (lambda (db)
             (magit-insert-section (mongodb-database db t)
               (magit-insert-heading (propertize (concat (car db) "\n") 'face 'magit-branch-local))
               (mongodb--insert-property "Storage Size" (format "%fGB" (cdr db)) 1)
               (let ((colls (mongodb-shell-list-collections mongo-shell (car db))))
                 (magit-insert-section (mongodb-database-collections (car db))
                   (magit-insert-heading
                     (propertize
                      (format "%sCollections" (mongodb--make-indentation 1))
                      'face 'bold)
                     (format " (%d)" (length colls)))
                   (seq-do
                    (lambda (coll)
                      (magit-insert-section (mongodb-collection coll)
                        (insert (propertize
                                 (concat (mongodb--make-indentation 2) coll "\n")
                                 'face 'magit-branch-remote))))
                    colls)
                   ))
               ))
           dbs))))))

(define-derived-mode
  mongodb-mode
  magit-section-mode
  "MongoDB"
  "Major mode for MongoDB buffers")

(defvar mongodb-mode-map nil "map")
(progn
  (setq mongodb-mode-map (make-sparse-keymap))
  (define-key mongodb-mode-map [remap evil-previous-line] 'evil-previous-visual-line)
  (define-key mongodb-mode-map [remap evil-next-line] 'evil-next-visual-line)
  (define-key mongodb-mode-map (kbd "<tab>") 'magit-section-toggle)
  (define-key mongodb-mode-map (kbd "r") (lambda () (interactive) (mongodb-connect "mongodb://localhost:27017/blah"))))
