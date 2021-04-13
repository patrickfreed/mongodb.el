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

(let ((mongo-process (mongodb-shell-start "mongodb://localhost:27017/test")))
  (message "==============")
  (message "%S" (mongodb-shell-list-databases mongo-process))
  (mongodb-shell-command mongo-process "exit"))

(defvar-local mongo-shell nil)

(defun mongodb-connect (uri)
  (interactive "sConnection String:")
  (switch-to-buffer (get-buffer-create (format "mongodb: %S" uri)))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (mongodb-mode)
    (when (not mongo-shell)
      (setq-local mongo-shell (mongodb-shell-start uri)))
    (magit-insert-section (mongodb-buffer)
     (let ((dbs (mongodb-shell-list-databases mongo-shell)))
       (magit-insert-section (mongodb-databases)
         (magit-insert-heading "Databases")
         (seq-do
          (lambda (db)
            (message "%S" db)
            (magit-insert-section (mongodb-database)
              (insert (concat (car db) "\n"))))
          dbs))))))

(define-derived-mode
  mongodb-mode
  magit-section-mode
  "MongoDB"
  "Major mode for MongoDB buffers")

(defvar mongodb-mode-map nil "map")
(progn
  (setq mongodb-mode-map (make-sparse-keymap))
  (define-key mongodb-mode-map (kbd "<tab>") 'magit-section-toggle))
