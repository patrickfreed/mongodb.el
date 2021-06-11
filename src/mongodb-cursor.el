;;; -*- lexical-binding: t; -*-

(provide 'mongodb-cursor)

(require 'mongodb-shell)

(cl-defstruct mongodb-cursor
  name
  shell)

(defun mongodb-cursor-has-next (cursor)
  (let* ((shell (mongodb-cursor-shell cursor))
         (uuid (mongodb-cursor-name cursor))
         (result (mongodb-shell-command shell (format "%s.hasNext()" uuid))))
    (message "has-next: %s" result)
    (string= result "true")))

(defun mongodb-cursor-next (cursor)
  (when (mongodb-cursor-has-next cursor)
    (mongodb-shell-command
     (mongodb-cursor-shell cursor)
     (format "%s.next()" (mongodb-cursor-name cursor)))))
