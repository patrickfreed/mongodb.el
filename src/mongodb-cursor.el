;;; -*- lexical-binding: t; -*-

(provide 'mongodb-cursor)

(require 'mongodb-shell)

(cl-defstruct mongodb-cursor
  name
  shell)

(defun mongodb-cursor-generate-name (shell)
  (concat "cursor_" (mongodb-shell-generate-uuid shell)))

(defun mongodb-cursor-has-next-p (cursor)
  (let* ((shell (mongodb-cursor-shell cursor))
         (cursor-name (mongodb-cursor-name cursor))
         (result (mongodb-shell-command shell (format "%s.hasNext()" cursor-name))))
    (string= result "true")))

(defun mongodb-cursor-next (cursor)
  (when (mongodb-cursor-has-next cursor)
    (mongodb-shell-command
     (mongodb-cursor-shell cursor)
     (format "%s.next()" (mongodb-cursor-name cursor)))))

(defun mongodb-cursor-to-list (cursor)
  (let ((results))
    (while (mongodb-cursor-has-next-p cursor)
      (setq results (append results (list (mongodb-cursor-next cursor)))))
    results))

(defun mongodb-cursor-close (cursor)
  (mongodb-shell-command (mongodb-cursor-shell cursor) (format "%s.close()" (mongodb-cursor-name cursor))))
