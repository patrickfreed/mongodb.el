;;; -*- lexical-binding: t; -*-

(provide 'mongodb-query)

(require 'mongodb-shell)

(require 'cl-lib)

(defvar-local mongodb-query-body nil)
(defvar-local mongodb-query-cursor-id nil)
(defvar-local mongodb-is-cursor-result nil)

(cl-defun mongodb-query-input (title shell body &key no-cursor (input-type 'document) (num-inputs 1) headings)
  (switch-to-buffer (get-buffer-create "*mongodb query input*"))
  (erase-buffer)
  (mongodb-query-mode)
  (setq-local mongodb-shell-process shell)
  (setq-local mongodb-query-body body)
  (setq-local mongodb-is-cursor-result (not no-cursor))
  (insert "// " title "\n")
  (insert "// Press C-c C-c to submit." "\n")
  (cond
   ((eq input-type 'array) (mongodb-query-insert-prompt "[" "]" num-inputs headings))
   (t (mongodb-query-insert-prompt "{" "}" num-inputs headings))))

(defun mongodb-query-insert-prompt (open close num headings)
  (let ((end-point))
    (dotimes (c num)
      (when headings
        (newline)
        (insert "// " (nth c headings))
        (newline))
      (insert open)
      (newline)
      (indent-for-tab-command)
      (when (= c 0) (setq end-point (point)))
      (newline)
      (insert close)
      (when (< c (- num 1))
        (insert ",")
        (newline))
      (newline))
    (goto-char end-point)))

(defun mongodb-query-execute ()
  (interactive)
  (goto-char (point-min))
  (flush-lines "^//")
  (flush-lines "^$")
  (let ((query-result (funcall mongodb-query-body (buffer-string)))
        (shell-process mongodb-shell-process)
        (is-cursor-result mongodb-is-cursor-result))
    (kill-buffer "*mongodb query input*")
    (switch-to-buffer-other-window (get-buffer-create "*mongodb query results*"))
    (erase-buffer)
    (mongodb-query-results-mode)
    (hs-minor-mode)
    (if is-cursor-result
        (let ((cursor-id query-result))
          (setq-local mongodb-query-cursor-id cursor-id)
          (while (mongodb-shell-cursor-live-pretty-p shell-process cursor-id)
            (insert (mongodb-shell-cursor-pretty-next shell-process cursor-id)))
          (goto-char (point-min)))
      (insert query-result))))

(defvar mongodb-query-mode-map nil "Keymap for MongoDB query input buffers")
(progn
  (setq mongodb-query-mode-map (make-sparse-keymap))

  ;; (when (require 'evil nil t)
  ;;   (evil-define-key 'normal mongodb-collection-mode-map
  ;;     "?" 'mongodb-collection-dispatch
  ;;     "c" 'mongodb-collection--use-collection
  ;;     ;; "D" 'mongodb-collection--drop
  ;;     ))
  (define-key mongodb-query-mode-map (kbd "C-c C-c") 'mongodb-query-execute)
  )

(define-derived-mode
  mongodb-query-mode
  javascript-mode
  "MongoDB Query"
  "Major mode for creating MongoDB queries")

(defvar mongodb-query-results-map nil "Keymap for MongoDB query input buffers")
(progn
  (setq mongodb-query-results-mode-map (make-sparse-keymap))

  (when (require 'evil nil t)
    (evil-define-key 'normal mongodb-query-results-mode-map
      (kbd "<tab>") 'hs-toggle-hiding))
  (define-key mongodb-query-results-mode-map (kbd "<tab>") 'hs-toggle-hiding)
  )
(define-derived-mode
  mongodb-query-results-mode
  javascript-mode
  "MongoDB Query Results"
  "Major mode for viewing MongoDB query results")
