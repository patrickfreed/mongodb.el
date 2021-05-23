;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'magit-section)

(require 'mongodb-shell)
(require 'mongodb-database)

(defvar-local mongodb-shell-process nil)

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

;;;###autoload
(defun mongodb-connect (uri)
  (interactive "sConnection String: ")
  (message "connecting")
  (switch-to-buffer (get-buffer-create (format "mongodb: %S" uri)))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (mongodb-mode)
    (when (not mongodb-shell-process)
      (setq-local mongodb-shell-process (mongodb-shell-start uri)))
    (magit-insert-section (mongodb-buffer)
      (magit-insert-section (mongodb-deployment-info)
        (mongodb--insert-header-line "Connection String" uri)
        (mongodb--insert-header-line "MongoDB Server Version" (mongodb-shell-server-version mongodb-shell-process))
        (mongodb--insert-header-line "MongoDB Shell Version" (mongodb-shell-shell-version mongodb-shell-process))
        (mongodb--insert-header-line "Topology" (mongodb-shell-topology-type mongodb-shell-process)))
      (newline)
      (let ((dbs (mongodb-shell-list-databases mongodb-shell-process)))
        (magit-insert-section (mongodb-databases)
          (magit-insert-heading
            (propertize "Databases" 'face 'magit-section-heading)
            (propertize (format " (%d)\n" (length dbs)) 'face 'default))
          (seq-do
           (lambda (db)
             (magit-insert-section (mongodb-database-section (car db) t)
               (magit-insert-heading (propertize (concat (car db) "\n") 'face 'magit-branch-local))
               (mongodb--insert-property "Storage Size" (format "%fGB" (cdr db)) 1)
               (let ((colls (mongodb-shell-list-collections mongodb-shell-process (car db))))
                 (magit-insert-section (mongodb-database-collections-section (car db))
                   (magit-insert-heading
                     (propertize
                      (format "%sCollections" (mongodb--make-indentation 1))
                      'face 'bold)
                     (format " (%d)" (length colls)))
                   (seq-do
                    (lambda (coll)
                      (magit-insert-section (mongodb-collection-section `((db . ,(car db)) (coll . ,coll)))
                        (insert (propertize
                                 (concat (mongodb--make-indentation 2) coll "\n")
                                 'face 'magit-branch-remote))))
                    colls)))))
           dbs))))))

(defun mongodb-inspect-at-point ()
  (interactive)
  (message "inspecting")
  (when-let ((db (magit-section-value-if 'mongodb-database-section)))
    (mongodb-view-database mongodb-shell-process db))
  (when-let ((ns (magit-section-value-if 'mongodb-collection-section)))
    (mongodb-view-collection mongodb-shell-process (alist-get 'db ns) (alist-get 'coll ns))))


(define-derived-mode
  mongodb-base-mode
  magit-section-mode
  "MongoDB"
  "Major mode for MongoDB buffers")

(defvar mongodb-base-mode-map nil "map")
(progn
  (setq mongodb-base-mode-map (make-sparse-keymap))
  (when (require 'evil nil t)
    (evil-define-key 'normal mongodb-base-mode-map
      (kbd "<tab>") 'magit-section-toggle))
  (define-key mongodb-base-mode-map [remap evil-previous-line] 'evil-previous-visual-line)
  (define-key mongodb-base-mode-map [remap evil-next-line] 'evil-next-visual-line)
  (define-key mongodb-base-mode-map (kbd "<tab>") 'magit-section-toggle))

(define-derived-mode
  mongodb-mode
  mongodb-base-mode
  "MongoDB"
  "Major mode for MongoDB buffers")

(defvar mongodb-mode-map nil "map")
(progn
  (setq mongodb-mode-map (make-sparse-keymap))
  (when (require 'evil nil t)
    (evil-define-key 'normal mongodb-mode-map
      (kbd "<RET>") 'mongodb-inspect-at-point))
  ;; (define-key mongodb-mode-map [remap evil-previous-line] 'evil-previous-visual-line)
  ;; (define-key mongodb-mode-map [remap evil-next-line] 'evil-next-visual-line)
  ;; (define-key mongodb-mode-map (kbd "<tab>") 'magit-section-toggle)
  (define-key mongodb-mode-map (kbd "r") (lambda () (interactive) (mongodb-connect "mongodb://localhost:27017/blah")))
  (define-key mongodb-mode-map (kbd "<ret>") 'mongodb-inspect-at-point))

