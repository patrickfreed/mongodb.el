(require 'mongodb)

(let ((mongo-process (mongodb-shell-start "mongodb://localhost:27017/woooo")))
  (message "==============")
  (with-current-buffer (get-buffer-create "*mongo-scratch*")
    (erase-buffer)
    (insert (format "%d document(s)\n" (mongodb-shell-collection-count mongo-process "many" "many")))
    (mongodb-shell-find-pretty mongo-process "many" "many" "{}")
    ;; (let* ((result (mongodb-shell-find mongo-process "many" "many" "{}"))
    ;;        (first-batch (car result))
    ;;        (cursor-id (cdr result)))
    ;;   (insert (format "%S" first-batch))
    ;;   ;; (when cursor-id
    ;;   ;;   (while (mongodb-shell-cursor-live-p mongo-process cursor-id)
    ;;   ;;     (insert (mongodb-shell-cursor-next mongo-process cursor-id))))
    ;;   )
    ;; (message (mongodb-shell-topology-type mongo-process))
    ;; (insert (format "%S" (mongodb-shell-list-collections mongo-process "admin")) "\n")
    ;; (insert (format "%S" (mongodb-shell-command mongo-process "db.blah.find()")) "\n")
    ;; (insert (format "%S" (mongodb-shell-command mongo-process "use blah")) "\n")
    ;; (insert (format "%S" (mongodb-shell-command mongo-process "db.blah.insertOne({})")) "\n")
    ;; (insert (format "%S" (mongodb-shell-command mongo-process "db.blah.find()")) "\n")
    ;; (insert (mongodb-shell-get-topology mongo-process))
    (mongodb-shell-command mongo-process "exit")
    ))
