;;; -*- lexical-binding: t; -*-

(provide 'mongodb-query)

(require 'mongodb-shell)

(require 'cl-lib)

(defvar-local mongodb-query-cache nil)
(defvar-local mongodb-query-use-same-window nil)

(defvar-local mongodb-query-results-results nil)
(defvar-local mongodb-query-results-raw-input nil)
(defvar-local mongodb-query-results-query-string nil)

(defvar mongodb-query-keywords
  '("$eq" "$gt" "$gt" "$gte" "$in" "$lt" "$lte" "$ne" "$nin"
    "$and" "$not" "$nor" "$or"
    "$exists" "$type"
    "$expr" "$jsonSchema" "$mod" "$regex" "$text" "$where"
    "$geoIntersects" "$geoWithin" "$near" "$nearSphere"
    "$all" "$elemMatch" "$size"
    "$bitsAllClear" "$bitsAllSet" "$bitsAnyClear" "$bitsAnySet"
    "$" "$meta" "$slice"
    "$comment" "$rand"
    ;; agg stages
    "$addFields" "$bucket" "$bucketAuto" "$collStats" "$count" "$facet"
    "$geoNear" "$graphLookup" "$group" "$indexStats" "$limit" "$listSessions"
    "$lookup" "$match" "$merge" "$out" "$planCacheStats" "$project"
    "$replaceRoot" "$replaceWith" "$sample" "$search" "$set"
    "$setWindowFields" "$skip" "$sort" "$sortByCount" "$unionWith"
    "$unset" "$unwind"
    ;; agg operators
    "$abs" "$add" "$ceil" "$divide" "$exp" "$floor" "$ln" "$log" "$log10"
    "$mod" "$multiply" "$pow" "$round" "$sqrt" "$trunc"
    ;; array expression operators
    "$arrayElemAt" "$arrayToObject" "$concatArrays" "$filter" "$first"
    "$in" "$indexOfArray" "$isArray" "$last" "$map" "$objectToArray"
    "$range" "$reduce" "$reverseArray" "$size" "$slice" "$zip"
    ;; conditional expression operators
    "$cond" "$ifNull" "$switch"
    ;; custom agg expression operators
    "$accumulator" "$function"
    ;; data size operators
    "$binarySize" "$bsonSize"
    ;; date expression operators
    "$dateAdd" "$dateDiff" "$dateFromParts" "$dateFromString" "$dateSubtract"
    "$dateToParts" "$dateToString" "$daeTrunc" "$dayOfMonth" "$dayOfWeek"
    "$dayOfYear" "$hour" "$isoDayOfWeek" "$isoWeek" "$millisecond" "$minute"
    "$second" "$toDate" "$week" "$year" "$subtract"
    ;; literal
    "$literal"
    ;; misc
    "$getField" "$rand" "$sampleRate"
    ;; object expression operators
    "$mergeObjects" "$objectToArray" "$setField"
    ;; set expression operators
    "$allElementsTrue" "$anyElementTrue" "$setDifference" "$setEquals"
    "$setIntersection" "$setIsSubset" "$setUnion"
    ;; string expression operators
    "$concat" "$dateFromString" "$dateToString" "$indexOfBytes" "$indexOfCP"
    "$ltrim" "$regexFind" "$regexFindAll" "$regexMatch"
    "$replaceAll" "$rtrim" "$split" "$strLenBytes" "$strLenCP" "$strcasecmp"
    "$substr" "$substrBytes" "$substrCP" "$toLower" "$toString" "$trim"
    "$toUpper"
    ;; text expression operators
    "$meta"
    ;; trig operators
    "$sin" "$cos" "$tan" "$asin" "$acos" "$atan" "$atan2" "$asinh" "$acosh"
    "$atanh" "$sinh" "$cosh" "$tanh" "$degreesToRadians" "$radiansToDegrees"
    ;; type expression operators
    "$convert" "$isNumber" "$toBool" "$toDate" "$toDecimal" "$toDouble"
    "$toInt" "$toLong" "$toObjectId" "$toString" "$type"
    ;; accumulators
    "$accumulator" "$addToSet" "$avg" "$count" "$first" "$last" "$max"
    "$mergeObjects" "$min" "$push" "$stdDevPop" "$sum"
    ;; variable expression operators
    "$let"
    ;; window operators
    "$addToSet" "$avg" "$count" "$covariancePop" "$covarianceSamp" "$denseRank"
    "$derivative" "$documentNumber" "$expMovingAvg" "$first" "$integral"
    "$last" "$min" "$max" "$push" "$rank" "$shift" "$stdDevPop" "$stdDevSamp"
    "$sum"
    ;; update modifiers
    "$currentDate" "$inc" "$min" "$max" "$mul" "$rename" "set" "$setOnInsert"
    "$unset" "$[]" "$addToSet" "$pop" "$pull" "$push" "$pullAll" "$each"
    "$position" "$slice" "$sort" "$bit"))

(defvar mongodb-query-option-keywords
  '(;; geoNear options
    "distanceField" "distanceMultiplier" "includeLocs" "key" "maxDistance"
    "minDistance" "near" "query" "spherical" "uniqueDocs"
    ;; graphLookup options
    "from" "startWith" "connectFromField" "connectToField" "as" "maxDepth" "depthField"
    "restrictSearchWithMatch"))

(cl-defstruct mongodb-query
  title
  shell
  db
  coll
  build-fn
  body-fn
  is-cursor-result
  input-type
  num-inputs
  headings)

(cl-defun mongodb-query-input (title shell db coll build-query body &key no-cursor (input-type 'document) (num-inputs 1) headings)
  (mongodb-query-input-query
   (make-mongodb-query
    :title title
    :shell shell
    :db db
    :coll coll
    :build-fn build-query
    :body-fn body
    :is-cursor-result (not no-cursor)
    :input-type input-type
    :num-inputs num-inputs
    :headings headings)))

(defun mongodb-query-input-query (query &optional same-window)
  (switch-to-buffer (get-buffer-create "*mongodb query input*"))
  (erase-buffer)
  (mongodb-query-mode)
  (setq-local mongodb-query-cache query)
  (setq-local mongodb-query-use-same-window same-window)
  (insert "// " (mongodb-query-title query) "\n")
  (insert "// Press C-c C-c to submit." "\n")
  (let ((num-inputs (mongodb-query-num-inputs query))
        (headings (mongodb-query-headings query)))
    (cond
     ((eq (mongodb-query-input-type query) 'array) (mongodb-query-insert-prompt "[" "]" num-inputs headings))
     (t (mongodb-query-insert-prompt "{" "}" num-inputs headings)))))

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
  (let ((raw-input (buffer-string)))
    (flush-lines "^//")
    (flush-lines "^$")
    (let* ((query mongodb-query-cache)
           (db (mongodb-query-db mongodb-query-cache))
           (coll (mongodb-query-coll mongodb-query-cache))
           (build-fn (mongodb-query-build-fn mongodb-query-cache))
           (body-fn (mongodb-query-body-fn mongodb-query-cache))
           (input (funcall build-fn (string-trim-right (buffer-string))))
           (query-result (funcall body-fn (buffer-string)))
           (use-same-window mongodb-query-use-same-window))
      (kill-buffer "*mongodb query input*")
      (let ((next-buffer (generate-new-buffer "*mongodb query results*")))
        (if use-same-window
            (switch-to-buffer next-buffer)
          (switch-to-buffer-other-window next-buffer)))

      (let ((results
             (if (mongodb-query-is-cursor-result query)
                 (progn
                   (let ((results (make-mongodb-query-result-set :cursor query-result :documents '())))
                     (mongodb-query-result-set-more results 20)
                     results))
               (make-mongodb-query-result-set
                :documents (list (mongodb-query-result-doc-new query-result))
                :cursor nil))))
        (mongodb-query-results-view (current-buffer) query input raw-input results)))))

(defun mongodb-query-results-view (buffer query input raw-input results)
  (switch-to-buffer buffer)
  (mongodb-query-results-mode)
  (read-only-mode -1)
  (erase-buffer)
  (setq-local mongodb-query-results-query-string (string-trim-right input))
  (setq-local mongodb-query-results-raw-input raw-input)
  (setq-local mongodb-query-results-results results)

  (setq-local mongodb-query-cache query)

  (magit-insert-section (mongodb-query-results-info-section)
    (mongodb--insert-header-line "Database Name" (propertize (mongodb-query-db query) 'face 'magit-branch-local))
    (mongodb--insert-header-line "Collection Name" (propertize (mongodb-query-coll query) 'face 'magit-branch-remote)))
  (newline)

  (save-excursion
    (magit-insert-section (mongodb-query-results-root)
      (magit-insert-section (mongodb-query-results-input nil t)
        (magit-insert-heading (propertize "Query" 'face 'magit-section-heading))
        (insert mongodb-query-results-query-string)
        (newline))
      (newline)
      (mongodb-query-result-set-insert mongodb-query-results-results "Results")
      (when (mongodb-query-result-set-has-more-p mongodb-query-results-results)
        (insert-text-button "Type + to preview more documents"))))
  (read-only-mode))

(defun mongodb-query-results-rerun ()
  (interactive)
  (mongodb-query-input-query mongodb-query-cache t))

(defun mongodb-query-results--redraw ()
  (interactive)
  (let ((p (point)))
    (mongodb-query-results-view
     (current-buffer)
     mongodb-query-cache
     mongodb-query-results-query-string
     mongodb-query-results-raw-input
     mongodb-query-results-results)
    (goto-char p)))

(defun mongodb-query-results--more ()
  (interactive)
  (when (mongodb-query-result-set-has-more-p mongodb-query-results-results)
    (mongodb-query-result-set-more mongodb-query-results-results 20)
    (mongodb-query-results--redraw)))

(cl-defstruct mongodb-query-result-doc
  doc
  collapsed)

(defun mongodb-query-result-doc-new (doc)
  (make-mongodb-query-result-doc
   :doc doc
   :collapsed (with-temp-buffer
                (insert doc)
                (> (count-lines (point-min) (point-max)) 25))))

(cl-defstruct mongodb-query-result-set
  documents
  cursor)

(defun mongodb-query-result-set-has-more-p (results)
  (when-let* ((cursor (mongodb-query-result-set-cursor results)))
    (mongodb-cursor-has-next-p cursor)))

(defun mongodb-query-result-set-more (results n)
  (when-let ((cursor (mongodb-query-result-set-cursor results)))
    (let ((i 0))
      (while (and (mongodb-cursor-has-next-p cursor) (< i n))
        (setf (mongodb-query-result-set-documents results)
              (append (mongodb-query-result-set-documents results)
                      (list (mongodb-query-result-doc-new (mongodb-cursor-next cursor)))))
        (setq i (1+ i))))))

(defun mongodb-query-result-set-insert (results title)
  (magit-insert-section (mongodb-query-results)
    (magit-insert-heading
      (propertize title 'face 'magit-section-heading))
    (seq-do
     (lambda (doc)
       (magit-insert-section (mongodb-query-results-document doc t)
         (insert
          (with-temp-buffer
            (if (not (mongodb-query-result-doc-collapsed doc))
                (insert (mongodb-document-string (mongodb-query-result-doc-doc doc)) "\n")
              (insert "{ " (propertize "...Document contents collapsed, press <TAB> to expand" 'face 'shadow) " }\n"))
            (buffer-string)))))
     (mongodb-query-result-set-documents results))))

(defun mongodb-query-result-set-toggle-at-point ()
  (when-let* ((section (magit-current-section))
         (doc (cdr (car (magit-section-ident section)))))
    (setf (mongodb-query-result-doc-collapsed doc) (not (mongodb-query-result-doc-collapsed doc)))
    t))

(defun mongodb-query-results-toggle-at-point ()
  (interactive)
  (if (mongodb-query-result-set-toggle-at-point)
      (mongodb-query-results--redraw)
    (when-let ((section (magit-current-section)))
        (magit-section-toggle section))))

(defvar mongodb-query-mode-map nil "Keymap for MongoDB query input buffers")
(progn
  (setq mongodb-query-mode-map (make-sparse-keymap))
  (define-key mongodb-query-mode-map (kbd "C-c C-c") 'mongodb-query-execute))

(defun mongodb-query-completion-function ()
  (if-let* ((start (save-excursion (re-search-backward "\\$" nil t)))
            (end (or (cdr (bounds-of-thing-at-point 'word)) start)))
      (list start end mongodb-query-keywords)
    (when-let* ((bounds (bounds-of-thing-at-point 'word)))
      (list (car bounds) (cdr bounds) mongodb-query-option-keywords))))

(define-derived-mode
  mongodb-query-mode
  javascript-mode
  "MongoDB Query"
  "Major mode for creating MongoDB queries"
  (add-hook 'completion-at-point-functions #'mongodb-query-completion-function nil t))

(defvar mongodb-query-results-map nil "Keymap for MongoDB query result buffers")
(progn
  (setq mongodb-query-results-mode-map (make-sparse-keymap))

  (when (require 'evil nil t)
    (evil-define-key 'normal mongodb-query-results-mode-map
      (kbd "<tab>") 'mongodb-query-results-toggle-at-point
      (kbd "r") 'mongodb-query-results-rerun
      (kbd "+") 'mongodb-query-results--more))
  (define-key mongodb-query-results-mode-map (kbd "<tab>") 'mongodb-query-results-toggle-at-point))
  (define-key mongodb-query-results-mode-map (kbd "r") 'mongodb-query-results-rerun)
  (define-key mongodb-query-results-mode-map (kbd "+") 'mongodb-query-results--more)

(define-derived-mode
  mongodb-query-results-mode
  mongodb-base-mode
  "MongoDB Query Results"
  "Major mode for viewing MongoDB query results")
