;;; -*- lexical-binding: t; -*-

(provide 'mongodb-query)

(require 'mongodb-shell)

(require 'cl-lib)

(defvar-local mongodb-query-body nil)
(defvar-local mongodb-query-cursor-id nil)
(defvar-local mongodb-is-cursor-result nil)

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

(defun mongodb-query-completion-function ()
  (when-let* ((start (save-excursion (re-search-backward "\\$" nil t)))
              (end (or (cdr (bounds-of-thing-at-point 'word)) start)))
    (list start end mongodb-query-keywords)))

(define-derived-mode
  mongodb-query-mode
  javascript-mode
  "MongoDB Query"
  "Major mode for creating MongoDB queries"
  (add-hook 'completion-at-point-functions #'mongodb-query-completion-function nil t))

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
