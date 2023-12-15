(defun aoc/hash-string (string)
  (--reduce-from (mod (* (+ acc it) 17) 256) 0 (append string nil)))

(defun add-to-boxes (boxes seq lens-to-add)
  "If lens-to-add is nil, remove the lens"
  (let ((box-id (aoc/hash-string seq)))
    (setf (alist-get seq (aref boxes box-id) nil t #'string=)
          (when lens-to-add (string-to-number lens-to-add)))))

(with-current-buffer "input.txt"
  (goto-char (point-min))
  (let ((boxes (cl-loop with boxes = (make-vector 256 nil)
                     while (re-search-forward (rx (group (1+ alnum)) (or (seq ?= (group digit)) ?-)) nil t)
                     do (add-to-boxes boxes (match-string-no-properties 1) (match-string-no-properties 2))
                     finally return boxes)))
    (seq-map-indexed (lambda (box box-id) (let ((box-length (length box))) (seq-map-indexed (lambda (slot-content slot-id) (* (1+ box-id) (- box-length slot-id) (cdr slot-content))) box)))
                     boxes)))
