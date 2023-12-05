;; -*- lexical-binding: t; -*-

(defconst map-names
  '(seed-to-soil
    soil-to-fertilizer
    fertilizer-to-water
    water-to-light
    light-to-temperature
    temperature-to-humidity
    humidity-to-location))

(cl-defstruct range
  (start 0 :type number)
  (length 0 :type number))

(defun range-start< (range &rest others)
  (eval `(< (range-start ,range) ,@(mapcar (lambda (range) `(range-start ,range)) others))))

(defun range-end (range)
  (+ (range-start range) (range-length range)))

(defun range-containsp (range position)
  (and (<= (range-start range) position)
       (< position (range-end range))))

(defun range-intersectp (range other-range)
  (or (range-containsp range (range-start other-range))
      (range-containsp other-range (range-start range))))

(cl-defstruct (transformation-range
               (:constructor nil)
               (:constructor new-transformation-range (&key source-range-start
                                                             source-range-length
                                                             destination-range-start
                                                        &aux (source-range (make-range :start source-range-start :length source-range-length)))))

  (destination-range-start 0 :type number)
  (source-range nil :type range))

(defun transformation-range-start< (range &rest others)
  (eval `(range-start< (transformation-range-source-range ,range) ,@(mapcar (lambda (range) `(transformation-range-source-range ,range)) others))))

(defun transformation-range-start (tr-range)
  (range-start (transformation-range-source-range tr-range)))

(defun transformation-range-transformation (tr-range)
  (- (transformation-range-destination-range-start tr-range) (transformation-range-start tr-range)))

(defun transformation-range-end (tr-range)
  (+ (transformation-range-start tr-range) (range-length (transformation-range-source-range tr-range))))

(defun apply-transformation-ranges (tr-ranges start-range)
  (cl-labels ((next-tr-range (current-range) (--first (range-start< current-range (transformation-range-source-range it)) tr-ranges))
              (currently-in-range (current-range) (--first (range-containsp (transformation-range-source-range it) (range-start current-range)) tr-ranges))
              (create-range-to (current-range next-tr)
                (let* ((distance-to-next (- (transformation-range-start next-tr) (range-start current-range)))
                       (length-to-create (min distance-to-next (range-length current-range)))
                       (new-current-range (make-range
                                           :start (+ (range-start current-range) length-to-create)
                                           :length (- (range-length current-range) length-to-create)))
                       (range-created (make-range
                                       :start (range-start current-range)
                                       :length length-to-create)))
                  (cons new-current-range range-created)))
              (create-range-in (current-range tr)
                (let* ((distance-to-end (- (transformation-range-end tr) (range-start current-range)))
                       (length-to-create (min distance-to-end (range-length current-range)))
                       (new-current-range (make-range
                                           :start (+ (range-start current-range) length-to-create)
                                           :length (- (range-length current-range) length-to-create)))
                       (range-created (make-range
                                       :start (+ (range-start current-range) (transformation-range-transformation tr))
                                       :length length-to-create)))
                  (cons new-current-range range-created)))
              (create-end-range (current-range) (cons (make-range :start (range-end current-range) :length 0) current-range))
              (apply-ranges (range-left ranges-created)
                (let ((in-tr (currently-in-range range-left))
                      (next-tr (next-tr-range range-left)))
                  (cond ((= (range-length range-left) 0) ranges-created) ; End condition
                        (in-tr (pcase-let ((`(,new-range . ,range-created) (create-range-in range-left in-tr)))
                                 (apply-ranges new-range (cons range-created ranges-created)))) ; In a range? Run as far as possible
                        (next-tr (pcase-let ((`(,new-range . ,range-created) (create-range-to range-left next-tr)))  ; Not in a TR but there's one? Create a range to one
                                       (apply-ranges new-range (cons range-created ranges-created))))
                        (t (pcase-let ((`(,new-range . ,range-created) (create-end-range range-left)))
                             (apply-ranges new-range (cons range-created ranges-created))))))))
    (apply-ranges start-range nil)))

(defun apply-transformation-from-hash (hashmap map-key n)
  (pcase n
    ((cl-type list) (--mapcat (apply-transformation-ranges (gethash map-key hashmap) it) n))
    (_ (apply-transformation-ranges (gethash map-key hashmap) n))))

(defun read-range-list ()
  (cl-loop while (re-search-forward "\\([[:digit:]]+\\) \\([[:digit:]]+\\)" (line-end-position) t)
           collect (make-range :start (string-to-number (match-string 1))
                               :length (string-to-number (match-string 2)))))

(defun read-map (map-name)
  (goto-char (point-min))
  (-sort #'transformation-range-start< (cl-loop initially do (search-forward (symbol-name map-name) nil t)
                                 do (forward-line)
                                 until (and (bolp) (eolp))
                                 do (re-search-forward "\\([[:digit:]]+\\) \\([[:digit:]]+\\) \\([[:digit:]]+\\)")
                                 collect (new-transformation-range :destination-range-start (string-to-number (match-string 1))
                                                                   :source-range-start (string-to-number (match-string 2))
                                                                   :source-range-length (string-to-number (match-string 3))))))

(defmacro apply-all-transformations (var)
  `(->> ,var
        ,@(mapcar (lambda (map-symbol)
                    `(apply-transformation-from-hash stored-maps (quote ,map-symbol)))
                  map-names)))

(with-current-buffer "input.txt"
  (goto-char (point-min))
  (let* ((seeds (read-range-list))
         (stored-maps (make-hash-table)))
    (cl-loop for map-name in map-names
             do (puthash map-name (read-map map-name) stored-maps))
    (cl-loop for seed in seeds
             minimize (-min (-map #'range-start (apply-all-transformations seed))))))
