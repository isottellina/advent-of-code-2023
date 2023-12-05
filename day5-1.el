;; -*- lexical-binding: t; -*-

(defconst map-names
  '(seed-to-soil
    soil-to-fertilizer
    fertilizer-to-water
    water-to-light
    light-to-temperature
    temperature-to-humidity
    humidity-to-location))

(cl-defstruct transformation-range
  (start-destination 0 :type number)
  (start-source 0 :type number)
  (length 0 :type number))

(defun apply-transformation-range (tr-range n)
  (let* ((start-source (transformation-range-start-source tr-range))
         (end-source (+ start-source (transformation-range-length tr-range)))
         (start-destination (transformation-range-start-destination tr-range)))
    (if (<= start-source n end-source)
        (+ start-destination (- n start-source))
      nil)))

(defun apply-transformation-ranges (tr-ranges n)
  (cl-loop for tr-range in tr-ranges
           for range-result = (apply-transformation-range tr-range n)
           when range-result
           return range-result
           finally return n))

(defun apply-transformation-from-hash (hashmap map-key n)
  (apply-transformation-ranges (gethash map-key hashmap) n))

(defun read-number-list ()
  (cl-loop while (re-search-forward "[[:digit:]]+" (line-end-position) t)
           collect (string-to-number (match-string 0))))

(defun read-map (map-name)
  (goto-char (point-min))
  (cl-loop initially do (search-forward (symbol-name map-name) nil t)
           do (forward-line)
           until (and (bolp) (eolp))
           do (re-search-forward "\\([[:digit:]]+\\) \\([[:digit:]]+\\) \\([[:digit:]]+\\)")
           collect (make-transformation-range :start-destination (string-to-number (match-string 1))
                                              :start-source (string-to-number (match-string 2))
                                              :length (string-to-number (match-string 3)))))

(defmacro apply-all-transformations (var)
  `(->> ,var ,@(mapcar (lambda (map-symbol) `(apply-transformation-from-hash stored-maps (quote ,map-symbol))) map-names)))

(with-current-buffer "input.txt"
  (goto-char (point-min))
  (let* ((seeds (read-number-list))
         (stored-maps (make-hash-table)))
    (cl-loop for map-name in map-names
             do (puthash map-name (read-map map-name) stored-maps))
    (cl-loop for seed in seeds
             minimize (apply-all-transformations seed))))
