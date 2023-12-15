(defun aoc/hash-string (string)
  (--reduce-from (mod (* (+ acc it) 17) 256) 0 (append string nil)))


(with-current-buffer "input.txt"
  (-sum (mapcar #'aoc/hash-string (string-split (string-trim (buffer-string)) ","))))
