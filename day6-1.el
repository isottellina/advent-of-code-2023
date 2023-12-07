(defun number-of-solutions (race-time race-record)
  (let* ((a -1)
         (b race-time)
         (c (- 0 race-record))
         (delta (- (* b b) (* 4 a c)))
         (first-solution (/ (- 0 b (sqrt delta)) (* 2 a)))
         (second-solution (/ (+ (- 0 b) (sqrt delta)) (* 2 a)))
         (low-solution (min first-solution second-solution))
         (high-solution (max first-solution second-solution))
         (floored-low (floor low-solution))
         (floored-high (floor high-solution)))
    (cond ((= low-solution high-solution) 0)
          ((= high-solution floored-high) (- floored-high floored-low 1))
          (t (- floored-high floored-low)))))

(defun parse-line-of-numbers ()
  (cl-loop while (re-search-forward "[[:digit:]]+" (line-end-position) t)
           collect (string-to-number (match-string 0))))

(with-current-buffer "input.txt"
  (goto-char (point-min))
  (let (time distance)
    (setq time (parse-line-of-numbers))
    (forward-line)
    (setq distance (parse-line-of-numbers))
    (-product (cl-loop for race-time in time
                       for race-record in distance
                       collect (number-of-solutions race-time race-record)))))