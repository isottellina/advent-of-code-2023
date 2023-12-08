;; -*- lexical-binding: t; -*-

(require 's)

(defun day8/next-node (current-node move)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (rx (seq (literal current-node) " = ("
                                (group (= 3 alnum)) ", " (group (= 3 alnum))
                                ")")))
    (match-string-no-properties (cond ((= move ?L) 1)
                                      ((= move ?R) 2)))))

(defun day8/time-to-end-node (start-node path)
  (cl-loop for counter from 0
           for move-to-do = (seq-elt path (mod (- counter 1) (seq-length path)))
           for current-node = start-node then (day8/next-node current-node move-to-do)
           count (/= counter 0)
           until (s-ends-with? "Z" current-node)))

(with-current-buffer "input.txt"
  (goto-char (point-min))
  (let* ((path (buffer-substring (point-min) (line-end-position)))
         (times-to-end (cl-loop while (re-search-forward "\\([[:alnum:]]\\{2\\}A\\) = " nil t)
                                collect (day8/time-to-end-node (match-string 1) path))))
    (apply #'cl-lcm times-to-end)))
