;; -*- lexical-binding: t; -*-

(defun day8/next-node (current-node move)
  (goto-char (point-min))
  (re-search-forward (rx (seq (literal current-node)
                              " = ("
                              (group (= 3 upper))
                              ", "
                              (group (= 3 upper))
                              ")")))
  (match-string-no-properties (cond ((= move ?R) 2)
                                    ((= move ?L) 1))))

(with-current-buffer "input.txt"
  (goto-char (point-min))
  (1- (cl-loop with path = (buffer-substring (point-min) (line-end-position))
               for counter from 0
               for move-to-do = (seq-elt path (mod (- counter 1) (seq-length path)))
               for current-node = "AAA" then (day8/next-node current-node move-to-do)
               count 1
               until (string= current-node "ZZZ"))))
