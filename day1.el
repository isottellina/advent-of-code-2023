(require 'dash)

(-sum (-map
       (lambda (line)
         (let ((digits (with-temp-buffer
                         (insert line)
                         (goto-char (point-min))
                         (cl-loop while (re-search-forward (rx (or digit "one" "two" "three" "four" "five" "six" "seven" "eight" "nine")) nil t)
                                  collect (let ((match-content (match-string-no-properties 0)))
                                            (cond ((string= match-content "one") "1")
                                                  ((string= match-content "two") "2")
                                                  ((string= match-content "three") "3")
                                                  ((string= match-content "four") "4")
                                                  ((string= match-content "five") "5")
                                                  ((string= match-content "six") "6")
                                                  ((string= match-content "seven") "7")
                                                  ((string= match-content "eight") "8")
                                                  ((string= match-content "nine") "9")
                                                  (t match-content)))
                                  do (goto-char (1+ (match-beginning 0)))))))
           (string-to-number (concat (-first-item digits) (-last-item digits)))))
       (with-temp-buffer
         (insert-file-contents "input.txt")
         (split-string (buffer-string) "\n" t))))
