(-sum (-map
       (lambda (line)
         (let (digits)
           (with-temp-buffer
             (insert line)
             (while (re-search-backward (rx (or digit "one" "two" "three" "four" "five" "six" "seven" "eight" "nine")) nil t)
               (setq digits (cons (cond ((string= (match-string-no-properties 0) "one") "1")
                                        ((string= (match-string-no-properties 0) "two") "2")
                                        ((string= (match-string-no-properties 0) "three") "3")
                                        ((string= (match-string-no-properties 0) "four") "4")
                                        ((string= (match-string-no-properties 0) "five") "5")
                                        ((string= (match-string-no-properties 0) "six") "6")
                                        ((string= (match-string-no-properties 0) "seven") "7")
                                        ((string= (match-string-no-properties 0) "eight") "8")
                                        ((string= (match-string-no-properties 0) "nine") "9")
                                        (t (match-string-no-properties 0))) digits))
               (goto-char (1- (match-end 0)))))
           (string-to-number (concat (-first-item digits) (-last-item digits)))))
       (with-temp-buffer
         (insert-file-contents "input.txt")
         (split-string (buffer-string) "\n" t))))
