;;; result-utils.el --- Utility functions for displaying results

(defun display-results (results messages)
  "Display each result with its corresponding message in a separate buffer."
  (let ((buffer (get-buffer-create "*Advent of Code Results*")))  ; Create a new buffer
    (with-current-buffer buffer
      (erase-buffer)  ; Clear existing content
      ;; Insert timestamp
      (insert (format "Results as of: %s\n\n" (current-time-string)))
      ;; Insert each message and corresponding result
      (cl-loop for result in results
               for message in messages
               do (insert (format "%s: %s\n" message result))))
    (switch-to-buffer-other-window buffer)))  ; Display the buffer
                                        ;
(provide 'result-utils)
;;; result-utils.el ends here
