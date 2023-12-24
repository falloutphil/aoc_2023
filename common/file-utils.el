;;; file-utils.el --- Utility functions for file operations

;; Define a function to read lines from a file
(defun read-lines (file-path)
  "Read FILE-PATH and return a list of lines."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(provide 'file-utils)
;;; file-utils.el ends here
