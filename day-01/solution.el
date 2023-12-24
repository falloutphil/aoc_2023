;;; solution.el --- Day 1 Advent of Code Solution

;; Calculate the absolute path to the 'common' directory
(defvar aoc-root-dir (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))
(defvar aoc-common-dir (expand-file-name "common" aoc-root-dir))
;; Add 'common' to the load-path
(add-to-list 'load-path aoc-common-dir)

(defvar input-file (expand-file-name "input.txt" (file-name-directory (or load-file-name (buffer-file-name)))))

;; Ensure the file-utils are loaded
(require 'file-utils)

(defun extract-calibration-value (line)
  "Extract the calibration value from a line of text."
  (let ((digits (seq-filter (lambda (char) (and (>= char ?0) (<= char ?9))) line)))
    (if (>= (length digits) 2)
      (string-to-number (concat (list (car digits) (car (last digits)))))
      (string-to-number (concat (list (car digits) (car digits)))))))

(defun sum-calibration-values (file-path)
  "Calculate the sum of calibration values from the file at FILE-PATH."
  (apply '+ (mapcar 'extract-calibration-value (read-lines file-path))))

(defun day-01-part-01 ()
  "Part 1"
  (message "The sum of all calibration values is: %s" (sum-calibration-values input-file)))

(day-01-part-01)
