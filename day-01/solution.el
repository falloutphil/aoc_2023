;;; solution.el --- Day 1 Advent of Code Solution

;; Calculate the absolute path to the 'common' directory
(defvar aoc-root-dir (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))
(defvar aoc-common-dir (expand-file-name "common" aoc-root-dir))
;; Add 'common' to the load-path
(add-to-list 'load-path aoc-common-dir)

(defvar input-file (expand-file-name "input.txt" (file-name-directory (or load-file-name (buffer-file-name)))))

;; Ensure the file-utils are loaded
(require 'file-utils)

(defvar str2num
  '(("one" . "o1e") ("two" . "t2o") ("three" . "t3e") ("four" . "f4r")
    ("five" . "f5e") ("six" . "s6x") ("seven" . "s7n") ("eight" . "e8t") ("nine" . "n9e"))
  "Mapping of word digits to a numerical string.  Keep first and last letters in case adjacent numbers share letters.")

(defun replace-words (text)
  "Replace all instances of words in TEXT according to the str2num mapping."
  (let ((result text))
    (dolist (pair str2num result)
      (setq result (replace-regexp-in-string (car pair) (cdr pair) result)))))

(defun extract-calibration-value (line)
  "Extract the calibration value from a line of text."
  (let ((digits (seq-filter (lambda (char) (and (>= char ?0) (<= char ?9))) line)))
    (if (>= (length digits) 2)
      (string-to-number (concat (list (car digits) (car (last digits)))))
      (string-to-number (concat (list (car digits) (car digits)))))))

(defun sum-calibration-values (extractor file-path)
  "Calculate the sum of calibration values from the file at FILE-PATH."
  (apply '+ (mapcar extractor (read-lines file-path))))

(defun day-01-part-01 ()
  "Part 1"
  (message "The sum of all calibration values is: %s"
           (sum-calibration-values 'extract-calibration-value input-file)))

(defun day-01-part-02 ()
  "Part 2"
  (message "The sum of all calibration values is: %s"
           (sum-calibration-values (lambda (line) (extract-calibration-value (replace-words line))) input-file)))

(day-01-part-01)
(day-01-part-02)
