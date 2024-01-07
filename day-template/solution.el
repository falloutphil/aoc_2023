;;; solution.el --- Day X Advent of Code Solution

;; Calculate the absolute path to the 'common' directory
(defvar aoc-root-dir (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))
(defvar aoc-common-dir (expand-file-name "common" aoc-root-dir))
;; Add 'common' to the load-path
(add-to-list 'load-path aoc-common-dir)

(defvar day-XX-input-file (expand-file-name "input.txt" (file-name-directory (or load-file-name buffer-file-name))))

(require 'file-utils)
(require 'result-utils)
(require 'ert)
(require 'cl-lib)

(defun parse-input (line)
  "Parse a single line of the input into a structure."
  '(structured data to return 1234))


(defvar day-XX-test-data
  (mapcar 'parse-input
          '("SOME NUMBERS AND STUFF TO TEST WITH"
            "ANOTHER LINE OF TEST DATA"
            ))
  "Example schematic for day X.")

(defun some-function (parsed-line)
  1234)

(defun some-other-function (parsed-line)
  5678)


(ert-deftest day-XX-tests ()
  (should (= (some-function day-XX-test-data) 1234))
  (should (= (some-other-function day-XX-test-data) 5678)))

(defun day-XX-part-01 (lines)
  "Day X, Part 1."
  (some-function (mapcar 'parse-input lines)))

(defun day-XX-part-02 (lines)
  "Day X, Part 2."
  (some-other-function (mapcar 'parse-input lines)))


;(let ((lines (read-lines day-XX-input-file)))
;  (display-results (list (day-XX-part-01 lines)
;                         (day-XX-part-02 lines))
;                   '("Part 01 - Some text to describe"
;                     "Part 02 - Some more text to describe")))

(ert-run-tests-interactively "day-XX-tests")
