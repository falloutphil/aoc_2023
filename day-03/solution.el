;;; solution.el --- Day 3 Advent of Code Solution

;; Calculate the absolute path to the 'common' directory
(defvar aoc-root-dir (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))
(defvar aoc-common-dir (expand-file-name "common" aoc-root-dir))
;; Add 'common' to the load-path
(add-to-list 'load-path aoc-common-dir)

(defvar day-03-input-file (expand-file-name "input.txt" (file-name-directory (or load-file-name buffer-file-name))))

(require 'file-utils)
(require 'result-utils)
(require 'ert)
(require 'cl-lib)

(defun parse-schematic (line)
  "Parse a single line of the schematic into a vector of characters."
  (vconcat (split-string line "" t)))

(defun is-adjacent-to-symbol? (x y schematic)
  "Check if the position (x, y) is adjacent to any symbol."
  (catch 'found-symbol
    (cl-loop for dx from -1 to 1 do
             (cl-loop for dy from -1 to 1 do
                      (when (and (not (and (= dx 0) (= dy 0)))
                                 (>= (+ x dx) 0) (>= (+ y dy) 0)
                                 (< (+ x dx) (length schematic))
                                 (< (+ y dy) (length (aref schematic 0))))
                        (let ((cell (aref (aref schematic (+ y dy)) (+ x dx))))
                          (when (string-match-p "[^0-9.]" cell)
                            (message "DEBUG: Adjacent symbol found at %d,%d for gear at %d,%d" (+ x dx) (+ y dy) x y)
                            (throw 'found-symbol t))))))
    nil))


(defun sum-part-numbers (schematic)
  "Calculate the sum of all part numbers in the schematic."
  (let ((sum 0))
    (cl-loop for y from 0 below (length schematic) do
             (cl-loop for x from 0 below (length (aref schematic y)) do
                      (let ((cell (aref (aref schematic y) x)))
                        (when (string-match-p "[0-9]" cell)
                          (let ((end x)
                                (number-string cell))
                            (while (and (< (+ end 1) (length (aref schematic y)))
                                        (string-match-p "[0-9]" (aref (aref schematic y) (+ end 1))))
                              (cl-incf end)
                              (setq number-string (concat number-string (aref (aref schematic y) end))))
                            (when (cl-loop for i from x to end thereis (is-adjacent-to-symbol? i y schematic))
                              (cl-incf sum (string-to-number number-string)))
                            (setq x end))))))
    sum))

;; part 2

(defun find-full-number (x y schematic)
  "Find the full number starting from position (x, y)"
  (let ((cell (aref (aref schematic y) x))
        (start x)
        (end x)
        (number-string cell))
    ;; Extend the number to the left as far as possible
    (while (and (> start 0)
                (string-match-p "[0-9]" (aref (aref schematic y) (- start 1))))
      (cl-decf start)
      (setq number-string (concat (aref (aref schematic y) start) number-string)))
    ;; Extend the number to the right as far as possible
    (while (and (< end (- (length (aref schematic y)) 1))
                (string-match-p "[0-9]" (aref (aref schematic y) (+ end 1))))
      (cl-incf end)
      (setq number-string (concat number-string (aref (aref schematic y) end))))
    (string-to-number number-string)))

(defun find-adjacent-parts (x y schematic)
  "Find all part numbers adjacent to position (x, y)."
  (let ((parts '()))
    (cl-loop for dx from -1 to 1 do
             (cl-loop for dy from -1 to 1 do
                      (when (and (not (and (= dx 0) (= dy 0))) ; Exclude the center cell
                                 (>= (+ x dx) 0) (>= (+ y dy) 0) ; Ensure indices are within bounds
                                 (< (+ x dx) (length schematic)) ; x + dx less than schematic width
                                 (< (+ y dy) (length (aref schematic 0)))) ; y + dy less than schematic height
                        (let* ((adj-x (+ x dx))
                               (adj-y (+ y dy))
                               (cell (aref (aref schematic adj-y) adj-x)))
                          (when (and (string-match-p "^[0-9]" cell) ; Check if the cell starts with a digit
                                     (< (length parts) 2)) ; Ensure we only consider the first two parts
                            (let ((full-number (find-full-number adj-x adj-y schematic)))
                              (when (and full-number (not (member full-number parts)))
                                (push full-number parts))))))))
    (if (= (length parts) 2)
        parts
      nil)))

(defun sum-gear-ratios (schematic)
  "Calculate the sum of all the multiplied gear ratios."
  (let ((sum 0))
    (cl-loop for y from 0 below (length schematic) do
             (cl-loop for x from 0 below (length (aref schematic y)) do
                      (let ((cell (aref (aref schematic y) x)))
                        (when (equal cell "*") ; check for gear symbol
                          ;(message "DEBUG: Checking gear at %d,%d" x y)
                          (let ((adjacent-parts (find-adjacent-parts x y schematic)))
                            (when adjacent-parts ; valid gear found
                              ;(message "DEBUG: Gear at %d,%d with adjacent parts %s" x y adjacent-parts)
                              (cl-incf sum (* (nth 0 adjacent-parts) (nth 1 adjacent-parts)))
                              ;(message "DEBUG: New sum after gear at %d,%d: %d" x y sum)
                              ))))))
    sum))


(defvar day-03-test-data
  (vector
   (parse-schematic "467..114..")
   (parse-schematic "...*......")
   (parse-schematic "..35..633.")
   (parse-schematic "......#...")
   (parse-schematic "617*......")
   (parse-schematic ".....+.58.")
   (parse-schematic "..592.....")
   (parse-schematic "......755.")
   (parse-schematic "...$.*....")
   (parse-schematic ".664.598.."))
  "Example schematic for day 3.")

(ert-deftest day-03-tests ()
  (should (= (sum-part-numbers day-03-test-data) 4361))
  (should (= (sum-gear-ratios day-03-test-data) 467835)))

(defun day-03-part-01 (lines)
  "Calculate the sum of part numbers for Day 3, Part 1."
  (sum-part-numbers (vconcat (mapcar 'parse-schematic lines))))

(defun day-03-part-02 (lines)
  "Calculate the sum of gear ratios for Day 3, Part 2."
  (sum-gear-ratios (vconcat (mapcar 'parse-schematic lines))))


(let ((lines (read-lines day-03-input-file)))
  (display-results (list (day-03-part-01 lines)
                         (day-03-part-02 lines))
                   '("Part 01 - The sum of the part numbers"
                     "Part 02 - The sum of the gear ratios")))

(ert-run-tests-interactively "day-03-tests")
