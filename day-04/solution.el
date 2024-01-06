;;; solution.el --- Day 4 Advent of Code Solution

;; Calculate the absolute path to the 'common' directory
(defvar aoc-root-dir (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))
(defvar aoc-common-dir (expand-file-name "common" aoc-root-dir))
;; Add 'common' to the load-path
(add-to-list 'load-path aoc-common-dir)

(defvar day-04-input-file (expand-file-name "input.txt" (file-name-directory (or load-file-name buffer-file-name))))

(require 'file-utils)
(require 'result-utils)
(require 'ert)
(require 'cl-lib)

(defun parse-card (line)
  "Parse a single line of the schematic into a card structure."
  (let* ((card-number (progn
                        (string-match "Card\\s-*\\([0-9]+\\):" line)
                        (string-to-number (match-string 1 line))))
         (clean-line (replace-match "" nil nil line))  ; Remove the "Card x:" part
         (parts (split-string clean-line "|"))  ; Split the line at the '|'
         (winning-part (car parts))  ; The first part contains winning numbers
         (your-part (cadr parts))  ; The second part contains your numbers
         (winning-numbers (mapcar 'string-to-number (split-string winning-part)))
         (your-numbers (mapcar 'string-to-number (split-string your-part))))
    `((card . ,card-number) (winning . ,winning-numbers) (your . ,your-numbers))))


(defun count-matches (card)
  "Count the number of matches for a given card."
  (let ((winning (alist-get 'winning card))
        (your (alist-get 'your card)))
    (length (cl-intersection winning your :test 'equal))))


(defun sum-scratchcard-points (cards)
  "Sum the points of all scratchcards."
  (let ((total-points 0))
    (dolist (card cards total-points)
      (let ((matches (count-matches card)))
        (when matches  ; Only calculate if there are any matches
          (cl-incf total-points (lsh 1 (1- matches))))))))


(defun count-total-scratchcards-inefficient (cards)
  "Count the total number of scratchcards after processing all originals and copies."
  (let ((total-cards 0)  ; Counter for total cards.
        (to-process (copy-sequence cards)))  ; Queue to hold cards to process.
    (while to-process
      (let* ((card (pop to-process))  ; Take the first card from the queue.
             (matches (count-matches card)))  ; Count matches for this card.
        (cl-incf total-cards)  ; Increment total cards processed.
        (dotimes (j matches)  ; For each match, add a copy of the next cards to the queue.
          (let ((next-index (+ (alist-get 'card card) j)))
            (when (< next-index (length cards))  ; Ensure the index is valid.
              (setq to-process (append to-process (list (nth next-index cards)))))))))  ; Add copies to the end of the queue.
    total-cards))  ; Return the total count of cards processed.

(defun count-total-scratchcards (cards)
  "Count the total number of scratchcards after processing all originals and copies."
  (let* ((total-cards (length cards))  ; Total number of different cards.
         (card-counts (make-vector total-cards 1))  ; Count of each card, initially 1 each.
         (processed (make-vector total-cards nil)))  ; Track whether a card has been processed.
    (dotimes (i total-cards)
      (unless (aref processed i)  ; Only process unprocessed cards.
        (aset processed i t)  ; Mark the card as processed.
        (let* ((card (nth i cards))
               (matches (count-matches card))
               (copies (aref card-counts i)))  ; How many of this card do we have?
          (dotimes (j matches)
            (let ((next-index (+ (alist-get 'card card) j)))
              (when (< next-index total-cards)
                (cl-incf (aref card-counts next-index) copies)))))))  ; Add copies to subsequent cards.
    (apply '+ (append card-counts nil))))  ; Sum the counts of all cards.


(defvar day-04-test-data
  (mapcar 'parse-card
          '("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
            "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
            "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
            "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
            "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
            "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
            ))
  "Example schematic for day 4.")

(ert-deftest day-04-tests ()
  (should (= (sum-scratchcard-points day-04-test-data) 13))
  (should (= (count-total-scratchcards day-04-test-data) 30)))

(defun day-04-part-01 (lines)
  "Day 4, Part 1."
  (sum-scratchcard-points (mapcar 'parse-card lines)))


(defun day-04-part-02 (lines)
  "Day 4, Part 2."
  (count-total-scratchcards (mapcar 'parse-card lines)))


(let ((lines (read-lines day-04-input-file)))
  (display-results (list (day-04-part-01 lines)
                         (day-04-part-02 lines))
                   '("Part 01 - How many points?"
                     "Part 02 - How many total scratchcards?")))

(ert-run-tests-interactively "day-04-tests")
