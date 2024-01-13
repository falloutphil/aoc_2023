;;; solution.el --- Day 5 Advent of Code Solution

;; Calculate the absolute path to the 'common' directory
(defvar aoc-root-dir (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))
(defvar aoc-common-dir (expand-file-name "common" aoc-root-dir))
;; Add 'common' to the load-path
(add-to-list 'load-path aoc-common-dir)

(defvar day-05-input-file (expand-file-name "input.txt" (file-name-directory (or load-file-name buffer-file-name))))

(require 'file-utils)
(require 'result-utils)
(require 'ert)
(require 'cl-lib)

;; maps: ((seed-to-soil ((52 50 48) (50 98 2))))  --- realistically would have seeds in here too
;; existing-maps: (seed-to-soil ((52 50 48) (50 98 2)))
;; existing-ranges: ((52 50 48) (50 98 2))
(defun parse-input (lines)
  "Parse the entire input into a structured format."
  (let ((current-map nil)
        (maps ()))
    (dolist (line lines)
      (cond
       ;; Handle seed lines by converting numbers to a list of integers.
       ((string-match "^seeds: \\([0-9 ]+\\)$" line)
        (push (cons 'seeds (mapcar 'string-to-number (split-string (match-string 1 line) " "))) maps))

       ;; Update the current map identifier when a new map is encountered, with a symbol.
       ((string-match "^\\(\\w+\\)-to-\\(\\w+\\) map:$" line)
        (setq current-map (intern (concat (match-string 1 line) "-to-" (match-string 2 line)))))

       ;; Update mapping for the current map type.
       ((string-match "^\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)$" line)
        (let* ((dest-start (string-to-number (match-string 1 line)))
               (src-start (string-to-number (match-string 2 line)))
               (length (string-to-number (match-string 3 line)))
               (existing-maps (assoc current-map maps)))
          (if existing-maps
              ;; Prepend the new range to the existing ranges.
              (push (list dest-start src-start length) (cdr existing-maps))
            ;; Else, create a new mapping entry.
            (push (cons current-map (list (list dest-start src-start length))) maps))))

       ;; Ignore lines that don't match the expected patterns.
       (t nil)))
    (nreverse maps)))   ; order of maps must refelect insertion order


(defun day-05-test-data (parser)
  (funcall parser
    '("seeds: 79 14 55 13"
      ""
      "seed-to-soil map:"
      "50 98 2"
      "52 50 48"
      ""
      "soil-to-fertilizer map:"
      "0 15 37"
      "37 52 2"
      "39 0 15"
      ""
      "fertilizer-to-water map:"
      "49 53 8"
      "0 11 42"
      "42 0 7"
      "57 7 4"
      ""
      "water-to-light map:"
      "88 18 7"
      "18 25 70"
      ""
      "light-to-temperature map:"
      "45 77 23"
      "81 45 19"
      "68 64 13"
      ""
      "temperature-to-humidity map:"
      "0 69 1"
      "1 0 69"
      ""
      "humidity-to-location map:"
      "60 56 37"
      "56 93 4")))


(defun map-lookup (candidate mapping)
  "Find the corresponding value in the mapping or return the same value if not found."
  (let ((found (cl-find-if (lambda (range)      ; range is of the form (destination source length)
                             (and (<= (nth 1 range) candidate)  ; source lower bound <= candidate
                                  (> (+ (nth 1 range) (nth 2 range)) candidate)))       ; source+length > candidate
                           (cdr mapping))))     ; list of all the ranges to test until first match
    (if found
        ;; find how far candidate is into source and add that to destination start
        ;; to find the mapped values in the destination.
        (+ (nth 0 found) (- candidate (nth 1 found)))
      candidate)))      ; if not found spec is to return the original candidate


(defun find-location (seed maps)
  "Find the location corresponding to the given seed by iterating through maps in sequence."
  (dolist (map maps seed) ; this assumes we've removed the seeds from the map, and the transforms are in order
    (setq seed (map-lookup seed map))))


(defun lowest-location (maps)
  "Find the lowest location for the initial seeds."
  (let* ((seeds (cdr (assoc 'seeds maps)))
         (remaining-maps (cdr maps)))  ; Extract the maps excluding the seeds
    ;; For each seed in our map, find all the locations and then the min
    (apply 'min (mapcar (lambda (seed) (find-location seed remaining-maps)) seeds))))

(defun remap (remaining-inputs translated-ranges mappings) ; initially start end will be a seed range
  "Partially remap start-end input range to a destination range for a specific set of mappings for a single map-type"
  (let* ((input-range (pop remaining-inputs)) ; pop each input range and remap it if possible
         (start (car input-range))
         (end (cdr input-range)))

  (catch 'break
    (dolist (mapping mappings) ; each map-type has many mappings, loop over them
      (let* ((destination-range-start (nth 0 mapping))
             (source-range-start (nth 1 mapping))
             (range-length (nth 2 mapping))
             (overlap-start (max start source-range-start))
             (overlap-end (min end (+ source-range-start range-length))))
        (when (< overlap-start overlap-end) ; when there is some overlap
          ;; translate and prepend the overlap range
          (push (cons (+ destination-range-start (- overlap-start source-range-start)) ; start point in destination range
                      (+ destination-range-start (- overlap-end source-range-start))) ; end point in destination range
                translated-ranges) ; append the translated range in the queue for the next map-type

          ;; any unmapped input sub-ranges either side of overlapping ranges
          ;; get placed back into the remaining input ranges in case a further map
          ;; in this map-type can handle it with a subsequent call to remap
          (when (< overlap-end end)
            (push (cons overlap-end end) remaining-inputs))
          (when (< start overlap-start)
            (push (cons start overlap-start) remaining-inputs))

	  ;; early exit - we only map the first matching source range
          (throw 'break nil))))
    ;; if you get to the end of the dolist without throwing, there is nothing else
    ;; we can do in this map-type so we carry the untranslated range
    ;; forward to the next map-type
    (push input-range translated-ranges)))
  ;; return both the remaining and translated lists
  (cons remaining-inputs translated-ranges))


(defun lowest-location-ranges (parsed-data)
  "Find the lowest location for the initial seed ranges."
  (let* ((seeds (cdr (assoc 'seeds parsed-data)))
         (inputs (cl-loop for (start length) on seeds by 'cddr ; jump by cddr - windows of 2 elements
                          collect (cons start (+ start length)))) ; collect results translating length to (upper . lower)
         (map-types (mapcar 'cdr (cdr parsed-data))))
    (dolist (maps map-types) ; loop over each map-type (eg seed-to-soil) in sequence - order is important!
      (let ((translated-ranges '()))  ; Initialize the results for this map-type
        (while inputs
          (let ((results (remap inputs translated-ranges maps)))
            (setq inputs (car results)
                  translated-ranges (cdr results))))
        ;;(message "Final translated-range: %s" translated-range)
        (setq inputs translated-ranges)))       ; take translated ranges and run through the next map-type
    ;;(message "Final inputs: %s" inputs)
    (apply 'min (mapcar 'car inputs))))


(ert-deftest day-05-tests ()
  (should (= (lowest-location (day-05-test-data 'parse-input)) 35))
  (should (= (lowest-location-ranges (day-05-test-data 'parse-input)) 46)))

(defun day-05-part-01 (lines)
  "Day 5, Part 1."
  (lowest-location (parse-input lines)))

(defun day-05-part-02 (lines)
  "Day 5, Part 2."
  (lowest-location-ranges (parse-input lines)))

(let ((lines (read-lines day-05-input-file)))
  (display-results (list (day-05-part-01 lines)
                         (day-05-part-02 lines))
                   '("Part 01 - The lowest location number"
                     "Part 02 - The lowest location number")))

(ert-run-tests-interactively "day-05-tests")
