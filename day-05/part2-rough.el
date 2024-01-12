
(setq seeds '((79 . 93) (55 . 68)))

(setq maps '(((50 98 2) (52 50 48))
            ((0 15 37) (37 52 2) (39 0 15))
            ((49 53 8) (0 11 42) (42 0 7) (57 7 4))
            ((88 18 7) (18 25 70))
            ((45 77 23) (81 45 19) (68 64 13))
            ((0 69 1) (1 0 69))
            ((60 56 37) (56 93 4))))

;; NOTE: Non-overlapping sub-ranges of a given seed range retain their original values,
;; rather than assuming they could overlap a different map candidate.
;; If we get a partial match, i.e. some of our seed range overlaps with only some of the source transformation -
;; we map to the overlapped inputs destination, but for the unmapped seeds we carry them over as-is,
;; with no transformation into the destination, rather than search other candidate maps.
;; This isn't massively clear from the puzzle text but we have the quote:
;; "Any source numbers that aren't mapped correspond to the same destination number."
;; Although this is mentioned in Part 1, we are to assume it also applies in Part 2!
;; This is important because we can short-circuit as soon as a seed-range partially overlaps
;; any candidate map for seed-to-soil, and so on.
(defun remap (start end new-seeds m)
  (catch 'break
    (dolist (mapping m) ; each map-type has many mappings loop over them
      (let* ((destination-range-start (nth 0 mapping))
             (source-range-start (nth 1 mapping))
             (range-length (nth 2 mapping))
             (overlap-start (max start source-range-start))
             (overlap-end (min end (+ source-range-start range-length))))
        (when (< overlap-start overlap-end) ; when there is some overlap
          (push (cons (+ destination-range-start (- overlap-start source-range-start))
                      (+ destination-range-start (- overlap-end source-range-start)))
                new-seeds)
          (when (< start overlap-start)
            (push (cons start overlap-start) new-seeds))
          (when (< overlap-end end)
            (push (cons overlap-end end) new-seeds))
	  ;; early exit - big assumption - once we've found an overlapping map we assume there are no others for our seed range
	  ;; So any non-overlapping segments retain their original values rather than assuming they could overlap elsewhere!
          (throw 'break nil))))
    (push (cons start end) new-seeds))
  new-seeds)

(dolist (m maps seeds) ; loop over each map-type in sequence - order is important!
  ;;(message "Start Seeds: %s" seeds)
  ;;(message "Map: %s" m)
  (setq new-seeds '())  ; Initialize new-seeds as an empty list for each map
  (while seeds
    (let ((pair (pop seeds)))
      (setq start (car pair))
      (setq end (cdr pair))
      (setq new-seeds (remap start end new-seeds m))))
  ;;(message "New Seeds: %s" new-seeds)
  (setq seeds new-seeds))

(let ((min-car (apply 'min (mapcar 'car seeds))))
  (message "Minimum location: %d" min-car))
