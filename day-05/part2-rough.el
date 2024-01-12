
(setq seeds '((79 . 93) (55 . 68)))

(setq maps '(((50 98 2) (52 50 48))
            ((0 15 37) (37 52 2) (39 0 15))
            ((49 53 8) (0 11 42) (42 0 7) (57 7 4))
            ((88 18 7) (18 25 70))
            ((45 77 23) (81 45 19) (68 64 13))
            ((0 69 1) (1 0 69))
            ((60 56 37) (56 93 4))))

(defun remap (start end new-seeds m)
  (catch 'break
    (dolist (mapping m)
      (let* ((destination-range-start (nth 0 mapping))
             (source-range-start (nth 1 mapping))
             (range-length (nth 2 mapping))
             (overlap-start (max start source-range-start))
             (overlap-end (min end (+ source-range-start range-length))))
        (when (< overlap-start overlap-end)
          (push (cons (+ destination-range-start (- overlap-start source-range-start))
                      (+ destination-range-start (- overlap-end source-range-start)))
                new-seeds)
          (when (< start overlap-start)
            (push (cons start overlap-start) new-seeds))
          (when (< overlap-end end)
            (push (cons overlap-end end) new-seeds))
          (throw 'break nil)))) ; early exit
    (push (cons start end) new-seeds))
  new-seeds)


(dolist (m maps seeds)
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
