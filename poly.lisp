(in-package noise-range-test)

(defun rtrim (c)
  (let ((z (1+ (position 0 c :test '/= :from-end t))))
    (if (eql z (length c))
        c
        (subseq c 0 z))))

(defun poly* (a b)
  (let ((c (make-array  (+ (length a) (length b)) :initial-element 0)))
    (loop for ac across a
          for i from 0
          do (loop for bc across b
                   for j from 0
                   do (incf (aref c (+ i j))
                            (* ac bc))))
    (rtrim c)))

(defun int (a)
  (let ((c (make-array (1+ (length a)) :initial-element 0)))
    (loop for ac across a
          for i from 0
          do (setf (aref c (1+ i))
                   (/ ac (1+ i))))
    c))

(defun pexpt (a x)
  (cond
    ((zerop x)
     #(1))
    ((minusp x)
     (error "todo"))
    ((not (integerp x))
     (error "todo"))
    (t
     (loop with i = #(1)
           repeat x
           do (setf i (poly* i a))
           finally (return i)))))

(defun peval (p x)
  (loop for c across p
        for xn = 1 then (* xn x)
        sum (* c xn)))

#++
(loop for i below 6 do (print (int (pexpt #(1 0 -1) i))))

#++
(int (pexpt #(1 0 -1) 1))
#++
(poly* #(1 0 -1 0 0) #(1 0 -1 0 0))
