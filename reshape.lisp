(in-package noise-range-test)

(defconstant +max-polynomials+ 1000)

;; contains coefficients for odd powers of X, since even powers are 0
;; for all k > 0. All entries have an extra 0 at end to simplify
;; evaluating at k and k+1 simultaneously
(deftype polynomial () '(simple-array double-float (*)))
(defvar *polynomials* (make-array +max-polynomials+ :initial-element nil))
(declaim (type (simple-array double-float (*)) *polynomial-scales*))
(defvar *polynomial-scales* (make-array +max-polynomials+
                                        :element-type 'double-float
                                        :initial-element 0d0))


(defun make-reshape-polynomial (p)
  (let ((r (make-array (1+ (floor (length p) 2))
                       :element-type 'double-float :initial-element 0d0)))
    (loop for i from 1 below (length p) by 2
          for j from 0
          do (setf (aref r j) (coerce (aref p i) 'double-float)))
    r))

;; special case k=0 since 0 doesn't have a simplified polynomial
(defun reshape0 (x ks)
  (* (a:lerp ks x
             (* (- x (/ (expt x 3) 3d0))
                (aref *polynomial-scales* 1)))))

(defun ensure-polynomial (k)
  (or (aref *polynomials* k)
      (let* ((p (int (pexpt #(1 0 -1) k)))
             (s (/ (peval p 1d0))))
        (setf (aref *polynomial-scales* k) s)
        (setf (aref *polynomials* k) (make-reshape-polynomial p)))))


(defun reshape (x k)
  (declare (optimize speed)
           (type (real -2 2) x)
           (type (real 0 #.+max-polynomials+) k))
  (let ((x (coerce x 'double-float))
        (k (coerce k 'double-float)))
    (assert (<= 0 k (- +max-polynomials+ 2)))
    (multiple-value-bind (k ks) (floor k)
      (cond
        ((zerop k)
         (return-from reshape (reshape0 x ks)))
        (t
         (loop with x2 double-float = (* x x)
               with p1 of-type polynomial = (ensure-polynomial k)
               with s1 = (aref *polynomial-scales* k)
               with p2 of-type polynomial = (ensure-polynomial (1+ k))
               with s2 = (aref *polynomial-scales* (1+ k))
               for c1 double-float across p1
               for c2 double-float across p2
               for xn double-float = x then (* xn x2)
               sum (* c1 xn) into e1 double-float
               sum (* c2 xn) into e2 double-float
               finally (return (coerce (a:lerp ks (* e1 s1) (* e2 s2))
                                       'single-float))))))))

#++
(loop for i below 6 collect (int (pexpt #(1 0 -1) i)))
#++
(#(0 1)
  #(0 1 0 -1/3)
  #(0 1 0 -2/3 0 1/5)
  #(0 1 0 -1 0 3/5 0 -1/7)
 #(0 1 0 -4/3 0 6/5 0 -4/7 0 1/9)
  #(0 1 0 -5/3 0 2 0 -10/7 0 5/9 0 -1/11))
#++(ensure-polynomial 1)#++#(1.0 -0.33333334 0.0)
#++(ensure-polynomial 2)#++#(1.0 -0.6666667 0.2 0.0)
