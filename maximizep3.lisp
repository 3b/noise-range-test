(defpackage maximize-p3
  (:use :cl)
  (:local-nicknames (:a :alexandria-2)))
(in-package maximize-p3)

(defvar *centers* (make-array (expt 12 8)
                              :element-type 'single-float
                              :initial-element 0.0))
#++
(setf *centers* nil)
(defun p3* (cell)
  (declare (type fixnum cell)
           (optimize speed))
  (flet ((lerp (x a b)
           (+ a (* x (- b a))))
         (grad (h x y z)
           (declare (type (mod 12) h)
                    (single-float x y z))
           (let* ((u (if (< h 8) x y))
                  (v (if (< h 4) y z)))
             (declare (type single-float u v))
             (+ (if (logbitp 0 h) (- u) u)
                (if (logbitp 1 h) (- v) v)))))
    (declare (inline lerp grad))
    (let ((n (make-array 8 :element-type '(mod 12)
                           :initial-element 8)))
      (loop with x of-type (mod #.(expt 12 8)) = cell
            with v = 0
            for i below 8
            do (setf (values x v) (floor x 12))
               (setf (aref n i) v))
     ; (format t "~s: n = ~s~%" cell n)
      (let ((x 0.5)
            (y 0.5)
            (z 0.5)
            (u 0.5)
            (v 0.5)
            (w 0.5))
        (declare (type single-float x y z u v w))
        (coerce
         (lerp w
               (lerp v
                     (lerp u
                           (grad (aref n 0)
                                 x y z)
                           (grad (aref n 1)
                                 (1- x) y z))
                     (lerp u
                           (grad (aref n 2) x (1- y) z)
                           (grad (aref n 3) (1- x) (1- y) z)))
               (lerp v
                     (lerp u
                           (grad (aref n 4) x y (1- z))
                           (grad (aref n 5) (1- x) y (1- z)))
                     (lerp u
                           (grad (aref n 6) x (1- y) (1- z))
                           (grad (aref n 7) (1- x) (1- y) (1- z)))))
         'single-float))

      ))
  )

(declaim (inline p3b))
(defun p3b (cell x y z)
  (%p3b cell
        (coerce x 'single-float)
        (coerce y 'single-float)
        (coerce z 'single-float)))
(defun %p3b (cell x y z)
  (declare (type fixnum cell)
           (type single-float x y z)
           (optimize speed))
  (flet ((lerp (x a b)
           (+ a (* x (- b a))))
         (grad (h x y z)
           (declare (type (mod 12) h)
                    (single-float x y z))
           (let* ((u (if (< h 8) x y))
                  (v (if (< h 4) y z)))
             (declare (type single-float u v))
             (+ (if (logbitp 0 h) (- u) u)
                (if (logbitp 1 h) (- v) v))))
         (fade (x)
           (* x x x (+ (* x (- (* x 6) 15)) 10))))
    (declare (inline lerp grad))
    (let ((n (make-array 8 :element-type '(mod 12)
                           :initial-element 8)))
      (loop with x of-type (mod #.(expt 12 8)) = cell
            with v = 0
            for i below 8
            do (setf (values x v) (floor x 12))
               (setf (aref n i) v))
      (let ((u (fade x))
            (v (fade y))
            (w (fade z)))
        (declare (type single-float x y z u v w))
        (coerce
         (lerp w
               (lerp v
                     (lerp u
                           (grad (aref n 0) x y z)
                           (grad (aref n 1) (1- x) y z))
                     (lerp u
                           (grad (aref n 2) x (1- y) z)
                           (grad (aref n 3) (1- x) (1- y) z)))
               (lerp v
                     (lerp u
                           (grad (aref n 4) x y (1- z))
                           (grad (aref n 5) (1- x) y (1- z)))
                     (lerp u
                           (grad (aref n 6) x (1- y) (1- z))
                           (grad (aref n 7) (1- x) (1- y) (1- z)))))
         'single-float)))))

;;;(delete-duplicates(loop repeat 1000000 collect (p3* (random (expt 12 8)))))
;;;(loop for i from 188000000 below (expt 12 8)
;;;      do (setf (aref *centers* i) (p3* i))
;;;      when (Zerop (mod i 1000000))
;;;        do (format t "i = ~s = ~s%~%" i
;;;                   (float (* 100 (/ i (expt 12 8))))))
;;;(reduce 'max *centers*)1.0
;;;(reduce 'min *centers*)-1.0
;;;(count 1.0 *centers*)6561
;;;(count -1.0 *centers*)6561
;;;(defvar *hist* (let ((hist (make-hash-table)))
;;;                 (loop for i across *centers*
;;;                       do (incf (gethash i hist 0)))
;;;                 hist))
;;;(sort (a:hash-table-alist *hist*) '< :key 'car)
;;;;;((  -1.0 .     6561)
;; (-0.875 .   104976)
;; ( -0.75 .   787320)
;; (-0.625 .  3674160)
;; (  -0.5 . 11941020)
;; (-0.375 . 28658448)
;; ( -0.25 . 52540488)
;; (-0.125 . 75057840)
;; (   0.0 . 84440070)
;; ( 0.125 . 75057840)
;; (  0.25 . 52540488)
;; ( 0.375 . 28658448)
;; (   0.5 . 11941020)
;; ( 0.625 .  3674160)
;; (  0.75 .   787320)
;; ( 0.875 .   104976)
;; (   1.0 .     6561))
(defvar *max-cells* (coerce
                     (loop for i below (length *centers*)
                           for a = (aref *centers* i)
                           when (= a 1.0)
                             collect i)
                     'vector))
(defvar *min-cells* (coerce
                     (loop for i below (length *centers*)
                           for a = (aref *centers* i)
                           when (= a -1.0)
                             collect i)
                     'vector))

(loop for i across *max-cells*
      for mm = (loop repeat 100000
                     maximize (p3b i (random 1.0) (random 1.0) (random 1.0)))
      maximize mm into mmm
      do (format t "~s = ~s : ~s~%" i mm mmm)
)

;;285623148 = 1.0363283 | 1.0363283
;;429708344 = 0.9999986 | 1.0363283

(defun spx (c d)
  (let* ((d d)
         (x 0.5)
         (y 0.5)
         (z 0.5)
         (h (p3b c x y z)))
    (declare (single-float d x y z))
;    (format t "~s ~s ~s = ~s~%" x y z h)
    (flet ((step1 ()
             #++(let ((x1 (p3b c (- x (/ d 1)) 0.5 0.5))
                   (x2 (p3b c (+ x (/ d 1)) 0.5 0.5))
                   (y1 (p3b c 0.5 (- y (/ d 1)) 0.5))
                   (y2 (p3b c 0.5 (+ y (/ d 1)) 0.5))
                   (z1 (p3b c  0.5 0.5 (- z (/ d 1))))
                   (z2 (p3b c  0.5 0.5 (+ z (/ d 1)))))
               (when (or (> x1 h) (> x2 h))
                 (incf x (* d (signum (- x2 x1)))))
               (when (or (> y1 h) (> y2 h))
                 (incf y (* d (signum (- y2 y1)))))
               (when (or (> z1 h) (> z2 h))
                 (incf z (* d  (signum (- z2 z1)))))
               ;(setf d (/ d 2))
               
               (setf h (p3b c x y z)))

             (let* ((d (coerce
                       (loop for i below (expt 3 3)
                             collect (list (* d (1- (mod i 3)))
                                           (* d (1- (mod (floor i 3) 3)))
                                           (* d (1- (mod (floor i 9) 3)))))
                       'vector))
                   (v #++(vector (p3b c (- x (/ d 1)) 0.5 0.5)
                                 (p3b c (+ x (/ d 1)) 0.5 0.5)
                                 (p3b c 0.5 (- y (/ d 1)) 0.5)
                                 (p3b c 0.5 (+ y (/ d 1)) 0.5)
                                 (p3b c 0.5 0.5 (- z (/ d 1)))
                                 (p3b c 0.5 0.5 (+ z (/ d 1))))
                      (coerce
                       (loop for i below 8
                             for di across d
                             collect (apply #'p3b c (mapcar '+ di (list x y z)))
                             )
                       'vector))
                   (mi 0)
                   (m 0))
               (loop for i below 8
                     for x = (aref v i)
                     when (> x m)
                       do (setf mi i m x))
               (when (> m h)
                 (destructuring-bind (dx dy dz) (aref d mi)
                   (incf x dx)
                   (incf y dy)
                   (incf z dz)))
               #++(when (> m h)
                 (ecase mi
                   (0 (decf x d))
                   (1 (incf x d))
                   (2 (decf y d))
                   (3 (incf y d))
                   (4 (decf z d))
                   (5 (incf z d))))
               (setf h (p3b c x y z)))))
      (loop repeat 20
            do (setf d (/ d 2))
               (loop repeat 10000
                     for p = h
                     do (step1)
                     #++do (format t "~s ~s ~s = ~s~%" x y z h)
                     while (< p h))))
    h)
  )

(defun sp (c &optional (eps 0.001))
  (let* ((x 0.5)
         (y 0.5)
         (z 0.5)
         (h (p3b c x y z)))
    (declare (single-float x y z h eps)
             (optimize speed)
             (type (mod #.(expt 12 8)) c))

    (loop for z1 of-type single-float in '(0.33 0.67)
          do (loop for y1 single-float in '(0.33 0.67)
                   do (loop for x1 single-float in '(0.33 0.67)
                            for h2 single-float = (p3b c x1 y1 z1)
                            when (> h2 h)
                              do (setf h h2
                                       x x1
                                       y y1
                                       z z1))))
    (loop for i below 100
          for g = (p3g c x y z eps)
          for l = (sb-cga:vec-length g)
          for gn = (if (< (abs l) eps)
                       (v3 0 0 0)
                       (sb-cga:vec* (sb-cga:normalize g) 0.01))
          for p2 = (sb-cga:vec+ (v3 x y z) gn)
          for h2 single-float = (p3b c (aref p2 0) (aref p2 1) (aref p2 2))
          until (< (abs l) eps)
          do (setf x (aref p2 0)
                   y (aref p2 1)
                   z (aref p2 2)
                   h (max h h2))
          while (or (< 0 x 1)
                    (< 0 y 1)
                    (< 0 z 1))
          do (setf x (a:clamp x 0.0 1.0))
             (setf y (a:clamp y 0.0 1.0))
             (setf z (a:clamp z 0.0 1.0)))
    h))

(time (loop for i from 2.0 below 128.0
            maximize (spx 285623148 (/ i))))
1.0363538
(time (loop repeat 20000 do (sp 285623148)))
1.0363533

(sp 285623148 (/ 64.0))
(sp 271439724 (/ 123.0))
(p3* 271439724 )
;271439724: n = #(0 1 2 3 10 10 6 7)
1.0
#++
(loop repeat 100000000
      maximize (p3b 285623148 (random 1.0) (random 1.0) (random 1.0)))
1.0363458
(/ 1.0363458)
1.0363399


;;0.49902344 0.5 0.40527344 = 1.0310934
;;0.49609375 0.5 0.40234375 = 1.0319762
;;0.4765625 0.5 0.3359375 = 1.0357361


(loop for s from 2.0 to 128.0
      maximize (loop for i across *max-cells*
                     for mm = (sp i (/ s))
                     maximize mm into mmm
                     when (> mm 1.03635)
                       do (format t "~s @ ~s = ~s : ~s~%" i s mm mmm)
                     finally (return mmm)
                     )
        into m
      do (format t "~s ~s~%" s m))
;=> 1.0363531
;; 429708344 = 1.0 | 1.035276
(/ 1.0363225)
(/ 0.9649214285521897d0)1.0363538112118038d0
(/ 1.0363538)0.9649214
(* (sqrt 3/4) (sqrt 3))1.5 1.2247448

(expt 12 8)429,981,696
(expt 32 16)1208925819614629174706176
(log 1208925819614629174706176 2)80.0
(expt 12 8)429981696
(expt 32 8)1 099 511 627 776

(loop with n = 0
      with h = (p3* n)
      with work = nil
      for i below 8
      do (loop with n1 = n
               for j from 1 below 12
               for n2 = (+ n1 (* j (expt 12 i)))
               for h2 = (p3* n2)
               when (>= h2 h)
                 do (setf h h2 n n2))
         (format t "~s @ ~s~%" n (p3* n)))

(defvar *rsmax* (list 0 0))
(defun rsearch3x (n i)
  (let ((work))
    (cond
      ((< i 8)
       (let ((h  (p3* n)))
         (loop with n1 = n
               for j from 0 below 12
               for n2 = (+ n1 (* j (expt 12 i)))
               for h2 = (p3* n2)
               when (= h2 h)
                 do (push n2 work)
               when (> h2 h)
                 do (setf h h2
                          n n2
                          work (list n2))))
       (loop for w in work do (rsearch3 w (1+ i))))
      (t
       (let ((h (sp n (/ 123.0))))
         ;(format t "~s = ~s~%" n h)
         (when (> h (car *rsmax*))
           (setf *rsmax* (list h n))))))))

(defun rsearch3 (n i eps)
  (let ((work))
    (cond
      ((< i 8)
       (let ((h  (p3* n)))
         (loop with n1 = n
               for j from 0 below 12
               for n2 = (+ n1 (* j (expt 12 i)))
               for h2 = (p3* n2)
               when (= h2 h)
                 do (push n2 work)
               when (> h2 h)
                 do (setf h h2
                          n n2
                          work (list n2))))
       (loop for w in work do (rsearch3 w (1+ i) eps)))
      (t
       (let ((h (sp n eps)))
         ;(format t "~s = ~s~%" n h)
         (when (> h (car *rsmax*))
           (format t "~s = ~s~%" n h)
           (setf *rsmax* (list h n))))))))

#++
(progn
  (setf *rsmax* (list 0 0))
  (time (rsearch3 0 0 0.005))
  (print *rsmax*))

*rsmax*(1.0363535 113725544)
#++
(sp 113721708 (/ 122.0))
#++
(sp 429708344 (/ 12.0))
#++
(loop for i from 2.0 below 128.0
      maximize (sp 285624164 (/ i)))
#++
(aref *centers* 429708344)

#++
(loop for i below 12 collect (p3* i)
 )
;;0: n = #(0 0 0 0 0 0 0 0)
;;1: n = #(1 0 0 0 0 0 0 0)
;;2: n = #(2 0 0 0 0 0 0 0)
;;3: n = #(3 0 0 0 0 0 0 0)
;;4: n = #(4 0 0 0 0 0 0 0)
;;5: n = #(5 0 0 0 0 0 0 0)
;;6: n = #(6 0 0 0 0 0 0 0)
;;7: n = #(7 0 0 0 0 0 0 0)
;;8: n = #(8 0 0 0 0 0 0 0)
;;9: n = #(9 0 0 0 0 0 0 0)
;;10: n = #(10 0 0 0 0 0 0 0)
;;11: n = #(11 0 0 0 0 0 0 0)
;;(0.0 -0.125 -0.125 -0.25
;; 0.0 -0.125 -0.125 -0.25
;; 0.0 -0.125 -0.125 -0.25)

#++
(require 'sb-cga)

(declaim (inline v3))
(Defun v3 (x y z)
  (sb-cga:vec (coerce x 'single-float)
              (coerce y 'single-float)
              (coerce z 'single-float)))

(declaim (inline p3g))
(defun p3g (cell x y z &optional (e 0.01))
  (%p3g cell
        (coerce x 'single-float)
        (coerce y 'single-float)
        (coerce z 'single-float)
        (coerce e 'single-float)))

#++
(defun %p3g (cell x y z)
  (declare (type fixnum cell)
           (type single-float x y z)
           (optimize speed))
  ;; return gradient at xyz
  (flet ((lerp (x a b)
           (sb-cga:vec-lerp a b x)
           #++(+ a (* x (- b a))))
         (grad (h x y z)
           (declare (type (mod 12) h)
                    ;(type (signed-byte 2) x y z)
                    )
           (let ((-x (- x))
                 (-y (- y))
                 (-z (- z)))
            (case h
              (0 (v3  x  y  0))
              (1 (v3 -x  y  0))
              (2 (v3  x -y  0))
              (3 (v3 -x -y  0))
              (4 (v3  x  0  z))
              (5 (v3  x  0 -z))
              (6 (v3 -x  0  z))
              (7 (v3 -x  0 -z))
              (8 (v3  0  y  z))
              (9 (v3  0  y -z))
              (10 (v3  0 -y  z))
              (11 (v3  0 -y -z))
              #++(12 (v3  x  y  0))
              #++(13 (v3 -x  y  0))
              #++(14 (v3  0 -y  z))
              #++(15 (v3  0 -y -z)))))
         (fade (x)
           (* x x x (+ (* x (- (* x 6) 15)) 10))))
    (declare (inline lerp grad))
    (let ((n (make-array 8 :element-type '(mod 12)
                           :initial-element 8)))
      (loop with x of-type (mod #.(expt 12 8)) = cell
            with v = 0
            for i below 8
            do (setf (values x v) (floor x 12))
               (setf (aref n i) v))
      (let ((u (fade x))
            (v (fade y))
            (w (fade z)))
        (declare (type single-float x y z u v w))

        (lerp w
              (lerp v
                    (lerp u
                          (grad (aref n 0)  1  1  1)
                          (grad (aref n 1) -1  1  1))
                    (lerp u
                          (grad (aref n 2)  1 -1  1)
                          (grad (aref n 3) -1 -1  1)))
              (lerp v
                    (lerp u
                          (grad (aref n 4)  1  1 -1)
                          (grad (aref n 5) -1  1 -1))
                    (lerp u
                          (grad (aref n 6)  1 -1 -1)
                          (grad (aref n 7) -1 -1 -1))))))))
#++
(defun %p3g (cell x y z)
  (declare (type fixnum cell)
           (type single-float x y z)
           (optimize speed))
  ;; return (approximation of?) gradient at xyz
  (flet ((lerp (x a b)
           (+ a (* x (- b a))))
         (grad (h x y z)
           (declare (type (mod 12) h)
                    (single-float x y z))
           (let* ((u (if (< h 8) x y))
                  (v (if (< h 4) y z)))
             (declare (type single-float u v))
             (+ (if (logbitp 0 h) (- u) u)
                (if (logbitp 1 h) (- v) v))))
         (fade (x)
           (* x x x (+ (* x (- (* x 6) 15)) 10))))
    (declare (inline lerp grad))
    (let ((n (make-array 8 :element-type '(mod 12)
                           :initial-element 8))
          (e 0.01))
      (loop with x of-type (mod #.(expt 12 8)) = cell
            with v = 0
            for i below 8
            do (setf (values x v) (floor x 12))
               (setf (aref n i) v))
      (let ((u (fade x))
            (v (fade y))
            (w (fade z))
            (u1 (fade (- x e)))
            (v1 (fade (- y e)))
            (w1 (fade (- z e)))
            (u2 (fade (+ x e)))
            (v2 (fade (+ y e)))
            (w2 (fade (+ z e))))
        (declare (type single-float x y z u v w))
        (let ((g0 (grad (aref n 0) x y z))
              (g1 (grad (aref n 1) (1- x) y z))
              (g2 (grad (aref n 2) x (1- y) z))
              (g3 (grad (aref n 3) (1- x) (1- y) z))
              (g4 (grad (aref n 4) x y (1- z)))
              (g5 (grad (aref n 5) (1- x) y (1- z)))
              (g6 (grad (aref n 6) x (1- y) (1- z)))
              (g7 (grad (aref n 7) (1- x) (1- y) (1- z))))
          (v3 (- (lerp w
                       (lerp v (lerp u2 g0 g1) (lerp u2 g2 g3))
                       (lerp v (lerp u2 g4 g5) (lerp u2 g6 g7)))
                 (lerp w
                       (lerp v (lerp u1 g0 g1) (lerp u1 g2 g3))
                       (lerp v (lerp u1 g4 g5) (lerp u1 g6 g7))))
              (- (lerp w
                       (lerp v2 (lerp u g0 g1) (lerp u g2 g3))
                       (lerp v2 (lerp u g4 g5) (lerp u g6 g7)))
                 (lerp w
                       (lerp v1 (lerp u g0 g1) (lerp u g2 g3))
                       (lerp v1 (lerp u g4 g5) (lerp u g6 g7))))
              (- (lerp w2
                       (lerp v (lerp u g0 g1) (lerp u g2 g3))
                       (lerp v (lerp u g4 g5) (lerp u g6 g7)))
                 (lerp w1
                       (lerp v (lerp u g0 g1) (lerp u g2 g3))
                       (lerp v (lerp u g4 g5) (lerp u g6 g7))))
              )))))
)




(defun %p3g (cell x y z &optional (e 0.01))
  (v3
   (/ (- (p3b cell (+ x e) y z)
         (p3b cell (- x e) y z))
      (* 2 e))
   (/ (- (p3b cell x (+ y e) z)
         (p3b cell x (- y e) z))
      (* 2 e))
   (/ (- (p3b cell x y (+ z e))
         (p3b cell x y (- z e)))
      (* 2 e)))
  )

(p3g (cc 4 5 9 9 6 7 2 11) 0.5 0.5 0.5 0.0000001)
#(0.11920929 -0.5066395 0.14901161)
#(0.12218952 -0.500679 0.12218952)
#(0.12457371 -0.49978495 0.12516975)
#(0.12496113 -0.49996373 0.12502074)
#(0.1249969 -0.5000025 0.12500286)
#(0.12500018 -0.5000001 0.12499988)
#(0.1249969 -0.5000025 0.12500286)

(p3g (cc 4 5 9 9 6 7 2 11) 0.5 0.5 0.5)
#(0.002499938 -0.01000005 0.0025000572)
#(0.002499938 -0.01000005 0.0025000572)
0.0025000572
(p3g (cc 4 5 9 9 6 7 2 11) 0.1 0.1 0.5)
#(0.015897214 0.0118973255 -6.7949295e-5)

(p3b 160694675 0.3 0.2 0.2)
(p3b 402005008 0.30000305 0.19999695 0.19999695)0.6237606

-0.46760777
0.5003469




(defun cc (&rest hashes)
  (loop for x in hashes
        for i from 0
        sum (* x (expt 12 i))))

(cc 4 5 9 9 6 7 2 11)

(sp (maximize-p3::cc 4 5 9 9 6 7 2 11) (/ 123.0))
(loop for x = (random 1.0)
      for y = (random 1.0)
      for z = (random 1.0)
      repeat 10000000
      ;maximize (p3b (maximize-p3::cc 4 5 9 9 6 7 11 11) x y z) into a
      maximize (p3b (maximize-p3::cc 4 5 9 9 0 10 2 11) x y z)
        into a
      maximize (p3b (maximize-p3::cc 4 5 9 9 6 7 2 11) x y z) into b
      finally (return (list a b)))

;;maximize (p3b (maximize-p3::cc 4 5 9 3 6 7 2 11) x y z) into a
(1.0352584 1.035783)
(1.0352659 1.0357959)

;;maximize (p3b (maximize-p3::cc 4 5 9 9 6 7 11 11) x y z) into a
(1.0352492 1.0357765)
(1.0352577 1.0357813)
(1.0352612 1.0357804)
1.0352672
1.0352559
1.035001
