(in-package noise-range-test/original)

;; original perlin noise, based on C code from
;; https://mrl.cs.nyu.edu/~perlin/doc/oscar.html#noise



;; subset of double-float that still has enough fraction bits to be
;; meaningful as input to noise functions (and which can be safely
;; FLOORed to a sb64)
(deftype double-input-type () `(double-float ,(- (expt 2d0 50))
                                             ,(expt 2d0 50)))

(defconstant +b+ #x100)
(defconstant +n+ #x1000)
#++
(defconstant +np+ 12)

(declaim (inline v2 v3))
(deftype dvec (n) `(simple-array double-input-type (,n)))
(declaim (ftype (function (real real) (values (dvec 2) &optional))
                v2))
(declaim (ftype (function (real real real) (values (dvec 3) &optional))
                v3))
(Defun v2 (x y)
  (make-array 2 :element-type 'double-input-type
                :initial-contents (list (coerce x 'double-input-type)
                                        (coerce y 'double-input-type))))
(Defun v3 (x y z)
  (make-array 3 :element-type 'double-input-type
                :initial-contents (list (coerce x 'double-input-type)
                                        (coerce y 'double-input-type)
                                        (coerce z 'double-input-type))))

(defconstant +2b+2+ (+ +b+ +b+ 2))
(declaim (type (simple-array (unsigned-byte 8) (#.+2b+2+)) *p*))

(declaim (type (simple-array double-float (#.+2b+2+)) *g1*))
(declaim (type (simple-array (dvec 2) (#.+2b+2+)) *g2*))
(declaim (type (simple-array (dvec 3) (#.+2b+2+)) *g3*))

(defvar *p* (make-array (+ +b+ +b+ 2) :element-type '(unsigned-byte 8)
                                      :initial-element 0))
(defvar *g1* (make-array (+ +b+ +b+ 2) :element-type 'double-float
                                       :initial-element 0d0))
(defvar *g2* (make-array (+ +b+ +b+ 2) :element-type '(dvec 2)
                                       :initial-element (v2 0 0)))
(defvar *g3* (make-array (+ +b+ +b+ 2) :element-type '(dvec 3)
                                       :initial-element (v3 0 0 0)))


(defvar *start* t)

(declaim (inline s-curve lerp))
(defun s-curve (x)
  (* x x (- 3 (* 2 x))))

(defun lerp (x a b)
  (+ a (* x (- b a))))

(defmacro setup (i b0 b1 r0 r1)
  `(progn
     (setf tt (+ (aref vec ,i) +n+))
     (setf ,b0 (ldb (byte 8 0) (floor tt)))
     (setf ,b1 (ldb (byte 8 0) (+ ,b0 1)))
     (setf ,r0 (- tt (floor tt)))
     (setf ,r1 (- ,r0 1))))

(declaim (inline original-noise1 original-noise2 original-noise3))

(defun %original-noise1 (arg)
  (declare (optimize speed)
           (type double-input-type arg))
  (let (bx0 bx1
        (rx0 0d0) (rx1 0d0)
        (sx 0d0)
        (tt 0d0)
        (u 0d0) (v 0d0)
        (vec (v2 0 0)))
    (declare (type double-float rx0 rx1 sx tt u v))
    (setf (aref vec 0) arg)
    (when *start*
      (setf *start* nil)
      (init))
    (setup 0 bx0 bx1 rx0 rx1)
    (setf sx (s-curve rx0))
    (setf u (* rx0 (aref *g1* (aref *p* bx0))))
    (setf v (* rx1 (aref *g1* (aref *p* bx1))))
    (coerce (lerp sx u v) 'single-float)))


(defun original-noise1 (x)
  (%original-noise1 (coerce x 'double-input-type)))

(defun %original-noise2 (vec)
  (declare (optimize speed)
           (type (simple-array double-input-type (2)) vec))
  (let (bx0 bx1 by0 by1 b00 b10 b01 b11
        (rx0 0d0) (rx1 0d0) (ry0 0d0) (ry1 0d0)
        q (sx 0d0) (sy 0d0)
        (a 0d0) (b 0d0)
        (tt 0d0)
        (u 0d0) (v 0d0)
        i j)
    (when *start*
      (setf *start* nil)
      (init))

    (setup 0 bx0 bx1 rx0 rx1)
    (setup 1 by0 by1 ry0 ry1)

    (setf i (aref *p* bx0))
    (setf j (aref *p* bx1))

    (setf b00 (aref *p* (+ i by0)))
    (setf b10 (aref *p* (+ j by0)))
    (setf b01 (aref *p* (+ i by1)))
    (setf b11 (aref *p* (+ j by1)))

    (setf sx (s-curve rx0))
    (setf sy (s-curve ry0))

    (macrolet ((at2 (rx ry)
                 `(+ (* ,rx (aref q 0))
                     (* ,ry (aref q 1)))))

      (setf q (aref *g2* b00) u (at2 rx0 ry0))
      (setf q (aref *g2* b10) v (at2 rx1 ry0))
      (setf a (lerp sx u v))

      (setf q (aref *g2* b01) u (at2 rx0 ry1))
      (setf q (aref *g2* b11) v (at2 rx1 ry1))
      (setf b (lerp sx u v))

      (coerce (lerp sy a b) 'single-float))))


(defun original-noise2 (x y)
  (%original-noise2 (v2 x y)))


(defun %original-noise3 (vec)
  (declare (optimize speed)
           (type (simple-array double-input-type (3)) vec))
  (let (bx0 bx1 by0 by1 bz0 bz1 b00 b10 b01 b11
        (rx0 0d0) (rx1 0d0) (ry0 0d0) (ry1 0d0) (rz0 0d0) (rz1 0d0)
        q
        (sy 0d0) (sz 0d0)
        (a 0d0) (b 0d0) (c 0d0) (d 0d0)
        (tt 0d0)
        (u 0d0) (v 0d0)
        i j)
    (when *start*
      (setf *start* nil)
      (init))

    (setup 0 bx0 bx1 rx0 rx1)
    (setup 1 by0 by1 ry0 ry1)
    (setup 2 bz0 bz1 rz0 rz1)

    (setf i (aref *p* bx0))
    (setf j (aref *p* bx1))

    (setf b00 (aref *p* (+ i by0)))
    (setf b10 (aref *p* (+ j by0)))
    (setf b01 (aref *p* (+ i by1)))
    (setf b11 (aref *p* (+ j by1)))

    (setf tt (s-curve rx0))
    (setf sy (s-curve ry0))
    (setf sz (s-curve rz0))

    (macrolet ((at3 (rx ry rz)
                 `(+ (* ,rx (aref q 0))
                     (* ,ry (aref q 1))
                     (* ,rz (aref q 2)))))

      (setf q (aref *g3* (+ b00 bz0)) u (at3 rx0 ry0 rz0))
      (setf q (aref *g3* (+ b10 bz0)) v (at3 rx1 ry0 rz0))
      (setf a (lerp tt u v))

      (setf q (aref *g3* (+ b01 bz0)) u (at3 rx0 ry1 rz0))
      (setf q (aref *g3* (+ b11 bz0)) v (at3 rx1 ry1 rz0))
      (setf b (lerp tt u v))

      (setf c (lerp sy a b))

      (setf q (aref *g3* (+ b00 bz1)) u (at3 rx0 ry0 rz1))
      (setf q (aref *g3* (+ b10 bz1)) v (at3 rx1 ry0 rz1))
      (setf a (lerp tt u v))

      (setf q (aref *g3* (+ b01 bz1)) u (at3 rx0 ry1 rz1))
      (setf q (aref *g3* (+ b11 bz1)) v (at3 rx1 ry1 rz1))
      (setf b (lerp tt u v))

      (setf d (lerp sy a b))

      (coerce (lerp sz c d) 'single-float))))

(defun original-noise3 (x y z)
  (%original-noise3 (v3 x y z)))

(defun normalize2 (v)
  (let ((s (sqrt (+ (expt (aref v 0) 2)
                    (expt (aref v 1) 2)))))
    (setf (aref v 0) (/ (aref v 0) s))
    (setf (aref v 1) (/ (aref v 1) s))
    v))

(defun normalize3 (v)
  (let ((s (sqrt (+ (expt (aref v 0) 2)
                    (expt (aref v 1) 2)
                    (expt (aref v 2) 2)))))
    (setf (aref v 0) (/ (aref v 0) s))
    (setf (aref v 1) (/ (aref v 1) s))
    (setf (aref v 2) (/ (aref v 2) s))
    v))

(defun init ()
  (loop for i from 0 below +b+
        do (setf (aref *p* i) i)
           (setf (aref *g1* i) (/ (- (random (* +b+ 2)) +b+)
                                  (float +b+ 1d0)))
           (loop for j below 2
                 do (setf (aref (aref *g2* i) j)
                          (/ (- (random (* +b+ 2)) +b+)
                             (float +b+ 1d0))))
           (normalize2 (aref *g2* i))
           (loop for j below 3
                 do (setf (aref (aref *g3* i) j)
                          (/ (- (random (* +b+ 2)) +b+)
                             (float +b+ 1d0))))
           (normalize3 (aref *g3* i)))
  (alexandria:shuffle *p* :end 256)
  (loop for i below (+ +b+ 2)
        do (setf (aref *p* (+ +b+ i)) (aref *p* i))
           (setf (aref *g1* (+ +b+ i)) (aref *g1* i))
           (setf (aref *g2* (+ +b+ i)) (aref *g2* i))
           (setf (aref *g3* (+ +b+ i)) (aref *g3* i)))
)
