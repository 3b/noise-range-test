(defpackage maximize-p4
  (:use :cl)
  (:local-nicknames (:a :alexandria-2)))
(in-package maximize-p4)


(defun p4* (cell)
  (declare (type unsigned-byte cell)
           (optimize speed))
  (flet ((lerp (x a b)
           (+ a (* x (- b a))))
         (grad (h x y z w)
           (let* ((a x)
                  (b y)
                  (c w))
             (case (ash h -3)
               (1 (setf a w b x c y))
               (2 (setf a z b w c x))
               (3 (setf a y b z c w)))
             (+ (if (logbitp 2 h) a (- a))
                (if (logbitp 1 h) b (- b))
                (if (logbitp 0 h) c (- c))))))
    (declare (inline lerp grad))
    (let ((n (make-array 16 :element-type '(mod 32)
                            :initial-element 0)))
      (loop with x of-type (mod #.(expt 32 16)) = cell
            with v = 0
            for i below 16
            do (setf (values x v) (floor x 32))
               (setf (aref n i) v))
                                        ; (format t "~s: n = ~s~%" cell n)
      (let ((x 0.5)
            (y 0.5)
            (z 0.5)
            (w 0.5)
            (lx 0.5)
            (ly 0.5)
            (lz 0.5)
            (lw 0.5))
        (declare (type single-float x y z w lx ly lz lw))
        (coerce
         (lerp lw
               (lerp lz
                     (lerp ly
                           (lerp lx
                                 (grad (aref n 0) x y z w)
                                 (grad (aref n 1) (1- x) y z w))
                           (lerp lx
                                 (grad (aref n 2) x (1- y) z w)
                                 (grad (aref n 3) (1- x) (1- y) z w)))
                     (lerp ly
                           (lerp lx
                                 (grad (aref n 4) x y (1- z) w)
                                 (grad (aref n 5) (1- x) y (1- z) w))
                           (lerp lx
                                 (grad (aref n 6) x (1- y) (1- z) w)
                                 (grad (aref n 7) (1- x) (1- y) (1- z) w))))
               (lerp lz
                     (lerp ly
                           (lerp lx
                                 (grad (aref n 8) x y z (1- w))
                                 (grad (aref n 9) (1- x) y z (1- w)))
                           (lerp lx
                                 (grad (aref n 10) x (1- y) z (1- w))
                                 (grad (aref n 11) (1- x) (1- y) z (1- w))))
                     (lerp ly
                           (lerp lx
                                 (grad (aref n 12) x y (1- z) (1- w))
                                 (grad (aref n 13) (1- x) y (1- z) (1- w)))
                           (lerp lx
                                 (grad (aref n 14) x (1- y) (1- z) (1- w))
                                 (grad (aref n 15) (1- x) (1- y) (1- z) (1- w))))))
         'single-float)))))

(defun p4b (cell x y z w)
  (declare (type unsigned-byte cell)
           (single-float x y z w)
           (optimize speed))
  (flet ((lerp (x a b)
           (+ a (* x (- b a))))
         (grad (h x y z w)
           (let* ((a x)
                  (b y)
                  (c w))
             (case (ash h -3)
               (1 (setf a w b x c y))
               (2 (setf a z b w c x))
               (3 (setf a y b z c w)))
             (+ (if (logbitp 2 h) a (- a))
                (if (logbitp 1 h) b (- b))
                (if (logbitp 0 h) c (- c)))))
         (fade (x)
           (* x x x (+ (* x (- (* x 6) 15)) 10))))
    (declare (inline lerp grad fade))
    (let ((n (make-array 16 :element-type '(mod 32)
                            :initial-element 0)))
      (loop with x of-type (mod #.(expt 32 16)) = cell
            with v = 0
            for i below 16
            do (setf (values x v) (floor x 32))
               (setf (aref n i) v))
      (let ((lx (fade x))
            (ly (fade y))
            (lz (fade z))
            (lw (fade w)))
        (declare (type single-float lx ly lz lw))
        (coerce
         (lerp lw
               (lerp lz
                     (lerp ly
                           (lerp lx
                                 (grad (aref n 0) x y z w)
                                 (grad (aref n 1) (1- x) y z w))
                           (lerp lx
                                 (grad (aref n 2) x (1- y) z w)
                                 (grad (aref n 3) (1- x) (1- y) z w)))
                     (lerp ly
                           (lerp lx
                                 (grad (aref n 4) x y (1- z) w)
                                 (grad (aref n 5) (1- x) y (1- z) w))
                           (lerp lx
                                 (grad (aref n 6) x (1- y) (1- z) w)
                                 (grad (aref n 7) (1- x) (1- y) (1- z) w))))
               (lerp lz
                     (lerp ly
                           (lerp lx
                                 (grad (aref n 8) x y z (1- w))
                                 (grad (aref n 9) (1- x) y z (1- w)))
                           (lerp lx
                                 (grad (aref n 10) x (1- y) z (1- w))
                                 (grad (aref n 11) (1- x) (1- y) z (1- w))))
                     (lerp ly
                           (lerp lx
                                 (grad (aref n 12) x y (1- z) (1- w))
                                 (grad (aref n 13) (1- x) y (1- z) (1- w)))
                           (lerp lx
                                 (grad (aref n 14) x (1- y) (1- z) (1- w))
                                 (grad (aref n 15) (1- x) (1- y) (1- z) (1- w))))))
         'single-float)))))

(declaim (inline v4))
(deftype v4 () '(simple-array single-float (4)))
(defun v4 (x y z w)
  (make-array 4 :element-type 'single-float
              :initial-contents (list (coerce x 'single-float)
                                      (coerce y 'single-float)
                                      (coerce z 'single-float)
                                      (coerce w 'single-float))))

(declaim (ftype (function (v4) (values single-float &optional)) v4-length))

(defun v4-length (v)
  (declare (type v4 v))
  (sqrt (+ (expt (aref v 0) 2)
           (expt (aref v 1) 2)
           (expt (aref v 2) 2)
           (expt (aref v 3) 2))))

(declaim (ftype (function (v4 single-float) (values v4 &optional)) v4*))
(defun v4* (v f)
  (declare (type v4 v) (single-float f))
  (v4 (* (aref v 0) f)
      (* (aref v 1) f)
      (* (aref v 2) f)
      (* (aref v 3) f)))

(declaim (ftype (function (v4 v4) (values v4 &optional)) v4+))
(defun v4+ (a b)
  (declare (type v4 a b) )
  (v4 (+ (aref a 0) (aref b 0))
      (+ (aref a 1) (aref b 1))
      (+ (aref a 2) (aref b 2))
      (+ (aref a 3) (aref b 3))))

(declaim (ftype (function (v4) (values v4 &optional)) v4-normalize))
(defun v4-normalize (v)
  (let ((l (v4-length v)))
    (if (zerop l)
        (v4 0.0 0.0 0.0 0.0)
        (let ((l (/ l)))
          (v4 (* (aref v 0) l)
              (* (aref v 1) l)
              (* (aref v 2) l)
              (* (aref v 3) l))))))

(defun %p4g (cell x y z w &optional (e 0.01))
  (v4
   (/ (- (p4b cell (+ x e) y z w)
         (p4b cell (- x e) y z w))
      (* 2 e))
   (/ (- (p4b cell x (+ y e) z w)
         (p4b cell x (- y e) z w))
      (* 2 e))
   (/ (- (p4b cell x y (+ z e) w)
         (p4b cell x y (- z e) w))
      (* 2 e))
   (/ (- (p4b cell x y z (+ w e))
         (p4b cell x y z (- w e)))
      (* 2 e))))

(declaim (inline p4g))
(defun p4g (cell x y z w &optional (e 0.01))
  (%p4g cell
        (coerce x 'single-float)
        (coerce y 'single-float)
        (coerce z 'single-float)
        (coerce w 'single-float)
        (coerce e 'single-float)))



(noise-range-test/improved:improved-noise4 1.23 2.34 3.45 4.56)0.037092663
(loop with n = 0
      for x in (reverse '(11 7 30 18 16 18 10 22 0 10 5 21 19 13 6 1 ))
      for i from 0
      do (setf (ldb (byte 5 (* i 5)) n) x)
      finally (return n))
(p4b 45364765028613534349547 0.23 0.34 0.45 0.56)0.03709265


(loop for i below (expt 3 4)
      collect (list (* 1 (1- (mod i 3)))
                    (* 1 (1- (mod (floor i 3) 3)))
                    (* 1 (1- (mod (floor i 9) 3)))
                    (* 1 (1- (mod (floor i 27) 3)))))
#++
(defun sp (c d)
  (let* ((d1 d)
         (x 0.5)
         (y 0.5)
         (z 0.5)
         (w 0.5)
         (h (p4b c x y z w))
         (d (make-array (expt 3 4)))
         (v (make-array (expt 3 4))))
    (declare (single-float d1 x y z))
    (loop for i below (length d) do (setf (aref d i) (list 0 0 0 0)))
    (flet ((step1 ()
             (let* ((mi 0)
                    (m 0))
               (loop for x across d
                     for i from 0
                     do (setf (car x) (* d1 (1- (mod i 3))))
                        (pop x)
                        (setf (car x) (* d1 (1- (mod (floor i 3) 3))))
                        (pop x)
                        (setf (car x) (* d1 (1- (mod (floor i 9) 3))))
                        (pop x)
                        (setf (car x) (* d1 (1- (mod (floor i 27) 3)))))
               (loop for di across d
                     for i from 0
                     do (setf (aref v i)
                              (apply #'p4b c (mapcar '+ di (list x y z w)))))
               (loop for i below (length v)
                     for x = (aref v i)
                     when (> x m)
                       do (setf mi i m x))
               (when (> m h)
                 (destructuring-bind (dx dy dz dw) (aref d mi)
                   (incf x dx)
                   (incf y dy)
                   (incf z dz)
                   (incf w dw)))
               (setf h (p4b c x y z w)))))
      (loop repeat 20
            do (setf d1 (/ d1 2))
               (loop repeat 1000
                     for p = h
                     do (step1)
                     while (< p h))))
    h))

(defun sp (c &optional (eps 0.001))
  (let* ((x 0.5)
         (y 0.5)
         (z 0.5)
         (w 0.5)
         (h (p4b c x y z w)))
    (declare (single-float x y z h eps)
             (optimize speed)
             (type (mod #.(expt 32 16)) c))

    (loop for w1 of-type single-float in '(0.25 0.75)
          do (loop for z1 of-type single-float in '(0.25 0.75)
                   do (loop for y1 single-float in '(0.25 0.75)
                            do (loop for x1 single-float in '(0.25 0.75)
                                     for h2 single-float = (p4b c x1 y1 z1 w1)
                                     when (> h2 h)
                                       do (setf h h2
                                                x x1
                                                y y1
                                                z z1
                                                w w1)))))
    (loop for i below 100
          for g = (p4g c x y z w eps)
          for l = (v4-length g)
          for gn = (if (< (abs l) eps)
                       (v4 0 0 0 0)
                       (v4* (v4-normalize g) (* (expt 0.97 i) 0.01)))
          for p2 = (v4+ (v4 x y z w) gn)
          for h2 single-float = (p4b c
                                     (aref p2 0) (aref p2 1)
                                     (aref p2 2) (aref p2 3))
          until (< (abs l) eps)
          do (setf x (aref p2 0)
                   y (aref p2 1)
                   z (aref p2 2)
                   w (aref p2 3)
                   h (max h h2))
          while (or (< 0 x 1)
                    (< 0 y 1)
                    (< 0 z 1)
                    (< 0 w 1))
          do (setf x (a:clamp x 0.0 1.0))
             (setf y (a:clamp y 0.0 1.0))
             (setf z (a:clamp z 0.0 1.0))
             (setf w (a:clamp w 0.0 1.0)))
    (values h x y z w)))

(time (loop repeat 1000 do (sp 314118820924936286855167 0.001)))
(sp 330930727582781472108543)
(time
 (loop with c = 314118820924936286855167
       repeat 100000
       for x = (random 1.0)
       for y = (random 1.0)
       for z = (random 1.0)
       for w = (random 1.0)
       maximize (p4b c x y z w)))
1.5326588
1.5267444


(defvar *rsmax* (list 0 0))
(defun pn (n)
  (loop for i below 16
        collect (mod n 32)
        do (setf n (ash n -5))))

(defparameter *stop* t)

(defun rsearch4 (n i &optional (eps 0.001))
 (when (<= 4 i 9)
    (format t "a:~a@ ~s~%" i (pn n)))
  (let ((work))
    (cond
      ((< i 16)
       (let ((h  (p4* n)))
         (loop with n1 = n
               for j from 0 below 32
               for n2 = (+ n1 (* j (expt 32 i)))
               for h2 = (p4* n2)
               when (= h2 h)
                 do (push n2 work)
               when (> h2 h)
                 do (setf h h2
                          n n2
                          work (list n2))))
       (unless *stop*
         (loop for w in work do (rsearch4 w (1+ i) eps))))
      (t
       (let ((h (sp n eps)))
         (when (> h (car *rsmax*))
           (setf *rsmax* (list h n))
           (format t "a -> ~s~%" *rsmax*)))))))

(defparameter *stopw* nil)
(defun rsearch4a (n i w1)
 (when (<= 4 i 9)
    (format t "a:~a@ ~s~%" i (pn n)))
  (let ((work))
    (cond
      ((< i 16)
       (let ((h  (sp n)))
         (loop with n1 = n
               for j from 0 below 32
               for n2 = (+ n1 (* j (expt 32 i)))
               for h2 = (sp n2)
               when (= h2 h)
                 do (push n2 work)
               when (> h2 h)
                 do (setf h h2
                          n n2
                          work (list n2))))
       (unless *stopw*
         (loop for w in work do (rsearch4a w (1+ i)
                                          (list* (length work) w1)))))
      (t
       (format t "w = ~s~%" w1)
       ))))
(* 32 12 8 6 2 2 2 2 1 1 1 2 2 2 2 1)4718592

(rsearch4a 0 0 nil)
(setf *stopp* t)
(declaim (type fixnum *cc*))
(sb-ext:defglobal *cc* 0)
(expt 4 10) (* 8 1048576)8388608
(/ (expt 4 16) 44200.0)97171.2 (/ 97171.2 3600)26.992

(defun rsearch4p (n i &optional (eps 0.001))
 (when (<= 2 i 7)
   (format t "a:~a@ ~s~%" i (pn n)))
  (let ((work))
    (cond
      ((< i 16)
       (let ((h (p4* n)))
         (loop with n1 = n
               for j from 0 below 32
               for n2 = (+ n1 (* j (expt 32 i)))
               for h2 = (p4* n2)
               when (= h2 h)
                 do (push n2 work)
               when (> h2 h)
                 do (setf h h2
                          n n2
                          work (list n2)))
         (setf work (a:shuffle work)))
       (if (= i 1)
           (progn
             (bt:make-thread (lambda ()
                               (rsearch4p (first work) (1+ i) eps)
                               (rsearch4p (second work) (1+ i) eps))
                             :name (format nil "r4p ~s" n))
             (bt:make-thread (lambda ()
                               (rsearch4p (third work) (1+ i) eps)
                               (rsearch4p (fourth work) (1+ i) eps))
                             :name (format nil "r4p ~s" n)))
           (unless *stop*
             (loop for w in work do (rsearch4p w (1+ i) eps)))))
      (t
       (sb-ext:atomic-incf *cc*)
       (let ((h (sp n eps)))
         (when (> h (car *rsmax*))
           (setf *rsmax* (list h n))
           (format t "a -> ~s~%" *rsmax*)))))))
(progn
  (setf *stop* t)
  (setf *cc* 0)
  (sleep 1)
  (setf *stop* nil)
  (setf *rsmax* (list 0 0))
  (rsearch4p 0 0)
  (print *rsmax*))

(defparameter *stopp* nil)
(loop for x = *cc* then y
      for y = *cc*
      do (sleep 10)
         (format t "~s = ~s%: ~s/sec == ~s~%"
                 y
                 (* 100.0 (/ y (expt 4 16)))
                 ;(* 100.0 (/ y (expt 2 16)))
                 ;(* 100.0 (/ y 4718592))

                 (/ (- y x) 5.0)
                 (car *rsmax*))
      until *stopp*)

(setf *stopp* t)

(print
 (loop repeat 100
       for i = (loop repeat 5000000
                   for x = (random 1.0)
                   for y = (random 1.0)
                   for z = (random 1.0)
                   for w = (random 1.0)
                   maximize  (p4b 625598271410972627351655 x y z w))
       when (> i 1.536)
         do (print i)
       maximize i))

(let ((x1 0.49999154)
      (y1 0.5185142)
      (z1 0.3552527)
      (w1 0.50012124)
      (h1 1.536354 ))
 (flet ((s (x)
          (let ((s (signum x)))
            (* s (- 1 (expt (- 1 (abs x)) 1.464)) ))))
   (S (* 0.6507949348645372 (p4b 625598271410972627351655 x1 y1 z1 w1)))
   ))0.9999975


(let ((x1 0.49999154)
      (y1 0.5185142)
      (z1 0.3552527)
      (w1 0.50012124)
      (h1 1.536354 ))
  (print
 (loop
   for dw from -5 to 5
   sum (loop
         for dz from -5 to 5
         sum (loop
               for dy from -5 to 5
               sum (loop
                     for dx from -5 to 5
                     for x = (+ x1 (* dx single-float-epsilon))
                     for y = (+ y1 (* dy single-float-epsilon))
                     for z = (+ z1 (* dz single-float-epsilon))
                     for w = (+ w1 (* dw single-float-epsilon))
                     for h = (p4b 625598271410972627351655 x y z w)
                     count (>= h h1)
                     when (>= h h1)
                       do (format t "~s @ ~s ~s ~s ~s~%"
                                  h x y z w))))
   )))
1.5341963
1.5345141
1.5359251
1.5348861
1.5361549
1.5342666
(sp 625598271410972627351655 0.001)
1.536354 @ 0.49999154 0.5185142 0.3552527 0.50012124

1.536036
1.5363486
1.5363537
1.536354
() 1.5363349


74653329 = 1.7381582%: 44048.6/sec == 1.5317159
77965350 = 1.8152721%: 43973.4/sec == 1.5363538
760823934 = 17.714314%: 48809.4/sec == 1.536354
3638943457 = 84.725746%: 44278.8/sec == 1.536354
4294967296 = 100.0%: 190.4/sec == 1.536354

(1.536354 625598271410972627351655)
(1.5363538 935638464315390049008343)

*rsmax*
(1.536354 625598271410972627351655)

(1.5048398 927805318915952394231783) @ 328185270

*rsmax* w sp @ 12610686 / done
(1.0098848 294751317600221474853163)

*rsmax* @ 7706317

(1.5041462 307035487363140371214327)
(/ 1.5191232)0.6582745
(/ 0.6507949348645372d0)1.5365823340468219d0
a:shuffle

*rsmax*
(1.5005525 625599012138762121375743)
(1.536354 314118820924936286855167)
(1.527693 330930727582781472108543)
(sp 314118820924936286855167 0.001)
(sp 330930727582781472108543)
(let ((ow 5) (oh 5))
 (locally (declare (optimize speed))
   (let* ((n  (* ow oh))
         (foo (make-array n :element-type '(unsigned-byte 32) :initial-element 00)))
     (loop :for i :below n :do (setf (aref foo i) i))
     (a:shuffle foo)
     ;;(make-array (* ow oh) :element-type '(unsigned-byte 32) :initial-contents foo)
     )))

(pn 936094830189969902792703) (31 31 27 27 29 29 25 25 30 30 26 26 28 28 24 24)
(pn 633863375286312609116159) (31 31 27 27 29 29 25 25 30 30 26 26 28 28 24 16)
(pn 330930727582781841207295) (31 31 27 27 29 29 25 12 30 20 26 20 28 9 24 8)
(pn 330930727582987630538751) (31 31 27 27 29 18 25 18 30 20 26 20 28 9 24 8)

(format nil "~3,' x" '(1 2 3 4 11))
(defvar *rsmax2* (list 0 0))
(defun rsearch4b (n i)
  (when (<= 4 i 9)
    (format t "b:~a@ ~a~%" i (pn n)))
  (let ((work))
    (cond
      ((< i 16)
       (let ((h  (p4* n)))
         (loop with n1 = n
               for j from 0 below 32
               for n2 = (+ n1 (* j (expt 32 i)))
               for h2 = (p4* n2)
               when (= h2 h)
                 do (push n2 work)
               when (> h2 h)
                 do (setf h h2
                          n n2
                          work (list n2))))
       (loop for w in (reverse work) do (rsearch4p w (1+ i))))
      (t
       (let ((h (sp n (/ 123.0))))
         (when (> h (car *rsmax2*))
           (setf *rsmax2* (list h n))
           (format t "b -> ~s~%" *rsmax2*)))))))
(rsearch4b 0 0)
(expt 4 16)
4294967296
(upgraded-array-element-type '(unsigned-byte 5))
(LIST *rsmax* *rsmax2*)

((1.536354 314118820924936286855167) (1.536354 935638463665578676622439))
((1.5359575 314382845227147403685887) (1.536354 935638463665578676622439))
((1.5359575 314382845227147403685887) (1.536354 935638463665578676622439))
((1.5359575 314382845227147403685887) (1.536354 935638463665578676622439))
((1.5355563 314382845227147403849727) (1.536354 935638463665578676622439))
((1.5355563 314382845227147403849727) (1.5324321 927817044178319740802151))
((1.528318 330930727582781304336383) (1.5324321 927817044178319740802151))
(/ 0.6507949348645372d0)1.5365823340468219d0

(- 1.5365823340468219d0 1.536354)2.2838831470517462d-4
a:7@ (31 31 5 12 7 18 5 0 0 0 0 0 0 0 0 0)
b:7@ (7 3 27 22 7 18 5 0 0 0 0 0 0 0 0 0)
