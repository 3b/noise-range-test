
#++
(ql:quickload '(alexandria sb-cga cl-opengl
                3b-glim/example/s 3b-glim/2d 3bgl-shader))

(in-package noise-range-test)

(defvar *format*
  (glim:compile-vertex-format
   '(1
     (0 :vec4) ;; position
     (1 :vec4) ;; uv
     (2 :vec4) ;; color
     )))

(declaim (inline vertex vertex-v color normal uv))
(defun vertex (x &optional (y 0.0) (z 0.0) (w 1.0))
  (glim:attrib-f 0 x y z w))
(defun vertex-v (v)
  (glim:attrib-fv 0 v))
(defun uv (x &optional (y 0.0) (z 0.0) (w 1.0))
  (glim:attrib-f 1 x y z w))
(defun color (x &optional (y 0.0) (z 0.0) (w 1.0))
  (glim:attrib-f 2 x y z w))

(defparameter *debug* 0)
(defparameter *flags* (make-array 10 :initial-element nil))

(defclass vis4 (scratchpad)
  ()
  (:default-initargs :shaders '((:tex :vertex vertex :fragment fragment)
                                (:solid :vertex vertex :fragment frag-solid))))


(defvar *w* nil)

(defun uniforms ()
  (glim:uniform 'mv (glim:ensure-matrix :modelview))
  (glim:uniform 'mvp (sb-cga:matrix*
                      (glim:ensure-matrix :projection)
                      (glim:ensure-matrix :modelview))))



(defun v3 (x y z)
  (sb-cga:vec (coerce x 'single-float)
              (coerce y 'single-float)
              (coerce z 'single-float)))

(defparameter *histogram*
  (loop repeat 4 collect (make-instance 'hist)))

(defun b2 (tt r1)
  (let* ((t2 (* tt tt))
         (mt (- 1 tt))
         (mt2 (* mt mt))
         (f (vector (/ mt2 2) (* 2 r1 mt tt) (/ t2 2)))
         (basis (reduce '+ f)))
    (/ (+ (aref f 2)) basis)))

;(1-x2)
;(1-2x2+x4)
;(1-3x2+x4-x6)
(defun ik (x k)
  (case k
    (0 x)
    (1 (+ x (* -1/3 x x x)))
    (2 (+ x (* -2/3 x x x) (* 1/5 x x x x x)))
    (3 (+ (* -1/7 (expt x 7))
          (* 3/5 (expt x 5))
          (* -1 (expt x 3))
          x))
    (4 (+ (* 1/9 (expt x 9))
          (* -4/7 (expt x 7))
          (* 6/5 (expt x 5))
          (* -4/3 (expt x 3))
          x))
    (5 (+ (* -1/11 (expt x 11))
          (* 5/9 (expt x 9))
          (* -10/7 (expt x 7))
          (* 2 (expt x 5))
          (* -5/3 (expt x 3))
          x))
    (6 (+ (* 1/13 (expt x 13))
          (* -6/11 (expt x 11))
          (* 5/3 (expt x 9))
          (* -20/7 (expt x 7))
          (* 3 (expt x 5))
          (* -2 (expt x 3))
          x))
)
)
(loop for k below 6
      do (loop for i below 1 by 0.125
       do (format t "~s: ~s: ~s ? ~s~%" k i
                  (ik i k)
                  (peval (int (pexpt #(1 0 -1) k)) i))))
(defun b3 (tt r1)
  (let* ((w #(0.0 1 1 1))
         (t2 (* tt tt))
         (t3 (* t2 tt))
         (mt (- 1 tt))
         (mt2 (* mt mt))
         (mt3 (* mt2 mt))
         (f (vector mt3
                    ;(* 3 r1 mt2 tt)
                    (* (/ r1) 3 mt2 tt)
                    
                    (* r1 3 mt t2)
                    t3))
         (basis (reduce '+ f)))
    (/ (reduce '+ (map 'vector '* f w))
       basis)))

#++
(defun reshape (x k1 ks)
  (let* ((p1 (int (pexpt #(1 0 -1) k1)))
         (p2 (int (pexpt #(1 0 -1) (1+ k1))))
         (s1 (peval p1 x))
         (w1 (peval p1 1))
         (s2 (peval p2 x))
         (w2 (peval p2 1)))
    (/ (a:lerp ks s1 s2)
       (a:lerp ks w1 w2))))

(defvar *e* 0.0)

(defvar *k* 0)
(defvar *ks* 0.9)
(defmethod display ((w vis4) now)
  (setf *w* w)

  (glim:with-state (*format*)
    (glim:matrix-mode :projection)
    (glim:load-identity)
    (glim:matrix-mode :modelview)
    (glim:load-identity)
    (gl:enable :depth-test :multisample :texture-2d )
    (glim:uniform 'tex 0)
    (glim:uniform 'debug1 (if (aref *flags* 1) 1 0))
    (gl:line-width 1)
    (glim:with-pushed-matrix (:modelview)
      (glim:translate -1 -1 0)
      (glim:scale (/ 2 (wx w)) (/ 2 (wy w)) 1)
      (glim:translate 64 64 0)

      (gl:line-width 1)
      (uniforms)
      (glim:with-draw (:lines :shader :solid)
        (color 1 0 0 1)
        (vertex -50 0 0)
        (vertex 1024 0 0)
        (color 0 1 0 1)
        (vertex 0 -50 0)
        (vertex 0 1024 0)
        (color 0 0 1 1)
        )
      (let ((y1 32)
            (x1 10)
            (w1 1000)
            (h1 512))
        (glim:with-draw (:lines :shader :solid)
          (color 1 0 0 1)
          (vertex x1 (+ 0.5 y1) 0)
          (vertex (+ x1 w1) (+ 0.5 y1) 0)
          (color 0 1 0 1)
          (vertex (+ x1 0.5 (/ w1 2)) y1 0)
          (vertex (+ x1 0.5 (/ w1 2)) (+ h1 y1) 0)
          (let* ((h (car *histogram*))
                 (n (nsamples h))
                 (s (/ (float w1) n))
                 (x@-1 (* (bucket-for-value h -1.0) s))
                 (x@-2 (* (bucket-for-value h -2.0) s))
                 (x@1 (* (bucket-for-value h 1.0) s))
                 (x@2 (* (bucket-for-value h 2.0) s))
                 (rx (* (bucket-for-value h 0.146) s)))
            (color 1 1 1 1)
            (loop for i from -2 upto 2 by 0.1
                  for x = (+ x1 (* (bucket-for-value h i) s))
                  do (vertex (+ x) (+ y1 -10) 0)
                     (vertex (+ x) (+ y1) 0))
            (when rx
              (vertex (+ x1 rx) (+ y1 0) 0)
              (vertex (+ x1 rx) (+ y1 h1) 0)

)

            (vertex (+ x1 x@-1) (+ y1 -20) 0)
            (vertex (+ x1 x@-1) (+ y1 4) 0)

            (vertex (+ x1 x@-2) (+ y1 -20) 0)
            (vertex (+ x1 x@-2) (+ y1 4) 0)

            (vertex (+ x1 x@1) (+ y1 -20) 0)
            (vertex (+ x1 x@1) (+ y1 4) 0)
            (vertex (+ x1 x@2) (+ y1 -20) 0)
            (vertex (+ x1 x@2) (+ y1 4) 0))
          
          (color 0 0 1 1)
          )
        (loop
          with m = (reduce 'max *histogram* :key 'max-count)
          for hist in *histogram*
          for cc in '((1 0 0 1) (1 0 0.5 1) (0 1 0 1) (0.3 0.2 1 1))
          ;for m = (max-count hist)
          for dy from 10 by 10
          for n = (nsamples hist)
          for s = (/ (float w1) n)
          do (unless (zerop m)
               (glim:with-draw (:line-strip :shader :solid)
                 (apply #'color cc)
                 (loop for i below (nsamples hist)
                       for h = (aref (samples hist) i)
                       do (vertex (+ x1 (* i (/ (float w1)
                                                (nsamples hist))))
                                  (+ y1 (* h (/ (* 1 (float h1))
                                                (float m)))))))
               (glim:with-draw (:lines :shader :solid)
                 (let ((min (+ x1 (* s (bucket-for-value hist (smin hist)))))
                       (max (+ x1 (* s (bucket-for-value hist (smax hist))))))
                   (vertex min (+ y1 -15))
                   (vertex min (+ y1 -30))
                   (vertex max (+ y1 -15))
                   (vertex max (+ y1 -30)))
                 )

               (glim:with-draw (:lines :shader :solid)
                 (let* ((dev (stddev hist))
                        (mean (mean hist))
                        (min (+ x1 (* s (bucket-for-value hist (- mean dev)))))
                        (max (+ x1 (* s (bucket-for-value hist (+ mean dev))))))
                   (vertex min (+ y1 50 dy))
                   (vertex min (+ y1 100 dy))
                   (vertex max (+ y1 50 dy))
                   (vertex max (+ y1 100 dy)))))))
      (glim:with-draw (:line-strip :shader :solid)
        (loop for i from 0 to 1 by 0.01
              do (vertex (* i 500) (* 500 (reshape i 64/100)))))
      (dispatch-draws w))))
;(/ 0.145)6.896552
#++
(progn
  #++(map 'nil 'reset *histogram*)
  (time
   (loop repeat 100000000
         for x = (random 256.0)
         for y = (random 256.0)
         for z = (random 256.0)
         for w = (random 256.0)
         for h2 = (noise-range-test/improved:improved-noise2 x y)
         for h3 = (*                    ;0.9649214285521897
                   (noise-range-test/improved:improved-noise3 x y z))
         for h4 = (*                    ; 0.6507949348645372
                   (noise-range-test/improved:improved-noise4 x y z w))
         do (sample (first *histogram*) h2)
            (sample (third *histogram*) h3)
            (sample (fourth *histogram*) h4)))

)
;;(mapcar 'smax *histogram*)
;;(mapcar 'stddev *histogram*)
;;(0.25560435866526304d0 0.0d0 0.2608955267008607d0 0.26089574934346343d0) @ 1.464
;;(0.25559553202876684d0 0.0d0 0.2608959851041246d0 0.26077668690364736d0) @ 1.463
;;(0.2556617968614749d0 0.0d0 0.26099582261099674d0 0.1919339211052863d0)
;;(0.25558242610774756d0 0.0d0 0.26102898533362134d0 0.19198917775853425d0)
;;
;;(0.25576655668461473d0 0.0d0 0.26108734976581366d0 0.25914506804512033d0)
#++
(flet ((s (x)
         (* (signum x) (- 1 (expt (- 1 (abs x)) 1.464)))
         #++(* s (expt (abs x) (/ 1.464)) )
))
  #++  (map 'nil 'reset *histogram*)
  (time
   (loop repeat 10000000
         for x = (random 256.0)
         for y = (random 256.0)
         for z = (random 256.0)
         for w = (random 256.0)
         for h2 = (noise-range-test/improved:improved-noise2 x y)
         for h3 = (* 0.9649214285521897
                   (noise-range-test/improved:improved-noise3 x y z))
         for h4 = (* 0.6507949348645372
                   (noise-range-test/improved:improved-noise4 x y z w))
         do (sample (first *histogram*) h2)
            (sample (third *histogram*) h3)
            (sample (fourth *histogram*) (s h4))))

)
;(/ 0.9 0.6507949348645372)
;(/ 1.25)
#++
(flet ((s (x)
         (let ((s (signum x)))
           (* s (- 1 (expt (- 1 (abs x)) 1.1375)) ))
))
  (map 'nil 'reset *histogram*)
  (time
   (loop repeat 100000000
         for x = (random 256.0)
         for y = (random 256.0)
         for z = (random 256.0)
         for w = (random 256.0)
         for h2 = (noise-range-test/improved:improved-noise2 x y)
         for h3 = (* 0.9649214285521897
                   (noise-range-test/improved:improved-noise3 x y z))
         for h4 = (a:clamp
                   (* 0.8
                      (noise-range-test/improved:improved-noise4 x y z w))
                   -1.0 1.0)
         maximize h4
         do (sample (first *histogram*) h2)
            (sample (third *histogram*) h3)
            (sample (fourth *histogram*) (s h4))))

)
#++
(flet ((s (x)
         (1- (* (expt (abs (* (1+ x) 0.5)) 2) 2))
))
  (map 'nil 'reset *histogram*)
  (time
   (loop repeat 10000000
         for x = (random 256.0)
         for y = (random 256.0)
         for z = (random 256.0)
         for w = (random 256.0)
         for h2 = (noise-range-test/improved:improved-noise2 x y)
         for h3 = (* 0.9649214285521897
                   (noise-range-test/improved:improved-noise3 x y z))
         for h4 = (* 0.6507949348645372
                   (noise-range-test/improved:improved-noise4 x y z w))
         do (sample (first *histogram*) h2)
            (sample (third *histogram*) h3)
            (sample (fourth *histogram*) (s h4))))

)



#++
(reset (first *histogram*))
#++
(time
 (loop repeat 100000000
       for x = (random 256.0)
       for y = (random 256.0)
       for h = (noise-range-test/improved:improved-noise3 x y 0)
       do (sample (first *histogram*) h)))

#++
(reset (second *histogram*))
#++
(loop with m = (sb-cga:rotate-around (v3 1 0 0) 0.3)
      repeat 100000000
      for v = (sb-cga:transform-point (v3 (random 256.0)
                                          (random 256.0) 0)
                                      m)
      for h = (noise-range-test/improved:improved-noise3
               (aref v 0) (aref v 1) (aref v 2))
      do (sample (second *histogram*) h))
#++
(reset (third *histogram*))
#++
(loop repeat 100000000
      for x = (random 256.0)
      for y = (random 256.0)
      for z = (random 256.0)
      for h = (* 0.9649214285521897
                 (noise-range-test/improved:improved-noise3 x y z))
      do (sample (third *histogram*) h))
#++
(loop repeat 100000000
      for x = (random 256.0)
      for y = (random 256.0)
      for z = (random 256.0)
      for h = (* 0.9649214285521897
                 (noise-range-test/improved:improved-noise3 x y z))
      do (sample (third *histogram*) h))

#++
(reset (fourth *histogram*))
#++
(loop repeat 100000000
      for x = (random 256.0)
      for y = (random 256.0)
      for z = (random 256.0)
      for w = (random 256.0)
      for h = (* 0.6507949348645372
                 (noise-range-test/improved:improved-noise4 x y z w))
      do (sample (fourth *histogram*) h))

(setf *print-length* 1000)
#++
(defvar *3samples* (make-array 50000000 :element-type 'single-float
                              :initial-element 0.0))
#++
(defvar *4samples* (make-array 50000000 :element-type 'single-float
                              :initial-element 0.0))
#++
(defvar *4fsamples* (make-array 50000000 :element-type 'single-float
                                         :initial-element 0.0))
#++
(defvar *4fsamplesr* (make-array 50000000 :element-type 'single-float
                                          :initial-element 0.0))

#++
(prog1 nil
 (map-into *3samples*
           (lambda ()
             (let ((x (random 256.0))
                   (y (random 256.0))
                   (z (random 256.0))
                   (w (random 256.0)))
               (* 0.9649214285521897
                  (noise-range-test/improved:improved-noise3 x y z))
               #++(* 0.6507949348645372
                     (noise-range-test/improved:improved-noise4 x y z w))))))
#++
(prog1 nil
 (map-into *4samples*
           (lambda ()
             (let ((x (random 256.0))
                   (y (random 256.0))
                   (z (random 256.0))
                   (w (random 256.0)))
               (* 0.9649214285521897
                  (noise-range-test/improved:improved-noise3 x y z))
               (* 0.6507949348645372
                  (noise-range-test/improved:improved-noise4 x y z w))))))

#++
(prog1 nil
 (map-into *4fsamples*
           (lambda ()
             (let ((x (random 256.0))
                   (y (random 256.0))
                   (z (random 256.0))
                   (w (random 256.0)))
               (noise-range-test::fbm4 x y z w 0.0 2.0 8 0.5)))))
#++
(prog1 nil
 (map-into *4fsamplesr*
           (lambda ()
             (let ((x (random 256.0))
                   (y (random 256.0))
                   (z (random 256.0))
                   (w (random 256.0)))
               (noise-range-test::fbm4 x y z w 0.0 2.0 8 0.75)))))

;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#++
(let (#++(e (setf *e* 1))
      (k (setf *k* 0))
      (ks (setf *ks* 0.81)))
  (flet ((s (x &optional (k k))
           ;; (* 1 (signum x) (- 1 (expt (- 1 (abs x)) 2.2)))
           (* (signum x)
              #++(- 1 (b3 (- 1 (abs x)) e))
              #++(b3 (abs x) e)
              (reshape (abs x) k))
          ))
   (map nil 'reset  *histogram*)
   (loop repeat 400000
         for h3 across *3samples*
         for h4 across *4samples*
         for f4 across *4fsamples*
         for f4r across *4fsamplesr*
         do (sample (first *histogram*) (s f4 3.9))
            (sample (second *histogram*) (s f4r 8.1))
            (sample (third *histogram*) h4)
            (sample (fourth *histogram*) (s h4 0.32)))
    (print (mapcar 'stddev *histogram*))))

;(mapcar 'smax *histogram*)(0.9998858 0.990728 1.000395 1.1867055)
;(mapcar 'smin *histogram*)(-0.89181435 -0.9956575 -1.0080997 -1.1959826)
;(mapcar 'stddev *histogram*)
;(0.0d0 0.0d0 0.26088462043707195d0 0.19201314941076741d0)
;(/ (expt (* 2 0.26088462043707195d0) 1.464) 2)0.1929116665243136d0
;(/ (expt (* 2 0.1929116665243136d0) (/ 1.464)) 2)0.2608846219039033d0

#++(let ((a #++ 0.26088462043707195d0 0.45)
      (b 0.19201314941076741d0))
  ;; a = (/ (expt (* 2 b) (/ N)) 2)
  ;; (* 2 a) = (expt (* 2 b) (/ n))
  ;; (log (* 2 a) (* 2 b)) = (/ n)
  (/ (log (* 2 a) (* 2 b)))
)
;9.083516417317005d0
;0.6903614891877375d0
;1.380722978375475d0

(defmethod mouse ((window vis4) button state x y)
  (format t "click ~s ~s~%" button state)
  )

(defmethod mouse-wheel ((window vis4) button state x y)
  (case state
    ((:up :wheel-up)
     (decf *rot* 2))
    ((:down :wheel-down)
     (incf *rot* 2)))
  )


(defmethod keyboard ((window vis4) key x y)
  (declare (ignore x y))

  (case key
    ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
     (let ((i (digit-char-p key)))
       (when i
         (setf (aref *flags* i) (not (aref *flags* i)))
         (format t "flags ~s -> ~s~%" i (aref *flags* i)))))
    (#\space
     (setf *cell* (random (expt 12 8)))
     )
    (#\Esc
     (glut:destroy-current-window))))

(defmethod init-gl ((w vis4))
  (gl:pixel-store :unpack-alignment 1)

  (gl:disable :dither))

(defun vis4 (&rest args)
  (glut:display-window (apply #'make-instance 'vis4 args)))

#++
(ql:quickload 'noise-range-test)
#++
(vis4 :pos-x 2440 :pos-y 140 :width 1400 :height 850)
#++
(glut:show-window)
