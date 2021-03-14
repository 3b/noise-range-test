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

(defclass vis3 (scratchpad)
  ()
  (:default-initargs :shaders '((:tex :vertex vertex :fragment fragment)
                                (:solid :vertex vertex :fragment frag-solid))))


(defvar *w* nil)

(defun uniforms ()
  (glim:uniform 'mv (glim:ensure-matrix :modelview))
  (glim:uniform 'mvp (sb-cga:matrix*
                      (glim:ensure-matrix :projection)
                      (glim:ensure-matrix :modelview))))



(defvar *y* 0.0)

(defvar *min* 0)
(defvar *max* 0)
(defun v3 (x y z)
  (sb-cga:vec (coerce x 'single-float)
              (coerce y 'single-float)
              (coerce z 'single-float)))
(defvar *rot* 0)
(defvar *cell* 0)
(defmethod display ((w vis3) now)
  (setf *w* w)

  (glim:with-state (*format*)
                                        ;(glim:uniform 'proj sb-cga:+identity-matrix+)
                                        ;(glim:matrix-mode :projection)
                                        ;(glim:load-identity)
    (glim:matrix-mode :modelview)
    (glim:load-identity)
    
    (glim:look-at (v3 3 0.0 3) (v3 0 0 0) (v3 0 1 0))

    (gl:enable :depth-test :multisample :texture-2d )

    (glim:uniform 'tex 0)

    (glim:uniform 'debug1 (if (aref *flags* 1) 1 0))
    #++
    (let ((s 10.0))
      (glim:with-pushed-matrix (:modelview)
        (glim:translate 0.0 0.0 0)
        (glim:scale (/ (1+ s)) (/ (1+ s)) 1)
        (gl:line-width 1)
        (uniforms)

        (glim:with-draw (:lines :shader :solid)
          (color 1 0 0 1)
          (vertex -10 0 0) (vertex 10 0 0)
          (vertex 0 -10 0) (vertex 0 10 0)
          (color 0.4 0.1 0.1 1)
          (loop for i from -10 to 10
                do (vertex i -10 0) (vertex i 10 0))
          (vertex -10 -10 0) (vertex 10 -10 0)
          (vertex -10 10 0) (vertex 10 10 0)
          (color 0.3 0.05 0.05 1)
          (vertex -10 -5 0) (vertex 10 -5 0)
          (vertex -10 5 0) (vertex 10 5 0)

          (color 0.03 0.5 0.8 1)
          (vertex -10 (* s *min*) 0) (vertex 10 (* s *min*) 0)
          (vertex -10 (* s *max*) 0) (vertex 10 (* s *max*) 0))

        (dispatch-draws w)
        (gl:line-width 1)
        (let ((y *y*))
          (incf *y* 0.13456)
          #++(when (> *y* 1)
               (setf *y* 0))
          (glim:with-draw (:line-strip :shader :solid)
            (color 1 0 y 1)
            (loop for i from -10 below 10 by 0.125
                  for n = (improved-noise3 i y 0.5)
                  do (when (> n *max*)
                       (format t "max ~s -> ~s~%" *max* n)
                       (setf *max* n))
                     (when (< n *min*)
                       (format t "min ~s -> ~s~%" *min* n)
                       (setf *min* n))
                     (vertex i
                             (* s n)
                             0)))
          (glim:with-draw (:line-strip :shader :solid)
            (color 0 y 0 1)
            (loop with y = (/ y 4)
                  for i from -10 below 10 by 0.05
                  do (vertex i
                             (* s
                                #++(improved-noise1 i)
                                (original-noise1 (+ i y))
                                #++(let ((i (+ i y)))
                                     (improved-noise4 i i i i))
                                #++(improved-noise4 (+ y i)
                                                    (+ (/ i 256) 0.5)
                                                    0.5 0.5)
                                #++(improved-noise4 (+ y  i) 0.0 0.0 0.0))
                             0))))
        (dispatch-draws w)
        ))

    (gl:line-width 1)
    (let ((s 1)
          (z (if (aref *flags* 5)
                 0.5
                 (coerce
                  (mod (* 0.2 (- (/ (get-internal-real-time)
                                    (float internal-time-units-per-second 1d0))
                                 303233))
                       1)
                  'single-float)))
          (cell (if (aref *flags* 6)
                    (maximize-p3::cc 4 5 9 9 6 7 2 11)
                    *cell*
                    #++(maximize-p3::cc 4 5
                                     5 4
                                     5 4
                                     4 5)))
                                        ;(cell 402005008)
          #++(z (mod (* 0.2 (/ (get-internal-real-time)
                               (float internal-time-units-per-second 1d0)))
                     1)))
      (glim:with-pushed-matrix (:modelview)
        (glim:translate -0.5 -0.5 0)
        (glim:scale (/ (1+ s)) (/ (1+ s)) 1)
        (glim:scale 2 2 1)
        (gl:line-width 1)
        (uniforms)
        #++
        (glim:with-draw (:lines :shader :solid)
          (color 1 0 0 1)
          (vertex -10 0 0) (vertex 10 0 0)
          (vertex 0 -10 0) (vertex 0 10 0)
          (color 0.4 0.1 0.1 1)
          (loop for i from -10 to 10
                do (vertex i -10 -1) (vertex i 10 -1))
          (vertex -10 (- s) 0) (vertex 10 (- s) -1)
          (vertex -10 s 0) (vertex 10 s -1)
          (color 0.3 0.05 0.05 1)
          (vertex -10 -5 0) (vertex 10 -5 -1)
          (vertex -10 5 0) (vertex 10 5 -1)

          (color 0.03 0.5 0.8 1)
                                        ;(vertex -10 (* s *min*) 0) (vertex 10 (* s *min*) 0)
                                        ;(vertex -10 (* s *max*) 0) (vertex 10 (* s *max*) 0)
          )
        
        (dispatch-draws w)
        #++
        (glim:with-draw (:points :shader :solid)
          (loop
            for x from 0.38 below 0.63 by 0.001
            do (loop for y from 0.07 below 0.54 by 0.001
                     for z = 0.45
                     for r = (improved-noise3 (+ x 96) (+ y 207) (+ z 93))
                     when (< r 0)
                       do (color 0 (+ 1 r) 0)
                     else when (< r 1)
                            do (color 0 r r)
                     else do (color 1 (* 10 (1- r)) (* 10 (1- r)))
                     do (vertex x y z)))
          #++
          (loop for i below 10000
                for x = (+ 0.38 (random 0.25d0))
                for y = (+ 0.07 (random 0.44d0))
                for z = 0.45 #++(+ 0.4 (random 0.1d0))
                for r = (improved-noise3 (+ x 96) (+ y 207) (+ z 93))
                when (< r 0)
                  do (color 0 (+ 1 r) 0)
                else when (< r 1)
                       do (color 0 r r)
                else do (color 1 (* 10 (1- r)) (* 10 (1- r)))
                do (vertex x y z)))

        #++
        (progn
          (color 0.5 0.1 0.4 1)
          (gl:line-width 1)

          (glim:with-draw (:line-strip :shader :solid)
            (loop for i from 0 below 1 by 0.01
                  do (vertex i (noise-range-test/improved::fade i) 0)))
          (color 0.1 0.5 0.4 1)
          (glim:with-draw (:line-strip :shader :solid)
            (loop for i from 0 below 1 by 0.01
                  do (vertex i (noise-range-test/improved::fade (- 1 i)) 0)))
          (color 1.0 0.1 0.4 1)
          (glim:with-draw (:line-strip :shader :solid)
            (loop for i from 0 below 1 by 0.01
                  for y = (+ (noise-range-test/improved::fade (float i 1d0))
                             (noise-range-test/improved::fade (- 1d0 i)))
                  do (vertex i y 0)
                     (setf *max* (max y *max*)))))
        (glim:with-pushed-matrix (:modelview)
          (glim:translate 0.5 0 0.5)
         
          (if (aref *flags* 1)
              (glim:rotate (* 6 (- (/ (get-internal-real-time)
                                      (float internal-time-units-per-second 1d0))
                                   299433))
                           0 1 0)
              (glim:rotate *rot* 0 1 0))
          (glim:translate -0.5 0 -0.5)
          (uniforms)
          (glim:with-draw (:points :shader :solid)
            #++(loop
                 for x from 0.38 below 0.63 by 0.01
                 do (loop for y from 0.07 below 0.54 by 0.01
                          for z = 0.45
                          for r = (improved-noise3 (+ x 96) (+ y 207) (+ z 93))
                          when (< r 0)
                            do (color 0 (+ 1 r) 0)
                          else when (< r 1)
                                 do (color 0 r r)
                          else do (color 1 (* 10 (1- r)) (* 10 (1- r)))
                          do (vertex x y r)
                          #+do (vertex (1+ y) x r)))
           
            (loop
              for x from 0 below 1 by 0.0125
              do (loop for y from 0.0 below 1 by 0.0125
                       for r = (maximize-p3::p3b cell x y z)
                       when (< r 0)
                         do (color 0 (+ 1 r) 0)
                       else when (< r 1)
                              do (color 0 r r)
                       else do (color 1 (* 10 (1- r)) (* 10 (1- r)))
                       do (vertex x y r)
                       #+do (vertex (1+ y) x r)))
            (when (aref *flags* 2)
              (loop
                for x from -1 below 2 by 0.0078125
                do (loop for y from -1 below 2 by 0.0078125
                         unless (and (<= 0 x 1)
                                     (<= 0 y 1))
                           do (if (or (= (rem x 1) 0)
                                      (= (rem y 1) 0))
                                  (color 0 0 0 1)
                                  (color (/ (1+ x) 3)
                                         0.0
                                         (/ (1+ y) 3)
                                         1))
                              (vertex x
                                      y
                                      (improved-noise3 (+ x 96) (+ y 207) (+ z 93))))))


            #++
            (loop for i below 10000
                  for x = (+ 0.38 (random 0.25d0))
                  for y = (+ 0.07 (random 0.44d0))
                  for z = 0.45 #++(+ 0.4 (random 0.1d0))
                  for r = (improved-noise3 (+ x 96) (+ y 207) (+ z 93))
                  when (< r 0)
                    do (color 0 (+ 1 r) 0)
                  else when (< r 1)
                         do (color 0 r r)
                  else do (color 1 (* 10 (1- r)) (* 10 (1- r)))
                  do (vertex x y z)))
          
          (glim:with-draw (:lines :shader :solid)
            (color 0.5 0.5 0 1)
            (flet ((z (z)
                     (loop
                       for x from 0.0 below 1 by 0.0625
                       do (loop
                            for y from 0.0 below 1 by 0.0625
                            for g = (maximize-p3::p3g cell x y z)
                            for h = (maximize-p3::p3b cell x y z)
                            for sg = (sb-cga:vec+ (sb-cga:vec x y z)
                                                  (sb-cga:vec* g 0.0625))
                            do (color 0.15 0.15 0 1)
                               (vertex x y z)
                               (color (abs (aref g 0))
                                      (abs (aref g 1))
                                      (abs (aref g 2)) 1)
                               (vertex-v sg)
                            #++(vertex (aref sg 0) (aref sg 1) z)
                            ))
                     ))
              (if (aref *flags* 3)
                  (loop for z from 0.0625 upto 1.0 by 0.125 do
                    (z z))
                  (z z)))
            (let (#++(*random-state* (make-random-state *random-state*)))
              (loop repeat 40 do
                (loop with x = (random 1.0)
                      with y = (random 1.0)
                      with cg = (random 1.0)
                      with cb = (random 1.0)
                      with eps = 0.0001
                      for i below 100
                      for g = (maximize-p3::p3g cell x y z (* 5 eps))
                      for l = (sb-cga:vec-length g)
                      for gn = (if (< (abs l) eps)
                                   (v3 0 0 0)
                                   #++(v3 (- (random 0.002) 0.0010)
                                          (- (random 0.002) 0.001)
                                          0)
                                   (sb-cga:vec* (sb-cga:normalize g) 0.01))
                      for p2 = (sb-cga:vec+ (v3 x y z)
                                            (v3 (aref gn 0) (aref gn 1) 0))
                      for h1 = (maximize-p3::p3b cell x y z)
                      for h2 = (maximize-p3::p3b cell (aref p2 0) (aref p2 1)
                                                 z)
                      until (< (abs l) eps)
                      for c = (/ i 100.0)
                      do (color c cg cb 1)
                      #++(vertex-v xyz)
                      #++(vertex-v p2)
                         (vertex x y (+ h1 0.3))
                         (setf x (aref p2 0)
                               y (aref p2 1))
                         (vertex x y (+ h2 0.3))
                      while (and (<= 0 x 1)
                                 (<= 0 y 1))


                      )))
            (color 1 0 0 1)
            (vertex 0 0 0) (vertex 0 0 1)
            (vertex 0 0 1) (vertex 0 1 1)
            (vertex 0 1 1) (vertex 0 1 0)
            (vertex 0 0 0) (vertex 0 1 0)
            (vertex 0 0 0) (vertex 1 0 0)
            (vertex 1 0 0) (vertex 1 0 1)
            (vertex 0 0 1) (vertex 1 0 1)
            (vertex 1 1 0) (vertex 1 0 0)
            (vertex 1 1 0) (vertex 0 1 0)
            (vertex 1 1 1) (vertex 1 1 0)
            (vertex 1 1 1) (vertex 1 0 1)
            (vertex 1 1 1) (vertex 0 1 1)
            (loop with h1 = cell
                  for i below 8
                  with l = 0.1
                  for h = (mod h1 12)
                  for x = (ldb (byte 1 0) i)
                  for y = (ldb (byte 1 1) i)
                  for z = (ldb (byte 1 2) i)
                  for x2 = (if (logbitp 0 i) (- l) l)
                  for y2 = (if (logbitp 1 i) (- l) l)
                  for z2 = (if (logbitp 2 i) (- l) l)

                  do (setf h1 (floor h1 12))
                     (color 0.5 0.5 0.5 1)
                     (vertex x y z)
                     (apply #'vertex
                            (mapcar '+
                                    (list x y z)
                                    (noise-range-test/improved::grad* h x2 y2 z2)))
                     (apply 'color
                            (mapcar 'abs
                                    (noise-range-test/improved::grad* h 1 1 1)))
                     (vertex x y z)
                     (apply #'vertex
                            (mapcar '+
                                    (list x y z)
                                    (noise-range-test/improved::grad* h l l l)))))))
      (dispatch-draws w)
      (gl:point-size 16)
      (glim:with-draw (:points :shader :solid)
        (color 1 1 1 1)
        (vertex 0 0 0)
        (color 1 0 0 1)
        (vertex 1 0 0)
        (color 0 1 0 1)
        (vertex 0 1 0)
        (color 0 0 1 1)
        (vertex 0 0 1)
        )
      (dispatch-draws w)
      (gl:point-size 0.4))

    ))
(sb-cga:normalize (v3 1 1 0))#(0.7069092 0.7069092 0.0)
(sb-cga:normalize (v3 1 1 1))#(0.5772705 0.5772705 0.5772705)
(sqrt 2/4)0.70710677
(/ (sqrt 2) 2)0.70710677
(sqrt 3/4)0.8660254
(/ 0.9649214285521897d0)1.0363538112118038d0
(defmethod mouse ((window vis3) button state x y)
  (format t "click ~s ~s~%" button state)
  )

(defmethod mouse-wheel ((window vis3) button state x y)
  (case state
    ((:up :wheel-up)
     (decf *rot* 2))
    ((:down :wheel-down)
     (incf *rot* 2)))
  )


#++
(loop for i below 1 by 0.1
      for a = (noise-range-test/improved::fade i)
      for b = (noise-range-test/improved::fade (- 1 i))
      do (format t "~s + ~s = ~s~%"
                 (* a 1) (* b 0.3)
                 (a (* a 1) (* b 0.3))
                 ))
(defmethod keyboard ((window vis3) key x y)
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

(defmethod init-gl ((w vis3))
  (gl:pixel-store :unpack-alignment 1)

  (gl:disable :dither))

(defun vis3 (&rest args)
  (glut:display-window (apply #'make-instance 'vis3 args)))

#++
(ql:quickload 'noise-range-test)
#++
(vis3 :pos-x 2440 :pos-y 140 :width 1400 :height 850)
#++
(glut:show-window)




#++
(setf (shapes *w*) (car *undo*))
#++
(setf (spacing *w*) 4)
#++
(setf (spacing *w*) 16)

#++
(loop for p in (shapes *w*)
      do (move-polyline p 0 -16))

