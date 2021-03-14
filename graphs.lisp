#++ (ql:quickload '(vecto skippy zpng sb-cga lparallel))
(in-package noise-range-test)

(defvar *seed* 4521945779207718781 #++(random most-positive-fixnum))

(defvar *hist3* (make-instance 'hist))
(loop repeat 10000000
      for h3 = (* 0.9649214285521897
                  (noise-range-test/improved:improved-noise3
                   (random 256.0) (random 256.0) (random 256.0)))
      do (sample *hist3* h3))

(with-graph ("/tmp/2d-rotated.gif" :xmin -1.5 :xmax 1.5 :ymin -0.2 :ymax 1.25)
  (let ((h1 *hist3*))
    (loop ;for i from -1/4 to 1/4 by 1/128 ;below 90 by 2
          with a1 = 0
          with a2 = 360
          with d = 5/2
          for i from a1 to a2 by d
          for hist = (make-instance 'hist)
          for m = (sb-cga:rotate-around (v3 1 0 0)
                                        (float (* i (/ pi 180)) 1.0))
          do (loop with *random-state* = (sb-ext:seed-random-state *seed*)
                   repeat 10000000
                   for v = (v3 (random 256.0) (random 256.0) 0)
                   for r = (sb-cga:transform-point v m)
                   for h3 = (* 0.9649214285521897
                               (noise-range-test/improved:improved-noise3
                                (aref r 0) (aref r 1) (aref r 2)))
                   do (sample hist h3))
             (new-frame 5)
             (format t "draw ~s~%" i)
             (vecto:with-graphics-state
               (apply #'vecto:set-rgb-stroke *fg-color*)
               (let ((v (sb-cga:transform-point (v3 0 1 0) m))
                     (c (sb-cga:transform-point (v3 0 0.1 0) m)))
                 #++(when (> (- a2 a1) 5)
                   (setf v (sb-cga:vec* v (/ d))))
                 (vecto:translate 64 (- 512 128))
                 (vecto:move-to (* 64 (aref c 1))
                                (* 64 (aref c 2)))
                 (vecto:line-to (* 64 (aref v 1))
                                (* 64 (aref v 2)))
                 (vecto:stroke)
)
)
             (plot-hist h1 0 1 0)
             (plot-hist hist 1 0 0))))

(with-graph ("/tmp/2d-offset.gif" :xmin -1.5 :xmax 1.5 :ymin -0.2 :ymax 1.25)
  (let ((h1 *hist3*))
    (loop with a1 = 0
          with a2 = 1
          with d = 1/32
          for i from a1 to a2 by d
          for hist = (make-instance 'hist)
          do (loop with *random-state* = (sb-ext:seed-random-state *seed*)
                   repeat 1000000
                   for h3 = (* 0.9649214285521897
                               (noise-range-test/improved:improved-noise3
                                (random 256.0) (random 256.0) i))
                   do (sample hist h3))
             (new-frame 5)
             (format t "draw ~s~%" i)
             (vecto:with-graphics-state
               (apply #'vecto:set-rgb-stroke *fg-color*)
               (vecto:translate 64 (- 512 128.5))
               (vecto:move-to 16 0)
               (vecto:line-to 48 0)
               (vecto:move-to 16 64)
               (vecto:line-to 48 64)
               (loop for y below 64 by 16
                     do (vecto:move-to 24 y)
                        (vecto:line-to 40 y))
               (loop for y below 64 by 8
                     do (vecto:move-to 28 y)
                        (vecto:line-to 36 y))
               (loop for y below 64 by 64/10
                     do (vecto:move-to 0 (round y))
                        (vecto:line-to 8 (round y)))
               (vecto:move-to 0 (* 64 i))
               (vecto:line-to 64 (* 64 i))
               (vecto:stroke)

)
             (plot-hist h1 0 1 0)
             (plot-hist hist 1 0 0))))

(with-graph ("/tmp/2d-rotated-0.gif" :xmin -1.5 :xmax 1.5 :ymin -0.2 :ymax 1.25)
  (let ((h1 *hist3*))
    (loop ;for i from -1/4 to 1/4 by 1/128 ;below 90 by 2
          with a1 = -1
          with a2 = 1
          with d = 1/64
          for i from a1 to a2 by d
          for hist = (make-instance 'hist)
          for m = (sb-cga:rotate-around (v3 1 0 0)
                                        (float (* i (/ pi 180)) 1.0))
          do (loop with *random-state* = (sb-ext:seed-random-state *seed*)
                   for v = (v3 (random 256.0) (random 256.0) 0)
                   for r = (sb-cga:transform-point v m)
                   for h3 = (* 0.9649214285521897
                               (noise-range-test/improved:improved-noise3
                                (aref r 0) (aref r 1) (aref r 2)))
                   do (sample hist h3))
             (new-frame 5)
             (format t "draw ~s~%" i)
             (vecto:with-graphics-state
               (apply #'vecto:set-rgb-stroke *fg-color*)
               (let ((v (sb-cga:transform-point (v3 0 1 0) m))
                     (c (sb-cga:transform-point (v3 0 0.1 0) m)))
                 #++(when (> (- a2 a1) 5)
                   (setf v (sb-cga:vec* v (/ d))))
                 (vecto:translate 64 (- 512 128))
                 (vecto:move-to (* 64 (aref c 1))
                                (* 64 (aref c 2)))
                 (vecto:line-to (* 64 (aref v 1))
                                (* 64 (aref v 2)))
                 (vecto:stroke)
)
)
             (plot-hist h1 0 1 0)
             (plot-hist hist 1 0 0))))

(with-graph ("/tmp/2d-rotated-45.gif" :xmin -1.5 :xmax 1.5 :ymin -0.2 :ymax 1.25)
  (let ((h1 *hist3*))
    (loop ;for i from -1/4 to 1/4 by 1/128 ;below 90 by 2
          with a1 = 44
          with a2 = 46
          with d = 1/64
          for i from a1 to a2 by d
          for hist = (make-instance 'hist)
          for m = (sb-cga:rotate-around (v3 1 0 0)
                                        (float (* i (/ pi 180)) 1.0))
          do (loop with *random-state* = (sb-ext:seed-random-state *seed*)
                   repeat 10000000
                   for v = (v3 (random 256.0) (random 256.0) 0)
                   for r = (sb-cga:transform-point v m)
                   for h3 = (* 0.9649214285521897
                               (noise-range-test/improved:improved-noise3
                                (aref r 0) (aref r 1) (aref r 2)))
                   do (sample hist h3))
             (new-frame 5)
             (format t "draw ~s~%" i)
             (vecto:with-graphics-state
               (apply #'vecto:set-rgb-stroke *fg-color*)
               (let ((v (sb-cga:transform-point (v3 0 1 0) m))
                     (c (sb-cga:transform-point (v3 0 0.1 0) m)))
                 #++(when (> (- a2 a1) 5)
                   (setf v (sb-cga:vec* v (/ d))))
                 (vecto:translate 64 (- 512 128))
                 (vecto:move-to (* 64 (aref c 1))
                                (* 64 (aref c 2)))
                 (vecto:line-to (* 64 (aref v 1))
                                (* 64 (aref v 2)))
                 (vecto:stroke)
)
)
             (plot-hist h1 0 1 0)
             (plot-hist hist 1 0 0))))


(with-png-graph ("/tmp/perlin-improved-unscaled.png"
                 :wx 800 :wy 400
                  :xmin -2 :xmax 2 :ymin -0.15 :ymax 1.2)
  (let ((hist2 (make-instance 'hist))
        (hist3 (make-instance 'hist))
        (hist4 (make-instance 'hist))
        (*plot-ofs* 0)
        (*random-state* (sb-ext:seed-random-state *seed*)))
   (time
    (loop repeat 100000000
          for x = (random 256.0)
          for y = (random 256.0)
          for z = (random 256.0)
          for w = (random 256.0)
          for h2 = (noise-range-test/improved:improved-noise2 x y)
          for h3 = (*                   ;0.9649214285521897
                    (noise-range-test/improved:improved-noise3 x y z))
          for h4 = (*                   ; 0.6507949348645372
                    (noise-range-test/improved:improved-noise4 x y z w))
          do (sample hist2 h2)
             (sample hist3 h3)
             (sample hist4 h4)))
    (with-open-file (s "/tmp/perlin-improved-unscaled.txt"
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
      (format s "seed = ~s~%" *seed*)
      (format s "samples ~s, ~s, ~s~%"
              (reduce '+ (samples hist2))
              (reduce '+ (samples hist3))
              (reduce '+ (samples hist4)))
      (format s "2d:~% ~s - ~s, mean ~s, dev ~s~%"
              (smin hist2) (smax hist2) (mean hist2) (stddev hist2))
      (format s "3d:~% ~s - ~s, mean ~s, dev ~s~%"
              (smin hist3) (smax hist3) (mean hist3) (stddev hist3))
      (format s "4d:~% ~s - ~s, mean ~s, dev ~s~%"
              (smin hist4) (smax hist4) (mean hist4) (stddev hist4)))
    (let ((*plot-hmax* (max (max-count hist2)
                            (max-count hist3)
                            (max-count hist4))))
      (setf *plot-ofs* 8)
      (plot-hist hist4 0.1 0.1 1)
      (setf *plot-ofs* 00)
      (plot-hist hist3 0 1 0)
      (setf *plot-ofs* -8)
      (plot-hist hist2 1 0 0))

   ))

(with-png-graph ("/tmp/perlin-improved-scaled.png"
                 :wx 800 :wy 400
                 :xmin -2 :xmax 2 :ymin -0.15 :ymax 1.2)
  (let ((hist2 (make-instance 'hist))
        (hist3 (make-instance 'hist))
        (hist4 (make-instance 'hist))
        (*plot-ofs* 0)
        (*random-state* (sb-ext:seed-random-state *seed*))
        (m (sb-cga:rotate-around (v3 1 0 0)
                                 (float (* 30 (/ pi 180)) 1.0))))
    (time
     (loop repeat 100000000
           for x = (random 256.0)
           for y = (random 256.0)
           for z = (random 256.0)
           for w = (random 256.0)
           ;;for h2 = (noise-range-test/improved:improved-noise2 x y)
           for r = (sb-cga:transform-point (v3 x y 0) m)
           for h2 = (* 0.9649214285521897
                       (noise-range-test/improved:improved-noise3
                      (aref r 0) (aref r 1) (aref r 2)))
           for h3 = (* 0.9649214285521897
                       (noise-range-test/improved:improved-noise3 x y z))
           for h4 = (*  0.6507949348645372
                        (noise-range-test/improved:improved-noise4 x y z w))
           do (sample hist2 h2)
              (sample hist3 h3)
              (sample hist4 h4)))
    (with-open-file (s "/tmp/perlin-improved-scaled.txt"
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
      (format s "seed = ~s~%" *seed*)
      (format s "samples ~s, ~s, ~s~%"
              (reduce '+ (samples hist2))
              (reduce '+ (samples hist3))
              (reduce '+ (samples hist4)))
      (format s "2d:~% ~s - ~s, mean ~s, dev ~s~%"
              (smin hist2) (smax hist2) (mean hist2) (stddev hist2))
      (format s "3d:~% ~s - ~s, mean ~s, dev ~s~%"
              (smin hist3) (smax hist3) (mean hist3) (stddev hist3))
      (format s "4d:~% ~s - ~s, mean ~s, dev ~s~%"
              (smin hist4) (smax hist4) (mean hist4) (stddev hist4)))
    (let ((*plot-hmax* (max (max-count hist2)
                            (max-count hist3)
                            (max-count hist4))))
      (setf *plot-ofs* 8)
      (plot-hist hist4 0.1 0.1 1)
      (setf *plot-ofs* 00)
      (plot-hist hist3 0 1 0)
      (setf *plot-ofs* -8)
      (plot-hist hist2 1 0 0))

    ))

(with-graph ("/tmp/4d-scaling.gif" :xmin -1.5 :xmax 1.5 :ymin -0.2 :ymax 1.25)
  (let* ((n 10000000)
         (samples (make-array n :element-type 'single-float
                                :initial-element 0.0))
         (h1 (make-instance 'hist)))
    (setf (aref samples 0) -1.0)
    (setf (aref samples 1) 1.0)
    (loop with *random-state* = (sb-ext:seed-random-state *seed*)
          for i from 2 below n
          for h = (* 0.6507949348645372
                      (noise-range-test/improved:improved-noise4
                       (random 256.0) (random 256.0)
                       (random 256.0) (random 256.0)))
          do (setf (aref samples i) h))
    (loop for i across samples
          do (sample h1 i))
    (loop with a1 = 0
          with a2 = 2
          with d = 1/32
          for i from a1 to a2 by d
          for hist = (make-instance 'hist)
          do (flet ((s (x)
                      (* (signum x) (- 1 (expt (- 1 (abs x)) i)))))
               (loop for i across samples
                     do (sample hist (s i))))
             (new-frame 5)
             (format t "draw ~s~%" i)
             (vecto:with-graphics-state
               (apply #'vecto:set-rgb-stroke *fg-color*)
               (vecto:translate 64 (- 512 128.5))
               (vecto:move-to 16 0)
               (vecto:line-to 48 0)
               (vecto:move-to 16 64)
               (vecto:line-to 48 64)
               (loop for y below 128 by 32
                     do (vecto:move-to 20 y)
                        (vecto:line-to 44 y))
               (loop for y below 128 by 16
                     do (vecto:move-to 24 y)
                        (vecto:line-to 40 y))
               (loop for y below 128 by 8
                     do (vecto:move-to 28 y)
                        (vecto:line-to 36 y))
               (loop for y upto 128 by 64/10
                     do (vecto:move-to 0 y)
                        (vecto:line-to 8 y))
               (vecto:move-to 0 (* 64 i))
               (vecto:line-to 64 (* 64 i))
               (vecto:stroke))

             (plot-hist h1 0 1 0)
             (plot-hist hist 1 0 0))))

(with-graph ("/tmp/4d-scaling-expt1.gif" :xmin -1.5 :xmax 1.5 :ymin -0.2 :ymax 1.25)
  (let* ((n 10000000)
         (samples (make-array n :element-type 'single-float
                                :initial-element 0.0))
         (h1 (make-instance 'hist)))
    (setf (aref samples 0) -1.0)
    (setf (aref samples 1) 1.0)
    (loop with *random-state* = (sb-ext:seed-random-state *seed*)
          for i from 2 below n
          for h = (* 0.6507949348645372
                      (noise-range-test/improved:improved-noise4
                       (random 256.0) (random 256.0)
                       (random 256.0) (random 256.0)))
          do (setf (aref samples i) h))
    (loop for i across samples
          do (sample h1 i))
    (loop with a1 = 0
          with a2 = 2
          with d = 1/32
          for i from a1 to a2 by d
          for hist = (make-instance 'hist)
          do (flet ((s (x)
                      (* (signum x) (expt (abs x) i))))
               (loop for i across samples
                     do (sample hist (s i))))
             (new-frame 5)
             (format t "draw ~s~%" i)
             (vecto:with-graphics-state
               (apply #'vecto:set-rgb-stroke *fg-color*)
               (vecto:translate 64 (- 512 128.5))
               (vecto:move-to 16 0)
               (vecto:line-to 48 0)
               (vecto:move-to 16 64)
               (vecto:line-to 48 64)
               (loop for y below 128 by 32
                     do (vecto:move-to 20 y)
                        (vecto:line-to 44 y))
               (loop for y below 128 by 16
                     do (vecto:move-to 24 y)
                        (vecto:line-to 40 y))
               (loop for y below 128 by 8
                     do (vecto:move-to 28 y)
                        (vecto:line-to 36 y))
               (loop for y upto 128 by 64/10
                     do (vecto:move-to 0 y)
                        (vecto:line-to 8 y))
               (vecto:move-to 0 (* 64 i))
               (vecto:line-to 64 (* 64 i))
               (vecto:stroke))

             (plot-hist h1 0 1 0)
             (plot-hist hist 1 0 0))))
(/ 1.0363535)0.9649218
(/ 1.536354)0.65089166

(defmethod int:sample ((sampler mod:expt) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (let ((sample (the u:f32 (int:sample (source sampler) x y z w))))))


(with-graph ("/tmp/4d-scaling-expt.gif" :xmin -1.5 :xmax 1.5 :ymin -0.2 :ymax 1.25)
  (let* ((n 10000000)
         (samples (make-array n :element-type 'single-float
                                :initial-element 0.0))
         (h1 (make-instance 'hist)))
    (setf (aref samples 0) -1.0)
    (setf (aref samples 1) 1.0)
    (loop with *random-state* = (sb-ext:seed-random-state *seed*)
          for i from 2 below n
          for h = (* 0.6507949348645372
                      (noise-range-test/improved:improved-noise4
                       (random 256.0) (random 256.0)
                       (random 256.0) (random 256.0)))
          do (setf (aref samples i) h))
    (loop for i across samples
          do (sample h1 i))
    (loop with a1 = 0
          with a2 = 2
          with d = 1/32
          for i from a1 to a2 by d
          for hist = (make-instance 'hist)
          do (flet ((s (x)
                      (1- (* (expt (abs (* (1+ x) 0.5)) i) 2))))
               (loop for i across samples
                     do (sample hist (s i))))
             (new-frame 5)
             (format t "draw ~s~%" i)
             (vecto:with-graphics-state
               (apply #'vecto:set-rgb-stroke *fg-color*)
               (vecto:translate 64 (- 512 128.5))
               (vecto:move-to 16 0)
               (vecto:line-to 48 0)
               (vecto:move-to 16 64)
               (vecto:line-to 48 64)
               (loop for y below 128 by 32
                     do (vecto:move-to 20 y)
                        (vecto:line-to 44 y))
               (loop for y below 128 by 16
                     do (vecto:move-to 24 y)
                        (vecto:line-to 40 y))
               (loop for y below 128 by 8
                     do (vecto:move-to 28 y)
                        (vecto:line-to 36 y))
               (loop for y upto 128 by 64/10
                     do (vecto:move-to 0 y)
                        (vecto:line-to 8 y))
               (vecto:move-to 0 (* 64 i))
               (vecto:line-to 64 (* 64 i))
               (vecto:stroke))

             (plot-hist h1 0 1 0)
             (plot-hist hist 1 0 0))))

(with-png-graph ("/tmp/perlin-improved-scaled-gamma.png"
                 :wx 800 :wy 400
                 :xmin -2 :xmax 2 :ymin -0.15 :ymax 1.2)
  (let ((hist2 (make-instance 'hist))
        (hist3 (make-instance 'hist))
        (hist4 (make-instance 'hist))
        (*plot-ofs* 0)
        (*random-state* (sb-ext:seed-random-state *seed*))
        (m (sb-cga:rotate-around (v3 1 0 0)
                                 (float (* 30 (/ pi 180)) 1.0))))
    (flet ((s (x)
             (* (signum x) (- 1 (expt (- 1 (abs x)) 1.464)))))
      (time
       (loop repeat 100000000
             for x = (random 256.0)
             for y = (random 256.0)
             for z = (random 256.0)
             for w = (random 256.0)
             ;;for h2 = (noise-range-test/improved:improved-noise2 x y)
             for r = (sb-cga:transform-point (v3 x y 0) m)
             for h2 = (* 0.9649214285521897
                         (noise-range-test/improved:improved-noise3
                          (aref r 0) (aref r 1) (aref r 2)))
             for h3 = (* 0.9649214285521897
                         (noise-range-test/improved:improved-noise3 x y z))
             for h4 = (*  0.6507949348645372
                          (noise-range-test/improved:improved-noise4 x y z w))
             do (sample hist2 h2)
                (sample hist3 h3)
                (sample hist4 (s h4)))))
    (with-open-file (s "/tmp/perlin-improved-scaled-gamma.txt"
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
      (format s "seed = ~s~%" *seed*)
      (format s "samples ~s, ~s, ~s~%"
              (reduce '+ (samples hist2))
              (reduce '+ (samples hist3))
              (reduce '+ (samples hist4)))
      (format s "2d:~% ~s - ~s, mean ~s, dev ~s~%"
              (smin hist2) (smax hist2) (mean hist2) (stddev hist2))
      (format s "3d:~% ~s - ~s, mean ~s, dev ~s~%"
              (smin hist3) (smax hist3) (mean hist3) (stddev hist3))
      (format s "4d:~% ~s - ~s, mean ~s, dev ~s~%"
              (smin hist4) (smax hist4) (mean hist4) (stddev hist4))
)
    (let ((*plot-hmax* (max (max-count hist2)
                            (max-count hist3)
                            (max-count hist4))))
      (setf *plot-ofs* 8)
      (plot-hist hist4 0.1 0.1 1)
      (setf *plot-ofs* 00)
      (plot-hist hist3 0 1 0)
      (setf *plot-ofs* -8)
      (plot-hist hist2 1 0 0))

    ))

(with-png-graph ("/tmp/2d-test.png"
                 :wx 800 :wy 400
                 :xmin -2 :xmax 2 :ymin -0.15 :ymax 1.2)
  (let ((hist2 (make-instance 'hist))
        (hist3 (make-instance 'hist))
        (hist4 (make-instance 'hist))
        (*plot-ofs* 0)
        (*random-state* (sb-ext:seed-random-state *seed*))
        (m (sb-cga:rotate-around (v3 1 0 0)
                                 (float (* 30 (/ pi 180)) 1.0))))
    (flet ((s (x)
             (* (signum x) (- 1 (expt (- 1 (abs x)) 1.464)))))
      (time
       (loop repeat 1000000
             for x = (random 256.0)
             for y = (random 256.0)
             for z = (random 256.0)
             for w = (random 256.0)
             ;;for h2 = (noise-range-test/improved:improved-noise2 x y)
             for r = (sb-cga:transform-point (v3 x y 0) m)
             for h2 = (* 0.9649214285521897
                         (noise-range-test/improved:improved-noise3 x y 0))
             for h3 = (* 0.6507949348645372
                         (noise-range-test/improved:improved-noise4 x y 0 0))
             for h4 = (* 0.6507949348645372
                         (noise-range-test/improved:improved-noise4 x y z 0))
             do (sample hist2 h2)
                (sample hist3 h3)
                (sample hist4 (s h4)))))
    (with-open-file (s "/tmp/2d-test.txt"
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (format s "seed = ~s~%" *seed*)
      (format s "samples ~s, ~s, ~s~%"
              (reduce '+ (samples hist2))
              (reduce '+ (samples hist3))
              (reduce '+ (samples hist4)))
      (format s "2d:~% ~s - ~s, mean ~s, dev ~s~%"
              (smin hist2) (smax hist2) (mean hist2) (stddev hist2))
      (format s "3d:~% ~s - ~s, mean ~s, dev ~s~%"
              (smin hist3) (smax hist3) (mean hist3) (stddev hist3))
      (format s "4d:~% ~s - ~s, mean ~s, dev ~s~%"
              (smin hist4) (smax hist4) (mean hist4) (stddev hist4))
      )
    (let ((*plot-hmax* (max (max-count hist2)
                            (max-count hist3)
                            (max-count hist4))))
      (setf *plot-ofs* 8)
      (plot-hist hist4 0.1 0.1 1)
      (setf *plot-ofs* 00)
      (plot-hist hist3 0 1 0)
      (setf *plot-ofs* -8)
      (plot-hist hist2 1 0 0))

    ))
∴
