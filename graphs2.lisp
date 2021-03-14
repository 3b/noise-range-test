#++ (ql:quickload '(vecto skippy zpng sb-cga lparallel))
(in-package noise-range-test)

#++
(with-graph ("/tmp/k0-32-single.gif" :xmin -1.5 :xmax 1.5 :ymin -1.5 :ymax 1.5)
  (loop for k from 0d0 to 32 by 1/8
        do (new-frame 5)
           (format t "draw ~s~%" k)
           (vecto:with-graphics-state
             (apply #'vecto:set-rgb-stroke *fg-color*)
             (vecto:set-rgb-stroke 1 0 0)
             (move-to -1 -1)
             (loop for i from -1d0 to 1.0 by 1/32
                   do (line-to i (reshape i k)))
             (vecto:stroke))))

(flet ((s (x &optional (e 1.464))
         (* (signum x) (- 1 (expt (- 1 (abs x)) e)))))
  (with-graph ("/tmp/expt0-16.gif" :xmin -1.5 :xmax 1.5 :ymin -1.5 :ymax 1.5)
    (loop for k from 0.125 to 16 by 1/8
          do (new-frame 5)
             (format t "draw ~s~%" k)
             (vecto:with-graphics-state
               (apply #'vecto:set-rgb-stroke *fg-color*)
               (vecto:set-rgb-stroke 1 0 0)
               (move-to -1 -1)
               (loop for i from -1d0 to 1.0 by 1/32
                     do (line-to i (s i k)))
               (vecto:stroke))))
  )

(with-graph ("/tmp/k0-32.gif" :xmin -1.5 :xmax 1.5 :ymin -1.5 :ymax 1.5)
  (loop for k from 0d0 to 32 by 1/8
        do (new-frame 5)
           (format t "draw ~s~%" k)
           (vecto:with-graphics-state
             (apply #'vecto:set-rgb-stroke *fg-color*)
             (vecto:set-rgb-stroke 1 0 0)
             (move-to -1 -1)
             (loop for i from -1d0 to 1.0 by 1/32
                   do (line-to i (reshape i k)))
             (vecto:stroke))))

(with-png-graph ("/tmp/reshaping-before.png"
                 :wx 800 :wy 400
                 :xmin -2 :xmax 2 :ymin -0.15 :ymax 1.2)
  (let ((hist1 (make-instance 'hist))
        (hist2 (make-instance 'hist))
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
             for h1 = (n3 x y z)
             for h2 = (n4 x y z w)
             for f1 = (fbm4 x y z w 0.0 2.0 8 0.5)
             for f2 = (fbm4 x y z w 0.0 2.0 8 0.8)
             do (sample hist1 h1)
                (sample hist2 h2)
                (sample hist3 f1)
                (sample hist4 f2))))
    (with-open-file (s "/tmp/reshaping-before.txt"
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
      (format s "seed = ~s~%" *seed*)
      (format s "samples ~s, ~s, ~s~%"
              (reduce '+ (samples hist2))
              (reduce '+ (samples hist3))
              (reduce '+ (samples hist4)))
      (format s "3d:~% ~s - ~s, mean ~s, dev ~s~%"
              (smin hist1) (smax hist1) (mean hist1) (stddev hist1))
      (format s "4d:~% ~s - ~s, mean ~s, dev ~s~%"
              (smin hist2) (smax hist2) (mean hist2) (stddev hist2))
      (format s "fbm 0.5:~% ~s - ~s, mean ~s, dev ~s~%"
              (smin hist3) (smax hist3) (mean hist3) (stddev hist3))
      (format s "fbm 0.8:~% ~s - ~s, mean ~s, dev ~s~%"
              (smin hist4) (smax hist4) (mean hist4) (stddev hist4)))
    (let ((*plot-hmax* (max (max-count hist1)
                            (max-count hist2)
                            (max-count hist3)
                            (max-count hist4))))
      (setf *plot-ofs* 8)
      (plot-hist hist4 1 0 1)
      (setf *plot-ofs* 8)
      (plot-hist hist3 0.1 0.1 1)
      (setf *plot-ofs* 00)
      (plot-hist hist2 0 1 0)
      (setf *plot-ofs* -8)
      (plot-hist hist1 1 0 0))))



(with-png-graph ("/tmp/reshaping-after.png"
                 :wx 800 :wy 400
                 :xmin -2 :xmax 2 :ymin -0.15 :ymax 1.2)
  (let ((hist1 (make-instance 'hist))
        (hist2 (make-instance 'hist))
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
             for h1 = (n3 x y z)
             for h2 = (n4 x y z w)
             for f1 = (fbm4 x y z w 0.0 2.0 8 0.5)
             for f2 = (fbm4 x y z w 0.0 2.0 8 0.8)
             do (sample hist1 h1)
                (sample hist2 (reshape h2 0.795))
                (sample hist3 (reshape f1 3.95))
                (sample hist4 (reshape f2 9.535)))))
    (with-open-file (s "/tmp/reshaping-after.txt"
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
      (format s "seed = ~s~%" *seed*)
      (format s "samples ~s, ~s, ~s~%"
              (reduce '+ (samples hist2))
              (reduce '+ (samples hist3))
              (reduce '+ (samples hist4)))
      (format s "3d:~% ~s - ~s, mean ~s, dev ~s~%"
              (smin hist1) (smax hist1) (mean hist1) (stddev hist1))
      (format s "4d:~% ~s - ~s, mean ~s, dev ~s~%"
              (smin hist2) (smax hist2) (mean hist2) (stddev hist2))
      (format s "fbm 0.5:~% ~s - ~s, mean ~s, dev ~s~%"
              (smin hist3) (smax hist3) (mean hist3) (stddev hist3))
      (format s "fbm 0.8:~% ~s - ~s, mean ~s, dev ~s~%"
              (smin hist4) (smax hist4) (mean hist4) (stddev hist4))
)
    (let ((*plot-hmax* (max (max-count hist1)
                            (max-count hist2)
                            (max-count hist3)
                            (max-count hist4))))
      (setf *plot-ofs* 8)
      (plot-hist hist4 1 0 1)
      (setf *plot-ofs* 8)
      (plot-hist hist3 0.1 0.1 1)
      (setf *plot-ofs* 00)
      (plot-hist hist2 0 1 0)
      (setf *plot-ofs* -8)
      (plot-hist hist1 1 0 0))))

(let ((hist (vector (make-instance 'hist :nsamples 254)
                    (make-instance 'hist :nsamples 254)
                    (make-instance 'hist :nsamples 254)))
      (wx 256)
      (wy 256))
  (with-noise-png ("/tmp/reshape1.png"
                   :scale 5.0 :wx wx :wy wy)
    :pixel
    (values
     (sample (aref hist 0) (n3 x y z))
     (sample (aref hist 1) (n4 x y z w))
     (sample (aref hist 2) (fbm4 x y z w 0.0 2.0 8 0.5)))
    :extra
    (vecto:with-graphics-state
      (vecto:set-rgb-stroke 1 0 0)
      (vecto:translate (* n wx) 0)
      (loop with h = (reduce 'max (map 'list 'max-count hist))
           for i from 0 below wx
           for s across (samples (aref hist n))
           do (if (= i 0)
                  (vecto:move-to i (* s (/ (1- wy) h)))
                  (vecto:line-to i (* s (/ (1- wy) h)))))
      (vecto:stroke))))

(flet ((s (x &optional (e 1.464))
         (* (signum x) (- 1 (expt (- 1 (abs x)) e)))))
 (loop for i below 10000000
       for x = (random 256.0)
       for y = (random 256.0)
       for z = (random 256.0)
       for w = (random 256.0)
       for h = #++(n3 x y z)
                  (s (n4 x y z w) 1.464)
       #++(s (fbm4 x y z w 0.0 2.0 8 0.5) 2.72)
       count h into s0
       sum h into s1 double-float
       sum (* h h) into s2 double-float
       when (and (> s0 2)
                 (zerop (mod i 100000)))
         do (format t "~s: ~s~%" i
                    (coerce
                     (/ (sqrt (- (* s0 s2)
                                 (expt s1 2)))
                        s0)
                     'single-float))))

(flet ((s (x &optional (e 1.464))
         (* (signum x) (- 1 (expt (- 1 (abs x)) e)))))
 (let ((hist (vector (make-instance 'hist :nsamples 254)
                     (make-instance 'hist :nsamples 254)
                     (make-instance 'hist :nsamples 254)))
       (wx 256)
       (wy 256))
   (with-noise-png ("/tmp/reshape2.png"
                    :scale 5.0 :wx wx :wy wy)
     :pixel
     (values
      (sample (aref hist 0) (n3 x y z))
      (sample (aref hist 1) (s (n4 x y z w)))
      (sample (aref hist 2) (s (fbm4 x y z w 0.0 2.0 8 0.5) 2.72)))
     :extra
     (vecto:with-graphics-state
      (format t "dev = ~s~%" (map 'vector 'stddev hist))
       (vecto:set-rgb-stroke 1 0 0)
       (vecto:translate (* n wx) 0)
       (loop with h = (reduce 'max (map 'list 'max-count hist))
             for i from 0 below wx
             for s across (samples (aref hist n))
             do (if (= i 0)
                    (vecto:move-to i (* s (/ (1- wy) h)))
                    (vecto:line-to i (* s (/ (1- wy) h)))))
       (vecto:stroke)))))


(loop for i below 10000000
      for x = (random 256.0)
      for y = (random 256.0)
      for z = (random 256.0)
      for w = (random 256.0)
      for h = #++(n3 x y z)
                 #++(reshape (n4 x y z w) 0.79)
                    (reshape (fbm4 x y z w 0.0 2.0 8 0.5) 3.95)
      count h into s0
      sum h into s1 double-float
      sum (* h h) into s2 double-float
      when (and (> s0 2)
                (zerop (mod i 100000)))
        do (format t "~s: ~s~%" i
                   (coerce
                    (/ (sqrt (- (* s0 s2)
                                (expt s1 2)))
                       s0)
                    'single-float)))
(let ((hist (vector (make-instance 'hist :nsamples 254)
                    (make-instance 'hist :nsamples 254)
                    (make-instance 'hist :nsamples 254)))
      (wx 256)
      (wy 256))

  (with-noise-png ("/tmp/reshape2p.png"
                   :scale 5.0 :wx wx :wy wy)
    :pixel
    (values
     (sample (aref hist 0) (n3 x y z))
     (sample (aref hist 1) (reshape (n4 x y z w) 0.79))
     (sample (aref hist 2) (reshape (fbm4 x y z w 0.0 2.0 8 0.5) 3.95)))
    :extra
    (vecto:with-graphics-state
      (format t "dev = ~s~%" (map 'vector 'stddev hist))
      (vecto:set-rgb-stroke 1 0 0)
      (vecto:translate (* n wx) 0)
      (loop with h = (reduce 'max (map 'list 'max-count hist))
            for i from 0 below wx
            for s across (samples (aref hist n))
            do (if (= i 0)
                   (vecto:move-to i (* s (/ (1- wy) h)))
                   (vecto:line-to i (* s (/ (1- wy) h)))))
      (vecto:stroke))))

(flet ((s (x &optional (e 1.464))
         (* (signum x) (- 1 (expt (- 1 (abs x)) e)))))
 (let ((hist (vector (make-instance 'hist :nsamples 254)
                     (make-instance 'hist :nsamples 254)
                     (make-instance 'hist :nsamples 254)))
       (wx 256)
       (wy 256))
   (with-noise-png ("/tmp/reshape3.png"
                    :scale 5.0 :wx wx :wy wy)
     :pixel
     (values
      (sample (aref hist 0) (n3 x y z))
      (sample (aref hist 1) (s (fbm4 x y z w 0.0 2.0 10 0.9) 1))
      (sample (aref hist 2) (s (fbm4 x y z w 0.0 2.0 10 0.9) 5.5)))
     :extra
     (vecto:with-graphics-state
       (vecto:set-rgb-stroke 1 0 0)
       (vecto:translate (* n wx) 0)
       (loop with h = (reduce 'max (map 'list 'max-count hist))
             for i from 0 below wx
             for s across (samples (aref hist n))
             do (if (= i 0)
                    (vecto:move-to i (* s (/ (1- wy) h)))
                    (vecto:line-to i (* s (/ (1- wy) h)))))
       (vecto:stroke)))))


(flet ((s (x &optional (e 1.464))
         (* (signum x) (- 1 (expt (- 1 (abs x)) e)))))
 (let* ((n 256)
        (hist (vector (make-instance 'hist :nsamples (- n 2))
                         (make-instance 'hist :nsamples (- n 2))
                         (make-instance 'hist :nsamples (- n 2))))
       (wx n)
       (wy n))
   (with-noise-png ("/tmp/reshape4.png"
                    :scale 10.0 :wx wx :wy wy)
     :pixel
     (values
      (sample (aref hist 0) (s (n4 x y z w)))
      (sample (aref hist 1) (s (n4 x y z w) 4))
      (sample (aref hist 2) (s (n4 x y z w) 8)))
     :extra
     (vecto:with-graphics-state
       (vecto:set-rgb-stroke 1 0 0)
       (vecto:translate (* n wx) 0)
       (loop with h = (reduce 'max (map 'list 'max-count hist))
             for i from 0 below wx
             for s across (samples (aref hist n))
             do (if (= i 0)
                    (vecto:move-to i (* s (/ (1- wy) h)))
                    (vecto:line-to i (* s (/ (1- wy) h)))))
       (vecto:stroke)))))


(flet ((s (x &optional (e 1.464))
         (* (signum x) (- 1 (expt (- 1 (abs x)) e)))))
  (let ((a (make-array 1000000 :element-type 'single-float :initial-element 0.0)))
    (map-into a
              (lambda ()
                (let ((x (random 256.0))
                      (y (random 256.0))
                      (z (random 256.0))
                      (w (random 256.0)))
                  (n4 x y z w))))

    (with-graph ("/tmp/reshape-n4.gif" :xmin -1.5 :xmax 1.5 :ymin -0.2 :ymax 1.25)
      (let* ((hists (time
                     (loop for k from 0d0 to 10 by 1/8
                           for hist = (make-instance 'hist)
                           do (loop for i across a do (sample hist (s i k)))
                           collect hist)))
             ;; k=0 has everything in 1 bucket, so just pick a more
             ;; reasonable max
             (max (/ (length a) 200)
                  #++(reduce 'max (cdddr (map 'list 'max-count hists)))))

        (loop with *plot-hmax* = max
              for i in hists
              for k from 0
              do (new-frame 5)
                 (format t "draw ~s ~s/~s~%" k (max-count i) max)
                 (plot-hist i 1 0 0))
        ))))

(let ((a (make-array 1000000 :element-type 'single-float :initial-element 0.0)))
  (map-into a
            (lambda ()
              (let ((x (random 256.0))
                    (y (random 256.0))
                    (z (random 256.0))
                    (w (random 256.0)))
                (n4 x y z w))))

  (with-graph ("/tmp/reshape-n4r.gif" :xmin -1.5 :xmax 1.5 :ymin -0.2 :ymax 1.25)
    (let* ((hists (time
                   (loop for k from 0d0 to 20 by 1/8
                         for hist = (make-instance 'hist)
                         do (loop for i across a do (sample hist (reshape i k)))
                         collect hist)))
           (max (reduce 'max (map 'list 'max-count hists))))
      (loop with *plot-hmax* = max
            for i in hists
            for k from 0
            do (new-frame 5)
               (format t "draw ~s ~s/~s~%" k (max-count i) max)
               (plot-hist i 1 0 0)))))


(defvar *devs* (make-array (list 100 2 2) :element-type 'single-float
                           :initial-element 0.0))
(flet ((s (x &optional (e 1.464))
         (* (signum x) (- 1 (expt (- 1 (abs x)) e)))))
 
 (let ((a (make-array 1000000 :element-type 'single-float :initial-element 0.0)))
   (map-into a
             (lambda ()
               (let ((x (random 256.0))
                     (y (random 256.0))
                     (z (random 256.0))
                     (w (random 256.0)))
                 (n4 x y z w))))
   (loop for k from 0.0 to 20.0 by (/ 20 100)
         for i from 0
         for d = (loop for s across a
                       for r1 = (s s k)
                       sum r1 into s1 double-float
                       sum (* r1 r1) into s2 double-float
                       finally (return (coerce
                                        (/ (sqrt (- (* (length a) s2)
                                                    (expt s1 2)))
                                           (length a))
                                        'single-float)))
         do (setf (aref *devs* i 0 0) d)
            (setf (aref *devs* i 0 1) k))
   (loop for k from 0.0 to 32.0 by (/ 32 100)
         for i from 0 below (array-dimension *devs* 0)
         for d = (loop for s across a
                       for r1 = (reshape s k)
                       sum r1 into s1 double-float
                       sum (* r1 r1) into s2 double-float
                       finally (return (coerce
                                        (/ (sqrt (- (* (length a) s2)
                                                    (expt s1 2)))
                                           (length a))
                                        'single-float)))
         do (setf (aref *devs* i 1 0) d)
            (setf (aref *devs* i 1 1) k))))

(defun find-dev (x)
  (loop with k1 = (if (< x (aref *devs* 0 0 0)) 0 nil)
        with k2 = (if (< x (aref *devs* 0 1 0)) 0 nil)
        for i below 99
        for a1 = (aref *devs* i 0 0)
        for b1 = (aref *devs* (1+ i) 0 0)
        for a2 = (aref *devs* i 1 0)
        for b2 = (aref *devs* (1+ i) 1 0)
        when (<= a1 x b1)
          do (let ((v (/ (- x a1)
                         (- b1 a1))))
               (setf k1 (a:lerp v
                                (aref *devs* i 0 1)
                                (aref *devs* (1+ i) 0 1))))
        when (<= a2 x b2)
          do (let ((v (/ (- x a2)
                         (- b2 a2))))
               (setf k2 (a:lerp v
                                (aref *devs* i 1 1)
                                (aref *devs* (1+ i) 1 1)))
               (format t "~s ~s ~s = ~s ~s~%" a2 x b2 v k2))
        finally (return (values k1 k2))))
#++
(loop for i below 99
      collect (aref *devs* i 1 0))
(find-dev 0.19)
(flet ((s (x &optional (e 1.464))
         (* (signum x) (- 1 (expt (- 1 (abs x)) e)))))
  (let ((n 256))
    (let ((a (make-array (list n n)
                         :element-type 'single-float :initial-element 0.0)))
      (loop for j below n
            do (loop for i below n
                     do (setf (aref a i j)
                              (n4 (* i (/ 4.0 n))
                                  (* j (/ 4.0 n))
                                  0.234
                                  0.321))))

      (with-gray-gif ("/tmp/reshape-compare.gif" img :wx (* 2 n) :wy n)
        (loop for dev from 0.19 below 0.71 by 0.005
              for (k1 k2) = (multiple-value-list (find-dev dev))
              do (new-frame 2)
                 (format t "dev ~s = ~s, ~s~%" dev k1 k2)
                 (loop with id = (skippy:image-data img)
                       for j below n 
                       do (loop for i below n
                                for h1 = (s (aref a j i) k1)
                                for h2 = (reshape (aref a j i) k2)
                                do (setf (aref id (+ i (* j n 2)))
                                         (floor (* (1+ h1) 1/2 255)))
                                   (setf (aref id (+ i n (* j n 2)))
                                         (floor (* (1+ h2) 1/2 255)))
                                ))
              )
        ))))



(flet ((s (x &optional (e 1.464))
         (* (signum x) (- 1 (expt (- 1 (abs x)) e)))))
  (let ((n 256))
    (let ((a (make-array 1000000 :element-type 'single-float
                                 :initial-element 0.0)))
      (map-into a
                (lambda ()
                  (let ((x (random 256.0))
                        (y (random 256.0))
                        (z (random 256.0))
                        (w (random 256.0)))
                    (n4 x y z w))))
      (with-graph ("/tmp/reshape-compare-hist.gif"
                   :xmin -1.5 :xmax 1.5 :ymin -0.2 :ymax 1.25)
        (loop for dev from 0.19 below 0.71 by 0.01
              for (k1 k2) = (multiple-value-list (find-dev dev))
              for hist1 = (make-instance 'hist)
              for hist2 = (make-instance 'hist)
              do (new-frame 4)
                 (loop for i across a
                       do (sample hist1 (s i k1))
                          (sample hist2 (reshape i k2)))
                 (format t "dev ~s = ~s, ~s @ ~s ~s~%" dev k1 k2
                         (stddev hist1) (stddev hist2))
                 (format t " ~s ~s~%"
                         (max-count hist1) (max-count hist2))
                 (let ((*plot-hmax* 4000))
                   (plot-hist hist1 1 0 0)
                   (plot-hist hist2 0 1 0))
                 
              )
        ))))



