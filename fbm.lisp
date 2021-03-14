(in-package noise-range-test)

(declaim (inline n3))
(defun n3  (x y z)
  (* 0.9649214285521897 (improved-noise3 x y z)))

(defun fbm3 (x0 y0 z0 ofs mult oct oscale)
  (declare (optimize speed)
           (type single-float x0 y0 z0 ofs mult oscale)
           (type (mod 16) oct))
  (let ((s (/ (loop for i fixnum below oct
                    for m single-float = 1.0 then (* m oscale)
                    sum (* m 1.0) single-float))))
    (* s
     (loop for i fixnum below oct
           for x single-float = x0 then (* (+ x ofs) mult)
           for y single-float = y0 then (* (+ y ofs) mult)
           for z single-float = z0 then (* (+ z ofs) mult)
           for m single-float = 1.0 then (* m oscale)
           sum (* m (* 0.9649214285521897 (improved-noise3 x y z))) single-float))))

(declaim (inline n4 n4g))
(defun n4  (x y z w)
  (* 0.6507949348645372 (improved-noise4 x y z w)))
(defun n4g (x y z w)
  (flet ((s (x)
              (declare (type (single-float -1.0 1.0) x))
              (* (signum x) (- 1 (expt (- 1 (abs x)) 1.464)))))
    (s (* 0.6507949348645372 (improved-noise4 x y z w)))))

(defun fbm4x (x0 y0 z0 w0 ofs mult oct oscale)
  (declare (optimize speed)
           (type single-float x0 y0 z0 w0 ofs mult oscale)
           (type (mod 16) oct))
  (let ((s (/ (loop for i fixnum below oct
                    for m single-float = 1.0 then (* m oscale)
                    sum (* m 1.0) single-float))))
    (* s
       (loop for i fixnum below oct
          for x single-float = x0 then (* (+ x ofs) mult)
          for y single-float = y0 then (* (+ y ofs) mult)
          for z single-float = z0 then (* (+ z ofs) mult)
          for w single-float = w0 then (* (+ w ofs) mult)
          for m single-float = 1.0 then (* m oscale)
          sum (* m (improved-noise4 x y z w)) single-float))))

(defun fbm4 (x0 y0 z0 w0 ofs mult oct oscale)
  (declare (optimize speed)
           (type single-float x0 y0 z0 w0 ofs mult oscale)
           (type (mod 16) oct))
  (let ((s (/ (loop for i fixnum below oct
                    for m single-float = 1.0 then (* m oscale)
                    sum (* m 1.0) single-float))))
    (* s
       (loop for i fixnum below oct
          for x single-float = x0 then (* (+ x ofs) mult)
          for y single-float = y0 then (* (+ y ofs) mult)
          for z single-float = z0 then (* (+ z ofs) mult)
          for w single-float = w0 then (* (+ w ofs) mult)
          for m single-float = 1.0 then (* m oscale)
          sum (* m (n4 x y z w)) single-float))))


(defun fbm4g (x0 y0 z0 w0 ofs mult oct oscale)
  (declare (optimize speed)
           (type single-float x0 y0 z0 w0 ofs mult oscale)
           (type (mod 16) oct))
  (let ((s (/ (loop for i fixnum below oct
                    for m single-float = 1.0 then (* m oscale)
                    sum (* m 1.0) single-float))))
    (*
     (loop for i fixnum below oct
           for x single-float = x0 then (* (+ x ofs) mult)
           for y single-float = y0 then (* (+ y ofs) mult)
           for z single-float = z0 then (* (+ z ofs) mult)
           for w single-float = w0 then (* (+ w ofs) mult)
           for m single-float = 1.0 then (* m oscale)
           sum (* m (n4g x y z w))
           single-float)
     s)))

#++
(with-png-graph ("/tmp/fbm3a.png"
                 :wx 800 :wy 400
                 :xmin -1.25 :xmax 1.25 :ymin -0.15 :ymax 1.2)
  (let ((hist2 (make-instance 'hist))
        (hist3 (make-instance 'hist))
        (hist4 (make-instance 'hist))
        (hist5 (make-instance 'hist))
        (*plot-ofs* 0)
        (*random-state* (sb-ext:seed-random-state *seed*)))
    (time
     (loop with 2+ = (sqrt 13/3)
           repeat 1000000
           for x = (random 256.0)
           for y = (random 256.0)
           for z = (random 256.0)
           for w = (random 256.0)
           for h2 = (n4 x y z w )
           for h3 = (n4g x y z w )
           for h4 = (fbm4 x y z w 0.0 2.0 8 0.5)
           for h5 = (fbm4g x y z w 0.0 2.0 8 0.5)
           do (sample hist2 h2)
              (sample hist3 h3)
              (sample hist4 h4)
              (sample hist5 h5)
           ))
    (format t "~s -> ~s~%~s -> ~s~%"
            (stddev hist2) (stddev hist4)
            (stddev hist3) (stddev hist5)
            )
    (let ((*plot-hmax* (max (max-count hist2)
                            (max-count hist3)
                            (max-count hist4))))
      (setf *plot-ofs* 8)
      (plot-hist hist4 0.1 0.1 1)
      (setf *plot-ofs* 00)
      (plot-hist hist3 0 1 0)
      (setf *plot-ofs* -8)
      (plot-hist hist2 1 0 0)
      (setf *plot-ofs* -16)
      (plot-hist hist5 1 0 1))

    ))


