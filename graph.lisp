#++ (ql:quickload '(vecto skippy zpng sb-cga lparallel))
(in-package noise-range-test)

(defclass graph ()
  ((xmin :initform -1 :initarg :xmin :reader xmin)
   (xmax :initform 1 :initarg :xmax :reader xmax)
   (ymin :initform -1 :initarg :ymin :reader ymin)
   (ymax :initform 1 :initarg :ymax :reader ymax)
   (wx :initform 512 :initarg :wx :reader wx)
   (wy :initform 512 :initarg :wy :reader wy)
   (pad :initform 6 :initarg :pad :reader pad)))

(defvar *bg-dark* #x0d1117) ;; 13/255, 17/255 23/255
(defvar *bg-light* #xfafbfc) ;; 250/255 251/255 252/255

(defvar *bg-color* '(13/255 17/255 23/255))
(defvar *fg-color* '(250/255 251/255 252/255))

(defvar *tick-len* 6)
(defun draw-axes (graph)
  (apply #'vecto:set-rgb-fill *bg-color*)
  (vecto:clear-canvas)
  (apply #'vecto:set-rgb-stroke *fg-color*)
  (let* ((wx (- (wx graph) (* 2 (pad graph))))
         (wy (- (wy graph) (* 2 (pad graph))))
         (xmin (xmin graph))
         (xmax (xmax graph))
         (ymin (ymin graph))
         (ymax (ymax graph))
         (pad (pad graph))
         (sx (/ wx (- xmax xmin)))
         (sy (/ wy (- ymax ymin)))
         (x0 (+ 0.5 (round (* sx (- 0 xmin)))))
         (y0 (+ 0.5 (round (* sy (- 0 ymin)))))
         ;(x1 (round (* sx xmin)))
         ;(y1 (round (* sy ymin)))
         ;(x2 (round (* sx xmax)))
         ;(y2 (round (* sy ymax)))
         )
    (vecto:with-graphics-state
      (apply #'vecto:set-rgb-stroke *fg-color*)
      (vecto:set-line-width 1)
      (vecto:translate pad pad)
      (when (< 0 x0 wx)
        (vecto:move-to x0 0)
        (vecto:line-to x0 wy)
        (flet ((tick (y)
                 (let ((d (* *tick-len* (if (zerop (mod y 1)) 2 1))))
                   (vecto:move-to (- x0 d) (+ y0 (round (* y sy))))
                   (vecto:line-to (+ x0 d) (+ y0 (round (* y sy)))))))
          (if (< 0 y0 wy)
              (progn
                (loop for y from 0 downto ymin by 1/10
                      do (tick y))
                (loop for y from 1/10 upto ymax by 1/10
                      do (tick y)))
              (loop for y from ymin upto ymax by 1/10 do (tick y))
              )))
      (when (< 0 y0 wy)
        (vecto:move-to 0 y0)
        (vecto:line-to wx y0)
        (flet ((tick (x)
                 (let ((d (* *tick-len* (if (zerop (mod x 1)) 2 1))))
                   (vecto:move-to (+ x0 (round (* x sx))) (- y0 d))
                   (vecto:line-to (+ x0 (round (* x sx))) (+ y0 d)))))
          (if (< 0 x0 wx)
              (progn
                (loop for x from 0 downto xmin by 1/10
                      do (tick x))
                (loop for x from 1/10 upto xmax by 1/10
                      do (tick x)))
              (loop for x from xmin upto xmax by 1/10 do (tick x))
              )))



      (vecto:stroke))
    ))
(defvar *plot-ofs* 0)
(defvar *plot-hmax* nil)
(defun %plot-hist (graph hist r g b)
  (let* ((wx (- (wx graph) (* 2 (pad graph))))
         (wy (- (wy graph) (* 2 (pad graph))))
         (xmin (xmin graph))
         (xmax (xmax graph))
         (ymin (ymin graph))
         (ymax (ymax graph))
         (pad (pad graph))
         (sx (/ wx (- xmax xmin)))
         (sy (/ wy (- ymax ymin)))
         (x0 (+ 0.5 (round (* sx (- 0 xmin)))))
         (y0 (+ 0.5 (round (* sy (- 0 ymin)))))
                                        ;(x1 (round (* sx 0)))
                                        ;(y1 (round (* sy 0)))
                                        ;(x2 (round (* sx (- xmax xmin))))
                                        ;(y2 (round (* sy (- ymax ymin))))
         (hmax (or *plot-hmax* (max-count hist))))
    (vecto:with-graphics-state
      (vecto:set-rgb-stroke r g b)

      (vecto:translate pad pad)
      (unless (zerop hmax)
        (loop with s = (/ ymax hmax)
              for i below (nsamples hist)
              for h = (/ (aref (samples hist) i) hmax)
              for x = (+ x0 (* sx (value-for-bucket hist i)))
              for y = (+ y0 (* h sy))
              do (if (Zerop i)
                     (vecto:move-to x y)
                     (vecto:line-to x y)))
        (vecto:stroke)
        (vecto:translate 0 *plot-ofs*)
        (vecto:move-to (+ x0 (* sx (smin hist))) (- y0 20))
        (vecto:line-to (+ x0 (* sx (smin hist))) (- y0 40))
        (vecto:move-to (+ x0 (* sx (smax hist))) (- y0 40))
        (vecto:line-to (+ x0 (* sx (smax hist))) (- y0 20))


        (vecto:move-to (+ x0 (* sx (stddev hist))) (+ y0 50))
        (vecto:line-to (+ x0 (* sx (stddev hist))) (+ y0 100))

        (vecto:move-to (+ x0 (* sx (- (stddev hist)))) (+ y0 50))
        (vecto:line-to (+ x0 (* sx (- (stddev hist)))) (+ y0 100))


        )
      (vecto:stroke))))

(defun %move-to (graph x y)
  (let* ((wx (- (wx graph) (* 2 (pad graph))))
         (wy (- (wy graph) (* 2 (pad graph))))
         (xmin (xmin graph))
         (xmax (xmax graph))
         (ymin (ymin graph))
         (ymax (ymax graph))
         (pad (pad graph))
         (sx (/ wx (- xmax xmin)))
         (sy (/ wy (- ymax ymin)))
         (x0 (+ 0.5 (round (* sx (- 0 xmin)))))
         (y0 (+ 0.5 (round (* sy (- 0 ymin))))))
    (vecto:move-to (+ pad (+ x0 (* sx x)))
                   (+ pad (+ y0 (* sy y))))))

(defun %line-to (graph x y)
  (let* ((wx (- (wx graph) (* 2 (pad graph))))
         (wy (- (wy graph) (* 2 (pad graph))))
         (xmin (xmin graph))
         (xmax (xmax graph))
         (ymin (ymin graph))
         (ymax (ymax graph))
         (pad (pad graph))
         (sx (/ wx (- xmax xmin)))
         (sy (/ wy (- ymax ymin)))
         (x0 (+ 0.5 (round (* sx (- 0 xmin)))))
         (y0 (+ 0.5 (round (* sy (- 0 ymin))))))
    (vecto:line-to (+ pad (+ x0 (* sx x)))
                   (+ pad (+ y0 (* sy y))))))

(defmacro with-graph ((file &key (xmin -1) (xmax 1)
                              (ymin -1) (ymax 1)
                              (wx 512) (wy 512) (pad 6)
                              (background-color ''(1 1 1))
                              (looping t))
                      &body body)
  (a:with-gensyms (graph data-stream image)
    `(vecto:with-canvas (:width ,wx :height ,wy)
       (labels ((%color (r g b data-stream)
                  (skippy:ensure-color (skippy:rgb-color (floor (* 255 r))
                                                         (floor (* 255 g))
                                                         (floor (* 255 b)))
                                       (skippy:color-table data-stream)))
                (%color* (rgb data-stream)
                  (%color (first ,background-color)
                          (second ,background-color)
                          (third ,background-color)
                          data-stream) ))
         (declare (ignorable #'%color #'%color*))
         (let* ((,graph (make-instance 'graph
                                       :xmin ,xmin :xmax ,xmax
                                       :ymin ,ymin :ymax ,ymax
                                       :wx ,wx :wy ,wy :pad ,pad))
                (,data-stream (skippy:make-data-stream :width ,wx :height ,wy
                                                       :color-table t
                                                       :loopingp ,looping))
                (,image nil))
           (labels ((plot-hist (hist r g b)
                      (%plot-hist ,graph hist r g b))
                    (move-to (x y)
                      (%move-to ,graph x y))
                    (line-to (x y)
                      (%line-to ,graph x y))
                    (add-colors ()
                      #++(loop for i below 32
                               do (%color (/ i 31.0) (/ i 31.0) (/ i 31.0)
                                          ,data-stream)
                               )
                      
                      #++ ;; just use a 6x6x6 color cube + some grays
                      (loop for r below 6
                            do (loop for g below 6
                                     do (loop for b below 6
                                              do (%color (/ r 5.0)
                                                         (/ g 5.0)
                                                         (/ b 5.0)
                                                         ,data-stream)))))
                    (remap-color (r g b)
                      (let (#++(ds ,data-stream)
                            (ct (skippy:color-table ,data-stream)))
                        (or (ignore-errors
                             (skippy:ensure-color
                              (skippy:rgb-color (logand r #xe0)
                                                (logand g #xe0)
                                                (logand b #xe0))
                              ct))
                            #++(skippy:find-color
                                (skippy:rgb-color (logand r #xe0)
                                                  (logand g #xe0)
                                                  (logand b #xe0))
                                ct)
                            (random 256))
                        #++(or (skippy:find-color (the (unsigned-byte 31)
                                                       (skippy:rgb-color r g b))
                                                  ct)
                               (let* ((a (/ (+ r g b) 3))
                                      (r1 (* 51 (round r 51)))
                                      (g1 (* 51 (round g 51)))
                                      (b1 (* 51 (round b 51)))
                                      (rgb2 #++ (* 31 (round a 31))
                                            (floor (* 255/31 (round a (/ 255 31))))
                                            )
                                      (d (sqrt (+ (expt (- r1 a) 2)
                                                  (expt (- g1 a) 2)
                                                  (expt (- b1 a) 2))))
                                      (d2 (sqrt (* 3 (expt (- rgb2 a) 2)))))
                                 #++
                                 (format t "  ~s,~s,~s -> ~s,~s,~s = ~s / ~s = ~s~%"
                                         r g b r1 g1 b1
                                         (skippy:find-color
                                          (skippy:rgb-color r1 g1 b1) ct)
                                         rgb2
                                         (skippy:find-color (skippy:rgb-color
                                                             rgb2 rgb2 rgb2)
                                                            ct))
                                 (if (< d d2)
                                     (skippy:find-color (the (unsigned-byte 31)
                                                             (skippy:rgb-color r1 g1 b1))
                                                        ct)
                                     (skippy:find-color (the (unsigned-byte 31)
                                                             (skippy:rgb-color
                                                              rgb2 rgb2 rgb2))
                                                        ct)))
                               (progn
                                 (format t "add ~s ~s ~s~%" r g b)
                                 (skippy:add-color (skippy:rgb-color r g b)
                                                   ct)))))
                    (finish-frame ()
                      (when ,image
                        (let ((v (vecto::image-data vecto::*graphics-state*))
                              (gif (skippy:image-data ,image)))
                          (loop for y below ,wy
                                do (loop for x below ,wx
                                         for i = (+ (* y ,wx) x)
                                         for i2 = (* i 4)
                                         for r = (aref v (+ i2 0))
                                         for g = (aref v (+ i2 1))
                                         for b = (aref v (+ i2 2))
                                         for c = (remap-color r g b)
                                         do (setf (aref gif i) c))))
                        (skippy:add-image ,image ,data-stream)
                        ))
                    (new-frame (&optional (delay 50))
                      (finish-frame)
                      (setf ,image (skippy:make-image
                                    :width ,wx :height ,wy
                                    :delay-time delay
                                    :disposal-method :none
                                    :data-stream ,data-stream
                                    :image-data (skippy:make-image-data
                                                 ,wx ,wy)))
                      (apply 'vecto:set-rgb-fill ,background-color)
                      (vecto:clear-canvas)
                      (draw-axes ,graph)))
             (declare (ignorable #'plot-hist #'move-to #'line-to))
             (add-colors)
             (prog1
                 (progn
                   ,@body)
               (finish-frame)
               (skippy:output-data-stream ,data-stream ,file))))))))

(defmacro with-png-graph ((file &key (xmin -1) (xmax 1)
                                  (ymin -1) (ymax 1)
                                  (wx 512) (wy 512) (pad 6))
                          &body body)
  (a:with-gensyms (graph )
    `(vecto:with-canvas (:width ,wx :height ,wy)
       (let ((,graph (make-instance 'graph
                                    :xmin ,xmin :xmax ,xmax
                                    :ymin ,ymin :ymax ,ymax
                                    :wx ,wx :wy ,wy
                                    :pad ,pad)))
         (flet ((plot-hist (hist r g b)
                  (%plot-hist ,graph hist r g b)))
          (declare (ignorable #'plot-hist))
          (prog1
              (progn
                (draw-axes ,graph)
                ,@body)
            (vecto:save-png ,file)))))))


(defun call-with-noise-png (thunk file wx wy
                            scale rotate translate
                            count extra)
  rotate ;; todo
  (vecto:with-canvas (:width (* wx count) :height wy)
    (let* ((png (vecto::image vecto::*graphics-state*))
           (image (zpng:data-array png)))
      (flet (#++(df (x)
                  (coerce x 'double-float))
             (px (x y h)
               (let ((g (floor (* 255/2 (1+ (a:clamp h -1d0 1d0))))))
                 (setf (aref image y x 0) (if (< h 1) g 0))
                 (setf (aref image y x 1) (if (< h -1) 0 g))
                 (setf (aref image y x 2) (if (< -1 h 1) g 0)))
               ))
        (declare (inline px))
        (vecto:set-rgba-fill 0 0 0 255)
        (vecto:clear-canvas)
        (loop for j below wy
              do (loop for i below wx
                       for x = (+ (aref translate 0) (* i (/ scale wx)))
                       for y = (+ (aref translate 0) (* j (/ scale wx)))
                       for z = (+ (aref translate 0) (* 0 (/ scale wx)))
                       for w = (+ (aref translate 0) (* 0 (/ scale wx)))
                       for h = (multiple-value-list
                                (funcall thunk x y z w))
                       do (loop for n from 0
                                for h in h
                                do (px (+ i (* wx n)) j h))))
        (when extra
          (loop for i below count do (funcall extra image i)))
        (zpng:write-png png file))))
  )

(defmacro with-noise-png ((file &key (wx 256) (wy 256)
                                  (vars '(x y z w))
                                  (extra-vars '(image n))
                                  (scale 4.0)
                                  (rotate #(0.234 0.345 0.123 0.321))
                                  (translate #(-2 -2 0.234 0.321)))
                          &body body
                          &key pixel extra)
  (declare (ignorable body))
  (a:with-gensyms (rest)
    (let ((lambda `(lambda (,@vars &rest ,rest)
                     (declare (ignore ,rest))
                     ,pixel)))
      `(call-with-noise-png
        ,lambda
        ,file ,wx ,wy ,scale ,rotate ,translate
        (length (multiple-value-list (,lambda 0.0 0.0 0.0 0.0)))
        ,(when extra
           `(lambda ,extra-vars ,extra))))))





(defmacro with-gray-gif ((file image &key (wx 256) (wy 256)) &body body)
  (a:with-gensyms (data-stream)
    `(let ((,data-stream (skippy:make-data-stream :width ,wx :height ,wy
                                                  :color-table t
                                                  :loopingp t))
           (,image nil))

       (labels ((color (r g b data-stream)
                  (skippy:ensure-color (skippy:rgb-color r g b)
                                       (skippy:color-table data-stream)))
                (add-colors ()
                  (loop for i below 256
                        do (color i i i ,data-stream)))
                    (finish-frame ()
                      (when ,image
                        (skippy:add-image ,image ,data-stream)))
                    (new-frame (&optional (delay 50))
                      (finish-frame)
                      (setf ,image (skippy:make-image
                                    :width ,wx :height ,wy
                                    :delay-time delay
                                    :disposal-method :none
                                    :data-stream ,data-stream
                                    :image-data (skippy:make-image-data
                                                 ,wx ,wy)))))
         (add-colors)
         (prog1 (progn ,@body)
           (finish-frame)
           (skippy:output-data-stream ,data-stream ,file))))))
