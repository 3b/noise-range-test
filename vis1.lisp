#++
(ql:quickload '(alexandria sb-cga cl-opengl
                3b-glim/example/s 3b-glim/2d 3bgl-shader))

(in-package noise-range-test)

(defvar *format*
  (glim:compile-vertex-format
   '(1
     (0 :vec4) ;; position
     (1 :vec4) ;; uv
     )))

(declaim (inline vertex vertex-v color normal uv))
(defun vertex (x &optional (y 0.0) (z 0.0) (w 1.0))
  (glim:attrib-f 0 x y z w))
(defun vertex-v (v)
  (glim:attrib-fv 0 v))
(defun uv (x &optional (y 0.0) (z 0.0) (w 1.0))
  (glim:attrib-f 1 x y z w))

(defparameter *debug* 0)
(defparameter *flags* (make-array 10 :initial-element nil))

(defclass noise (scratchpad)
  ((tex :initform nil :accessor tex))
  (:default-initargs :shaders '((:tex :vertex vertex :fragment fragment))))


(defvar *w* nil)

(defun uniforms ()
  (glim:uniform 'mv (glim:ensure-matrix :modelview))
  (glim:uniform 'mvp (sb-cga:matrix*
                      (glim:ensure-matrix :projection)
                      (glim:ensure-matrix :modelview))))



(defun load1 (w)
  (unless (tex w)
    (setf (tex w) (gl:gen-texture)))

  (gl:pixel-store :unpack-alignment 1)
  (gl:active-texture 0)
  (gl:bind-texture :texture-2d (tex w))

  (gl:tex-parameter :texture-2d :texture-min-filter :linear)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
  (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
  (let* ((w 512)
         (h 512)
         (x2 5.0)
         (x1 (- x2))
         (y1 x1)
         (y2 x2)
         (a (make-array (list h w) :element-type 'single-float
                                   :initial-element 1.0))
         (z 0 #++(print (1- (random 2.0)))))
    #++(let ((r (a:shuffle (coerce (a:iota 256) 'vector))))
      ; (replace *p* r) (replace *p* r :start1 256)
      )
    (time
     (loop for j below h
           for y from y1 by (/ (- y2 y1) h)
           do (loop for i below w
                    for x from x1 by (/ (- x2 x1) w)
                    do (setf (aref a j i)
                             (perlin x y z)
                             ))))
    (gl:tex-image-2d :texture-2d 0 :r32f w h 0
                     :red :float (make-array (* w h)
                                             :element-type 'single-float
                                             :displaced-to a))))




(defmethod display ((w noise) now)
  (setf *w* w)

  (glim:with-state (*format*)
    (glim:uniform 'proj sb-cga:+identity-matrix+)
    (glim:matrix-mode :projection)
    (glim:load-identity)
    (glim:matrix-mode :modelview)
    (glim:load-identity)

    (gl:enable :depth-test :multisample :texture-2d )

    (when (tex w)
      (gl:active-texture 0)
      (gl:enable :texture-2d)

      (gl:bind-texture :texture-2d (tex w)))

    (glim:uniform 'tex 0)

    (glim:uniform 'debug1 (if (aref *flags* 1) 1 0))

    (glim:with-pushed-matrix (:modelview)
      (glim:scale 1 1 1)
      (glim:scale 1.8 1.8 1)
      (glim:translate -0.5 -0.5 0)
      (gl:line-width 10)
      (uniforms)

      (uniforms)
      (glim:with-draw (:quads :shader :tex)

        (uv 0 1 0 0)
        (vertex 0 0)

        (uv 1 1 0 0)
        (vertex 1 0)

        (uv 1 0 0 0)
        (vertex 1 1)

        (uv 0 0 0 0)
        (vertex 0 1)


)
         (dispatch-draws w))
    (gl:line-width 1)
    (let ((*random-state* (make-random-state *random-state*)))
     (glim:with-draw (:lines :shader :tex)
       (loop repeat 10000
             do (vertex (* 12 (1- (random 2.0)))
                        (* 12 (1- (random 2.0)))
                        0
                        1
                        ))))
    (dispatch-draws w)

))

(defmethod mouse ((window noise) button state x y)
  (format t "click ~s ~s~%" button state)

  )

(defmethod keyboard ((window noise) key x y)
  (declare (ignore x y))

  (case key
    ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
     (let ((i (digit-char-p key)))
       (when i
         (setf (aref *flags* i) (not (aref *flags* i)))
         (format t "flags ~s -> ~s~%" i (aref *flags* i)))))
    (#\space
     (load1 window))
    (#\Esc
     (glut:destroy-current-window))))

(defmethod init-gl ((w noise))
  (gl:pixel-store :unpack-alignment 1)

  (gl:disable :dither))

(defun noise (&rest args)
  (glut:display-window (apply #'make-instance 'noise args)))

#++
(ql:quickload 'noise)
#++
(noise :pos-x 2440 :pos-y 140 :width 1400 :height 850)
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
