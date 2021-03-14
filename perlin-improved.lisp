(in-package noise-range-test/improved)

;; based on
;; 2d: https://mrl.cs.nyu.edu/~perlin/noise/ImprovedNoise2D.java
;; 3d: https://mrl.cs.nyu.edu/~perlin/noise/
;; 4d: https://mrl.cs.nyu.edu/~perlin/noise/ImprovedNoise4D.java
;; (see also https://mrl.cs.nyu.edu/~perlin/noise/INoise.java) 

(declaim (type (simple-array (unsigned-byte 8) (512)) *p*))
(defvar *p*
  (coerce
   '(;; permutation table from reference implementation
     151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30
     69 142 8 99 37 240 21 10 23 190 6 148 247 120 234 75 0 26 197 62 94
     252 219 203 117 35 11 32 57 177 33 88 237 149 56 87 174 20 125 136
     171 168 68 175 74 165 71 134 139 48 27 166 77 146 158 231 83 111 229
     122 60 211 133 230 220 105 92 41 55 46 245 40 244 102 143 54 65 25
     63 161 1 216 80 73 209 76 132 187 208 89 18 169 200 196 135 130 116
     188 159 86 164 100 109 198 173 186 3 64 52 217 226 250 124 123 5 202
     38 147 118 126 255 82 85 212 207 206 59 227 47 16 58 17 182 189 28
     42 223 183 170 213 119 248 152 2 44 154 163 70 221 153 101 155 167
     43 172 9 129 22 39 253 19 98 108 110 79 113 224 232 178 185 112 104
     218 246 97 228 251 34 242 193 238 210 144 12 191 179 162 241 81 51
     145 235 249 14 239 107 49 192 214 31 181 199 106 157 184 84 204 176
     115 121 50 45 127 4 150 254 138 236 205 93 222 114 67 29 24 72 243
     141 128 195 78 66 215 61 156 180
     ;; repeated twice
     151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30
     69 142 8 99 37 240 21 10 23 190 6 148 247 120 234 75 0 26 197 62 94
     252 219 203 117 35 11 32 57 177 33 88 237 149 56 87 174 20 125 136
     171 168 68 175 74 165 71 134 139 48 27 166 77 146 158 231 83 111 229
     122 60 211 133 230 220 105 92 41 55 46 245 40 244 102 143 54 65 25
     63 161 1 216 80 73 209 76 132 187 208 89 18 169 200 196 135 130 116
     188 159 86 164 100 109 198 173 186 3 64 52 217 226 250 124 123 5 202
     38 147 118 126 255 82 85 212 207 206 59 227 47 16 58 17 182 189 28
     42 223 183 170 213 119 248 152 2 44 154 163 70 221 153 101 155 167
     43 172 9 129 22 39 253 19 98 108 110 79 113 224 232 178 185 112 104
     218 246 97 228 251 34 242 193 238 210 144 12 191 179 162 241 81 51
     145 235 249 14 239 107 49 192 214 31 181 199 106 157 184 84 204 176
     115 121 50 45 127 4 150 254 138 236 205 93 222 114 67 29 24 72 243
     141 128 195 78 66 215 61 156 180)
   '(simple-array (unsigned-byte 8) (512))))

(declaim (inline fade grad lerp
                 improved-noise1
                 improved-noise2 improved-noise3 improved-noise4))
(defun fade (x)
  (* x x x (+ (* x (- (* x 6) 15)) 10)))

#++
(declaim (inline v3))
#++
(Defun v3 (x y z)
  (sb-cga:vec (coerce x 'single-float)
              (coerce y 'single-float)
              (coerce z 'single-float)))

(defun grad* (hash x y z)
  (let* ((h (ldb (byte 4 0) hash))
         (-x (- x))
         (-y (- y))
         (-z (- z)))
    (case h
      (0 (list  x  y  0))
      (1 (list -x  y  0))
      (2 (list  x -y  0))
      (3 (list -x -y  0))
      (4 (list  x  0  z))
      (5 (list  x  0 -z))
      (6 (list -x  0  z))
      (7 (list -x  0 -z))
      (8 (list  0  y  z))
      (9 (list  0  y -z))
      (10 (list  0 -y  z))
      (11 (list  0 -y -z))
      (12 (list  x  y  0))
      (13 (list -x  y  0))
      (14 (list  0 -y  z))
      (15 (list  0 -y -z)))))


;; shared by 2d and 3d
(defun grad (hash x y &optional (z 0d0))
  (let* ((h (ldb (byte 4 0) hash))
         (u (if (< h 8) x y))
         (v (if (< h 4) y
                (if (member h '(12 14))
                    x z))))
    (+ (if (logbitp 0 h) (- u) u)
       (if (logbitp 1 h) (- v) v))))

(defun lerp (x a b)
  ;; original lerp
  #++
  (+ a (* x (- b a)))
  ;; "numerically stable" lerp
  (+ (* (- 1.0 x) a) (* x b)))

;; subset of double-float that still has enough fraction bits to be
;; meaningful as input to noise functions (and which can be safely
;; FLOORed to a sb64)
(deftype double-input-type () `(double-float ,(- (expt 2d0 50))
                                             ,(expt 2d0 50)))

;; couldn't find code for 1d improved noise, so extrapolated from the
;; rest
(defun %improved-noise1 (x1)
  (declare (type double-input-type x1)
           (optimize speed))
  (flet ((grad (h x)
           (if (logbitp 0 h)
               x
               (- x))))
   (let* ((p *p*)
          (ix (ldb (byte 8 0) (floor x1)))
          (x (- x1 (floor x1)))
          (u (fade x)))

     (coerce
      (lerp u
            (grad (aref p ix) x)
            (grad (aref p (1+ ix)) (1- x)))
      'single-float))))


(defun improved-noise1 (x)
  (%improved-noise1 (coerce x 'double-input-type)))


(defun %improved-noise2 (x1 y1)
  (declare (type double-input-type x1 y1)
           (optimize speed))
  (let* ((p *p*)
         (ix (ldb (byte 8 0) (floor x1)))
         (iy (ldb (byte 8 0) (floor y1)))
         (x (- x1 (floor x1)))
         (y (- y1 (floor y1)))
         (u (fade x))
         (v (fade y))
         (a  (+ (aref p ix) iy))
         (aa (aref p a))
         (ab (aref p (1+ a)))
         (b  (+ (aref p (1+ ix)) iy))
         (ba (aref p b))
         (bb (aref p (1+ b))))

    (coerce
     (lerp v
           (lerp u
                 (grad (aref p aa) x y)
                 (grad (aref p ba) (1- x) y))
           (lerp u
                 (grad (aref p ab) x (1- y))
                 (grad (aref p bb) (1- x) (1- y))))
     'single-float)))

(defun improved-noise2 (x y)
  (%improved-noise2 (coerce x 'double-input-type)
                    (coerce y 'double-input-type)))

(declaim (ftype (function (real real real) (values single-float &optional))
                improved-noise3))
(declaim (ftype (function (double-input-type
                           double-input-type
                           double-input-type)
                          (values single-float &optional))
                %improved-noise3))

(defun %improved-noise3 (x1 y1 z1)
  (declare (type double-input-type x1 y1 z1)
           (optimize speed))
  (let* ((p *p*)
         (ix (ldb (byte 8 0) (floor x1)))
         (iy (ldb (byte 8 0) (floor y1)))
         (iz (ldb (byte 8 0) (floor z1)))
         (x (- x1 (floor x1)))
         (y (- y1 (floor y1)))
         (z (- z1 (floor z1)))
         (u (fade x))
         (v (fade y))
         (w (fade z))
         (a  (+ (aref p ix) iy))
         (aa (+ (aref p a) iz))
         (ab (+ (aref p (1+ a)) iz))
         (b  (+ (aref p (1+ ix)) iy))
         (ba (+ (aref p b) iz))
         (bb (+ (aref p (1+ b)) iz)))
    #++
    (locally (declare (optimize (speed 1)))
      (format t "@ ~s, ~s, ~s~%" x1 y1 z1)
      (format t " i = ~s ~s ~s~%" ix iy iz)
      (format t " r = ~s ~s ~s~%" x y z)
      (format t " u = ~s ~s ~s~%" u v w)
      (format t "a = ~s, aa = ~s, ab = ~s~%" a aa ab)
      (format t "b = ~s, ba = ~s, bb = ~s~%" b ba bb)
      (format t "grads:~%")
      (format t "  ~s ~s ~s ~s = ~s~%"
              (aref p aa) x y z (grad (aref p aa) x y z))
      (format t "  ~s ~s ~s ~s = ~s~%"
              (aref p ba) (1- x) y z (grad (aref p ba) (1- x) y z))
      (format t "  ~s ~s ~s ~s = ~s~%"
              (aref p ab) x (1- y) z (grad (aref p ab) x (1- y) z))
      (format t "  ~s ~s ~s ~s = ~s~%"
              (aref p bb) (1- x) (1- y) z (grad (aref p bb) (1- x) (1- y) z))
      (format t "  ~s ~s ~s ~s = ~s~%"
              (aref p (1+ aa)) x y (1- z) (grad (aref p (1+ aa)) x y (1- z)))
      (format t "  ~s ~s ~s ~s = ~s~%"
              (aref p (1+ ba)) (1- x) y (1- z) (grad (aref p (1+ ba)) (1- x) y (1- z)))
      (format t "  ~s ~s ~s ~s = ~s~%"
              (aref p (1+ ab)) x (1- y) (1- z) (grad (aref p (1+ ab)) x (1- y) (1- z)))
      (format t "  ~s ~s ~s ~s = ~s~%"
              (aref p (1+ bb)) (1- x) (1- y) (1- z) (grad (aref p (1+ bb)) (1- x) (1- y) (1- z)))

      (format t "lu @ ~s~%" u)
      (format t " ~s ~s = ~s~%"
              (grad (aref p aa) x y z) (grad (aref p ba) (1- x) y z)
              (lerp u (grad (aref p aa) x y z) (grad (aref p ba) (1- x) y z)))
      (format t " ~s ~s = ~s~%"
              (grad (aref p ab) x (1- y) z)
              (grad (aref p bb) (1- x) (1- y) z)
              (lerp u
                    (grad (aref p ab) x (1- y) z)
                    (grad (aref p bb) (1- x) (1- y) z)))
      (format t " ~s ~s = ~s~%"
              (grad (aref p (1+ aa)) x y (1- z))
              (grad (aref p (1+ ba)) (1- x) y (1- z))
              (lerp u
                    (grad (aref p (1+ aa)) x y (1- z))
                    (grad (aref p (1+ ba)) (1- x) y (1- z))))
      (format t " ~s ~s = ~s~%"
              (grad (aref p (1+ ab)) x (1- y) (1- z))
              (grad (aref p (1+ bb)) (1- x) (1- y) (1- z))
              (lerp u
                    (grad (aref p (1+ ab)) x (1- y) (1- z))
                    (grad (aref p (1+ bb)) (1- x) (1- y) (1- z))))

      (format t "lv = ~s~%" v)
      (format t " ~s ~s = ~s~%"
              (lerp u
                    (grad (aref p aa) x y z)
                    (grad (aref p ba) (1- x) y z))
              (lerp u
                    (grad (aref p ab) x (1- y) z)
                    (grad (aref p bb) (1- x) (1- y) z))
              (lerp v
                    (lerp u
                          (grad (aref p aa) x y z)
                          (grad (aref p ba) (1- x) y z))
                    (lerp u
                          (grad (aref p ab) x (1- y) z)
                          (grad (aref p bb) (1- x) (1- y) z))))
      (format t " ~s ~s = ~s~%"
              (lerp u
                    (grad (aref p (1+ aa)) x y (1- z))
                    (grad (aref p (1+ ba)) (1- x) y (1- z)))
              (lerp u
                    (grad (aref p (1+ ab)) x (1- y) (1- z))
                    (grad (aref p (1+ bb)) (1- x) (1- y) (1- z)))
              (lerp v
                    (lerp u
                          (grad (aref p (1+ aa)) x y (1- z))
                          (grad (aref p (1+ ba)) (1- x) y (1- z)))
                    (lerp u
                          (grad (aref p (1+ ab)) x (1- y) (1- z))
                          (grad (aref p (1+ bb)) (1- x) (1- y) (1- z)))))
      (format t "lw = ~s~% ~s ~s = ~s~%"
              w
              (lerp v
                    (lerp u
                          (grad (aref p aa) x y z)
                          (grad (aref p ba) (1- x) y z))
                    (lerp u
                          (grad (aref p ab) x (1- y) z)
                          (grad (aref p bb) (1- x) (1- y) z)))
              (lerp v
                    (lerp u
                          (grad (aref p (1+ aa)) x y (1- z))
                          (grad (aref p (1+ ba)) (1- x) y (1- z)))
                    (lerp u
                          (grad (aref p (1+ ab)) x (1- y) (1- z))
                          (grad (aref p (1+ bb)) (1- x) (1- y) (1- z))))
              (lerp w
                    (lerp v
                          (lerp u
                                (grad (aref p aa) x y z)
                                (grad (aref p ba) (1- x) y z))
                          (lerp u
                                (grad (aref p ab) x (1- y) z)
                                (grad (aref p bb) (1- x) (1- y) z)))
                    (lerp v
                          (lerp u
                                (grad (aref p (1+ aa)) x y (1- z))
                                (grad (aref p (1+ ba)) (1- x) y (1- z)))
                          (lerp u
                                (grad (aref p (1+ ab)) x (1- y) (1- z))
                                (grad (aref p (1+ bb)) (1- x) (1- y) (1- z)))))
              ))
    (coerce
     (lerp w
           (lerp v
                 (lerp u
                       (grad (aref p aa) x y z)
                       (grad (aref p ba) (1- x) y z))
                 (lerp u
                       (grad (aref p ab) x (1- y) z)
                       (grad (aref p bb) (1- x) (1- y) z)))
           (lerp v
                 (lerp u
                       (grad (aref p (1+ aa)) x y (1- z))
                       (grad (aref p (1+ ba)) (1- x) y (1- z)))
                 (lerp u
                       (grad (aref p (1+ ab)) x (1- y) (1- z))
                       (grad (aref p (1+ bb)) (1- x) (1- y) (1- z)))))
     'single-float)))
(log (expt 8 16)2 )48.0 281 474 976 710 656
(/ (expt 8 16) (expt 2 32))65536
(expt 2 4)16
(expt 10 2)

(defun improved-noise3 (x y z)
  (%improved-noise3 (coerce x 'double-input-type)
                    (coerce y 'double-input-type)
                    (coerce z 'double-input-type)))

(declaim (ftype (function (real real real real)
                          (values single-float &optional))
                improved-noise4))
(declaim (ftype (function (double-input-type
                           double-input-type
                           double-input-type
                           double-input-type)
                          (values single-float &optional))
                %improved-noise4))

(defun %improved-noise4 (x1 y1 z1 w1)
  (declare (type double-input-type x1 y1 z1 w1)
           (optimize speed))
  (flet ((grad (hash x y z w)
           (let* ((h (ldb (byte 5 0) hash))
                  (a x)
                  (b y)
                  (c w))
             #++(format t "~s " h)
             (case (ash h -3)
               (1 (setf a w b x c y))
               (2 (setf a z b w c x))
               (3 (setf a y b z c w)))
             (+ (if (logbitp 2 h) a (- a))
                (if (logbitp 1 h) b (- b))
                (if (logbitp 0 h) c (- c))))))
    (let* ((p *p*)
           (ix (ldb (byte 8 0) (floor x1)))
           (iy (ldb (byte 8 0) (floor y1)))
           (iz (ldb (byte 8 0) (floor z1)))
           (iw (ldb (byte 8 0) (floor w1)))
           (x (- x1 (floor x1)))
           (y (- y1 (floor y1)))
           (z (- z1 (floor z1)))
           (w (- w1 (floor w1)))
           (lx (fade x))
           (ly (fade y))
           (lz (fade z))
           (lw (fade w))

           (a  (+ (aref p ix) iy))
           (aa (+ (aref p a) iz))
           (ab (+ (aref p (1+ a)) iz))
           (b  (+ (aref p (1+ ix)) iy))
           (ba (+ (aref p b) iz))
           (bb (+ (aref p (1+ b)) iz))
           (aaa (+ (aref p aa) iw)) (aab (+ (aref p (1+ aa)) iw))
           (aba (+ (aref p ab) iw)) (abb (+ (aref p (1+ ab)) iw))
           (baa (+ (aref p ba) iw)) (bab (+ (aref p (1+ ba)) iw))
           (bba (+ (aref p bb) iw)) (bbb (+ (aref p (1+ bb)) iw)))
      (coerce
       (lerp lw
             (lerp lz
                   (lerp ly
                         (lerp lx
                               (grad (aref p aaa) x y z w)
                               (grad (aref p baa) (1- x) y z w))
                         (lerp lx
                               (grad (aref p aba) x (1- y) z w)
                               (grad (aref p bba) (1- x) (1- y) z w)))
                   (lerp ly
                         (lerp lx
                               (grad (aref p aab) x y (1- z) w)
                               (grad (aref p bab) (1- x) y (1- z) w))
                         (lerp lx
                               (grad (aref p abb) x (1- y) (1- z) w)
                               (grad (aref p bbb) (1- x) (1- y) (1- z) w))))
             (lerp lz
                   (lerp ly
                         (lerp lx
                               (grad (aref p (1+ aaa)) x y z (1- w))
                               (grad (aref p (1+ baa)) (1- x) y z (1- w)))
                         (lerp lx
                               (grad (aref p (1+ aba)) x (1- y) z (1- w))
                               (grad (aref p (1+ bba)) (1- x) (1- y) z (1- w))))
                   (lerp ly
                         (lerp lx
                               (grad (aref p (1+ aab)) x y (1- z) (1- w))
                               (grad (aref p (1+ bab)) (1- x) y (1- z) (1- w)))
                         (lerp lx
                               (grad (aref p (1+ abb)) x (1- y) (1- z) (1- w))
                               (grad (aref p (1+ bbb)) (1- x) (1- y) (1- z) (1- w))))))
       'single-float))))

(expt 12 8)429 981 696
(/ (expt 12 8) (expt 256 3)) (float 6561/256)25.628906

(defun improved-noise4 (x y z w)
  (%improved-noise4 (coerce x 'double-input-type)
                    (coerce y 'double-input-type)
                    (coerce z 'double-input-type)
                    (coerce w 'double-input-type)))

;;; extracted just the gradient code, so we can calculate max value
;;; with fixed gradients
(defun improved-noise1* (x1 masks)
  (flet ((grad (n x)
           (loop with mask = (nth n masks)
                 for i from 0
                 for v in (list x)
                 if (logbitp i mask)
                   sum v else sum (- v))))
   (let* ((x (- x1 (floor x1)))
          (u (fade x)))
     (format t "~s ~s ~s~%" u (grad 0 x) (grad 1 x))
     (lerp u
           (grad 0 x)
           (grad 1 x)))))

(defun improved-noise1 (x1 masks)
  (flet ((grad (n x)
           (loop with mask = (nth n masks)
                 for i from 0
                 for v in (list x)
                 if (logbitp i mask)
                   sum v else sum (- v))))
   (let* ((x (- x1 (floor x1)))
          (u (fade x)))
     (format t "~s ~s ~s~%" u (grad 0 x) (grad 1 x))
     (lerp u
           (grad 0 x)
           (grad 1 x)))))

(defun make-masks (d)
  (loop for i below (expt 2 d)
        collect i)
)
#++
(improved-noise1* 0.01625 (list 1 1))
#++
(loop for i below 10 by 0.05
      for n = (improved-noise1 i)
      for x = (+ 35 (floor (* n 35)))
      do (loop repeat x do (format t " ")) (format t "*~%")
)
(improved-noise3 96.3 207.2 93.2)
4 5 9 9 6 7 2 15 0.6237606
0.6237606
0.6237606
0.6237606
(sb-ext:gc :full t)
(loop for x in '(4 5 9 9 6 7 2 11)
      for i from 0
      sum (* x (expt 12 i)))
160694675
402005008
545332240
(- 93.2 93)
