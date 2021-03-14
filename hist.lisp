(in-package noise-range-test)

(defclass hist ()
  ;; range of samples accepted
  ((rmin :initform -2 :reader rmin :initarg :rmin)
   (rmax :initform 2 :reader rmax :initarg :rmax)
   ;; min/max values passed to sample (not calculated from histogram
   ;; and cached so we can preserve exact value)
   (smin :initform most-positive-single-float :reader smin)
   (smax :initform most-negative-single-float :Reader smax)
   ;; # of buckets within accepted range
   (nsamples :initform 2000 :reader nsamples :initarg :nsamples)
   ;; histogram data
   (samples :initform nil :Reader samples)
   ;; dirty flag indicating cached values should be recalculated
   (dirty :initform t :accessor dirty)
   ;; cached properties of histogram
   (max-count)
   (mean)
   (stddev)
   ;; values used to calculate running mean/stddev
   (s0 :initform 0d0 :accessor s0)
   (s1 :initform 0d0 :accessor s1)
   (s2 :initform 0d0 :accessor s2)

))

(defmethod reset ((o hist))
  (setf (slot-value o 'smax) most-negative-single-float)
  (setf (slot-value o 'smin) most-positive-single-float)
  (setf (s0 o) 0d0)
  (setf (s1 o) 0d0)
  (setf (s2 o) 0d0)
  (fill (samples o) 0)
  (setf (dirty o) t))

(defmethod initialize-instance :after ((o hist) &key)
  (setf (slot-value o 'samples)
        (make-array (+ 2 (nsamples o)) :element-type '(unsigned-byte 64)
                    :initial-element 0)))

;; calculate various properties of 
(defun update (o)
  ;; todo: calculate these in 1 pass
  (setf (slot-value o 'max-count) (reduce 'max (samples o)))
  (cond
    ((zerop (s0 o))
     (setf (slot-value o 'mean) 0d0)
     (setf (slot-value o 'stddev) 0d0))
    (t
     (setf (slot-value o 'mean) (/ (s1 o) (s0 o)))
     (setf (slot-value o 'stddev) (/ (sqrt (- (* (s0 o) (s2 o))
                                              (expt (s1 o) 2)))
                                     (s0 o))
)))
)

;; todo: make these thread safe?
(defmethod mean ((o hist))
  (when (dirty o) (update o))
  (slot-value o 'mean))

(defmethod stddev ((o hist))
  (when (dirty o) (update o))
  (slot-value o 'stddev))

(defmethod max-count ((o hist))
  (when (dirty o) (update o))
  (slot-value o 'max-count))

(defmethod sample ((o hist) x)
  (when (< x (smin o))
    (setf (slot-value o 'smin) x))
  (when (> x (smax o))
    (setf (slot-value o 'smax) x))
  (incf (s0 o))
  (incf (s1 o) x)
  (incf (s2 o) (expt x 2))
  (let* ((x1 (/ (- x (rmin o))
                (- (rmax o) (rmin o))))
         (i (a:clamp (1+ (round (* x1 (nsamples o))))
                     1 (+ 2 (nsamples o)))))
    (incf (aref (samples o) (1- i)))
    (setf (dirty o) t))
  x)

(defmethod value-for-bucket ((o hist) i)
  (cond
    ((zerop i)
     (rmin o))
    ((= i (+ 1 (nsamples o)))
     (rmax o))
    (t
     (coerce
      (+ (rmin o)
         (* (1- i) (/ (- (rmax o) (rmin o))
                      (nsamples o))))
      'single-float))))

(defmethod bucket-for-value ((o hist) x)
  (cond
    ((< x (rmin o))
     0)
    ((> x (rmax o))
     (1- (nsamples o)))
    (t
     (let* ((x1 (/ (- x (rmin o))
                   (- (rmax o) (rmin o))))
            (i (a:clamp (1+ (round (* x1 (nsamples o))))
                        0 (+ 2 (nsamples o)))))
       i))))
