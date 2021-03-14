(defpackage #:noise-range-test/original
  (:use :cl)
  (:local-nicknames (#:a #:alexandria-2))
  (:export
   #:original-noise1
   #:original-noise2
   #:original-noise3))

(defpackage #:noise-range-test/improved
  (:use :cl)
  (:local-nicknames (#:a #:alexandria-2))
  (:export
   #:improved-noise2
   #:improved-noise3
   #:improved-noise4
   #:improved-noise1))


(defpackage #:noise-range-test
  (:use :cl #:3b-glim-example/s #:noise-range-test-shaders
        #:noise-range-test/original
        #:noise-range-test/improved)
   (:local-nicknames (#:glim #:3b-glim/s)
                     (#:2d #:3b-glim/2d)
                     (#:a #:alexandria-2)))


