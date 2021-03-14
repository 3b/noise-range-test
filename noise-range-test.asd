(defsystem noise-range-test
  :depends-on (alexandria sb-cga cl-opengl
                          3b-glim/example/s 3b-glim/2d 3bgl-shader)
  :serial t
  :components ((:file "shaders")
               (:file "package")
               (:file "perlin-orig")
               (:file "perlin-improved")
               (:file "fbm")
               (:file "hist")
               (:file "poly")
               #++(:file "vis2")
               #++(:file "vis2")
               (:file "vis4")))

