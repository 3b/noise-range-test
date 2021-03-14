(defpackage #:noise-range-test-shaders
  (:use #:3bgl-glsl/cl)
  (:export #:vertex #:fragment #:frag-solid
           #:position #:uv
           #:mv #:mvp #:lut #:tex #:line-base #:line-step #:debug1))
(in-package #:noise-range-test-shaders)

(input position :vec4 :location 0)
(input uv :vec4 :location 1)
(input color :vec4 :location 2)

(output color :vec4 :stage :fragment)


;; uniforms
(uniform mv :mat4) ;; model-view matrix
(uniform mvp :mat4) ;; model-view-projection matrix
(uniform tex :sampler-2d)
(uniform debug1 :int)

(interface varyings (:out (:vertex outs)
                     :in (:fragment ins))
  (position :vec4)
  (uv :vec4)
  (color :vec4))

;; generic vertex shader used for a few lighting models
(defun vertex ()
  (setf gl-position (* mvp position))
  (setf (@ outs position) (* mv position)
        (@ outs uv) uv
        (@ outs color) color))

(defun fragment ()
  (let* ((uv (@ ins uv)))
    (setf color
          (vec4 (vec3
                 (* 0.5 (1+ (.x (texture tex (.xy uv))))))
                1))))

(defun frag-solid ()
  (setf color (@ ins color)))
