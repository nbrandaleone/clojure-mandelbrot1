(ns mandelbrot.core
  (:require [clojure.math.numeric-tower :as math])
  (:gen-class))

;;  Nick Brandaleone - January 2016
;;
;; This is a simple two character ASCII implementation of the Mandelbrot Set.
;; Inspired by a program from Google groups. Not sure of original author.
;; A complex point is in the Mandelbrot Set, if it is bounded.
;; We check 20 iteration (most check 100-1000).
;;    Function: z_n+1 = (z_n)^2 + c, z_0 = 0, c = complex point 


;; As clojure does not define Complex Numbers, I created a small
;; library that does complex multiplication, addition and ABS.
;; All complex arguments expect a vector having real and complex parts.
;; There is no reference to "i". Returns a vector [r i].
(defn c* [z1 z2]
  "Multiplies the real and imaginary parts of a complex number"
  (let [[a b] z1 [c d] z2]
    [(- (* a c) (* b d)) (+ (* a d) (* b c))]))

(defn c+ [z1 z2]
  "Complex or vector addition"
  (let [[a b] z1 [c d] z2]
    [(+ a c) (+ b d)]))

(defn c-abs [z]
  "The distance of the vector from the origin"
  (let [[r i] z]
    (math/sqrt (+ (* r r) (* i i)))))
     
(defn complex [x y]
  "Returns a vector representing a complex number"
  [x y])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mandelbrot? [z]
  (loop [c 1
         m (iterate #(c+ z (c* % %)) [0 0])]
    (if (and (> 20 c)
             (< (c-abs (first m)) 2))
      (recur (inc c)
             (rest m))
      (if (= 20 c) true false))))

(defn mandelbrot []
  (for [y (range 1 -1 -0.05)
        x (range -2 0.5 0.0315)]
    (if (mandelbrot? (complex x y)) "#" " ")))


(defn -main [& args]
  "Prints an ASCII version of the mandelbrot set"
  (println (interpose "\n" (map #(apply str %) (partition 80 (mandelbrot))))))
;; It wraps the output in parens. Not sure why.
