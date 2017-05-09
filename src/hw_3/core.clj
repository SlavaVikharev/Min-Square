(ns hw-3.core
  (:require [quil.core :as q]))

(def step (/ q/PI 720))

(def polygon [[100 100]
              [100 200]
              [200 300]
              [150 200]
              [200 150]
              [200 50]])


(defn rotate-on [angle [x y]]
  (let [sina (q/sin angle)
        cosa (q/cos angle)]
    [(+ (* cosa x) (* sina y))
     (- (* cosa y) (* sina x))]))

(defn rotate-points [points angle]
  (map (partial rotate-on angle) points))

(defn measure [polygon]
  (let [xs (map first polygon)
        ys (map second polygon)
        x-dist (- (apply max xs) (apply min xs))
        y-dist (- (apply max ys) (apply min ys))]
    (max x-dist y-dist)))


(def angles (range 0 (/ q/PI 2) step))

(def rotated-polygons
  (map #(identity [(rotate-points polygon %) %]) angles))

(def best-polygon-rotation
  (apply min-key (comp measure first) rotated-polygons))

(def best-rotated-polygon (first best-polygon-rotation))

(def best-angle (second best-polygon-rotation))

(def square-angle (- best-angle))

(def square-size (measure best-rotated-polygon))

(def square-position
  (let [xs (map first best-rotated-polygon)
        ys (map second best-rotated-polygon)]
    (rotate-on square-angle [(apply min xs) (apply min ys)])))

(def square
  (let [side1 (rotate-on square-angle [square-size 0])
        side2 (rotate-on square-angle [0 square-size])
        [ x  y] square-position
        [x0 y0] [0 0]
        [x1 y1] side1
        [x2 y2] side2
        [x3 y3] [(+ x1 x2) (+ y1 y2)]]
    [(+ x0 x) (+ y0 y)
     (+ x1 x) (+ y1 y)
     (+ x3 x) (+ y3 y)
     (+ x2 x) (+ y2 y)]))


(defn draw-polygon []
  (q/fill 40)
  (q/no-stroke)
  (q/begin-shape)
  (doall (map (partial apply q/vertex) polygon))
  (q/end-shape :close))

(defn draw-square []
  (q/no-fill)
  (q/stroke 255 0 0)
  (apply q/quad square))

(defn draw []
  (q/background 255)
  (draw-polygon)
  (draw-square))

(defn -main []
  (q/defsketch hw-3
    :size [600 600]
    :draw draw))
