(ns clj-algorithms.graph.a-star
  (:import [java.awt Color Graphics Dimension]
           [java.awt.image BufferedImage]
           [javax.swing JPanel JFrame]))

(def dim 50)

(def scale 10)

(def grid-lines (concat (map (fn [x] [x 0 x (* scale dim)])
                             (map (partial * scale) (range 0 (inc dim))))
                        (map (fn [y] [0 y (* scale dim) y])
                             (map (partial * scale) (range 0 (inc dim))))))

(defstruct cell :occupied :visited :dest)

(def grid (apply vector
                 (map (fn [_]
                        (apply vector
                               (map (fn [_] (ref (struct cell false false false)))
                                    (range dim))))
                      (range dim))))

(defn cell [x y]
  (-> grid (nth x) (nth y)))

(defn rand-cell [dim]
  [(rand-int dim)
   (rand-int dim)])

(defn fill-cell [#^Graphics g cell c]
  (let [[x y] cell]
    (doto g
      (.setColor c)
      (.fillRect
       (inc (* x scale)) (inc (* y scale))
       (- scale 1) (- scale 1)))))

(defn draw-line [#^Graphics g line c]
  (let [[x1 y1 x2 y2] line]
    (doto g
      (.setColor c)
      (.drawLine x1 y1 x2 y2))))

(defn render [g]
  (let [img (new BufferedImage (inc (* scale dim)) (inc (* scale dim))
                 (. BufferedImage TYPE_INT_ARGB))
        bg (. img (getGraphics))]
    (doto bg
      (.setColor (. Color white))
      (.fillRect 0 0 (. img (getWidth)) (. img (getHeight))))
    (dorun
     (for [line grid-lines]
       (draw-line bg line (. Color lightGray))))
    (fill-cell bg [0 0] (. Color red))
    (fill-cell bg (rand-cell dim) (. Color green))
    (. g (drawImage img 0 0 nil))
    (. bg dispose)))

(def panel (doto (proxy [JPanel] []
                   (paint [g] (render g)))
             (.setPreferredSize (new Dimension
                                     (inc (* scale dim))
                                     (inc (* scale dim))))))

(def frame (doto (new JFrame) (.add panel) .pack .show))

