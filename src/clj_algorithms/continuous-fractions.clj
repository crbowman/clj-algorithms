(ns clj-algorithms.continuous-fractions)


(defn gaus-to-frac [xs]
  (if (= 2 (count xs))
    (let [[x y] xs]
      (+ x (/ 1 y)))
    (let [rev   (vec (reverse xs))
          [y x & rem] rev
          rest (vec (reverse rem))
          frac (gaus-compose x y)]
      (gaus-to-frac (conj rest frac)))))


(defn frac-to-gaus
  ([frac]
   (frac-to-gaus frac []))
  ([frac gaus]
   (if (not (ratio? frac))
     (conj gaus (long frac))
     (let [n (numerator frac)
           d (denominator frac)
           q (long (quot n d))
           rem (/ 1 (- frac q))
           lets [n d q rem]]
       (frac-to-gaus rem (conj gaus q))))))


(frac-to-gaus (/ 45 16))
(gaus-to-frac [2 1 7])
(frac-to-gaus (/ 7 3))

