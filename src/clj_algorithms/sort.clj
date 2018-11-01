(ns clj-algorithms.sort)

(defmacro dbg [x]
  `(let [x# ~x]
     (println "dbg: " '~x " = " x#) x#))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Merge Sort ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn merge-sort [coll]
  "sort collection using the merge sort algorithm"
  (if (> (count coll) 1)
    (let [[x y] (split-at (/ (count coll) 2) coll)]
     (mrg (merge-sort x) (merge-sort y) []))
    coll))

(defn merge [[x & xrest :as X] [y & yrest :as Y] R]
  (if (and (not-empty X) (not-empty Y))
    (if (<= x y)
      (mrg xrest Y (conj R x))
      (mrg X yrest (conj R y)))
    (concat R X Y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Quicksort ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defn median-of-three [coll]
;;   (let [f {0 (first coll)}
;;         m {(/ (count coll) 2)  (coll (/ (count coll) 2))}
;;         l {(dec (count coll)) (last coll)}]
;;     (if )
;;     ))

(defn quicksort-first [[p & xs :as X]]
  "Quicksort algorithm that takes the first item in the sequence
   as its pivot. can cause a stack overflow error in the worst
   case (presorted input)"
  (if (> (count X) 1) ; base case
    (concat (quicksort (filter #(<= % p) xs))
            [p]
            (quicksort (filter #(> % p) xs)))
    X))

(defn quicksort-rand [xs]
  "Quicksort algorithm that takes its pivot at random, does worse
   than quicksort-first in the best base, but can handle presorted
   input"
  (if (> (count xs) 1)
    (let [i (rand-int (count xs))
          p (nth xs i)
          ys (concat (take i xs) (drop (inc i) xs))]
      (concat (quicksort-rand (filter #(<= % p) ys))
              [p]
              (quicksort-rand (filter #(> % p) ys))))
    xs))

;; (defn quicksort-median-of-three [xs]
;;   (if (> (count xs) 1)
;;     (let [p (median-of-three xs)]
;;       (concat ))))
