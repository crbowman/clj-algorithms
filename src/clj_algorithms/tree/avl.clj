(ns clj-algorithms.tree.avl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AVL Trees ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn height [node]
  "return height of a node, where the height is the number of
   edges from node to a leaf"
  (if node
    (if (node :height)
      (node :height)
      0)
    -1))

(defn balanced? [node]
  (cond
    (not node)
      true
    :else
    (and (<= (Math/abs (- (height (node :left))
                     (height (node :right))))
             1)
         (balanced? (node :left))
         (balanced? (node :right)))))

(defstruct avl-node :left :val :height :right)

(defn rotate-with-left-child [k2]
  (let [k1 (k2 :left)]
    (let [k2 (assoc k2 :left (k1 :right))
          k1 (assoc k1 :right k2)]
      (let [k2 (assoc k2 :height (inc (max (height (k2 :left))
                                           (height (k2 :right)))))
            k1 (assoc k1 :height (inc (max (height (k1 :left))
                                           (height k2))))]
        (assoc k1 :right k2)))))

(defn rotate-with-right-child [k1]
    (let [k2 (k1 :right)]
    (let [k1 (assoc k1 :right (k2 :left))
          k2 (assoc k2 :left k1)]
      (let [k1 (assoc k1 :height (inc (max (height (k1 :left))
                                           (height (k1 :right)))))
            k2 (assoc k2 :height (inc (max (height (k2 :right))
                                           (height k1))))]
        (assoc k2 :left k1)))))

(defn double-with-left-child [node] nil
  (->> (rotate-with-right-child (node :left))
       (assoc node :left )
       (rotate-with-left-child)))

(defn double-with-right-child [node] nil
  (->> (rotate-with-left-child (node :right))
       (assoc node :right )
       (rotate-with-right-child)))

(defn set-height [node]
  (assoc node :height (inc (max (height (node :left))
                                (height (node :right))))))

(defn avl-insert
  ([val]
     (struct avl-node nil val 0 nil))
  ([val node]
     (cond
       (not node)
         (struct avl-node nil val 0 nil)
       (< val (node :val))
         (let
           [n (assoc node :left (avl-insert val (node :left)))]
           (if (= (- (height (n :left)) (height (n :right))) 2)
               (if (< val ((n :left) :val))
                 (rotate-with-left-child n)
                 (double-with-left-child n)))
             (set-height n))
       (> val (node :val))
         (let [n (assoc node :right (avl-insert val (node :right)))]
           (if (= (- (height (n :right)) (height (n :left))) 2)
             (if (> val ((n :right) :val))
               (rotate-with-right-child n)
               (double-with-right-child n)))
           (set-height n)))))

(defn build-avl-tree [xs]
  "Builds a binary tree by iterating through xs, inserting
   each item in the tree"
  (loop [i 0
         tree nil]
    (if (>= i (count xs))
      tree
      (let [t (avl-insert (nth xs i) tree)]
        (recur (inc i) t)))))

