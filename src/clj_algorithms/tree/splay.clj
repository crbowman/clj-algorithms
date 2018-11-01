(ns clj-algorithms.tree.splay)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Splay Tree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct splay-node :left :key :val :right)

(defn get-splay-node [tree key]
  (cond
    (= key (:key tree))
      tree
    (< key (:key tree))
      (get-splay-node (:left tree) key)
    (> key (:key tree))
      (get-splay-node (:right tree) key)
    :else nil))

(defn left-of-parent [node parent]
  (= (:key node) (:key (:left parent))))

(defn right-of-parent [node]
  (= (:key node) (:key (:right (:parent node)))))

(defn splay-with-left-child [node]
  (let [b node
        a (b :left)
        y (a :right)]
    (let [b (assoc b :left (assoc y :parent b))
          a (assoc a :right b)]
      (let [a (assoc a :parent (:parent b))
            b (assoc b :parent a)])
      a)))

(defn splay-with-right-child [node]
  (let [a node
        b (a :right)
        y (b :left)]
    (let [a (assoc a :right (assoc y :parent a))
          b (assoc b :left a)]
      (let [b (assoc b :parent (:parent a))
            a (assoc a :parent b)])
      b)))

(defn splay [node]
  (let [par (:parent node)
        gpar (:parent (:parent node))]
    (if (not par)
      node
      (if (not gpar)
        (cond
         (left-of-parent node) (splay-with-left-child par)
         (right-of-parent node) (splay-with-right-child par))
        (cond
         (and (left-of-parent node) (left-of-parent par))
           (-> gpar
               (splay-with-left-child)
               (splay-with-left-child))
         (and (right-of-parent node) (right-of-parent par))
           (-> gpar
               (splay-with-right-child)
               (splay-with-right-child))
         (and (right-of-parent node) (left-of-parent par))
           (splay-with-left-child
            (:parent (splay-with-right-child par)))
         (and (left-of-parent node) (right-of-parent par))
           (splay-with-right-child
             (:parent (splay-with-left-child par))))))))

(defn splay-insert-helper
  [[key val] node par]
  (cond
   (not node)
     (struct splay-node nil key val par nil)
   (< key (node :key))
     (assoc node :left (splay-insert-helper [key val] (node :left) node))
   (> key (node :key))
     (assoc node :right (splay-insert-helper [key val] (node :right) node))))

(defn splay-insert
  ([[key val]]
     (struct splay-node nil key val nil nil))
  ([[key val] node]
     (cond
      (not node)
      (do (println "  splay-insert (not node)")
          (struct splay-node nil key val nil nil))
      (< key (node :key))
      (do (println "  splay-insert (< key (node :key))")
          (let [t (assoc node :left
                         (splay-insert-helper [key val] (node :left) node))]
            (println "    " t)
            (splay (get-splay-node t key))))
      (> key (node :key))
      (do (println "  splay-insert (> key (node :key))")
          (let [t (assoc node :right
                         (splay-insert-helper [key val] (node :right) node))]
            (splay (get-splay-node t key)))))))

(defn build-splay-tree [xs]
  "Builds a splay tree by iterating through xs, inserting
   each item in the tree"
  (loop [i 0
         t nil]
    (println  i " " t)
    (if  (>= i (count xs))
      t
      (recur (inc i)
             (splay-insert (nth xs i) t)))))

