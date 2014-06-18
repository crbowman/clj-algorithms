(ns clj-algorithms.trees)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Binary Search Trees ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct binary-node :left :val :right)

(defn binary-insert
  "Insert val into binary tree at node root, if root is
   not supplied create a new binary tree with one node"
  ([val]
     (struct binary-node nil val nil))
  ([val root]
     (cond
      (not root)
        (struct binary-node nil val nil)
      (< val (root :val))
        (assoc root :left (binary-insert val (root :left)))
      (> val (root :val))
        (assoc root :right (binary-insert val (root :right))))))

(defn build-binary-tree [xs]
  "Builds a binary tree by iterating through xs, inserting
   each item in the tree"
  (def tree nil)
  (loop [i 0]
    (when (< i (count xs))
      (do 
        (def t (binary-insert (nth xs i) tree))
        (def tree t)
        (recur (inc i)))))
  tree)

(defn binary-tree? [node]
  "True if node is the root of a binary tree"
  (cond
    (and (node :left) (node :right))
      (and (> (node :val) ((node :left) :val))
           (< (node :val) ((node :right) :val))
           (binary-tree? (node :left))
           (binary-tree? (node :right)))
    (node :left)
      (and (> (node :val) ((node :left) :val))
           (binary-tree? (node :left)))
    (node :right)  
      (and (< (node :val) ((node :right) :val))
           (binary-tree? (node :right)))
    (node :val)
      true
    :else
      false))

(defn leaf? [node]
  "True if node is a leaf node"
  (and (not (:left node))
       (not (:right node))))

(defn print-binary-tree
  ([node]
     (if node
       (do
         (println "* " (node :val))
         (if (node :left)
           (print-binary-tree (node :left) 1))
         (if (node :right)
           (print-binary-tree (node :right) 1)))))
  ([node depth]
     (if node
       (do
         (let [tabs (clojure.string/join (repeat depth "  "))]
           (println tabs "* " (node :val)))
         ( if (node :left)
           (print-binary-tree (node :left) (inc depth)))
         (if (node :right)
           (print-binary-tree (node :right) (inc depth)))))))

(defn binary-contains? [node val]
  "True if the binary tree at node contains a node 
   with the value val"
  (cond
   (not node)
     nil
   (< val (node :val))
     (binary-contains? (node :left) val)
   (> val (node :val))
     (binary-contains? (node :right) val)
   :else
     true))

(defn binary-find-min [node]
  "returns the node with the smallest value from the
   binary tree at node"
  (cond
   (not node)
     nil
   (node :left)
     (binary-find-min (node :left))
   :else
     node))

(defn binary-find-max [node]
  "returns the node with the greatest value from the 
   binary tree at node"
  (cond
   (not node)
     nil
   (node :right)
     (binary-find-max (node :right))
   :else
     node))

(defn replace-node-in-parent [par node child]
  "removes node from the binary tree by replacing parent par's 
   reference to node with a reference to node's child"
  (cond
    (= ((par :left) :val) (node :val))
      (assoc par :left child)
    (= ((par :right) :val) (node :val))
      (assoc par :right child)))

(defn binary-remove
  "removes the node with the value val from the binary tree
   at node. Ensures that the tree remains a binary tree"
  ([node val]
     (cond
       (not (binary-contains? node (node :val)))
         node
       (< val (node :val))
         (binary-remove (node :left) val node)
       (> val (node :val))
         (binary-remove (node :right) val node)
       :else
         (binary-remove node val nil)))
  ([node val par]
     (cond
       (< val (node :val))
         (binary-remove (node :left) val node)
       (> val (node :val))
         (binary-remove (node :right) val node)
      :else
        (cond
          (and (node :left) (node :right))
            (let [succ (binary-find-min (node :right))]
              (assoc node :val (succ :val))
              (binary-remove (node :right) (succ :val) node))
          (node :left)
            (replace-node-in-parent par node (node :left))
          (node :right)
            (replace-node-in-parent par node (node :right))
          :else
            (replace-node-in-parent par node nil)))))

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




