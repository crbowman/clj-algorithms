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







