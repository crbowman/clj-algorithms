(ns clj-algorithms.tree.binary)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Binary Search Trees ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct binary-node :val :left :right)

(defn binary-insert
  "Insert val into binary tree at node root, if root is
   not supplied create a new binary tree with one node."
  ([val]
     (struct binary-node val nil nil))
  ([val root]
     (cond
      (nil? root) (struct binary-node val nil nil)
      (< val (root :val)) (assoc root :left (binary-insert val (root :left)))
      (> val (root :val)) (assoc root :right (binary-insert val (root :right)))
      (= val (root :val)) root)))

(defn build-binary-tree
  "Recursively builds a binary tree out of the elements in xs"
  ([xs]
   (if (empty? xs)
     nil
     (build-tree (rest xs) (binary-insert (first xs)))))
  ([xs node]
   (if (empty? xs)
     node
     (build-tree (rest xs) (binary-insert (first xs) node)))))

(defn binary-search-tree? [node]
  "True if the tree satisfies the binary search property."
  (cond
    (and (node :left) (node :right))
      (and (> (node :val) ((node :left) :val))
           (< (node :val) ((node :right) :val))
           (binary-search-tree? (node :left))
           (binary-search-tree? (node :right)))
    (node :left)
      (and (> (node :val) ((node :left) :val))
           (binary-search-tree? (node :left)))
    (node :right)
      (and (< (node :val) ((node :right) :val))
           (binary-search-tree? (node :right)))
    (node :val) true
    :else false))

(defn leaf? [node]
  "True if node is a leaf node."
  (and (nil? (:left node))
       (nil? (:right node))))

(defn print-binary-tree
  ([node]
   (do
     (print-binary-tree (node :right) 1)
     (println (node :val))
     (print-binary-tree (node :left) 1)))
  ([node depth]
   (let [padding (clojure.string/join (repeat depth "    "))]
     (if (nil? node)
       (println padding ".")
       (do
         (print-binary-tree (node :right) (inc depth))
         (println padding (node :val))
         (print-binary-tree (node :left) (inc depth)))))))

(defn binary-contains? [node val]
  "True if the binary tree at node contains a node
   with the value val."
  (cond
    (not node) nil
    (< val (node :val)) (binary-contains? (node :left) val)
    (> val (node :val)) (binary-contains? (node :right) val)
    :else true))

(defn binary-find-min [node]
  "returns the smallest value in the
   binary tree."
  (cond
   (not node) nil
   (node :left) (binary-find-min (node :left))
   :else (node :val)))

(defn binary-find-max [node]
  "returns the largest value from the
   binary tree."
  (cond
   (not node) nil
   (node :right) (binary-find-max (node :right))
   :else (node :val)))

(defn binary-delete-min [node]
  "Delete the minimum value in a binary tree. Returns a vector with the
   value that was deleted, and the new tree without that value."
  (cond
    (nil? (node :left))
      [(node :val) (node :right)]
    (leaf? (node :left))
      [((node :left) :val) {:left nil :val (node :val) :right (node :right)}]
    :else
      (let [[min left] (binary-delete-min (node :left))]
        [min {:left left :val (node :val) :right (node :right)}])))

(defn binary-delete [node val]
  "Delete a value from a binary tree."
  (cond
    (< val (node :val))
    (assoc node :left (binary-delete (node :left) val))
    (> val (node :val))
    (assoc node :right (binary-delete (node :right) val))
    :else
    (cond
      (leaf? (node :right)) (node :left)
      (leaf? (node :left)) (node :right)
      :else (let [[min right] (binary-delete-min (node :right))]
              {:left (node :left) :val min :right right}))))
