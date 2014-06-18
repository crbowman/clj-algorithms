(ns ^{:author "Curtis Bowman"} clj-algorithms.graphs)

(def graph {:a #{:d}
            :b #{:c}
            :c #{:c :b :d :e} 
            :d #{:c}
            :e #{:c}
            :f #{}})

(defn ^:private edges-accumulator [[vertex neighbors] edges]
  (loop [es edges
         ns neighbors]
    (if (empty? ns)
      es
      (if (contains? es [(first ns) vertex])
        (recur es (rest ns))
        (recur (conj es [vertex (first ns)])
               (rest ns))))))

(defn edges [graph]
  (loop [edges #{}
         g graph]
    (if (empty? g)
      edges
      (recur (edges-accumulator (first g) edges)
             (rest g)))))

(defn vertices [graph]
  (keys graph))

(defn isolated-vertices [graph]
  (loop [iso nil
         g graph]
    (if (empty? g)
      iso
      (let [vertex ((first g) 0)
            nbrs ((first g) 1)]
        (recur
         (if (empty? nbrs)
           (cons vertex iso)
           iso)
         (rest g))))))

(defn add-vertex [graph vertex]
  (if (not (graph vertex))
    (assoc graph vertex #{})
    graph))

(defn add-edge-undirected [graph [v1 v2]]
  (-> (assoc graph v1
              (if (graph v1)
                (conj (graph v1) v2)
                [v2]))
      (assoc v2
             (if (graph v2)
               (conj (graph v2) v1)
               [v1]))))

(defn add-edge-directed [graph [v1 v2]]
  (-> (assoc graph v1
             (if (graph v1)
               (conj (graph v1) v2)
               [v2]))
      (add-vertex v2)))

(defn vertex-degree [graph vertex]
  (let [adjacents (graph vertex)]
    (if (contains? adjacents vertex)
      (inc (count adjacents))
      (count adjacents))))

(defn min-degree [graph]
  (loop [min (vertex-degree graph (first (vertices graph)))
         vs (rest (vertices graph))]
    (if (empty? vs)
      min
      (let [deg (vertex-degree graph (first vs))]
        (recur (Math/min deg min)
               (rest vs))))))

(defn max-degree [graph]
  (loop [max (vertex-degree graph (first (vertices graph)))
         vs (rest (vertices graph))]
    (if (empty? vs)
      max
      (let [deg (vertex-degree graph (first vs))]
        (recur (Math/max deg max)
               (rest vs))))))

(defn degree-seq [graph]
  (sort > (map #(vertex-degree graph %) (vertices graph))))

(defn density [graph]
  (let [E (count (edges graph))
        V (count (vertices graph))]
    (/ (* 2.0 E) (* V (dec V)))))
