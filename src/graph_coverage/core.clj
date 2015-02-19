(ns graph-coverage.core)

(defn bfs-paths
  ([graph start-set]
   (bfs-paths graph (constantly true) start-set))
  ([graph filter-fn start-set]
   ((fn rec-bfs [frontier]
      (lazy-seq
       (when (seq frontier)
         (let [path (peek frontier)
               next-nodes ((:edges graph) (peek path))
               next-paths (map #(conj path %) next-nodes)
               filtered-paths (filter filter-fn next-paths)
               new-frontier (into (pop frontier) filtered-paths)]
           (cons path (rec-bfs new-frontier))))))
    (reduce conj clojure.lang.PersistentQueue/EMPTY (map vector start-set)))))

(defn bfs-paths-from-start [graph]
  (bfs-paths graph (:init graph)))

(defn bfs-all-paths [graph]
  (bfs-paths graph (:nodes graph)))

(def paths-from-start (memoize bfs-paths-from-start))
(def all-paths (memoize bfs-all-paths))

(defn subpath? [path subpath]
  (let [subpaths-of-same-length (partition (count subpath) 1 path)]
    (some #(= % subpath) subpaths-of-same-length)))

(defn path-len [path] (- (count path) 1))

(defn paths-of-len-upto-n [graph n]
  {:pre [(>= n 0)]}
  (take-while #(<= (path-len %) n) (all-paths graph)))

(defn directly-tours? [test-path path] (subpath? test-path path))

(defn tours-with-detours? [test-path path]
  (or (empty? path)
      (when (seq test-path)
        (let [[next-node rest-path] path
              [matching-node rest-test-path] (drop-while #(not= next-node %) test-path)]
        (tours-with-detours? rest-test-path rest-path)))))

(defn tours-with-sidetrips? [test-path path]
  (tours-with-detours? (partition 2 1 test-path) (partition 2 1 path)))

(defn tours?
  ([test-path path] (tours? test-path path :directly))
  ([test-path path type]
   ((condp = type
      :directly directly-tours?
      :sidestrips tours-with-sidetrips?
      :detours tours-with-detours?
      (constantly false))
    test-path path)))

(defn tours-all?
  ([test-path paths] (tours-all? test-path paths :directly))
  ([test-path paths type]
   (every? #(tours? test-path % type) paths)))

(defn has-path-coverage-upto-len-n? [graph n test-path]
  (tours-all? test-path (paths-of-len-upto-n graph n)))

(defn covers-nodes? [graph test-path]
  (has-path-coverage-upto-len-n? graph 0 test-path))

(defn covers-edges? [graph test-path]
  (has-path-coverage-upto-len-n? graph 1 test-path))

(defn is-simple-path? [path]
  (let [len-diff (- (count path) (count (distinct path)))]
    (or (zero? len-diff)
        (and (= len-diff 1)
             (= (first path) (last path))))))

(defn simple-paths [graph]
  (bfs-paths graph is-simple-path? (:nodes graph)))

(defn subpath-of-any-other? [paths subpath]
  (let [paths-without-subpath (disj (set paths) subpath)]
    (some #(subpath? % subpath) paths-without-subpath)))

(defn is-prime-path? [path paths]
  (not (subpath-of-any-other? paths path)))

(defn prime-paths [graph]
  (let [simple (simple-paths graph)]
    (filter #(is-prime-path? % simple) simple)))

(def prime-memo (memoize prime-paths))

(defn covers-prime-paths? [graph test-path]
  (tours-all? test-path (prime-memo graph)))

(defn test-path? [graph path] (and (contains? (:init graph) (first path))
                                   (contains? (:final graph) (last path))))

(defn test-paths [graph] (filter #(test-path? graph %) (paths-from-start graph)))

(defn tests-that-cover-edges-but-not-prime-paths [graph]
  (filter #(covers-edges? graph %)
          (filter #(not(covers-prime-paths? graph %))
                  (test-paths graph))))

(defn tests-that-cover-nodes-but-not-edges [graph]
  (filter #(covers-nodes? graph %)
          (filter #(not(covers-edges? graph %))
                  (test-paths graph))))
