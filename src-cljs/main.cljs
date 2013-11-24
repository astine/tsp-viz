(ns main
  (:use [jayq.core :only [$ inner]])
  (:require-macros [tsp-viz.domacro :as domacro]))

(defn distance
  "Distance between two points on a two dimensional plane"
  [{x1 :x y1 :y}
   {x2 :x y2 :y}]
  (Math/sqrt (+ (Math/pow (- x1 x2) 2)
                (Math/pow (- y1 y2) 2))))

(defn rotate-path 
  "Given a route represented by a sequence of points, arrange them so point 1 comes first."
  [path index]
  (apply concat (reverse (split-at index path))))

(defn path-distance
  "Compute distance of a path"
  [path]
  (reduce + (map #(apply distance %) 
                 (conj (partition 2 1 path) 
                       (list (first path) (last path))))))

(defn nth-and-rest [index seq]
  (vector (nth seq index) (into (take index seq) (drop (inc index) seq))))

(defn least-in-seq [prefix seq]
  (loop [seq seq least (first seq)]
    (cond (empty? seq)
          least
          (< (prefix (first seq)) (prefix least))
          (recur (rest seq) (first seq))
          :else
          (recur (rest seq) least))))

;; Here's the simplest solution to the problem:
;;
;;This algorithm is O(!n) so it's not really worth optimizing it so that it's 
;;tail recursive.
(defn exhaust-permutations 
  "Returns a lazy-seq of lists representing every possible permutation of the items in 
   seq."
  [points]
  (when-let [s (seq points)]
    (if (> (count s) 1)
      (apply concat
             (for [index (range 0 (count s))]
               (lazy-seq
                (let [[item rest] (nth-and-rest index s)]
                  (for [list (exhaust-permutations rest)]
                    (conj list item))))))
      [s])))

(defn exhaust-permutations-less-head
  [points]
  (lazy-seq
   (when-let [s (seq points)]
     (for [list (exhaust-permutations (rest s))]
       (cons (first s) list)))))

(defn best-paths [paths & [old-path]]
  "Returns a lazy-seq of paths each representing the best path so far;
  ie. filter out all paths not better than previous paths."
  (lazy-seq
   (when-let [s (seq paths)]
     (if (or (empty? old-path)
              (< (path-distance (first s))
                 (path-distance old-path)))
       (cons (first s) (best-paths (rest s) (first s)))
       (best-paths (rest s) old-path)))))

(defn all-paths-plus-best [paths & [old-best]]
  (lazy-seq
   (if-let [s (seq paths)]
     (if (or (empty? old-best)
             (< (path-distance (first s))
                (path-distance old-best)))
       (cons (first s) (all-paths-plus-best (rest s) (first s)))
       (cons (first s) (all-paths-plus-best (rest s) old-best)))
     [old-best])))

;;opt sort
 
(defn points-to-edges 
  "Returns a list of pairs which represent all possible edges between
  points in passed in sequence of points."
  [path]
  (for [x (range 0 (count path)) 
        y (range (inc x) (count path))]
    [(nth path x) (nth path y)]))

(defn sort-edges [edges]
  (map :points
       (sort-by :length
                (map #(hash-map :points % 
                                :length (distance (first %) 
                                                  (second %)))
                     edges))))

(defn points-connected? 
  "Given an incomplete graph, show whether points p1 and p2 are connected."
  [graph p1 p2]
  (or (= (first (graph p1)) p2)
      (= (second (graph p1)) p2)
      (loop [point (first (graph p1)) last-point p1 false-on-nil false]
        (let [check-point (if (= (first (graph point)) last-point)
                            (second (graph point))
                            (first (graph point)))]
          (cond (= check-point p2)
                true
                (= check-point p1)
                false
                (= check-point nil)
                (if false-on-nil
                  false
                  (recur (second (graph p1)) p1 true))
                :else
                (recur check-point point false-on-nil))))))


(defn graph-to-path 
  "Covert graph to sequence."
  [graph]
  (domacro/do-graph [graph point nil result []]
            (conj result point)))

(defn shortest-edge-first
  "Builds the path by selecting connections between points first."
  [points]
  (let [p-count (count points)]
    (graph-to-path
     (loop [edges (sort-edges (points-to-edges points)) result-graph {}]
       (if (empty? edges)
         result-graph
         (let [[shortest & rest] edges
               [p1 p2] shortest]
           (if (and (< (count (result-graph p1)) 2)
                    (< (count (result-graph p2)) 2)
                    (or (and (= p-count (count result-graph))
                             (= (apply + (map count (vals result-graph))) 
                                (- (* (count result-graph) 2) 2)))
                        (not (points-connected? result-graph p1 p2))))
             (recur rest
                    (assoc result-graph
                      p1 (conj (result-graph p1) p2)
                      p2 (conj (result-graph p2) p1)))
             (recur rest
                    result-graph))))))))

(defn nearest-point-first
  "Builds the path by selecting nearest points first."
  [points]
  (let [[point points] (nth-and-rest (rand-int (count points)) points)]
    (loop [path [point] points points]
      (if (not-empty points)
        (let [point (least-in-seq #(distance % (peek path)) points)]
          (recur (conj path point) (remove #(= % point) points)))
        path))))
                      
(defn optimize 
  "This will remove obvious inefficiencies"
  [path]
  (if (> (count path) 3)
    (loop [path path 
           x 0
           y 2]
      (cond (= (count path) (inc y))
            (cond (= (+ x 3) y)
                  path
                  (> (distance (nth path x)
                               (nth path (inc x)))
                     (distance (nth path x)
                               (nth path y)))
                  (recur (into (take (inc x) path)
                               (reverse (drop (inc x) path)))
                         0 2)
                  :else
                  (recur path (inc x) (+ x 2)))
            (> (+ (distance (nth path x)
                            (nth path (inc x)))
                  (distance (nth path y)
                            (nth path (inc y))))
               (+ (distance (nth path x)
                            (nth path y))
                  (distance (nth path (inc x))
                            (nth path (inc y)))))
            (recur (reduce into [(take (inc x) path)
                                 (reverse (take (- y x) (drop (inc x) path)))
                                 (drop (inc y) path)]) 
                   0 2)
            :else
            (recur path x (inc y))))
    path))


(def points (atom #{}))

(def point-states (atom '()))

;(defn print-points []
  ;(-> (d3/select

(defn print-circles [points]
  ;(js/console (str (vec points)))
  (-> (d3/select "svg#field")
      (.selectAll "circle")
      (.remove))
  (-> (d3/select "svg#field")
      (.selectAll "circle")
      (.data (into-array points))
      (.enter)
      (.append "circle")
      (.transition)
      (.attr "cx" #(:x %))
      (.attr "cy" #(:y %))
      (.attr "r" 3)))

(def line-function 
  (-> (d3.svg/line)
      (.x #(:x %))
      (.y #(:y %))
      (.interpolate "linear-closed")))

(defn print-paths [paths]
  (when-let [paths (seq paths)]
    (console/log (str (first paths)))
    (-> (d3/select "svg#field")
        (.selectAll "path")
        (.transition)
        (.delay 100)
        (.duration 1000)
        (.attr "d" (line-function (into-array (first paths))))
        (.attr "stroke" "gray")
        (.attr "strokewidth" 2)
        (.attr "fill" "none")
        (.each "end" #(print-paths (rest paths))))))
      

(def svg (-> (d3/select "svg#field")
             (.on "click" #(this-as event 
                                    (swap! points conj 
                                           {:x (int (first (d3/mouse event)))
                                            :y (int (second (d3/mouse event)))})
                                    (console/log (str @points))
                                    (print-circles @points)
                                    (print-paths (all-paths-plus-best 
                                                  (exhaust-permutations-less-head 
                                                   @points)))))))


;swap! points
                    ;conj
                    ;(->> (d3/select "svg#field")
                         ;(d3/mouse)
                         ;(zipmap [:x :y])))))
                 
;(-> (d3/select "body")
    ;.transition
    ;(.duration 750)
    ;(.style "background-color" "white"))
