(ns tsp-viz.domacro)

(defmacro round-to 
  "Rounds number to places decimal places"
  [number places]
  (let [multiplier-form `(Math/pow 10 ~places)
        multiplier (if (number? places) (eval multiplier-form) multiplier-form)]
    `(let [multiplier# ~multiplier]
       (* (float (~'Math/round (* ~number multiplier#))) (float (/ 1 multiplier#))))))

(defmacro do-graph 
  "Executes body for every member of graph in path order. The result of each 
  iteration is bound to result each next iteration, so a return value can be 
  accumulated by return a function of result each iteration. Result is 
  initially bound to result-value. Start-point is the point in the graph to 
  start operating from."
  [[graph point & [start-point result result-value next-point]] & body]
  `(let [start-point# (or ~start-point (first (keys ~graph)))
         graph# ~graph]
     (loop [~point start-point# last-point# nil ~(or result (gensym)) ~result-value ]
       (assert ~point)
       (assert (= (count (graph# ~point)) 2))
       (let [next-point# (cond (= last-point# nil)
                               (first (graph# ~point))
                               (= last-point# (first (graph# ~point)))
                               (second (graph# ~point))
                               (= last-point# (second (graph# ~point)))
                               (first (graph# ~point))
                               :else 
                               (throw (RuntimeException. 
                                       (str "One way edge in graph: " 
                                            last-point# " " ~point " "
                                            (print-indexes graph#)))))
             result# (do ~@body)]
         (if (= next-point# start-point#)
           result#
           (recur next-point# ~point result#))))))
