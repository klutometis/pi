(ns pi.core
  (:use (incanter core stats charts datasets)))

(defn square [n] (* n n))

(def ^:dynamic data false)

(defn rho [n]
  (loop [m 0
         i n]
    (let [x (rand)
          y (rand)
          inside? (<= (+ (square x) (square y)) 1)]
      (when data (set! data (conj data {:x x :y y :inside? inside?})))
      (cond (zero? i) (/ m n)
            inside? (recur (+ m 1) (- i 1))
            :else (recur m (- i 1))))))

(defn pi [n] (* 4 (rho n)))

(defn plot-data [data]
  (let [data (to-dataset data)]
    (-> (scatter-plot (sel data :cols :x)
                      (sel data :cols :y)
                      :group-by (sel data :cols :inside?))
        (add-function (fn [x] (sqrt (- 1 (square x)))) 0 1))))

(defn -main [& args]
  (time (binding [data []]
          (let [pi (pi 10000)]
            (println pi (float pi))
            (view (plot-data data))))))
