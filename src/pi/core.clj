(ns pi.core
  (:use (incanter core charts)))

(defn square "Square x."
  [x] (* x x))

(def ^:dynamic data
  "Optionally collect data on the observations."
  false)

(defn rho [n]
  "Return an approximation to rho based on n observations; rho is pi /
4."
  (loop [m 0
         i n]
    (if (zero? i)
      (/ m n)
      (let [x (rand)
            y (rand)
            inside? (<= (+ (square x)
                           (square y))
                        1)]
        (when data (set! data (conj data {:x x :y y :inside? inside?})))
        (if inside?
          (recur (+ m 1) (- i 1))
          (recur m (- i 1)))))))

(defn pi [n]
  "Return an approximation to pi based on n observations."
  (* (rho n) 4))

(def default-n
  "The default number of observations."
  1000)

(defn plot-data [data]
  "Plot the observations along with the quarter circle."
  (let [data (to-dataset data)]
    (add-function
     (scatter-plot (sel data :cols :x)
                   (sel data :cols :y)
                   :group-by (sel data :cols :inside?))
     (fn [x] (sqrt (- 1 (square x))))
     0
     1)))

(defn -main
  "Main with optional n and a file for png."
  ([] (-main default-n))
  ([n] (-main n false))
  ([n png]
     (binding [data []]
       (let [n (if (string? n) (read-string n) n)
             pi (pi n)]
         (let [plot (plot-data data)]
           (if png (save plot png) (view plot)))
         (float pi)))))
