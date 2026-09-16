(ns gitsnitch.util)

(defn pct
  "Percentage of n/total, rounded via fmt (default \"%.1f\"). Returns 0.0 when total is zero."
  ([n total] (pct n total "%.1f"))
  ([n total fmt]
   (if (pos? total)
     (Double/parseDouble (format fmt (* 100.0 (/ n total))))
     0.0)))
