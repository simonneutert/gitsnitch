(ns gitsnitch.progress)

(def ^:private update-interval 200)

(defn wrap-progress
  "Wrap a lazy seq of commits with a stderr progress counter.
   total-hint is the expected total (or nil if unknown).
   Prints a \\r-overwritten line every `update-interval` items.
   Call `finish-progress!` after the seq is fully consumed."
  [commits total-hint]
  (let [counter (volatile! 0)]
    (map (fn [commit]
           (let [n (vswap! counter inc)]
             (when (or (= n 1) (zero? (mod n update-interval)))
               (binding [*out* *err*]
                 (if total-hint
                   (print (str "\r\u001b[2KProcessing commit " n "/" total-hint "..."))
                   (print (str "\r\u001b[2KProcessing commit " n "...")))
                 (flush))))
           commit)
         commits)))

(defn finish-progress!
  "Clear the progress line. Call after the commit seq is fully consumed."
  []
  (binding [*out* *err*]
    (print "\r\u001b[2K")
    (flush)))
