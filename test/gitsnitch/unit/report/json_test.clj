(ns gitsnitch.unit.report.json-test
  (:require [clojure.test :refer [deftest is]]
            [cheshire.core :as cheshire]
            [gitsnitch.report.json :as json]))

(deftest render-json-round-trips
  (let [data {:total-commits 3 :rows [{:path "a.clj" :changes 2}]}]
    (is (= data (cheshire/parse-string (json/render-json data) true)))))

(deftest render-json-is-pretty-printed
  (is (re-find #"\n" (json/render-json {:a 1}))))
