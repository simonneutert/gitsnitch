(ns gitsnitch.unit.util-test
  (:require [clojure.test :refer [deftest is testing]]
            [gitsnitch.util :as util]))

(deftest pct-basic
  (is (= 50.0 (util/pct 1 2)))
  (is (= 33.3 (util/pct 1 3)))
  (is (= 100.0 (util/pct 3 3))))

(deftest pct-zero-total-is-zero
  (is (= 0.0 (util/pct 0 0)))
  (is (= 0.0 (util/pct 5 0))))

(deftest pct-custom-format
  (testing "fmt arg controls rounding precision"
    (is (= 33.33 (util/pct 1 3 "%.2f")))
    (is (= 33.0 (util/pct 1 3 "%.0f")))))
