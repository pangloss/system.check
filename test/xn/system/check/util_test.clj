(ns xn.system.check.util-test
  (:use clojure.test)
  (:require [xn.system.check.util :refer [unchunk reverse-range]]))


(deftest test-reverse-range
  (is (= (reverse-range 1 10)
         (reverse (range 1 10)))))

(deftest test-unchunk
  (testing "SANITY: normal has numerous side effect"
    (let [data (atom [])]
      (first (map #(do (swap! data conj %) %) (range 10)))
      (is (= (range 10) @data))))
  (testing "unchunked only has 1 side effect"
    (let [data (atom [])]
      (first (map #(do (swap! data conj %) %) (unchunk (range 10))))
      (is (= [0] @data)))))

