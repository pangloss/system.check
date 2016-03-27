(ns macroscale.system.check.util-test
  (:use clojure.test
        macroscale.system.check.util))


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

(deftest test-tmap
  (is (= 2 (tmap number? inc 1)))
  (is (= [2 3 4]
         (tmap number? inc [1 2 3])))
  (is (= [4 3 2]
         (tmap number? inc (range 1 4)))
      "sequences will be reversed")
  (is (= [11 [4 3 2] 101]
         (tmap number? inc [10 (range 1 4) 100]))
      "recursively map across various types")
  (is (= [11 [3 3 1] 101]
         (tmap even? inc [10 (range 1 4) 100]))
      "conditionally apply the transform"))
