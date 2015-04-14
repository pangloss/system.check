(ns xn.system.check.util)

(defn unchunk
  "Borrowed from math.combinatorics"
  [s]
  (lazy-seq
   (when (seq s)
     (cons (first s) (unchunk (rest s))))))

(defn reverse-range [low high]
  (range (dec high) (dec low) -1))

(defn- tmap-empty [c]
  (if (record? c) c (or (empty c) [])))

(defn tmap
  "Map over a tree of data"
  [recurse? apply? f c]
  (cond
    (recurse? c)
    (into (tmap-empty c)
          (map #(tmap recurse? apply? f %)
               c))
    (apply? c)
    (f c)
    :else
    c))

