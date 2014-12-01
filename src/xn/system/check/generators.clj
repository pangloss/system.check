(ns xn.system.check.generators
  (:require [clojure.core :as core]
            [clojure.test.check.generators :refer
             [char-alpha-numeric choose elements fmap frequency generator?
              make-gen one-of return tuple]])
  (:refer-clojure :exclude [keyword]))

(defn map-size
  "Create a new generator with `size` always `(f n)`."
  [f {gen :gen}]
  (make-gen
    (fn [rnd size]
      (gen rnd (f size)))))

(def char-alpha
  "Generate alpha characters."
  (fmap char
        (one-of [(choose 65 90)
                 (choose 97 122)])))

(def char-symbol-special
  "Generate non-alphanumeric characters that can be in a symbol."
  (elements [\* \+ \! \- \_ \?]))

(def char-keyword-rest
  "Generate characters that can be the char following first of a keyword."
  (frequency [[2 char-alpha-numeric]
              [1 char-symbol-special]]))

(def char-keyword-first
  "Generate characters that can be the first char of a keyword."
  (frequency [[2 char-alpha]
              [1 char-symbol-special]]))

(def keyword
  "Generate keywords."
  (->> (tuple char-keyword-first (vector char-keyword-rest))
       (fmap (fn [[c cs]] (core/keyword (clojure.string/join (cons c cs)))))))

(defprotocol LiteralGenerator
  (literal* [this]))

(defn literal
  "Create a generator from an arbitrary data structure with generators nested within
   it using regular clojure syntax.

   Example:

       (def lit (gen/literal {:coords {:x gen/int :y gen/int}}))
       (gen/sample lit 6)
       ;; => ({:coords {:x 0, :y 0}} {:coords {:x 0, :y -1}} {:coords {:x 0, :y -2}}
       ;; =>  {:coords {:x -3, :y -2}} {:coords {:x 1, :y 1}} {:coords {:x 0, :y 2}})"
  [lit]
  (cond
    (generator? lit) lit
    (satisfies? LiteralGenerator lit) (literal* lit)
    (vector? lit) (apply tuple (mapv literal lit))
    (map? lit) (fmap (partial into {}) (literal (mapv vec lit)))
    (set? lit) (fmap set (literal (vec lit)))
    (list? lit) (fmap (partial apply list) (literal (vec lit)))
    :else (return lit)))

(defn literal-record
  "Turn a record instance into a literal generator.

   This is most useful as the implementation of the LiteralGenerator procotocol
   for your record, ie:

       (defrecord MyRecord [x y z]
         gen/LiteralGenerator
         (literal* [this] (gen/literal-record map->MyRecord this)))

   Once that definition is in place, you may use the extended record anywhere inside a
   literal generator:

       (gen/literal [gen/int (->MyRecord :a gen/string {})])"
  [map->record record]
  (fmap map->record (literal (into {} record))))

