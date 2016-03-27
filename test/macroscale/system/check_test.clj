(ns macroscale.system.check-test
  (:use clojure.test)
  (:require [macroscale.system.check :refer [simulator gen-operations simulator*]]
            [clojure.test.check :as tc]
            [clojure.set :as set :refer [map-invert]]
            [clojure.core.match :refer [match]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :as ct :refer (defspec)]))

(defn incorrect? [s result]
  (when-not (:t? s)
    (not= (into #{} (keys (:contents s))) result)))

(defn transient?  [x]
  (instance? clojure.lang.ITransientCollection x))

(def test-set
  (simulator
    {:initial-state
     (fn initial-state [] {:contents {} :t? false})
     :initial-target
     (fn initial-target [] #{})
     :run-command?
     (fn pre [{:keys [shrink t?]} [_ f _]]
       (condp = f
         `conj        (not t?)
         `disj        (not t?)
         `transient   (not t?)
         `conj!       t?
         `disj!       t?
         `persistent! t?))
     :next-state
     (fn next-state [state command result]
       (match
         command
         [_ `transient []] (assoc state :t? true)
         [_ `persistent! []] (assoc state :t? false)
         [_ `conj [v]] (update-in state [:contents] assoc v true)
         [_ `disj [v]] (update-in state [:contents] dissoc v)
         [_ `conj! [v]] (update-in state [:contents] assoc v true)
         [_ `disj! [v]] (update-in state [:contents] dissoc v)))
     :error?
     (fn error? [{:keys [contents] :as state} [_ f [arg]] result]
       (when (set? result)
         (condp = f
           `conj
           (cond (not= arg (result arg)) (str "Argument `" arg "` should be present in the result")
                 (incorrect? state result) "Expected state does not match result")
           `conj!
           (cond (not= arg (result arg)) (str "Argument `" arg "` should be present in the result")
                 (incorrect? state result) "Expected state does not match result")
           `disj
           (when (incorrect? state result) "Expected state does not match result")
           `disj!
           (when (incorrect? state result) "Expected state does not match result")
           `transient
           (when (not (transient? result)) "Result should be transient")
           `persistent!
           (when (transient? result) "Result should not be transient"))))}
    [{:keys [contents t?]}]
    (not t?) [:-> `conj [gen/int]]
    (and (not t?) (seq contents)) [:-> `disj [(gen/elements (vec (keys contents)))]]
    (not t?) [:-> `transient []]
    t? [:-> `persistent! []]
    t? [:-> `conj! [gen/int]]
    (and t? (seq contents)) [:-> `disj! [(gen/elements (vec (keys contents)))]]))


; With size > 100, this will almost certainly fail on Clojure 1.5.1:
(defspec transient-state-test 100 test-set)

(comment

  (tc/quick-check 1000 test-set)

  )


; =======================================================================================
; =======================================================================================
; This example is reproducing an example from erlang and isn't meant to be run beyond
; just generating the tests.

(defrecord State [^java.util.Set pids
                  ^java.util.Map regs
                  ^java.util.Set killed])

(def sim-config
  {:initial-state (fn initial-state [] (State. #{} {} #{}))
   :next-state
   (fn next-state [{:keys [killed regs] :as state} command result]
     (match command
            [_ `spawn _] (update-in state [:pids] conj result)
            [_ `kill [pid]] (-> state
                                (update-in [:killed] conj pid)
                                (update-in [:regs] #(dissoc % ((map-invert %) pid))))
            [_ `reg [n pid]] (if (and (not (killed pid))
                                      (not (regs n)))
                               (assoc-in state [:regs n] pid)
                               state)
            [_ `unreg [n]] (update-in state [:regs] dissoc n)
            [_ `proc_reg/where [n]] state
            :else (do (println "Unmatched command:")
                      (prn command)
                      state)))
   :postcondition
   (fn postcondition [{:keys [regs]} command result]
     (match [command result]
            [[_ `reg [n pid]] true] (not (regs n))
            [[_ `reg [n pid]] [:sim/exit _]] (regs n)
            [[_ `unreg [n]] true] (regs n)
            [[_ `unreg [n]] [:sim/exit _]] (not (regs n))
            [[_ `where [n]] _] (= (regs n) result)
            :else true))})


; result is a black box because it may be a result or it may be a var object
; when generating tests. Args that are generated from results are the same.


(def sim-gen
  (gen-operations
    sim-config
    [{:keys [pids regs] :as state}]
    true       [:apply `spawn []]
    (seq pids) [:apply `kill  [(gen/elements (vec pids))]]
    (seq pids) [:apply `reg [(gen/resize 1 gen/keyword) (gen/elements (vec pids))]]
    true       [:apply `unreg [(if (seq regs)
                                 (gen/one-of [(gen/resize 1 gen/keyword)
                                              (gen/elements (vec (keys regs)))])
                                 (gen/resize 1 gen/keyword))]]
    true       [:apply `proc_reg/where [(if (seq regs)
                                          (gen/one-of [(gen/resize 1 gen/keyword)
                                                       (gen/elements (vec (keys regs)))])
                                          (gen/resize 1 gen/keyword))]]))


(comment

  ; FIXME: not all generators are being evaluated...
  (clojure.pprint/pprint
    (gen/sample
      sim-gen
      10))

  (gen/call-gen sim-gen (gen/random) 0)

  )

; ====================================================
; Test set operations with union and difference

(defn set2-conj [data k v]
  (update-in data [k] conj v))

(defn set2-disj [data k v]
  (update-in data [k] disj v))

(defn set2-union [data a b]
  (update-in data [a] set/union (b data)))

(defn set2-union2 [data a s]
  (update-in data [a] set/union s))

(defn set2-difference [data a b]
  (update-in data [a] set/difference (b data)))

(defn set2-difference2 [data a s]
  (update-in data [a] set/difference s))

(defn to-map [keys]
  (reduce #(assoc %1 %2 true) {} keys))

(def counter (atom 0))

(def set2-config
  {:initial-state
   (fn []
     {:a {:model [] :data #{}}
      :b {:model [] :data #{}}})
   :initial-target
   (fn []
     (when (= 0 (mod @counter 100)) (println (str "> " @counter)))
     (swap! counter inc)
     {:a #{} :b #{}})
   :next-state
   (fn [state [_ _ [target & _] :as command] result]
     (let [state (assoc-in state [target :data] (target result))]
       (match
         command
         [:-> `set2-conj [_k _v]]
         (update-in state [_k :model] (comp vec distinct conj) _v)
         [:-> `set2-disj [_k _v]]
         (update-in state [_k :model] (fn [m] (vec (distinct (remove #(= _v %) m)))))
         [:-> `set2-union [_a _b]]
         (update-in state [_a :model]
                    (fn [a b] (vec (distinct (concat a b))))
                    (get-in state [_b :model]))
         [:-> `set2-union2 [_a _s]]
         (update-in state [_a :model]
                    #(vec (distinct (concat % _s))))
         [:-> `set2-difference [_a _b]]
         (update-in state [_a :model]
                    (fn [a b] (vec (remove b a)))
                    (to-map (get-in state [_b :model])))
         [:-> `set2-difference2 [_a _s]]
         (update-in state [_a :model]
                    (fn [a b] (vec (remove b a)))
                    (to-map _s)))))
   :postcondition
   (fn [state command result]
     (match
       command
       [:-> (:or `set2-union `set2-difference `set2-union2 `set2-difference2) [_a _b]]
       (= (to-map (get-in state [_a :model]))
          (to-map (get-in state [_a :data])))
       :else true))})

(defn gen-elements [{:keys [model]}]
  (gen/elements model))

(def gen-set
  (gen/fmap set (gen/vector gen/int)))

(def set2
  (letfn [(data? [{:keys [model]}] (seq model))]
    (simulator
      set2-config
      [{:keys [a b]}]
      true [:-> `set2-conj [:a gen/int]]
      true [:-> `set2-conj [:b gen/int]]
      (data? a) [:-> `set2-conj [:b (gen-elements a)]]
      (data? b) [:-> `set2-conj [:a (gen-elements b)]]
      (data? a) [:-> `set2-disj [:a (gen-elements a)]]
      (data? b) [:-> `set2-disj [:b (gen-elements b)]]
      true [:-> `set2-union2 [:a gen-set]]
      true [:-> `set2-union2 [:b gen-set]]
      true [:-> `set2-difference2 [:a gen-set]]
      true [:-> `set2-difference2 [:b gen-set]]
      true [:-> `set2-union [:a :b]]
      true [:-> `set2-union [:b :a]]
      true [:-> `set2-difference [:a :b]]
      true [:-> `set2-difference [:b :a]])))

(defspec test-set-union-and-difference 10 set2)
