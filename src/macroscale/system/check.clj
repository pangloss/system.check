(ns macroscale.system.check
  (:require [clojure.test.check.generators :as gen]
            [macroscale.system.check.generators :as sc-gen]
            [macroscale.system.check.util :as util]
            [clojure.test.check.properties :refer [for-all*]]
            [clojure.test.check.rose-tree :as rose]
            [clojure.test.check.random :refer [split-n]]
            [clojure.math.combinatorics :refer [combinations]]))

(defn variable [n]
  (with-meta (symbol (str "v" n)) {::var true}))

(defn variable? [v]
  (and (symbol? v) (::var (meta v))))

(defn idx-level
  "Arbitrary but stable way to quantify any index in a set from 0-1 to use together with command frequency."
  [idx indices]
  (let [top (apply max indices)]
    (if (zero? top)
      0
      (/ idx top))))

(defmacro state-command [state idx bindings commands indices]
  (let [commands (mapv (fn [[cond command]] `(when ~cond ~command)) commands)
        commands (condp = (count bindings)
                   1 `(let [~(first bindings) ~state] ~commands)
                   2 `(let [~(first bindings) ~state ~(second bindings) (variable :init)] ~commands)
                   (assert false "Generate commands with either [state] or [state-map initial-value-var] bindings."))]
    `(let [commands# (->> ~commands (filter identity) vec)]
       (when (seq commands#)
         (loop [idx# ~idx]
           (let [freq-idx# (idx-level idx# ~indices)
                 command# (nth commands# (mod idx# (count commands#)))
                 freq# (:freq (meta command#))]
             (if freq#
               (if (< freq-idx# (Math/abs freq#))
                 command#
                 (let [n# (long (* idx# (min freq-idx# 0.99)))]
                   (recur n#)))
               command#)))))))

(defn get-command* [op-rose]
  (second (rose/root op-rose)))

(defn subsets-rose [items]
  (rose/make-rose
    items
    (mapcat (fn [n]
              (map (comp subsets-rose vec)
                   (combinations items n)))
            (util/unchunk (util/reverse-range 1 (count items))))))

(defn extract-vars [root]
  (filter variable? (tree-seq coll? seq root)))

(defn shrink-operations* [{:keys [initial-state run-command? next-state]} op-roses]
  ; Build a custom rose tree that more effectively searches the space
  ; - search pairs then triples, etc.
  ; - relative sequence is preserved
  ; - discard combinations where a var used by one command is not available
  ; - understand preconditions and skip compositions with failing ones
  ; - shrink size before changing any arguments
  (let [size (count op-roses)
        init-var (variable :init)]
    (->> (subsets-rose (range (count op-roses)))
         (rose/fmap
           (fn [indices]
             (let [selected-roses (mapv #(nth op-roses %) indices)
                   operations (map rose/root selected-roses)
                   available-vars (into #{init-var} (map first operations))
                   commands (map second operations)
                   used-vars (extract-vars commands)]
               (when (and (every? available-vars used-vars)
                          (or (not run-command?)
                              (reduce (fn [state [var command]]
                                        (if (run-command? state command)
                                          (next-state state command var)
                                          (reduced nil)))
                                      (assoc (initial-state) ::shrink true)
                                      operations)))
                 ; Hopefully this does the following:
                 ; each rose should be [operations [less operations ...
                 ;                                  shrunk operations ...]]
                 ; first shrink by number of commands. If we pass all of those, shrink by operations
                 (rose/zip vector selected-roses)))))
         (rose/filter #(and % (rose/root %)))
         rose/join
         (rose/filter identity))))

(def index-generator
  (->> gen/pos-int
       (sc-gen/map-size #(* % 100000))
       gen/no-shrink
       gen/vector
       (sc-gen/map-size #(inc (* 2 %)))))

; - generate a vector of positive ints
; - inside the state loop, working through those ints:
;   - generate a command corresponding to the int given the current state.
;   - the command may have generators embedded.
;   - Turn the command into its own rose tree so that the command's arguments can be shrunk.
;   - use the root version of the command to progress to the next state
; - we now have a list of rose trees, one for each command.
; - combine those commands into one rose tree for this test which follows the ideas set out in shrink-operations*
(defmacro gen-operations [sim bindings & commands]
  ; TODO:
  ; - Allow a literal prefix of commands for setup to be passed in. Never shrunk.
  (assert (even? (count commands)) "Incorrect condition command pair.")
  (let [commands (partition 2 commands)]
    `(let [sim# ~sim
           initial-state# (:initial-state sim#)
           _# (assert (fn? initial-state#) "Simulation must specify :initial-state function")
           next-state# (:next-state sim#)
           max-size# (:max-size sim# 50)]
       (assert (fn? next-state#) "Simulation must specify :next-state function")
       (sc-gen/make-gen
         (fn [rnd# size#]
           (let [indices# (rose/root (gen/call-gen index-generator rnd# size#))
                 [op-roses# _# _#]
                 (reduce (fn [[op-roses# state# counter#] [rnd# idx#]]
                           (let [var# (variable counter#)
                                 command# (state-command state# idx# ~bindings ~commands indices#)
                                 operation# [var# command#]
                                 operation-rose# (gen/call-gen (sc-gen/literal operation#)
                                                               rnd#
                                                               (mod size# max-size#))]
                             [(conj op-roses# operation-rose#)
                              (next-state# state# (get-command* operation-rose#) var#)
                              (inc counter#)]))
                         [[] (initial-state#) 0]
                         (map vector (split-n rnd# (count indices#)) indices#))]
             (shrink-operations* sim# op-roses#)))))))

(defn prepare-command [target vars [method f args]]
  [method f (util/tmap variable? vars args)])

(defn eval-command [ns target vars [method f args]]
  (let [f' (if (and (not= :custom method) (symbol? f))
             (ns-resolve ns f)
             f)]
    ;(prn (name f))
    (if f'
      (try
        (condp = method
          :apply (apply f' args)
          :-> (apply f' target args)
          :->> (apply f' (concat args [target]))
          :custom (target vars [f args]))
        (catch Throwable t t))
      (throw (ex-info (str "Unable to resolve function " f) {:function f :method method :args args})))))

(defn error-message [{:keys [error cause] :as data}]
  (cond
    error (str "Error detected: " error)
    (instance? Throwable cause) (str "Simulation exception: " (.getMessage ^Throwable cause))
    :else "Postcondition failed"))

(defn quiet-on-error [{:keys [error cause] :as data}]
  (let [message (error-message data)
        data (select-keys data [:var :state :target :result :command])]
    (if cause
      (throw (ex-info message data cause))
      (throw (ex-info message data)))))

(defn on-error [data]
  (binding [*out* *err*]
    (println "\nFailure with " (count (:fail data)) " commands: " (error-message data))
    (when (> 20 (count (:fail data)))
      (doseq [c (:fail data)] (print "  ") (prn c))))
  (quiet-on-error data))

(defn error? [state command result]
  (when (instance? Throwable result)
    (.getMessage ^Throwable result)))

(defn keep-result-var?
  "When a method would be expected to give a bad result which is not a failure,
   you don't want subsequent tests to fail because the result of this expression
   is invalid. This method can control that."
  [state command result]
  (not (instance? Throwable result)))

(defn runner
  "Required: initial-state next-state
   Optional:
     run-command? - default to no precondition
     postcondition - default to no checks for bad state after command execution and state transition
     error? - default to no checks for bad state after command execution and state transition
   _
   If included, run-command? must return true for the command to be executed. It's
   useful for general state checks and also to validate functions when shrinking and some
   setup may have been removed.
   _
   postcondition or error? are interchangeable. error? allows you to return an
   actual error message, while postcondition just returns true if the state is
   ok, which is easier to write!
   _
   Optional behaviour configuration:
     ns - allows 'symbol to be resolved in the given namespace, in addition to `symbol.
     reduce - allows you to switch between reduce and reductions
     keep-result-var? - defaults that any Throwable is not to be stored in a Var
     on-error - default prints error and shrinking progress and throws ex-info
     eval-command - [state target vars [method f args]] -> (apply f args)"

  [{:keys [initial-state run-command? next-state postcondition ns] :as sim}]
  (assert (fn? initial-state) "Simulation must specify :initial-state function")
  (assert (fn? next-state) "Simulation must specify :next-state function")
  (let [eval-command     (get sim :eval-command eval-command)
        prepare-command  (get sim :prepare-command prepare-command)
        on-error         (get sim :on-error on-error)
        error?           (get sim :error? error?)
        initial-target   (get sim :initial-target (constantly nil))
        cleanup          (get sim :cleanup (constantly nil))
        keep-result-var? (get sim :keep-result-var? keep-result-var?)
        reduce           (get sim :reduce reduce)
        ns               (cond (symbol? ns)
                               (do (require ns)
                                   (the-ns ns))
                               ns ns
                               :else (the-ns 'user))]
    (fn [operations]
      (try
        (let [init-target (initial-target)
              vars (atom {(variable :init) init-target}) ]
          (try
            (let [state
                  (reduce
                    (fn [[state target :as ignore] [v pre-command]]
                      (try
                        (let [used-vars (extract-vars pre-command)
                              available-vars (set (keys @vars))
                              command (prepare-command target @vars pre-command)]
                          (if (and (every? available-vars used-vars)
                                   (or (not run-command?)
                                       (run-command? state command)))
                            (let [result (eval-command ns target @vars command)
                                  keep? (keep-result-var? state command result)
                                  state' (next-state state command result)]
                              (try
                                (let [failed (when postcondition (not (postcondition state' command result)))
                                      error (error? state' command result)]
                                  (when keep?
                                    (swap! vars assoc v result))
                                  (if (or error failed)
                                    (on-error {:error error :var v :vars @vars :fail operations
                                               :pre-state state :state state'
                                               :pre-command pre-command :command command
                                               :target target :result result})
                                    [state' result]))
                                (catch Throwable t
                                  (if (:state (ex-data t))
                                    (throw t)
                                    (on-error {:var v :vars @vars :fail operations
                                               :pre-state state :state state'
                                               :pre-command pre-command :command command
                                               :target target :result result :cause t})))))
                            ignore))
                        (catch Throwable t
                          (if (:state (ex-data t))
                            (throw t)
                            (on-error {:var v :vars @vars :fail operations :state state
                                       :command pre-command :target target :cause t})))))
                    [(assoc (initial-state) ::run true) init-target]
                    operations)]
              (try (cleanup init-target nil state)
                   (catch Throwable e
                     (prn e)))
              state)
            (catch Throwable e
              (cleanup init-target e nil)
              (throw e))))))))

(defn simulator* [sim sim-gen]
  (for-all* [sim-gen] (runner sim)))

(defmacro simulator
  "See arguments to runner and gen-operations."
  [sim & stuff]
  `(let [sim# ~sim
         ops# (gen-operations sim# ~@stuff)]
     (assoc (simulator* sim# ops#)
            :gen-operations ops#)))

(defn ->clj
  "Transform a list of commands into valid clojure.

   Currently only works with commands structured [:apply `f [& args]]

   Example:

   (def result-clj (->clj (get-in result [:shrunk :smallest])))
   (pprint result-clj)
   (eval result-clj)
   "
  [[commands] & {:keys [trace]}]
  (letfn [(->apply [[v [_ f args]]]
            `[~v (~f ~@args)])]
    (let [commands
          `(let ~(cond->> commands
                  true (map ->apply)
                  trace (mapcat (fn [[v c]]
                                  `[[~'_ (do (print "\n>>>>>> ") (prn '~c))]
                                    [~v ~c]
                                    [~'_ (do (print "   > ") (prn ~v))]]))
                  true (apply concat)
                  true vec)
            ~(first (last commands)))]
      `(fn [~'v:init]
         (let [~'result ~commands]
           [~'v:init ~'result])))))
