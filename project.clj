(defproject com.xnlogic/system.check "0.1.0-SNAPSHOT"
  :description "Plug simulation capability in to test.check"
  :url "https://xnlogic.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/test.check "0.7.0"]
                 [org.clojure/math.combinatorics "0.1.1"]]
  :profiles
  {:dev
   {:dependencies [[org.clojure/core.match "0.3.0-alpha4"]]}})
