(defproject spire-win-rate "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
;;  :plugins [[cider/cider-nrepl "0.49.0"]]
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [cheshire "5.13.0"]]
  :main ^:skip-aot spire-win-rate.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
