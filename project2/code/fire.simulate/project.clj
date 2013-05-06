(defproject fire.simulate "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :profiles {:dev {:repl-options {:port 64431}}}
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.typed "0.1.14-20130506.164101-31"]
                 [org.clojure/tools.cli "0.2.2"]]
  :main fire.main)
