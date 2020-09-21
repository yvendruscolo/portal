(ns user
  (:require [examples.data :refer [data]]
            [clojure.repl :as repl]
            [portal.api :as p]
            [portal.runtime.server.jvm :as s]
            [cheshire.core :as json]
            [shadow.cljs.devtools.api :as shadow]
            [clojure.java.io :as io]
            [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [Datafiable]])
  (:import [com.pty4j PtyProcess WinSize]
           [java.util Date]))

(defn cljs [] (shadow/repl :client))

(p/tap)

(extend-protocol Datafiable
  java.io.File
  (datafy [^java.io.File this]
    {:name          (.getName this)
     :absolute-path (.getAbsolutePath this)
     :flags         (->> [(when (.canRead this)     :read)
                          (when (.canExecute this)  :execute)
                          (when (.canWrite this)    :write)
                          (when (.exists this)      :exists)
                          (when (.isAbsolute this)  :absolute)
                          (when (.isFile this)      :file)
                          (when (.isDirectory this) :directory)
                          (when (.isHidden this)    :hidden)]
                         (remove nil?)
                         (into #{}))
     :size          (.length this)
     :last-modified (.lastModified this)
     :uri           (.toURI this)
     :files         (seq (.listFiles this))
     :parent        (.getParentFile this)}))

(def examples
  [;{:type :text :message "text input"}
   ;{:type :number :message "number input"}
   ;{:type :date}
   ;{:type :datetime}
   ;{:type :confirm :message "confirm input" :default false}
   ])

(defn get-bin []
  (str (System/getProperty "user.dir") "/bin"))

(defn get-env []
  (-> (into {} (System/getenv))
      (update "PATH" str ":" (get-bin))))

(defn spawn [command]
  (let [environment (get-env)
        process  (PtyProcess/exec (into-array command) environment)
        running? (fn [^Process process] (.isAlive process))
        resize (fn resize [cols rows]
                 (.setWinSize process (WinSize. cols rows))
                 nil)
        stdin  (.getOutputStream process)
        stdrrr (.getErrorStream process)
        stdout (.getInputStream process)]
    (resize 80 24)
    (with-meta
      {:process/pid (.getPid process)
       :process/status
       (fn status [] (.exitValue process))
       :process/resize resize
       :process/kill
       (fn kill [] (.destroy process))
       :process/stdin
       (fn in [data]
         (when (running? process)
           (.write stdin data)
           (.flush stdin))
         nil)
       :process/stderr
       (fn err []
         (when (running? process)
           (let [b (byte-array 4096)
                 n (.read stdrrr b)]
             (byte-array n b))))
       :process/stdout
       (fn out []
         (when (running? process)
           (let [b (byte-array 4096)
                 n (.read stdout b)]
             (byte-array n b))))}
      {:command command
       :process process
       :started (Date.)
       :environment environment})))

(defn swap-dev []
  (alter-var-root #'s/resource assoc "main.js" (io/file "target/resources/main.js")))

(def bump-version
  {:type :text
   :message "Version number"
   :default "0.5.1"
   :done (fn [version-number] version-number)})

(def test-tasts
  {:type :checkbox
   :message "Select test environments:"
   :default :dev
   :done (fn [commands] (map spawn commands))
   :choices
   [{:name "Clojrue - jvm" :value ["make" "test/jvm"]}
    {:name "Clojrue - babashka" :value ["make" "test/bb"]}]})

(def e2e-tasks
  {:type :checkbox
   :message "Select e2e environments:"
   :default :dev
   :done (fn [commands] (map spawn commands))
   :choices
   [{:name "Clojrue - jvm" :value ["make" "e2e/jvm"]}
    {:name "Clojrue - babashka" :value ["make" "e2e/bb"]}
    {:name "Clojurescript - node.js" :value ["make" "e2e/node"]}
    {:name "Clojurescript - Web Browser" :value ["make" "e2e/web"]}]})

(def switch-theme
  {:type :list
   :message "Select theme:"
   :done (fn [theme] theme)
   :default :portal.colors/nord
   :choices
   [{:name "Nord" :value :portal.colors/nord}
    {:name "Solarized Dark" :value :portal.colors/solarized-dark}
    {:name "Solarized Light" :value :portal.colors/solarized-light}]})

(def set-font-size
  {:type :number
   :message "Font Size"
   :done (fn [size] size)})

(def find-doc
  {:type :text
   :message "Find Documentation"
   :done (fn [name] (repl/find-doc name))})

(def find-source
  {:type :text
   :message "Find Source"
   :done (fn [string] (repl/source-fn (symbol string)))})

(def apropos
  {:type :text
   :message "Apropos"
   :done (fn [string] (mapv find-var (repl/apropos string)))})

(def ci
  {:type :checkbox
   :default #{}
   :done (fn [commands] (map spawn commands))
   :choices
   [{:name "Run Tests - Clojrue - jvm" :value ["make" "test/jvm"]}
    {:name "Run Tests - Clojrue - babashka" :value ["make" "test/bb"]}
    {:name "Lint Code - Cljfmt" :value ["make" "lint/cljfmt"]}
    {:name "Lint Code - Clj-Kondo" :value ["make" "lint/kondo"]}
    {:name "Lint Code - Check" :value ["make" "lint/check"]}]})

(defn execute-task [task]
  (case task
    :e2e e2e-tasks
    :test test-tasts
    :bump bump-version
    :theme switch-theme
    :font-size set-font-size
    :doc find-doc
    :source find-source
    :apropos apropos
    :swap (swap-dev)
    :ci ci
    (spawn ["make" (name task)])))

(def tasks
  {:type :list
   :message "Select a task to run:"
   :default :dev
   :done #'execute-task
   :choices
   [{:name "Lookup Doc" :value :doc}
    {:name "Lookup Source" :value :source}
    {:name "Apropos" :value :apropos}
    {:name "Start Dev server" :value :dev}
    {:name "Switch Theme" :value :theme}
    {:name "Set Font Size" :value :font-size}
    {:name "Bump Version" :value :bump}
    {:name "Swap to Dev" :value :swap}
    {:name "Build UI Client" :value :release}
    {:name "Build Demo" :value :demo}
    {:name "Format code" :value :fmt}
    {:name "CI Checks" :value :ci}
    {:name "End-To-End tests" :value :e2e}]})

(comment
  (swap-dev)

  (def process (spawn ["zsh"]))
  (.getPid (:process (meta process)))
  ((:process/kill process))
  ((:process/status process))
  (tap> process)
  (def portal (p/open))
  (reset! portal tasks)
  (swap! portal #(map deref %))
  (tap> tasks)
  (p/tap)
  (tap> [{:hello :world :old-key 123} {:hello :youtube :new-key 123}])
  (p/clear)
  (p/close)

  ((-> @portal :process/stdin)
   (.getBytes "ihello from portal"))
  (tap> portal)
  (swap! portal * 1000)
  (reset! portal 1)

  (tap> (datafy java.io.File))
  ;; should cause an error in portal because of transit
  (tap> {(with-meta 'k {:a :b}) 'v})

  (tap> (json/parse-stream (io/reader "package-lock.json")))
  (tap> (io/file "deps.edn"))
  (tap> data))
