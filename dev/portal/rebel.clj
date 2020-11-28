(ns portal.rebel
  (:require [portal.api :as p]
            [rebel-readline.clojure.main :as rebel]
            [rebel-readline.jline-api :as jline-api])
  (:import [java.io PipedInputStream PipedOutputStream]
           [org.jline.terminal Size TerminalBuilder]))

(defn- make-pipe []
  (let [out (PipedOutputStream.)
        in  (PipedInputStream. out)]
    [out in]))

(defn rebel []
  (let [[in stdin]   (make-pipe)
        [stdout out] (make-pipe)
        terminal (-> (TerminalBuilder/builder)
                     (.size (Size. 80 24))
                     (.streams stdin stdout)
                     (.build))
        resize (fn [cols rows] (.setSize terminal (Size. cols rows)))]
    (future
      (binding [jline-api/*terminal* terminal]
        (rebel/-main)))
    {:on-resize resize
     :on-stdin
     (fn stdin [data]
       (.write in data)
       (.flush in)
       nil)
     :on-stdout
     (fn stdout []
       (let [b (byte-array 4096)
             n (.read out b)]
         (byte-array n b)))
     :on-stderr
     (fn stderr [])}))

(comment
  (def portal (p/open))
  (def r (rebel))
  (p/render portal [:portal/xterm r]))
