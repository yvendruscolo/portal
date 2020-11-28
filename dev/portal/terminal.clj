(ns portal.terminal
  (:require [portal.api :as p])
  (:import [com.pty4j PtyProcess WinSize]))

(defn- get-bin []
  (str (System/getProperty "user.dir") "/bin"))

(defn- get-env []
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
    {:pid (.getPid process)
     :status
     (fn status [] (.exitValue process))
     :on-resize resize
     :on-kill
     (fn kill [] (.destroy process))
     :on-stdin
     (fn in [data]
       (when (running? process)
         (.write stdin data)
         (.flush stdin))
       nil)
     :on-stderr
     (fn err []
       (when (running? process)
         (let [b (byte-array 4096)
               n (.read stdrrr b)]
           (byte-array n b))))
     :on-stdout
     (fn out []
       (when (running? process)
         (let [b (byte-array 4096)
               n (.read stdout b)]
           (byte-array n b))))}))

(defn -main []
  (let [portal (p/open)
        zsh    (spawn ["zsh"])]
    (.addShutdownHook
     (Runtime/getRuntime) (Thread. (fn [] (p/close))))
    (p/render portal [:portal/xterm zsh])))

(comment
  (def portal (p/open))
  (def zsh (spawn ["zsh"]))
  (p/render portal [:portal/xterm zsh]))

