(ns portal.trace
  (:require [clojure.walk :as w]))

(defn get-original-forms [forms]
  (w/prewalk #(or (:form (meta %)) %) forms))

(def ^:dynamic *trace* nil)

(defn- trace-fn [f v]
  (fn wrap-fn [& args]
    (let [trace  (atom [])
          result (binding [*trace* trace]
                   (apply f args))
          arglists (group-by count (:arglists (meta v)))]
      (swap!
       *trace*
       conj
       {:type :fn
        :var v
        :args (zipmap (first (get arglists (count args))) args)
        :result result
        :trace  @trace})
      result)))

(defn- wrap-trace [v] #(trace-fn % v))

(defn instrument-fn [v]
  (alter-var-root v (wrap-trace v)))

(declare trace-form)

(defn- trace-let [_ bindings & body]
  (let [capture
        (->> (partition-all 2 bindings)
             (map (fn [[binding-name]]
                    [(list 'quote binding-name) binding-name]))
             (into {}))
        bindings
        (->> (partition-all 2 bindings)
             (mapcat
              (fn [[binding-name form]]
                [binding-name `(trace ~form)])))]
    `(let [~@bindings]
       {:bindings ~capture
        :result (do ~@body)})))

(defn- trace-1-form [form]
  (let [m (meta form)
        op (first form)
        expr (get-original-forms form)
        traced-form
        (case op
          let (apply trace-let form)
          `{:result ~form})]
    (with-meta
      (list
       'do
       `(let [trace#  (atom [])
              exception# (atom false)
              info# (try
                      (binding [*trace* trace#] ~traced-form)
                      (catch Exception e#
                        (reset! exception# true)
                        {:result e#}))
              result# (:result info#)]
          (swap!
           *trace*
           conj
           (merge
            {:type   :expr
             :ns     *ns*
             :line   ~(:line m)
             :column ~(:column m)
             :file   *file*
             :op     '~(first expr)
             :expr   '~expr
             :error  @exception#
             :result result#
             :trace  @trace#}
            info#))
          (if @exception#
            (throw result#)
            result#)))
      (merge m {:form form}))))

(defn- trace-form [form]
  (cond
    (list? form)
    (trace-1-form
     (with-meta
       (map trace-form form)
       (meta form)))

    :else form))

(defmacro trace [form]
  (trace-form form))

(defmacro tree>
  "tap> out the execution tree of the provided expression."
  [form]
  `(let [trace# (atom [])]
     (binding [*trace* trace#]
       (let [exception# (atom false)
             result# (try
                       (trace ~form)
                       (catch Exception e#
                         (reset! exception# true)
                         e#))]
         (tap> (first @trace#))
         (if @exception#
           (throw result#)
           result#)))))

(defn foo [x]
  (trace (+ 1 2 x)))

(defn bar []
  (+ 1 2 (foo 2)))

(instrument-fn #'foo)
(instrument-fn #'bar)

(comment
  (require 'portal.api)
  (portal.api/open)
  (portal.api/tap)

  (trace-let '[a 1 b 2 c 3] '(+ 1 2 3))

  (tree> (let [a 1 b 2 c 3] (+ a b c)))

  (tap>
   (macroexpand
    (trace-tree
     (+ 1 2 3 (+ 4 5 6)))))

  (tree>
   (* 2 (+ 1 2 3
           (inc 1)
           (dec 1))))

  (tap>
   '{:op fib
     :args [3]
     :return 2
     :trace
     [{:op fib
       :args [2]
       :return 1
       :trace
       [{:op fib
         :args [1]
         :return 1
         :trace []}
        {:op fib
         :args [0]
         :return 0
         :trace []}]}
      {:op fib
       :args [1]
       :return 1
       :trace []}]}))
