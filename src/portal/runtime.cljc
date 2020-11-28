(ns portal.runtime
  (:require [clojure.datafy :refer [datafy nav]]
            #?(:clj  [portal.sync  :as a]
               :cljs [portal.async :as a]))
  #?(:clj (:import [java.util UUID])))

#?(:clj (defn random-uuid [] (UUID/randomUUID)))

(defonce value-cache (atom {}))
(defonce instance-cache (atom {}))

(defn atom? [value]
  #?(:clj  (instance? clojure.lang.Atom value)
     :cljs (satisfies? cljs.core/IAtom value)))

(declare object->value)

(defn instance->uuid [instance]
  (let [k [:instance instance]]
    (-> instance-cache
        (swap!
         (fn [cache]
           (if (contains? cache k)
             cache
             (let [uuid (random-uuid)]
               (when (atom? instance)
                 (add-watch
                  instance
                  ::watch
                  (fn [_ _ _old _new]
                    (swap!
                     value-cache
                     assoc
                     uuid
                     (object->value instance uuid)))))
               (assoc cache [:uuid uuid] instance k uuid)))))
        (get k))))

(defn object-deref [m o]
  (if-not (atom? o)
    m
    (assoc m :deref @o)))

(defn object->value
  ([o]
   (object->value o (instance->uuid o)))
  ([o id]
   (-> {:id id
        :meta (meta o)
        :type (pr-str (type o))
        :string (pr-str o)}
       (object-deref o))))

(defn uuid->instance [uuid]
  (get @instance-cache [:uuid uuid]))

(defonce state (atom {:portal/value (list)}))

(defn update-setting [k v] (swap! state assoc k v) true)

(defn update-value [new-value]
  (swap! state update :portal/value conj new-value))

(defn clear-values
  ([] (clear-values nil identity))
  ([_request done]
   (swap! state assoc :portal/value (list))
   (reset! instance-cache {})
   (reset! value-cache {})
   (doseq [[_ instance] @instance-cache]
     (when (atom? instance)
       (remove-watch instance ::watch)))
   (done nil)))

(defn limit-seq [value]
  (if-not (seq? value)
    value
    (let [m     (meta value)
          limit (get m ::more-limit 100)
          [realized remaining] (split-at limit value)]
      (with-meta
        realized
        (merge
         m
         (when (seq remaining)
           {::more #(limit-seq (with-meta remaining m))}))))))

(defn load-state [_request done] (done {:portal/value state}))

(defn on-datafy [value done]
  (let [datafied (datafy value)]
    (if (= datafied value)
      ; allow untransformed promises to pass freely
      (done {:value datafied})
      ; wait for any newly returned promise to resolve
      (a/let [datafied datafied] (done {:value datafied})))))

(defn on-nav [request done]
  (let [[coll k v] (:args request)
        naved      (if coll (nav coll k v) v)]
    (done {:value naved})
    #_(if (= naved v)
      ; allow untransformed promises to pass freely
        (on-datafy naved done)
      ; wait for any newly returned promise to resolve
        (a/let [naved naved] (on-datafy naved done)))))

(defn invoke [{:keys [f args]} done]
  (try
    (done {:return (apply f args)})
    (catch #?(:clj Exception :cljs js/Error) e
      (done {:return e}))))

(def ops
  {:portal.rpc/clear-values clear-values
   :portal.rpc/load-state   load-state
   :portal.rpc/on-nav       on-nav
   :portal.rpc/invoke       invoke})
