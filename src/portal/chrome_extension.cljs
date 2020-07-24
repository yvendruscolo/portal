(ns portal.chrome-extension
  (:require [portal.core :as portal]
            [portal.async :as a]
            [clojure.datafy :refer [datafy nav]]))

(defn send! [data msg]
  (js/Promise.resolve
   (case (:op msg)
     :portal.rpc/clear-values nil
     :portal.rpc/load-state
     {:portal/complete? true
      :portal/value data}
     :portal.rpc/on-nav
     (a/let [res (apply nav (:args msg))]
       {:value (datafy res)}))))

(defn reload! [] (portal/reload!))

(defn main! []
  (try
    (let [json (js->clj (js/JSON.parse js/window.document.body.innerText)
                        :keywordize-keys true)]
      (set! js/document.body.style.margin "0")
      (set! js/document.body.innerHTML "<div id=\"root\"></div>")
      (portal/main! (portal/get-actions (partial send! json))))
    (catch js/Error _)))
