(ns portal.ui.core
  (:require [portal.ui.state :as st :refer [state]]
            [portal.ui.rpc :as rpc]
            [portal.ui.app :refer [app]]
            [reagent.dom :as rdom]))

(defn render-app []
  (rdom/render [app]
               (.getElementById js/document "root")))

(def default-settings
  {:font/family "monospace"
   :font-size "12pt"
   :limits/string-length 100
   :limits/max-depth 1
   :spacing/padding 8
   :border-radius "2px"})

(defn long-poll []
  (let [on-load (or (:portal/on-load @state)
                    #(js/Promise.resolve false))]
    (.then (on-load)
           (fn [complete?]
             (when-not complete? (long-poll))))))

(def get-actions st/get-actions)

(defn main!
  ([] (main! (st/get-actions rpc/request)))
  ([settings]
   (swap! state merge default-settings settings)
   ((:portal/on-load @state))
   ;(long-poll)
   (render-app)))

(defn reload! [] (render-app))
