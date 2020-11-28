(ns portal.ui.core
  (:require [portal.ui.state :as st :refer [state]]
            [portal.ui.rpc :as rpc]
            [portal.ui.app :as a]
            [portal.colors :as c]
            [portal.ui.render :as render]
            [reagent.dom :as rdom]))

(def default-settings
  {:font/family "monospace"
   :font-size "12pt"
   :limits/string-length 100
   :limits/max-depth 1
   :spacing/padding 8
   :border-radius "2px"})

(defn root []
  (let [settings (merge (get c/themes ::c/nord)
                        (st/get-actions rpc/request)
                        default-settings)]
    [render/render-tree
     settings
     @st/render-state]))

(defn render-app []
  (rdom/render [a/app]
               (.getElementById js/document "root")))

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
