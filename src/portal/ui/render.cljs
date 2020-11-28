(ns portal.ui.render
  (:require [portal.colors :as c]
            [portal.ui.components.terminal :as term]
            [portal.ui.styled :as s]
            [reagent.core :as r]))

(declare render-children)

(defn normalize [v]
  (if (map? (second v))
    v
    (into [(first v) {}] (rest v))))

(defn error-boundary []
  (let [error         (r/atom nil)
        last-children (atom nil)]
    (r/create-class
     {:display-name "ErrorBoundary"
      :component-did-catch
      (fn [_e info]
        (reset! error [@last-children info]))
      :reagent-render
      (fn [& children]
        (when-not (= children (first @error))
          (reset! last-children children)
          (reset! error nil))
        (if (nil? @error)
          (into [:<>] children)
          (let [[_ info] @error]
            [:pre [:code (pr-str info)]])))})))

(defn merge-style [a b] (merge b a))

(defn p [{:keys [settings attrs children]}]
  [s/styled
   :p
   (update attrs :style merge-style
           {:font-size (:font-size settings)
            :font-family "-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji"})
   [render-children settings children]])

(defn a [{:keys [settings attrs children]}]
  [s/styled
   :a
   (update attrs :style merge-style
           {:color (::c/uri settings)})
   [render-children settings children]])

(defn pre [{:keys [settings attrs children]}]
  [s/styled
   :pre
   (update attrs :style merge-style
           {:overflow :auto
            :padding (* 2 (:spacing/padding settings))
            :background (::c/background2 settings)
            :border-radius (:border-radius settings)})
   [render-children settings children]])

(defn code [{:keys [settings attrs children]}]
  (into
   [s/styled
    :code
    (update attrs :style merge-style
            {:background (::c/background2 settings)
             :border-radius (:border-radius settings)})]
   children))

(defn header [{:keys [tag settings attrs children]}]
  (let [{:keys [on-click]} attrs
        invoke (:portal/on-invoke settings)]
    [s/styled
     tag
     (->
      attrs
      (assoc :on-click #(when on-click (invoke on-click)))
      (update :style merge-style
              {:color (::c/namespace settings)
               :margin 0
               :padding-top (* 0.5 (:spacing/padding settings))
               :padding-bottom (* 0.5 (:spacing/padding settings))
               :margin-bottom (:spacing/padding settings)}))
     [render-children settings children]]))

(defn input [{:keys [settings attrs]}]
  (let [{:keys [on-change]} attrs
        invoke (:portal/on-invoke settings)]
    [s/styled
     :input
     (-> attrs
         (assoc :on-change #(when on-change (invoke on-change (.-value (.-target %)))))
         (update :style merge-style
                 {:background (::c/background settings)
                  :padding (:spacing/padding settings)
                  :box-sizing :border-box
                  :font-size (:font-size settings)
                  :color (::c/text settings)
                  :border (str "1px solid " (::c/border settings))}))]))

(defn button [{:keys [settings attrs children]}]
  (let [{:keys [on-click]} attrs
        invoke (:portal/on-invoke settings)]
    [s/styled
     :button
     (->
      attrs
      (assoc :on-click #(when on-click (invoke on-click)))
      (update :style merge-style
              {:background (::c/text settings)
               :color (::c/background settings)
               :border :none
               :font-size (:font-size settings)
               :font-family "Arial"
               :box-sizing :border-box
               :padding (:spacing/padding settings)
               :border-radius (:border-radius settings)
               :cursor :pointer}))
     [render-children settings children]]))

(defn select [{:keys [settings attrs children]}]
  (let [ids (map str (range))
        values (map (comp :value second) children)
        id->value (zipmap ids values)
        value->id (zipmap values ids)
        {:keys [on-change]} attrs
        invoke (:portal/on-invoke settings)]
    [s/styled
     :select
     (->
      attrs
      (assoc :on-change #(when on-change (invoke on-change (id->value (.-value (.-target %))))))
      (update :value value->id)
      (update :style merge-style
              {:background (::c/background settings)
               :margin (:spacing/padding settings)
               :padding (:spacing/padding settings)
               :box-sizing :border-box
               :font-size (:font-size settings)
               :color (::c/text settings)
               :border (str "1px solid " (::c/border settings))}))
     (for [child children]
       (let [[tag attrs & children] (normalize child)]
         (into [tag (-> attrs (update :value value->id))] children)))]))

(defn default [{:keys [tag settings attrs children]}]
  (let [invoke (:portal/on-invoke settings)
        {:keys [on-click]} attrs]
    [s/styled
     tag
     (assoc attrs :on-click #(when on-click (invoke on-click)))
     (when children [render-children settings children])]))

(defn terminal [{:keys [settings attrs]}]
  (let [invoke (:portal/on-invoke settings)
        {:keys [on-stdout on-stdin on-resize]} attrs]
    [term/terminal
     settings
     {:on-stdout #(invoke on-stdout)
      :on-stdin #(invoke on-stdin %)
      :on-resize #(invoke on-resize %1 %2)}]))

(def table
  {:h1 header
   :h2 header
   :h3 header
   :h4 header
   :h5 header
   :h6 header
   :pre pre
   :code code
   :select select
   :a a
   :p p
   :button button
   :input input
   :portal/xterm terminal})

(defn render [settings element]
  (cond
    (vector? element)
    (let [[tag attrs & children] (normalize element)
          component (get table tag default)]
      (into [component
             {:tag tag
              :attrs attrs
              :settings settings
              :children children}]))
    :else (str element)))

(defn render-children [settings children]
  (into [:<>] (for [child children] [render settings child])))

(defn container [settings & children]
  (into
   [s/div
    {:style
     {:background (::c/background settings)
      :color (::c/text settings)
      :font-family (:font/family settings)
      :font-size (:font-size settings)
      :min-height "100vh"}}]
   children))

(defn render-tree [settings tree]
  [container settings [render settings tree]])
