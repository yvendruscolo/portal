(ns portal.apropos
  (:require [clojure.string :as str]
            [portal.api :as p]))

(defonce state (atom ""))

(defn- var-name [v]
  (let [{:keys [name ns]} (meta v)]
    (str (ns-name ns) "/" name)))

(defn- find-vars [s]
  (->> (all-ns)
       (sort-by ns-name)
       (mapcat (comp vals ns-publics))
       (filter #(str/includes? (var-name %) s))))

(defn- var-info [v]
  (let [{:keys [ns name doc arglists]} (meta v)
        ns (ns-name ns)]
    [:div
     {:style {:padding 4}}
     [:div
      {:style
       {:display :flex
        :justify-content :space-between
        :align-items :flex-end
        :flex-wrap :wrap
        :padding-top 12
        :padding-bottom 12
        :padding-left 8
        :padding-right 8
        :background "rgba(0,0,0,0.1)"
        :box-sizing :border-box
        :border-radius 3}}
      [:code [:span {:style {:opacity 0.5}} ns "/"] [:b name]]
      (into
       [:div]
       (for [arglist arglists]
         [:code
          {:style {:margin-right 4}}
          (pr-str arglist)]))]
     #_(into
        [:<>]
        (for [arglist arglists]
          [:code
           {:style {:padding 4 :margin-right 4}}
           (pr-str (seq (into [name] arglist)))]))
     #_(when doc [:pre doc])]))

(defn apropos []
  (let [value @state
        vars (take 15 (find-vars value))]
    (into
     [:div
      {:style
       {:width 600
        :margin "0 auto"}}
      [:div
       {:style
        {:padding 4
         :box-sizing :border-box}}
       [:input
        {:value value
         :on-change #(reset! state %)
         :placeholder "Type to search for var..."
         :style
         {:font-family :monospace
          :width "100%"
          :display :block
          :border :none
          :padding 8
          :background "rgba(0,0,0,0.1)"
          :box-sizing :border-box
          :border-radius 3}}]]]
     (for [v vars] (var-info v)))))

(comment
  (def portal (p/open))
  (p/render portal (apropos))

  (add-watch
   state
   :render
   (fn [_ _ _ _]
     (p/render portal (apropos))))

  (remove-watch state :render)

  (-> @state)
  (reset! state {}))
