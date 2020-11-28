(ns portal.docs
  (:require [clojure.repl :as repl]
            [clojure.string :as str]
            [portal.api :as p]))

(defonce state (atom {}))

(defn vars-by-first-char [ns]
  (->> (ns-publics ns)
       (sort-by (comp str/lower-case str first))
       (group-by
        (fn [[k _]] (first (str/lower-case (str k)))))
       (sort-by first)))

(defn all-ns-with-publics []
  (->>
   (all-ns)
   (filter (comp not zero? count ns-publics))
   (sort-by ns-name)
   (remove #(str/includes? (ns-name %) "cider"))))

(defn ns-listing [{:keys [namespace on-select-var]}]
  (into
   [:div
    {:style
     {:padding 40
      :display :grid
      :grid-gap 5
      :box-sizing :border-box}}
    [:div
     {:style {:grid-column "1 / 3"}}
     [:h1 namespace]
     (when-let [doc (:doc (meta (find-ns namespace)))]
       [:pre doc])]]
   (for [[c vars] (vars-by-first-char namespace)]
     (into
      [:<>
       [:h2
        {:style
         {:grid-column "1 / 3"
          :border-bottom "solid #ccc 2px"}} c]]
      (for [[s var] vars]
        [:<>
         [:div
          {:on-click #(on-select-var var)
           :style
           {:grid-column "1"
            :text-align :right
            :margin-right 20
            :cursor :pointer}}
          s]
         [:div
          {:style
           {:grid-column "2"
            :overflow :hidden
            :text-overflow :ellipsis
            :white-space :nowrap}}
          (-> var meta :doc)]])))))

(defn var->symbol [v]
  (let [{:keys [name ns]} (meta v)]
    (symbol (str (ns-name ns)) (str name))))

(defn var-info [{:keys [selected-var on-select-ns]}]
  (let [{:keys [ns name doc arglists]} (meta selected-var)
        ns (ns-name ns)]
    [:div
     {:style
      {:padding 40
       :box-sizing :border-box}}
     [:div
      {:style
       {:display :flex
        :justify-content :space-between
        :align-items :flex-end}}
      [:h1 name]
      [:h2 {:on-click #(on-select-ns ns)} ns]]
     (into
      [:<>]
      (for [arglist arglists]
        [:code
         {:style {:padding 4 :margin-right 4}}
         (pr-str (seq (into [name] arglist)))]))
     (when doc [:pre doc])
     (when-let [source (repl/source-fn (var->symbol selected-var))]
       [:pre source])]))

(defn ns-selector [{:keys [namespaces selected on-select]}]
  (into
   [:div
    {:style
     {:padding 40
      :box-sizing :border-box
      :border-right "solid #ccc 2px"}}]
   (for [ns namespaces]
     (let [name (ns-name ns)]
       [:div
        {:on-click #(on-select name)
         :style
         {:padding-bottom 4
          :overflow :hidden
          :text-overflow :ellipsis
          :white-space :nowrap
          :cursor :pointer
          :color (if (= selected name)
                   :pink
                   :inherit)}}
        name]))))

(defn docs []
  (let [{:keys [selected-ns selected-var] :as st} @state
        st (assoc
            st
            :on-select-ns
            #(swap! state assoc
                    :selected-ns %
                    :selected-var nil)
            :on-select-var
            #(swap! state assoc :selected-var %))]
    [:div
     [:button {:on-click #(reset! state {})} "close"]
     (cond
       selected-var
       (var-info st)
       selected-ns
       (ns-listing
        {:namespace selected-ns
         :on-select-var #(swap! state assoc :selected-var %)})
       :else
       (ns-selector
        {:namespaces (all-ns-with-publics)
         :selected selected-ns
         :on-select #(swap! state assoc
                            :selected-ns %
                            :selected-var nil)}))]))

(comment
  (def portal (p/open))
  (p/render portal (docs))

  (add-watch
   state
   :render
   (fn [_ _ _ _]
     (p/render portal (docs))))

  (defn demo []
    (let [{:keys [counter value option]} @state]
      [:div
       {:style {:padding 20}}
       [:a {:href "https://www.google.com" :target "_blank"}
        "google"]
       [:h1 "counter"]

       [:input {:value value :on-change #(swap! state assoc :value %)}]

       [:pre counter]
       [:button {:on-click #(do
                              (prn @state)
                              (swap! state update :counter inc))}
        "click me"]
       [:select {:value option :on-change #(swap! state assoc :option %)}
        [:option {:value {:hello :world}} "hello"]
        [:option {:value {:abc 123}} "abc"]]]))

  (-> @state)
  (reset! state {}))

(p/render portal (docs))
