(ns portal.ui.viewer.prompt
  (:require [clojure.spec.alpha :as spec]
            [portal.colors :as c]
            [portal.ui.styled :as s]
            [reagent.core :as r]))

(defn container [settings & children]
  [s/div
   {:style
    {:background (::c/background settings)
     :font-family (:font/family settings)
     :font-size (:font-size settings)
     :color (::c/text settings)
     :width "100%"
     :display :flex
     :justify-content :center
     :align-items :center}}
   (into [s/div
          (merge
           {:style
            {:padding (:spacing/padding settings)
             :min-width "50%"
             :box-sizing :border-box
             :background (::c/background2 settings)
             :border (str "1px solid " (::c/border settings))
             :border-radius (:border-radius settings)}}
           (:props settings))]
         children)])

(defn prompt-list [_ input]
  (let [choices (map :value (concat (:choices input)
                                    (take 1 (:choices input))))
        default (some #(when (= (:value %) (:default input))
                         (:value %))
                      (:choices input))
        active (r/atom default)
        get-next (->> choices
                      (partition 2 1)
                      (map vec)
                      (into {}))
        get-prev  (->> choices
                       reverse
                       (partition 2 1)
                       (map vec)
                       (into {}))]
    (fn [settings input]
      (let [active?   #{@active}
            on-done   (:on-done settings)
            on-select #(reset! active %)
            on-next (partial (comp on-select get-next deref) active)
            on-prev (partial (comp on-select get-prev deref) active)]
        [container
         (assoc
          settings
          :props
          {:tab-index 0
           :on-key-down
           (fn [e]
             (when (= (.-key e) "Enter")
               (.stopPropagation e)
               (on-done @active))
             (when (= (.-key e) "ArrowUp")
               (.stopPropagation e)
               (on-prev))
             (when (= (.-key e) "ArrowDown")
               (.stopPropagation e)
               (on-next)))})
         [s/div (:message input)]
         (when-not (:portal/invoking? settings)
           (for [choice (:choices input)]
             (let [selected? (active? (:value choice))]
               [s/div
                {:key (hash choice)
                 :on-click #(on-select (:value choice))
                 :style
                 {:cursor :pointer
                  :padding (:spacing/padding settings)
                  :box-sizing :border-box
                  :color (if selected?
                           (::c/boolean settings)
                           (::c/text settings))
                  :display :flex}}
                [s/div
                 {:style {:width "1.5em"}}
                 (when selected? "> ")]
                [s/div
                 (:name choice)]])))]))))

(defn prompt-confirm [_ input]
  (let [choice (r/atom (:default input))]
    (fn [settings input]
      [container
       settings
       [s/div
        {:style
         {:display :flex}}
        (:message input)

        [s/div
         {:on-click #(reset! choice true)
          :tab-index 0
          :on-key-down
          (fn [e]
            (when (= (.-key e) "Enter")
              (.stopPropagation e)
              (reset! choice true)))
          :style {:cursor :pointer
                  :color (if (true? @choice)
                           (::c/boolean settings)
                           (::c/text settings))}}
         "yes"]
        "/"
        [s/div
         {:on-click #(reset! choice false)
          :tab-index 0
          :on-key-down
          (fn [e]
            (when (= (.-key e) "Enter")
              (.stopPropagation e)
              (reset! choice false)))
          :style {:cursor :pointer
                  :color (if (false? @choice)
                           (::c/boolean settings)
                           (::c/text settings))}}
         "no"]]])))

(defn prompt-input [_ input]
  (let [text (r/atom (get input :default ""))]
    (fn [settings input]
      (let [on-done (:on-done settings)]
        [container
         settings
         [s/input
          {:type (get settings :type :text)
           :placeholder (:message input)
           :value @text
           :auto-focus true
           :on-change #(reset! text (.-value (.-target %)))
           :on-key-down
           (fn [e]
             (when (= (.-key e) "Enter")
               (.stopPropagation e)
               (on-done @text)))
           :style
           {:display :block
            :width "100%"
            :color (::c/text settings)
            :font-family (:font/family settings)
            :font-size (:font-size settings)
            :background (::c/background settings)
            :border (str "1px solid " (::c/border settings))
            :border-radius (:border-radius settings)
            :padding (:spacing/padding settings)
            :box-sizing :border-box}}]]))))

(defn prompt-text [settings input]
  [prompt-input settings input])

(defn prompt-number [settings input]
  [prompt-input
   (assoc settings :type :number)
   input])

(defn prompt-date [settings input]
  [prompt-input
   (assoc settings :type :date)
   input])

(defn prompt-datetime [settings input]
  [prompt-input
   (assoc settings :type :datetime-local)
   input])

(defn prompt-checkbox [_ input]
  (let [default (->> (:choices input)
                     (keep #(when (contains? (:default input) (:value %))
                              (:value %)))
                     (into #{}))
        active (r/atom default)]
    (fn [settings input]
      [container
       settings
       [s/div (:message input)]
       (let [active?   @active
             on-done   (:on-done settings)
             on-select #(swap! active (if (active? %) disj conj) %)]
         [s/div
          (for [choice (:choices input)]
            (let [selected? (active? (:value choice))]
              [s/div
               {:key (hash choice)
                :on-click #(on-select (:value choice))
                :tab-index 0
                :on-key-down
                (fn [e]
                  (when (= (.-key e) " ")
                    (.stopPropagation e)
                    (on-select (:value choice)))
                  (when (= (.-key e) "Enter")
                    (.stopPropagation e)
                    (on-done @active)))
                :style
                {:cursor :pointer
                 :padding (:spacing/padding settings)
                 :box-sizing :border-box
                 :color (if selected?
                          (::c/boolean settings)
                          (::c/text settings))
                 :display :flex}}
               [s/div
                {:style {:width "1.5em"}}
                [:input {:type :radio :checked (some? selected?) :tab-index -1}]]
               [s/div
                (:name choice)]]))])])))

(declare prompt-layout)

(def type->component
  {:list prompt-list
   :confirm prompt-confirm
   :checkbox prompt-checkbox
   :text prompt-text
   :number prompt-number
   :date prompt-date
   :datetime prompt-datetime
   :layout prompt-layout})

(spec/def ::message string?)

(spec/def ::type (into #{} (keys type->component)))

(spec/def ::prompt
  (spec/keys :req-un [::type]
             :opt-un [::message]))

(defn prompt? [value]
  (spec/valid? ::prompt value))

(defn prompt-dispatch [settings input]
  (let [component (type->component (:type input))]
    (when (fn? component)
      [component settings input])))

(defn prompt-layout [settings value]
  [s/div
   {:style
    {:display :flex
     :width "100%"
     :flex-direction (get value :direction :column)}}
   (for [child (:children value)]
     ^{:key (hash child)}
     [prompt-dispatch settings child])])

(defn prompt [settings value]
  (let [on-invoke  (:portal/on-invoke settings)
        on-push    (:portal/on-push settings)
        on-done    (fn [choice]
                     (.then (on-invoke (:done value) choice) on-push))]
    ^{:key (hash value)}
    [prompt-dispatch (assoc settings :on-done on-done) value]))
