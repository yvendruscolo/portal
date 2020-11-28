(ns portal.ui.viewer.layout
  (:require [portal.colors :as c]
            [portal.ui.styled :as s]
            [reagent.core :as r]))

(defn layout? [value] (coll? value))

(def toggle-direction {:row :column :column :row})

(defn resize [settings on-mouse-down]
  (let [direction (get settings :direction :row)
        border-color (if (:resizing? settings)
                       (::c/namespace settings)
                       (::c/border settings))
        border (str "1px solid " border-color)]
    (case direction
      :row
      [s/div
       {:on-mouse-down on-mouse-down
        :style
        {:cursor :col-resize
         :width 2
         :height "100%"
         :background (when (:resizing? settings) (::c/namespace settings))
         :border-left border
         :border-right border}
        :style/hover {:background (::c/namespace settings)}}]

      :column
      [s/div
       {:on-mouse-down on-mouse-down
        :style
        {:cursor :row-resize
         :height 2
         :background (when (:resizing? settings) (::c/namespace settings))
         :border-top border
         :border-bottom border}
        :style/hover {:background (::c/namespace settings)}}])))

(defn layout []
  (let [parent    (atom nil)
        resizing? (r/atom false)]
    (fn [settings value]
      (let [{:keys [offsets depth]} settings
            direction (get settings :direction :row)
            [a & b] value
            offset (get @offsets depth (if b "50%" "100%"))]
        (when a
          [:<>
           (when @resizing?
             [s/div
              {:on-mouse-up
               (fn [] (reset! resizing? false))
               :on-mouse-move
               (fn [e]
                 (when (and @resizing? @parent)
                   (let [rect (.getBoundingClientRect @parent)]
                     (swap!
                      offsets
                      assoc
                      depth
                      (case direction
                        :row    (let [x (- (.-clientX e) (.-x rect))]
                                  (min (max x 0) (.-width rect)))
                        :column (let [y (- (.-clientY e) (.-y rect))]
                                  (min (max y 0) (.-height rect))))))))
               :style
               {:position :fixed
                :top 0
                :left 0
                :right 0
                :bottom 0
                :z-index 100}}])
           [s/div
            {:ref (fn [el] (reset! parent el))
             :style
             {:display :flex
              :flex-direction direction
              :width "100%"
              :height "100%"
              :flex "1"
              :position :relative}}
            [s/div
             {:style
              {:display :flex
               :width (case direction :row offset "100%")
               :height (case direction :row "100%" offset)}}
             [s/div
              {:style
               {:display :flex
                :align-items :center
                :justify-content :center
                :width "100%"
                :height "100%"
                :overflow :auto
                :background (::c/background2 settings)
           ;:border (str "1px solid " (::c/border settings))
           ;:background (::c/background2 settings)
           ;:border-radius (:border-radius settings)
                }}
              #_(pr-str {:offset offset})
              a]]
            (when (seq b)
              [:<>
               [resize
                (assoc settings :resizing? @resizing?)
                (fn [] (reset! resizing? true))]
               [layout (assoc settings
                              :direction (toggle-direction direction)
                              :depth (inc depth)) b]])]])))))

(defn layout-1 []
  (let [offsets (r/atom {})]
    (fn [settings value]
      [s/div
       {:style
        {:padding 20
         :box-sizing :border-box
         :height "100%"}}
       [s/div
        {:style
         {:border-radius (:border-radius settings)
          :border (str "1px solid " (::c/border settings))}}
        [layout (assoc settings
                       :depth 0
                       :offsets offsets) value]]])))

(def viewer
  {:predicate vector?
   :component layout-1
   :name :portal.viewer/layout})
