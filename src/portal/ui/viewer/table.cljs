(ns portal.ui.viewer.table
  (:require [portal.colors :as c]
            [portal.ui.inspector :as ins :refer [inspector]]
            [portal.ui.lazy :as l]
            [portal.ui.styled :as s]))

(defn- get-styles [settings]
  {:white-space :nowrap
   :border-bottom (str "1px solid " (::c/border settings))
   :border-right (str "1px solid " (::c/border settings))})

(defn- table-header-row [settings row]
  (let [[column direction] (:table/sort-by settings)
        contains-field? (get settings :table/contains-field? #{})
        set-settings! (:set-settings! settings)
        toggle-sort [row (if-not (= column row)
                           ::asc
                           (if (= direction ::asc) ::desc ::asc))]]
    [s/th
     {:title (str "Click to sort-by " (pr-str toggle-sort))
      :on-click #(set-settings!  {:table/sort-by toggle-sort})
      :style
      (assoc
       (get-styles settings)
       :top 0
       :left (if (#{::keys ::idx} row) 0 :auto)
       :z-index (if (#{::keys ::idx} row) 3 2)
       :position :sticky
       :text-align :left
       :padding (:spacing/padding settings)
       :background (ins/get-background settings))}
     [s/div
      {:style
       {:display :flex
        :justify-content :space-between
        :align-items :center}}
      [s/div
       {:style {:display :flex :align-items :center}}
       (when-not (#{::keys ::idx} row)
         [s/input
          {:type "checkbox"
           :checked (contains? contains-field? row)
           :title (str "Check to require " (pr-str row))
           :on-click #(.stopPropagation %)
           :on-change (fn [e]
                        (let [checked? (.-checked (.-target e))]
                          (set-settings!
                           {:table/contains-field?
                            ((if checked? conj disj) contains-field? row)})))}])
       (pr-str (case row ::keys 'key ::idx 'idx row))]
      (when (= column row)
        [s/div
         {:style {:margin-left (:spacing/padding settings)}}
         (cond
           (= direction ::asc) "▼"
           (= direction ::desc) "▲")])]]))

(defn- try-sort [settings values]
  (or
   (when-let [[column direction] (:table/sort-by settings)]
     (try
       (if (= column ::idx)
         (if (= direction ::asc) values (reverse values))
         (sort-by (case column ::keys first (comp column second))
                  (case direction ::asc < ::desc >) values))
       (catch js/Error _e)))
   values))

(defn- filter-values [settings values]
  (let [fields (:table/contains-field? settings)]
    (if (empty? fields)
      values
      (filter
       (fn [[_ m]]
         (every? #(contains? m %) fields))
       values))))

(defn- table-header-column [settings column]
  [s/th
   {:style
    (assoc
     (get-styles settings)
     :left 0
     :z-index 1
     :position :sticky
     :text-align :left
     :padding (:spacing/padding settings)
     :background (ins/get-background settings))}
   (pr-str column)])

(defn- table-data [settings child]
  [s/td {:style (get-styles settings)} child])

(defn- table [settings component rows cols]
  (let [transpose? false]
    [s/table
     {:style
      {:width "100%"
       :border-top (str "1px solid " (::c/border settings))
       :border-left (str "1px solid " (::c/border settings))
       :background (ins/get-background settings)
       :border-spacing 0
       :position :relative
       :color (::c/text settings)
       :font-size  (:font-size settings)
       :border-radius (:border-radius settings)}}
     [s/tbody
      [l/lazy-seq
       (map-indexed
        (fn [row-index row]
          [s/tr
           {:key row-index
            :style/hover
            {:background (str (::c/border settings) "55")}}
           (map-indexed
            (fn [col-index col]
              ^{:key col-index}
              [component
               (if-not transpose?
                 {:row row    :row-index row-index
                  :column col :column-index col-index}
                 {:row col    :row-index col-index
                  :column row :column-index row-index})])
            (if transpose? rows cols))])
        (if transpose? cols rows))]]]))

(defn- inspect-map-table [settings values]
  [table
   settings
   (fn [context]
     (let [{:keys [row column]} context]
       (cond
         (= column row ::header) [table-header-row settings ::keys]

         (= row ::header)
         [table-header-row settings column]

         (= column ::header)
         [table-header-column settings (first row)]

         :else
         [table-data
          settings
          (let [[_ row] row]
            (when (contains? row column)
              [inspector (assoc settings :coll row :k column) (get row column)]))])))
   (concat [::header]
           (->> values
                (filter-values settings)
                (try-sort settings)))
   (concat [::header] (into #{} (mapcat keys (vals values))))])

(defn- inspect-coll-table [settings values]
  [table
   settings
   (fn [context]
     (let [{:keys [row column]} context]
       (cond
         (= column row ::header) [table-header-row settings ::idx]

         (= row ::header)
         [table-header-row settings column]

         (= column ::header)
         [table-header-column settings (first row)]

         :else
         [table-data
          settings
          (let [[_ row] row]
            (when (contains? row column)
              [inspector (assoc settings :coll row :k column) (get row column)]))])))
   (concat [::header]
           (->> values
                (map vector (range))
                (filter-values settings)
                (try-sort settings)))
   (concat [::header] (into #{} (mapcat keys values)))])

(defn- inspect-vector-table [settings values]
  (let [n (reduce max (map count values))]
    [table
     settings
     (fn [context]
       (let [{:keys [row column]} context]
         [table-data
          settings
          (when (< column (count row))
            [inspector (assoc settings :coll row :k column) (get row column)])]))
     values
     (range n)]))

(defn- inspect-set-table [settings values]
  (let [columns (into #{} (mapcat keys values))]
    [table
     settings
     (fn [context]
       (let [{:keys [row column]} context]
         (cond
           (= row ::header)
           [table-header-row
            settings
            [inspector (assoc settings :coll row) column]]

           (contains? column row)
           [table-data
            settings
            [inspector (assoc settings :coll row :k column) (get row column)]])))
     (concat [::header] values)
     columns]))

(defn- get-component [value]
  (cond
    (and (or (vector? value) (list? value))
         (every? vector? value))
    inspect-vector-table

    (and (map? value) (every? map? (vals value)))
    inspect-map-table

    (and (set? value) (every? map? value))
    inspect-set-table

    (and (coll? value) (every? map? value))
    inspect-coll-table))

(defn table-view? [value] (some? (get-component value)))

(defn inspect-table [settings values]
  (let [component (get-component values)]
    [component settings values]))
