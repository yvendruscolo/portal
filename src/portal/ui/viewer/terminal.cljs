(ns portal.ui.viewer.terminal
  (:require [clojure.spec.alpha :as spec]
            [portal.colors :as c]
            [portal.ui.styled :as s]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            ["xterm" :refer [Terminal]]
            ["xterm-addon-fit" :refer [FitAddon]]))

(def style
  "
  .xterm {
    font-feature-settings: \" liga \" 0;
    position: relative;
    user-select: none;
    -ms-user-select: none;
    -webkit-user-select: none;
}

.xterm.focus,
.xterm:focus {
    outline: none;
}

.xterm .xterm-helpers {
    position: absolute;
    top: 0;
    /**
     * The z-index of the helpers must be higher than the canvases in order for
     * IMEs to appear on top.
     */
    z-index: 5;
}

.xterm .xterm-helper-textarea {
    padding: 0;
    border: 0;
    margin: 0;
    /* Move textarea out of the screen to the far left, so that the cursor is not visible */
    position: absolute;
    opacity: 0;
    left: -9999em;
    top: 0;
    width: 0;
    height: 0;
    z-index: -5;
    /** Prevent wrapping so the IME appears against the textarea at the correct position */
    white-space: nowrap;
    overflow: hidden;
    resize: none;
}

.xterm .composition-view {
    /* TODO: Composition position got messed up somewhere */
    background: #000;
    color: #FFF;
    display: none;
    position: absolute;
    white-space: nowrap;
    z-index: 1;
}

.xterm .composition-view.active {
    display: block;
}

.xterm .xterm-viewport {
    /* On OS X this is required in order for the scroll bar to appear fully opaque */
    overflow-y: scroll;
    cursor: default;
    position: absolute;
    right: 0;
    left: 0;
    top: 0;
    bottom: 0;
}

.xterm .xterm-screen {
    position: relative;
}

.xterm .xterm-screen canvas {
    position: absolute;
    left: 0;
    top: 0;
}

.xterm .xterm-scroll-area {
    visibility: hidden;
}

.xterm-char-measure-element {
    display: inline-block;
    visibility: hidden;
    position: absolute;
    top: 0;
    left: -9999em;
    line-height: normal;
}

.xterm {
    cursor: text;
    height: 100%;
}

.xterm.enable-mouse-events {
    /* When mouse events are enabled (eg. tmux), revert to the standard pointer cursor */
    cursor: default;
}

.xterm.xterm-cursor-pointer {
    cursor: pointer;
}

.xterm.column-select.focus {
    /* Column selection mode */
    cursor: crosshair;
}

.xterm .xterm-accessibility,
.xterm .xterm-message {
    position: absolute;
    left: 0;
    top: 0;
    bottom: 0;
    right: 0;
    z-index: 10;
    color: transparent;
}

.xterm .live-region {
    position: absolute;
    left: -9999px;
    width: 1px;
    height: 1px;
    overflow: hidden;
}

.xterm-dim {
    opacity: 0.5;
}

.xterm-underline {
    text-decoration: underline;
}

  ")

(def nord
  {:nord0 "#2E3440"
   :nord1 "#3B4252"
   :nord2 "#434C5E"
   :nord3 "#4C566A"
   :nord4 "#D8DEE9"
   :nord5 "#E5E9F0"
   :nord6 "#ECEFF4"
   :nord7 "#8FBCBB"
   :nord8 "#88C0D0"
   :nord9 "#81A1C1"
   :nord10 "#5E81AC"
   :nord11 "#BF616A"
   :nord12 "#D08770"
   :nord13 "#EBCB8B"
   :nord14 "#A3BE8C"
   :nord15 "#B48EAD"})

(defn theme [settings]
  #js {:background (::c/background2 settings)
       :black (:nord1 nord)
       :blue (:nord9 nord)
       :brightBlack (:nord3 nord)
       :brightBlue (:nord9 nord)
       :brightCyan (:nord7 nord)
       :brightGreen (:nord14 nord)
       :brightMagenta (:nord15 nord)
       :brightRed (:nord11 nord)
       :brightWhite (:nord6 nord)
       :brightYellow (:nord13 nord)
       :cursor (:nord4 nord)
       ;:cursorAccent (:nord4 nord)
       :cyan (:nord8 nord)
       :foreground (:nord4 nord)
       :green (:nord14 nord)
       :magenta (:nord15 nord)
       :red (:nord11 nord)
       :selection (:nord2 nord)
       :white (:nord5 nord)
       :yellow (:nord13 nord)})

(defn- make-terminal [settings value]
  (let [on-invoke (:portal/on-invoke settings)
        term (Terminal.
              #js {:convertEol true
                   :cursorBlink true
                   :fontFamily "Monaco, monospace"
                   :fontSize 16
                   :theme (theme settings)})
        fit-addon (FitAddon.)
        stdout (fn stdout []
                 (.then
                  (on-invoke (:process/stdout value))
                  (fn [data]
                    (.write term data)
                    (stdout))))

        #_stderr #_(fn []
                     (.then
                      (on-invoke (:process/stderr value))
                      (fn [data] (.write term data))))
        resize    #(.fit fit-addon)]
    (.loadAddon term fit-addon)
    (.onData term
             (fn [data]
               (on-invoke (:process/stdin value)
                          (.encode (js/TextEncoder. "utf-8") data))))
    (stdout)
    (.onResize term #(on-invoke (:process/resize value) (.-cols term) (.-rows term)))
    {:component-did-mount
     (fn [this]
       (.addEventListener js/window "resize" resize)
       (.open term (rdom/dom-node this))
       (.fit fit-addon))
     :component-will-unmount
     (fn [_this]
       (.removeEventListener js/window "resize" resize))}))

(defonce sessions (atom {}))

(defn- setup [settings value]
  (let [pid (:process/pid value)]
    (when-not (contains? @sessions pid)
      (swap! sessions assoc pid (make-terminal settings value)))
    (get @sessions pid)))

(spec/def ::process
  (spec/keys :req [:process/stdin
                   :process/stdout]))

(defn process? [value]
  (spec/valid? ::process value))

(defn terminal [settings value]
  (r/create-class
   (merge
    (setup settings value)
    {:should-component-update (constantly false)
     :reagent-render
     (fn []
       [:<>
        [s/div
         {:style
          {:border (str "1px solid " (::c/border settings))
           :background (:background2 settings)
           :border-radius (:border-radius settings)
           :width "100%"
           :height "600px"}}]
        [:style style]])})))
