(ns ^:figwheel-always snake.core
  (:require-macros [devcards.core :as cards])
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.test :as t :include-macros true :refer-macros [testing is]]))

(enable-console-print!)

(def board-size 20)
(def frame-rate 5)

(def init-state
  {:frame 0
   :grow? false
   :dead? false
   :score 0
   :apple [6 1]
   :snake {:dir [1 0]
           :body (list [3 5] [2 5] [1 5])}})

(defonce app-state
  (atom init-state))

(defn move [snake grow?]
  (let [body (:body snake)
        head (first body)
        dir (:dir snake)]
    (assoc snake :body
           (cons (mapv + dir head)
                 (if grow?
                   body
                   (butlast body))))))

(defn move? [state]
  (if (:dead? state)
    state
    (update state :snake
            move (:grow? state))))

(defn new-apple []
  [(inc (rand-int (dec board-size)))
   (inc (rand-int (dec board-size)))])

(defn eat? [state]
  (let [head (-> state :snake :body first)
        apple (:apple state)]
    (if (= head apple)
      (-> state
          (assoc :grow? true)
          (update :score inc)
          (assoc :apple (new-apple)))
      (-> state
          (assoc :grow? false)))))

(defn dead? [state]
  (let [[head & tail]
        (-> state :snake :body)]
    (if (or ((set tail) head)
            (some #(<= board-size %) head)
            (some #(<= % 0) head))
      (assoc state :dead? true)
      state)))

(defn tick [state]
  (-> state
      (update :frame inc)
      move?
      eat?
      dead?))

(defn timer []
  (js/setTimeout
   (fn []
     (swap! app-state tick)
     (timer))
   (quot 1000 frame-rate)))

(def dir->head-shape
  {[ 1  0] ">"
   [-1  0] "<"
   [ 0  1] "\\/"
   [ 0 -1] "/\\"})

(defn draw [state x y]
  (let [snake (:snake state)
        [head & tail] (:body snake)
        dir (:dir snake)
        apple (:apple state)]
    (cond
      (= head [x y]) (dir->head-shape dir)
      ((set tail) [x y]) "O"
      (= apple [x y]) "\uD83C\uDF4E"
      :else " ")))

(defn board []
  (let [state @app-state
        score (:score state)]
    [:div {:id "board"}
     ;; [:pre (str "board: " state)]
     (when (:dead? state)
       [:div {:id "game-over"}
        [:h1 "GAME OVER!"]
        [:h2 "Score: " score]
        [:button
         {:on-click
          #(reset! app-state
             init-state)}
         "Try again"]])
     [:table {:align "center"}
      [:caption (str "Score: " score)]
      (for [y (range 1 board-size)]
        [:tr {:key y}
         (for [x (range 1 board-size)]
           [:td {:key x}
            (draw state x y)])])]]))

(def keycode->dir
  {38 [0 -1]
   40 [0 1]
   37 [-1 0]
   39 [1 0]})

(defn on-keydown [event]
  (when-let [dir (keycode->dir (.-keyCode event))]
    (swap! app-state assoc-in [:snake :dir] dir)))

;; Side-effects
(defn ^:export init []
  (timer)
  (js/addEventListener "keydown" (partial on-keydown))
  (reagent/render-component [board]
                            (. js/document (getElementById "app"))))

;; Devcards
(cards/defcard state
  app-state)

(cards/defcard controls
  (cards/reagent
   [:div
    [:button
     {:on-click #(reset! app-state init-state)} "reset"]
    [:button {:on-click #(swap! app-state tick)} "tick"]
    [:button {:on-click #(on-keydown #js{:keyCode 37})} "<"]
    [:button {:on-click #(on-keydown #js{:keyCode 38})} "^"]
    [:button {:on-click #(on-keydown #js{:keyCode 39})} ">"]
    [:button {:on-click #(on-keydown #js{:keyCode 40})} "v"]]))

(cards/defcard board
  (cards/reagent [board])
  app-state)

(cards/deftest tests
  (testing "something"
    (is (= 1 1))
    (is (= 1 1))))

;; Figwheel helper
(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  (swap! app-state update-in [:frame] inc)
  )
