(ns nasher.core
  (:use [clojure.math.combinatorics :only [cartesian-product]]))

(defn- payoff
  "Finds the payoff for player for the given strategies."
  [player game player1-strategy player2-strategy]
  (((game player) player1-strategy) player2-strategy))

(defn- possible-strategies
  "Finds the possible strategies for the given player in the given game."
  [player game]
  (keys (game player)))

(defn- possible-payoffs
  "Returns a seq of maps of possible opponent strategies and their associated
  payoffs."
  [game player]
  (map (game player) (possible-strategies player game)))

(defn- best-payoff
  "Returns the value of the best possible payoff for the given pure strategy."
  [game player strategy]
  (let [payoffs (apply max-key #(% strategy) (possible-payoffs game player))]
    (payoffs strategy)))

(defn- players
  "Finds all of the players in a game."
  [game]
  (keys game))

(defn- best-responses
  "Returns the best responses for player."
  [game player]
  (let [other (first (filter #(not (= player %)) (players game)))
        first? (= player (first (players game))) ; used to keep moves in order
        others-strategies (possible-strategies other game)
        best-payoffs (apply hash-map
                            (flatten
                              (map #(vector % (best-payoff game player %))
                                   others-strategies)))
        possible-moves (cartesian-product (possible-strategies player game)
                                          others-strategies)]
    (map #(apply vector %)
         (map #(if (not first?) (reverse %) %)
              (filter #(= (best-payoffs (second %))
                          (payoff player game (first %) (second %)))
                      possible-moves)))))

(defn find-equilibria
  "Finds any Nash equilibria within the game. Returns a seq of moves."
  [game]
  (let [players (players game)
        p1-best-responses (best-responses game (first players))
        p2-best-responses (best-responses game (second players))
        paired-responses (cartesian-product p1-best-responses
                                            p2-best-responses)]
    (map first (filter #(let [s1 (first %)
                              s2 (second %)]
                          (and (= (first s1) (first s2))
                               (= (second s1) (second s2))))
                       paired-responses))))
