(ns nasher.test.core
  (:use [nasher.core])
  (:use [clojure.test]))

;;;             Keep Quiet  Confess
;;; Keep Quiet  -1, -1      -12, 0
;;; Confess     0, -12      -8, -8

(def prisoners-dilemma
  { :player1 { "Keep Quiet" { "Keep Quiet" -1 "Confess" -12 }
                "Confess"    { "Keep Quiet" 0 "Confess" -8 }}
    :player2 { "Keep Quiet" { "Keep Quiet" -1 "Confess" -12 }
                "Confess"    { "Keep Quiet" 0 "Confess" -8 }}})

(deftest data-extraction-test
  (testing "Fetching"
    (testing "payoffs"
      (is (= -1 (#'nasher.core/payoff :player1 prisoners-dilemma
                                      "Keep Quiet" "Keep Quiet"))))
    (testing "possible strategies"
      (let [strategies (#'nasher.core/possible-strategies :player1
                                                          prisoners-dilemma)]
        (is (= 2 (count strategies)))
        (is (every? #{"Keep Quiet" "Confess"}
                    (#'nasher.core/possible-strategies :player1
                                                        prisoners-dilemma))))
      (is (= (#'nasher.core/possible-strategies :player1 prisoners-dilemma)
             (#'nasher.core/possible-strategies :player2 prisoners-dilemma))))
    (testing "optimal strategy")
    (testing "players"
      (let [ps (#'nasher.core/players prisoners-dilemma)]
        (is (= 2 (count ps)))
        (is (every? #{:player1 :player2}
                    (#'nasher.core/players prisoners-dilemma)))))))

(deftest equilibria-test
  (testing "Finding"
    (testing "Nash equilibria"
      (is (= '(["Confess" "Confess"]) (find-equilibria prisoners-dilemma))))))
