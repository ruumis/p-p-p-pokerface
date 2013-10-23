(ns p-p-p-pokerface)

(def ranks-for-cards
  {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst snd] card]
  (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
    (get ranks-for-cards fst))))



(defn
  suit
  [card]
  (let [[fst snd] card]
  (str snd )))



(defn same-ranks [hand]
  (vals (frequencies (map rank hand))))

(defn same-rank? [hand ammount]
  (>= (apply max (same-ranks hand)) ammount))

(defn pair? [hand]
  (same-rank? hand 2))



(defn three-of-a-kind? [hand]
  (same-rank? hand 3))



(defn four-of-a-kind? [hand]
  (same-rank? hand 4))



(defn flush? [hand]
  (>= (apply max
             (vals (frequencies (map suit hand)))) 5))



(defn full-house? [hand]
  (and (same-rank? hand 3) (>= (apply min (same-ranks hand)) 2)))



(defn two-pairs? [hand]
  (< (get (frequencies (same-ranks hand)) 1) 2))



(defn straight? [hand]
  (let [sorted-cards (sort
                      (if (= (first (sort (map rank hand))) 2)
                        (replace {14 1} (map rank hand))
                        (map rank hand)))]
    (let [[first second third fourth fifth ] sorted-cards]
      (and (= (- second first)
              (- third second)
              (- fourth third)
              (- fifth fourth) 1)))))





(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))



(defn value [hand]
  (if (straight-flush? hand)
    8
    (if (four-of-a-kind? hand)
      7
      (if (full-house? hand)
        6
        (if (flush? hand)
          5
          (if (straight? hand)
            4
            (if (three-of-a-kind? hand)
              3
              (if (two-pairs? hand)
                2
                (if (pair? hand)
                  1
                  0)))))))))


