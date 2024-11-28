(ns tgco.tgco
  (:use [clojure.string :only [split split-lines]]))

(defn parse-input1 [txt]
  (->> (split-lines txt)
       (map read-string)))

(defn parse-input2 [txt]
  (->> (split txt #",")
       (map read-string)))

(defn parse-input3 [txt]
  (letfn [(parse-line [acc line]
            (let [{p0 0 p1 1 p5 5 p6 6 p10 10 p11 11} (split line #" ")]
              (case p0
                "value" (update-in acc [:values (read-string p5)] conj (read-string p1))
                "bot" (assoc-in acc [:bots (read-string p1)]
                                {:low  {(keyword p5) (read-string p6)}
                                 :high {(keyword p10) (read-string p11)}}))))]
    (->> (split-lines txt)
         (reduce parse-line {:values {} :bots {}}))))

(defn tails [coll]
  (reductions (fn [s _] (rest s)) coll coll))

(defn init-population [input]
  (let [population (vec (repeat 9 0))
        add-lf (fn [population lf] (update population lf inc))]
    (reduce add-lf population input)))

(defn tick-day [population]
  (let [{pop-0 0} population]
    (-> (subvec population 1)
        (update 6 + pop-0)
        (conj pop-0))))

(defn process-chip [bots]
  (fn [values]
    (let [[bot-id chips] (->> (filter (comp #(= % 2) count second) values)
                              first)
          {:keys [low high]} (get bots bot-id)
          sort-chip (fn [values low-high min-max]
                      (if-let [target-bot (get low-high :bot)]
                        (update values target-bot conj (apply min-max chips))
                        (update values :output conj (apply min-max chips))))]
      (-> (sort-chip values low min)
          (sort-chip high max)
          (dissoc bot-id)))))

(defn solve [part txt sample?]
  (let [input ((case part 1 parse-input1
                          2 parse-input2
                          3 parse-input3) txt)]
    (case part
      1 (let [target (if sample? 17 2020)]
          (-> (for [[x1 & xs] (tails input)
                    x2 xs
                    :when (= target (+ x1 x2))]
                (* x1 x2))
              first))

      2 (let [num-days (if sample? 18 80)
              population (init-population input)]
          (->> (iterate tick-day population)
               (#(nth % num-days))
               (reduce +)))

      3 (let [{:keys [values bots]} input
              target (if sample? [1 2] [17 61])
              solution? (fn [values] (->> (filter (comp #(= target %) sort second) values)
                                          ffirst))]
          (->> (iterate (process-chip bots) values)
               (some solution?))))))
