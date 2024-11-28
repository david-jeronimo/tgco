(ns tgco.core
  (:use [tgco.tgco :only [solve]]))

(defn -main []
  (doseq [file ["sample" "input"]
        part [1 2 3]]
    (let [txt (slurp (str "resources/" file part ".txt"))]
      (->> (solve part txt (= file "sample"))
           time
           println))))