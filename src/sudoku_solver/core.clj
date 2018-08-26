(ns sudoku-solver.core
  (:require [clojure.string :as string]))

(def sudoku-easy (str ".....8.52"
                      "....234.."
                      "..1.4.7.8"
                      "......52."
                      ".69..5..."
                      "14..3...."
                      ".387...9."
                      "9..3..1.5"
                      "4.6....7."))

(def sudoku-hard (str "8........"
                      "..36....."
                      ".7..9.2.."
                      ".5...7..."
                      "....457.."
                      "...1...3."
                      "..1....68"
                      "..85...1."
                      ".9....4.."))

(defn parse-sudoku [sudoku-string]
  (let [replacements [1 2 3 4 5 6 7 8 9]]
    (mapv (fn [c]
            (if (= c \. )
              replacements
              (- (int c) 48))) sudoku-string)))

(defn print-sudoku [sudoku]
  (->> sudoku
       (map (fn [cell] (if (vector? cell) "." (str cell))))
       (partition-all 9)
       (map (partial string/join " "))
       (run! prn)))

(defn debug-sudoku [sudoku]
  (->> sudoku
       (map (fn [cell] (if (vector? cell)
                         (format "[%-9s]" (string/join cell))
                         (format "%-11s" cell ""))))
       (partition-all 9)
       (map (partial string/join " "))
       (run! prn)))

(defn process-row [row]
  (let [fixed (set (filter int? row))]
    (mapv (fn [cell]
            (if (int? cell)
              cell
              (filterv (comp not fixed) cell)))
          row)))

(defn transpose [xs]
  (apply mapv vector xs))

(defn process-columns [suduko]
  (->> suduko
       (partition-all 9)
       transpose
       (mapv process-row)
       transpose
       (apply concat)
       vec))

(defn process-rows [sudoku]
  (->> sudoku
       (partition-all 9)
       (mapv process-row)
       (apply concat)
       vec))

(defn sub-grids-to-rows [rows]
  (let [rows* (map (partial partition 3) rows)]
    (apply map concat rows*)))

(defn process-groups [sudoku]
  (->> sudoku
       (partition 9)
       (partition 3)
       (mapcat sub-grids-to-rows)
       (mapv process-row)
       (partition 3)
       (mapcat sub-grids-to-rows)
       (apply concat)
       vec))

(defn solved? [sudoku]
  (every? int? sudoku))

(defn legal-state? [sudoku]
  (every? #(or (int? %) (seq %)) sudoku))

(defn candidates [sudoku]
  (let [idx (count (take-while int? sudoku))]
    (map #(assoc sudoku idx %) (get sudoku idx))))

(defn solve [sudoku]
  (cond
    (solved? sudoku)
    sudoku

    (legal-state? sudoku)
    (let [sudoku' (-> sudoku process-rows process-columns process-groups)]
      (loop [candidates (candidates sudoku')]
        (when (seq candidates)
          (if-let [solution (solve (first candidates))]
            solution
            (recur (rest candidates))))))))
