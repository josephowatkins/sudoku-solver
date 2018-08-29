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

(defn parse-row [cs]
  (let [replacements [1 2 3 4 5 6 7 8 9]]
    (mapv (fn [c]
            (if (= c \. )
              replacements
              (- (int c) 48)))
          cs)))

(defn parse-sudoku [sudoku-string]
  (->> sudoku-string
       (partition 9)
       (mapv parse-row)))

(defn row->debug-string [row]
  (->> row
       (map (fn [cell] (if (vector? cell)
                         (format "[%-9s]" (string/join cell))
                         (format "%-11s" cell ""))))
       (string/join " ")))

(defn row->string [row]
  (->> row
       (map (fn [cell] (if (vector? cell) "." (str cell))))
       (string/join " ")))

(defn print-sudoku [row-fn sudoku]
  (->> sudoku
       (map row-fn)
       (run! prn)))

(def debug! (partial print-sudoku row->debug-string))

(def print! (partial print-sudoku row->string))

(defn process-row [row]
  (let [fixed (set (filter int? row))]
    (mapv (fn [cell]
            (if (int? cell)
              cell
              (filterv (comp not fixed) cell)))
          row)))

(defn process-rows [sudoku]
  (mapv process-row sudoku))

(defn transpose [sudoku]
  (apply mapv vector sudoku))

(defn process-columns [suduko]
  (->> suduko
       transpose
       (mapv process-row)
       transpose))

;; todo: move all sub-grid logic in here...
(defn sub-grids-to-rows [rows]
  (let [rows* (map (partial partition 3) rows)]
    (apply map concat rows*)))

(defn process-groups [sudoku]
  (->> sudoku
       (partition 3)
       (mapcat sub-grids-to-rows)
       (mapv process-row)
       (partition 3)
       (mapcat sub-grids-to-rows)
       (mapv vec)))

(defn solved? [sudoku]
  (->> sudoku (apply concat) (every? int?)))

(defn legal-state? [sudoku]
  (->> sudoku (apply concat) (every? #(or (int? %) (seq %)))))

;; todo: ?
(defn candidates [sudoku]
  (let [[i j] (reduce (fn [i row]
                        (if (not-every? int? row)
                          (reduced [i (count (take-while int? row))])
                          (inc i)))
                      0 sudoku)]
    (map #(assoc-in sudoku [i j] %) (get-in sudoku [i j]))))

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
