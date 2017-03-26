;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(ns rainbow-posting.core
  (:require [clojure.string :as s])
  (:import [java.lang Math])
  (:gen-class))

(def rainbow-colors
  "Violet to red"
  (vec (map (fn [[r g b]] {:red r :green g :blue b})
            [[148 0 211]
             [75 0 130]
             [0 0 255]
             [0 255 0]
             [255 255 0]
             [255 127 0]
             [255 0 0]])))

(defn split-into-whitespace
  [text]
  (re-seq #"\S+" text))

(defn count-printing-chars
  "Counts non-whitespace chars"
  [text]
  (->> (split-into-whitespace text)
       (map count)
       (reduce +)))

(defn drop-nth
  "Drops nth item in coll"
  [n coll]
  (keep-indexed #(when (not= %1 n) %2) coll))

(def zip (partial map vector))

(defn generate-colors-by-text
  [text]
  (let* [printing-chars (count-printing-chars text)
         spread-factor (/ printing-chars (count rainbow-colors))]
    (cond (= (java.lang.Math/floor spread-factor) 1) rainbow-colors
          (< (java.lang.Math/floor spread-factor) 1) (loop [ret rainbow-colors]
                                                       (if (= (count ret) printing-chars)
                                                         ret
                                                         ;; continually drop the middle position
                                                         (recur (drop-nth (Math/round (float (/ (count ret) 2)))
                                                                          ret))))
          :else (let [intermediate-colors (max (int spread-factor) 1) ;; num of colors in between each rainbow-color
                      leftover-colors (mod printing-chars (count rainbow-colors))]
                  ;; 1-to-1 correspondence between 'colors' generated and chars
                  ;; total colors == (+ (* intermediate-colors (count rainbow-colors)) leftover-colors) == printing-chars
                  (loop [acc (if (zero? leftover-colors) []
                                 ;; put "half" the leftover colors in front as first color
                                 ;; use FLOOR at beginning (remember to use CEIL at end to balance it)
                                 (vec (repeat (Math/floor (/ leftover-colors 2))
                                              (first rainbow-colors))))
                         intermediate-count intermediate-colors
                         rainbow-colors rainbow-colors]
                    (if (empty? rainbow-colors)
                      ;; return ACC
                      ;; append LEFTOVER-COLORS if any
                      (if (zero? leftover-colors) acc
                          (into acc (vec (repeat (Math/ceil (/ leftover-colors 2)) ;; uses CEIL to balance out FLOOR priorly
                                                 (last acc)))))
                      (recur (let [{nred :red ngreen :green nblue :blue :as next} (or (second rainbow-colors) (first rainbow-colors))
                                   ;; use current color if it's is the last one
                                   {red :red green :green blue :blue :as this-color} (first rainbow-colors)
                                   ;; INTERMEDIATE-DISTANCE is relative completion of color change
                                   intermediate-distance (- 1 (/ intermediate-count intermediate-colors))
                                   transform-color (fn [old new]
                                                     (int (+ old (* intermediate-distance
                                                                    (- new old)))))]
                               (conj acc {:red (transform-color red nred)
                                          :green (transform-color green ngreen)
                                          :blue (transform-color blue nblue)}))
                             (if (zero? (dec intermediate-count))
                               intermediate-colors
                               (dec intermediate-count))
                             (if (zero? (dec intermediate-count))
                               (rest rainbow-colors)
                               rainbow-colors))))))))

(defn tag-chars-with-colors
  [text]
  (loop [colors (generate-colors-by-text text)
         text-letters (map #(s/split % #"")
                           (split-into-whitespace text))
         acc []]
    (if (or (empty? text-letters)
            (empty? colors))
      acc
      (recur (drop (count (first text-letters)) colors)
             (rest text-letters)
             (conj acc
                   (zip (take (count (first text-letters)) colors)
                        (first text-letters)))))))

(defn rgb-to-hex
  [{red :red green :green blue :blue}]
  (let [to-hex (fn [dec]
                 (format "%02x" dec))]
    (apply str (map to-hex [red green blue]))))


(def ^:dynamic *columns* 0)

(defn compile-to-tex
  "Compiles to TeX commands for MathJAX. Accepts OUTPUT from TAG-CHARS-WITH-COLORS."
  [tagged-output column-width {open :open close :close}]
  (binding [*columns* *columns*]
    (let [space "\\text{ }"
          color-and-text #(str "\\color{#" %1 "}{\\text{" %2 "}}")] ;; color in hexadecimal
      (str open
           (s/join space
                   (map (fn [word-vec]
                          (set! *columns* (+ *columns* (count word-vec)))
                          (str
                           (when (> *columns* column-width)
                             ;; this word will overflow, so make new delimiter (put this word on new line)
                             (set! *columns* (count word-vec))
                             (str close open))
                           (apply str (map (fn [[rgb char]]
                                             (color-and-text (rgb-to-hex rgb) char))
                                           word-vec))))
                        tagged-output))
           close))))

(defn -main
  "Does the only thing, compiles to mathJAX notation with word wrap at 60 cols."
  [text]
  (-> text
      tag-chars-with-colors
      ;; TO END USER: CUSTOMIZE HERE!
      ;;                 vv open and close tags
      (compile-to-tex 60 {:open "[eqn]" :close "[/eqn]"})
      ;;              ^^ column width for word wrap
      println))
