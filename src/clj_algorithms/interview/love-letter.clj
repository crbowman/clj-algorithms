;; Anonymous Love Letter

;; You have written an anonymous love letter and you don’t want your handwriting to
;; be recognized. Since you don’t have a printer within reach, you are trying to
;; write this letter by copying and pasting characters from a newspaper.

;; Given a string L representing the letter and a string N representing the newspaper,
;; return true if the L can be written entirely from N and false otherwise.
;; The letter includes only ascii characters.

(ns clj-algorithms.interview.love-letter
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn split-on-space [s]
  (str/split s #"\s"))

(defn remove-whitespace [s]
  (->> s
       split-on-space
       (remove str/blank?)
       (str/join "")))

(def love-letter-strings ["This is my love letter."
                          "brown fox"])
(def newspaper-strings ["The quick brown fox jumps over the lazy dog."
                       "The \t quick \n brown fox jumps over the lazy dog."])

(defn love-letter [letter-string news-string]
  (let [l (set (remove-whitespace letter-string))
        n (set (remove-whitespace news-string))]
    (if-not (set/subset? l n)
      false
      (loop [l (frequencies letter-string)
             n (frequencies news-string)]
        (let [[ch l-count] (first l)
              n-count (second (first n))]
          (cond
            (empty? l) true
            (> l-count n-count) false
            :else (recur (dissoc l ch) (dissoc n ch))))))))


(Love-letter (love-letter-strings 0) (newspaper-strings 0))
(love-letter (love-letter-strings 0) (newspaper-strings 1))
(love-letter (love-letter-strings 1) (newspaper-strings 0))
(love-letter (love-letter-strings 1) (newspaper-strings 1))




