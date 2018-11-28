;; Talking Clock
;;
;; No more hiding from your alarm clock! You've decided you want your computer to keep
;; you updated on the time so you're never late again. A talking clock takes a 24-hour
;; time and translates it into words.
;;
;; Input Description
;;
;; An hour (0-23) followed by a colon followed by the minute (0-59).
;;
;; Output Description
;;
;; The time in words, using 12-hour format followed by am or pm.
;;
;; Sample Input data:
;;
;; 00:00
;; 01:30
;; 12:05
;; 14:01
;; 20:29
;; 21:00
;;
;; Sample Output data:
;;
;; It's twelve am
;; It's one thirty am
;; It's twelve oh five pm
;; It's two oh one pm
;; It's eight twenty nine pm
;; It's nine pm
;;

(ns clj-algorithms.interview.talking-clock
  (:require [clojure.string :as str]
            [clojure.test :as t]))

(def inputs ["00:00" "01:30" "12:05" "14:01" "20:29" "21:00"])
(def outputs ["It's twelve am"
              "It's one thirty am"
              "It's twelve oh five pm"
              "It's two oh one pm"
              "It's eight twenty nine pm"
              "It's nine pm"])

(t/deftest clock-test [inputs]
  (t/testing "Testing Clock..."
    (let [d (clojure.data/diff (map clock inputs) outputs)]
      (t/is (= (first d) nil))
      (t/is (= (second d) nil)))))

(def hours {0 "twelve" 1 "one" 2 "two" 3 "three" 4 "four" 5 "five"
            6 "six" 7 "seven" 8 "eight" 9 "nine" 10 "ten" 11 "eleven"})

(def teens {0 "ten" 1 "eleven" 2 "twelve" 3 "thirteen" 4 "fourteen"
            5 "fifteen" 6 "sixteen" 7 "seventeen" 8 "eighteen" 9 "nineteen"})

(def tens {0 "oh" 2 "twenty" 3 "thirty" 4 "fourty" 5 "fifty"})

(def ones {1 "one" 2 "two" 3 "three" 4 "four" 5 "five"
           6 "six" 7 "seven" 8 "eight" 9 "nine"})

(defn hour [h]
  (hours (rem h 12)))

(defn minute [m]
  (let [ten (quot m 10)
        one (rem m 10)]
    (cond
      (= ten 0)
        (if (= one 0)
          nil
          (str/join " " ["oh" (ones one)]))
      (= ten 1) (teens one)
      :else (if (ones one)
              (str/join " " [(tens ten) (ones one)])
              (tens ten)))))

(defn meridiem [h]
  (if (< h 12)
    "am"
    "pm"))

(defn clock [time]
  (let [[h m] (map #(Integer/parseInt %) (str/split time #":"))
        mins  (minute m)
        hr    (hour h)
        mer   (meridiem h)]
    (if mins
      (str/join " " ["It's" hr mins mer])
      (str/join " " ["It's" hr mer]))))

