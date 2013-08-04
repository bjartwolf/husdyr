(ns husdyr.core
  (:require [copiedCombinatorics :as comb]))
; Naive helper function as .indexOf is not available in Clojurescript
(defn indexOf [arr val] 
  (cond (= (nth arr 0) val) 0
        (= (nth arr 1) val) 1
        (= (nth arr 2) val) 2
        (= (nth arr 3) val) 3
        (= (nth arr 4) val) 4
))
; Taken all ideas from here...
; http://programming-puzzler.blogspot.no/2013/03/logic-programming-is-overrated.html
(defn solve-logic-puzzle []
  (let [people [:ukranian :norwegian :japanese :spaniard :englishman]]
    (first
      (for [house (comb/permutations people)
            :let [[nr1 nr2 nr3 nr4 nr5] house]  ; 1 
            :when (= :norwegian nr1)
            [yellow blue red green ivory] (comb/permutations people)  ; 1 
            :when (= (- (indexOf house green) (indexOf house ivory)) 1) ;6
            :when (= red :englishman) ;2
            [milk tea water orangejuice coffee] (comb/permutations people) 
            :when (= :ukranian tea);5
            :when (= green coffee) ;4
            :when (= (nth house 2) milk)
            [kools chesterfield oldgold parliament luckystrike] (comb/permutations people) 
            :when (= yellow kools) ; 8
            :when (= luckystrike orangejuice) ;13
            [zebra snails dog horse fox] (comb/permutations people) 
            :when (or (= (- (indexOf house chesterfield) (indexOf house fox)) 1)
                      (= (+ (indexOf house chesterfield) (indexOf house fox)) 1))
            :when (or (= (- (indexOf house kools) (indexOf house horse)) 1)
                      (= (+ (indexOf house kools) (indexOf house horse)) 1))
            :when (or (= (- (indexOf house :norwegian) (indexOf house blue)) 1) ;15
                      (= (+ (indexOf house :norwegian) (indexOf house blue)) 1))
            :when (= oldgold snails) ;7
            :when (= :spaniard dog);3
            :when (= :japanese parliament)]; 13

; RETURN THE ANSWER        
       (array-map
          :yellow yellow :water water :kools kools :fox fox 
          :zebra zebra :snails snails :dog dog :horse horse :fox fox :tea tea
          )))))
(defn -main [& args]
       (println "Beb.....")
       (println (solve-logic-puzzle))
)

(set! *main-cli-fn* -main)
