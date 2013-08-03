(ns husdyr.core)

(defn- index-combinations
  [n cnt]
  (lazy-seq
   (let [c (vec (cons nil (for [j (range 1 (inc n))] (+ j cnt (- (inc n)))))),
	 iter-comb
	 (fn iter-comb [c j]
	   (if (> j n) nil
	       (let [c (assoc c j (dec (c j)))]
		 (if (< (c j) j) [c (inc j)]
		     (loop [c c, j j]
		       (if (= j 1) [c j]
			   (recur (assoc c (dec j) (dec (c j))) (dec j)))))))),
	 step
	 (fn step [c j]
	   (cons (rseq (subvec c 1 (inc n)))
		 (lazy-seq (let [next-step (iter-comb c j)]
			     (when next-step (step (next-step 0) (next-step 1)))))))]
     (step c 1))))

(defn combinations
  "All the unique ways of taking n different elements from items"
  [items n]      
  (let [v-items (vec (reverse items))]
    (if (zero? n) (list ())
	(let [cnt (count items)]
	  (cond (> n cnt) nil
		(= n cnt) (list (seq items))
		:else
		(map #(map v-items %) (index-combinations n cnt)))))))

(defn subsets
  "All the subsets of items"
  [items]
  (mapcat (fn [n] (combinations items n))
	  (range (inc (count items)))))

(defn cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
	step
	(fn step [v-seqs]
	  (let [increment
		(fn [v-seqs]
		  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
		    (if (= i -1) nil
			(if-let [rst (next (v-seqs i))]
			  (assoc v-seqs i rst)
			  (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
	    (when v-seqs
	       (cons (map first v-seqs)
		     (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))


(defn selections
  "All the ways of taking n (possibly the same) elements from the sequence of items"
  [items n]
  (apply cartesian-product (take n (repeat items))))


(defn- iter-perm [v]
  (let [len (count v),
	j (loop [i (- len 2)]
	     (cond (= i -1) nil
		   (< (v i) (v (inc i))) i
		   :else (recur (dec i))))]
    (when j
      (let [vj (v j),
	    l (loop [i (dec len)]
		(if (< vj (v i)) i (recur (dec i))))]
	(loop [v (assoc v j (v l) l vj), k (inc j), l (dec len)]
	  (if (< k l)
	    (recur (assoc v k (v l) l (v k)) (inc k) (dec l))
	    v))))))

(defn- vec-lex-permutations [v]
  (when v (cons v (lazy-seq (vec-lex-permutations (iter-perm v))))))

(defn lex-permutations
  "DEPRECATED as a public function.

In prior versions of the combinatorics library, there were two similar functions: permutations and lex-permutations.  It was a source of confusion to know which to call.  Now, you can always call permutations.  When appropriate (i.e., when you pass in a sorted sequence of numbers), permutations will automatically call lex-permutations as a speed optimization."
  {:deprecated "1.3"}
  [c]
  (lazy-seq
   (let [vec-sorted (vec (sort c))]
     (if (zero? (count vec-sorted))
       (list [])
       (vec-lex-permutations vec-sorted)))))

(defn- sorted-numbers?
  "Returns true iff s is a sequence of numbers in non-decreasing order"
  [s]
  (and (every? number? s)
       (every? (partial apply <=) (partition 2 1 s))))

(defn- multi-perm
  "Handles the case when you want the permutations of a list with duplicate items."
  [l]
  (let [f (frequencies l),
        v (vec (keys f)),
        indices (apply concat
                       (for [i (range (count v))]
                         (repeat (f (v i)) i)))]
    (map (partial map v) (lex-permutations indices))))
  
(defn permutations
  "All the distinct permutations of items, lexicographic by index."
  [items]
  (cond
   (sorted-numbers? items) (lex-permutations items),

   (apply distinct? items)
   (let [v (vec items)]
     (map #(map v %) (lex-permutations (range (count v)))))

   :else
   (multi-perm items)))

;Couldn't find .indexOf in Clojurescript
(defn bebIndex [arr val] 
  (cond (= (nth arr 0) val) 0
        (= (nth arr 1) val) 1
        (= (nth arr 2) val) 2
        (= (nth arr 3) val) 3
        (= (nth arr 4) val) 4
))

;Everything is taken from here....
;http://programming-puzzler.blogspot.no/2013/03/logic-programming-is-overrated.html
(defn solve-logic-puzzle []
  (let [people [:ukranian :norwegian :japanese :spaniard :englishman]]
;    (first
      (for [house (permutations people)
            :let [[yellow blue red ivory green] house]  ; 1 
            :when (= (bebIndex house :norwegian) 0)
            :when (= (- (bebIndex house green) (bebIndex house ivory) 1)) ;6
            :when (= red :englishman) ;2
            [milk tea water orangejuice coffee] (permutations people) ; cheeses
            :when (= :ukranian tea);5
            :when (= green coffee) ;4
            :when (= (nth house 2) milk)
            [kools chesterfield oldgold parliament luckystrike] (permutations people) 
            :when (= yellow kools) ; 8
            :when (= luckystrike orangejuice)
            [zebra snails dog horse fox] (permutations people) ; cheeses
            :when (or (= (- (bebIndex house chesterfield) (bebIndex house fox)) 1)
                      (= (+ (bebIndex house chesterfield) (bebIndex house fox)) 1))
            :when (or (= (- (bebIndex house kools) (bebIndex house horse)) 1)
                      (= (+ (bebIndex house kools) (bebIndex house horse)) 1))
            :when (or (= (- (bebIndex house :norwegian) (bebIndex house blue)) 1)
                      (= (+ (bebIndex house :norwegian) (bebIndex house blue)) 1))
            :when (= oldgold snails) ;7
            :when (= :spaniard dog);3
            :when (= :japanese parliament)]
; RETURN THE ANSWER        
        (array-map
          :yellow yellow :water water :kools kools :fox fox 
          :zebra zebra :snails snails :dog dog :horse horse :fox fox :tea tea
          ))))
(defn -main [& args]
       (println "Beb.....")
       (println (solve-logic-puzzle))
)

(set! *main-cli-fn* -main)
