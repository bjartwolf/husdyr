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

(defn indexOf [arr val] 
  (cond (= (nth arr 0) val) 0
        (= (nth arr 1) val) 1
        (= (nth arr 2) val) 2
        (= (nth arr 3) val) 3
        (= (nth arr 4) val) 4
))

(defn solve-logic-puzzle []
  (let [people [:ukranian :norwegian :japanese :spaniard :englishman]]
;    (first
      (for [house (permutations people)
            :let [[nr1 nr2 nr3 nr4 nr5] house]  ; 1 
            :when (= :norwegian nr1)
            [yellow blue red green ivory] (permutations people)  ; 1 
            :when (= (- (indexOf house green) (indexOf house ivory)) 1) ;6
            :when (= red :englishman) ;2
            [milk tea water orangejuice coffee] (permutations people) ; cheeses
            :when (= :ukranian tea);5
            :when (= green coffee) ;4
            :when (= (nth house 2) milk)
            [kools chesterfield oldgold parliament luckystrike] (permutations people) 
            :when (= yellow kools) ; 8
            :when (= luckystrike orangejuice) ;13
            [zebra snails dog horse fox] (permutations people) ; cheeses
            :when (or (= (- (indexOf house chesterfield) (indexOf house fox)) 1)
                      (= (+ (indexOf house chesterfield) (indexOf house fox)) 1))
            :when (or (= (- (indexOf house kools) (indexOf house horse)) 1)
                      (= (+ (indexOf house kools) (indexOf house horse)) 1))
            :when (or (= (- (indexOf house :norwegian) (indexOf house blue)) 1) ;15
                      (= (+ (indexOf house :norwegian) (indexOf house blue)) 1))
            :when (= oldgold snails) ;7
            :when (= :spaniard dog);3
            :when (= :japanese parliament)]; 13
; THE CONSTRAINTS IN PLAIN ENGLISH            
;        Of Landon and Jason, one has the 7:30pm reservation and the other loves mozzarella.
;        The blue cheese enthusiast subscribed to Fortune.
;        The muenster enthusiast didn't subscribe to Vogue.
;        The 5 people were the Fortune subscriber, Landon, the person with a reservation at 5:00pm, the mascarpone enthusiast, and the Vogue subscriber.
;        The person with a reservation at 5:00pm didn't subscribe to Time.
;        The Cosmopolitan subscriber has an earlier reservation than the mascarpone enthusiast.
;        Bailey has a later reservation than the blue cheese enthusiast.
;        Either the person with a reservation at 7:00pm or the person with a reservation at 7:30pm subscribed to Fortune.
;        Landon has a later reservation than the Time subscriber.
;        The Fortune subscriber is not Jamari.
;        The person with a reservation at 5:00pm loves mozzarella.

; THE CONSTRAINTS IN CLOJURE (in the same order)
            ;:when (= (set [seven-thirty mozzarella]) (set [:landon :jason]))
            ;:when (= (set [seven-thirty mozzarella]) (set [:landon :jason]))
            ;:when (= blue-cheese fortune)            
            ;:when (not= muenster vogue)
            ;:when (= (count (set [fortune :landon five mascarpone vogue])) 5)
            ;:when (not= five time)
;            :when (< (.indexOf reservations cosmopolitan) (.indexOf reservations mascarpone))
;            :when (> (.indexOf reservations :bailey) (.indexOf reservations blue-cheese))
            ;:when (#{seven seven-thirty} fortune)
;           :when (> (.indexOf reservations :landon) (.indexOf reservations time))
            ;:when (not= fortune :jamari)
            ;:when (= five mozzarella)]

; RETURN THE ANSWER        
        (do
            (println (= (- (indexOf house green) (indexOf house ivory)) 1))
            (println (- (indexOf house green) (indexOf house ivory)))
            (println :green (indexOf house green) :ivory (indexOf house ivory))
            (array-map
          :yellow yellow :water water :kools kools :fox fox 
          :zebra zebra :snails snails :dog dog :horse horse :fox fox :tea tea
          )))))
(defn -main [& args]
       (println "Beb.....")
       (println (solve-logic-puzzle))
)

(set! *main-cli-fn* -main)
