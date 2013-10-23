(ns looping-is-recursion)

(defn toggle [a-set elem]
   (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn power [base exp]
   (cond
    (== base 0) 0
    (zero? exp) 1
    :else (let [pawaa (fn [prod base exp]
                        (if (== exp 1)
                          prod
                          (recur (* prod base) base (dec exp))))]
            (pawaa base base exp))))

(defn last-element [a-seq]
   (if (empty? (rest a-seq))
     (first a-seq)
     (last-element (rest a-seq))))

(defn seq= [seq1 seq2]
   (cond
    (and (empty? seq1) (empty? seq2)) true
    (and (= (first seq1) (first seq2))
         (and (not (empty? seq1)) (not (empty? seq2)))) (seq= (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
   (loop [index 0
          n (count a-seq)]
     (cond
      (== index n) nil
      (pred (get a-seq index)) index
      :else (recur (inc index) n))))

(defn avg [a-seq]
   (loop [index 0
          sum 0
          n (count a-seq)]
     (if (== index n)
       (/ sum n)
       (recur (inc index) (+ sum (get a-seq index)) n))))

(defn parity [a-seq]
   (loop [teh-set (set [])
          index 0
          n (count a-seq)]
     (if (== index n)
       teh-set
       (recur (toggle teh-set (get a-seq index))
              (inc index)
              n))))

(defn fast-fibo [n]
   (cond
    (== n 0) 0
    (== n 1) 1
    :else (loop [n-1 1
                 n-2 0
                 index 2]
            (if (== index n)
              (+ n-1 n-2)
              (recur (+ n-1 n-2)
                     n-1
                     (inc index))))))

(defn cut-at-repetition [a-seq]
   (loop [new-seq [(first a-seq)]
          index 1
          n (count a-seq)]
     (cond
      (== index n) new-seq
      (boolean (some #{(get a-seq index)} new-seq)) new-seq
      :else (recur (conj new-seq (get a-seq index))
                   (inc index)
                   n))))





