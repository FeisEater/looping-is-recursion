(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc b n]
                 (if (zero? n)
                   acc
                   (recur (* acc b) b (dec n))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [s]
                 (if (empty? s)
                   nil
                   (if (empty? (rest s))
                     (first s)
                     (recur (rest s)))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (if (== (count seq1) (count seq2))
    (let [helper (fn [s1 s2]
                 (if (empty? (rest s1))
                   (= (first s1) (first s2))
                   (if (= (first s1) (first s2))
                     (recur (rest s1) (rest s2))
                     false)))]
    (helper seq1 seq2))
    false))

(defn find-first-index [pred a-seq]
  (loop [p pred
         s a-seq
         i 0]
    (if (empty? s)
      nil
      (if (p (first s))
        i
        (recur p (rest s) (inc i))))))

(defn avg [a-seq]
  (loop [acc 0
         i 0
         s a-seq]
    (if (empty? s)
      (/ acc i)
      (recur (+ acc (first s)) (inc i) (rest s)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [s a-seq
         a-set #{}]
    (if (empty? s)
      a-set
      (recur (rest s) (toggle a-set (first s))))))

(defn fast-fibo [number]
  (loop [i 0
         n number
         p1 0
         p2 0]
    (if (> i n)
      p1
      (cond (== i 0) (recur 1 n 0 0)
            (== i 1) (recur 2 n 1 0)
            :else (recur (inc i) n (+ p1 p2) p1)))))

(defn vec-contains? [a-vec elem]
  (loop [v a-vec
         e elem]
    (if (empty? v)
      false
      (if (= elem (first v))
        true
        (recur (vec (rest v)) elem)))))

(defn cut-at-repetition [a-seq]
  (loop [a-vec []
         s a-seq]
    (if (or (empty? s) (vec-contains? a-vec (first s)))
      a-vec
      (recur (conj a-vec (first s)) (rest s)))))


