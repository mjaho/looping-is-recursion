(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                  (if (zero? exp)
                    acc
                    (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

;(helper 1 5 3)
;(helper 5 5 2)
;(helper 25 5 1)
;(helper 125 5 0)
;=> 125


(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2))
      true
    (or (empty? seq1) (empty? seq2))
      false
    (= (first seq1) (first seq2))
      (recur (rest seq1) (rest seq2))
    :else
      false))

(defn find-first-index [pred a-seq]
  (loop [index 0 seq1 a-seq]
    (cond
      (empty? seq1)
        nil
     (pred (first seq1))
       index
     :else
       (recur (inc index) (rest seq1)))))

(defn avg [a-seq]
  (loop [index 0 sum 0 seq1 a-seq]
    (if (empty? seq1)
      (if (= 0 (count a-seq))
       nil
       (/ sum (count a-seq)))
      (recur (inc index) (+ sum (first seq1)) (rest seq1)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [odd-set #{} seq1 a-seq]
    (if (empty? seq1)
      odd-set
      (recur (toggle odd-set (first seq1)) (rest seq1)))))

(defn fast-fibo [n]
  (loop [index 0 fn 1 fn-1 0]
    (if (= n index)
      fn-1
      (recur (inc index) (+ fn fn-1) fn))))


(defn cut-at-repetition [a-seq]
  (loop [seq1 a-seq result [] seen #{}]
    (if (or (contains? seen (first seq1)) (empty? seq1))
      result
      (recur (rest seq1) (conj result (first seq1)) (conj seen (first seq1))))))

