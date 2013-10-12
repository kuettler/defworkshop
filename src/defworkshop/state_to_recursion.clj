(ns defworkshop.state-to-recursion
  (:require [workshoplib.tdd :refer […]]))

;; More often than not, state can be avoided by using recursion. Let's see how it works

(defn reverse-coll
  "Reverse the collection `coll`.

   You can use `loop/recur` construct to loop over the sequence.
   `cons` prepends items to the list, try that out."
  [coll]
  (loop [e (first coll) r (rest coll) s nil]
    (if e
      (recur (first r) (rest r) (cons e s))
      s)))

(defn recursive-sum
  "We've already implemented sum using reduce, now let's move to implementing it via recursion!"
  [[head & tail]]
  (if head
    (+ head (recursive-sum tail))
    0))

(defn recursive-sum-tc
  "with a tail-recursive version of sum, we can avoid stack overflows."
  ([coll]
     (recursive-sum-tc coll 0))
  ([[head & tail] acc]
     (if head
       (recur tail (+ acc head))
       acc)))

(defn max-from-list
  "Get the maximum from list using recursion"
  [[head & tail]]
  (if (empty? tail)
    head
    (max head (max-from-list tail))))

(defn my-reduce
  "generalizing the recursive sum example, write your own implementation of reduce! (for empty coll, just return nil.)"
  ([f [head & tail]]
     (my-reduce f head tail))
  ([f acc-init coll]
     (if (seq coll)
       (my-reduce f (f acc-init (first coll)) (rest coll))
       acc-init)))

(defn max-from-list-tc
  "Get the maximum from list using tail recursion (avoid stack overflow)"
  ([coll]
     (max-from-list-tc coll 0))
  ([[head & tail] m]
     (if head
       (recur tail (max m head))
       m
       )))

(defn loop-recur-sum
  "This implementation is somewhat easier to understand for people coming from imperative style."
  [numbers]
  (loop [s 0 [head & tail] numbers]
    (if head
      (recur (+ s head) tail)
      s)))

(defn map-from-keys-and-vals
  "Something that we've already implemented previously in terms of `zipmap`, but are going to implement again
   in terms of recursion. Usually you use `loop/recur` construct whenever you have a one or multiple accumulators
   or several collections you iterate over."
  [keys vals]
  (loop [[khead & ktail] keys
         [vhead & vtail] vals
         m {}]
    (if (and khead vhead)
      (recur ktail vtail (assoc m khead vhead))
      m
    )))

(defn parentheses-balanced?
  "Check wether the given string has balanced parantheses or no.

   You can use `cond` statement to avoid deeply nested ints.
   It's a recursive problem, so you'll have to build up stack to solve it.

   `inc` increments a number, `dec` decrements a number."
  ([str] (parentheses-balanced? str 0))
  ([[current & tail] count]
     (cond
      (= current \()
      (recur tail (inc count))
      (= current \))
      (recur tail (dec count))
      (not current)
      (= count 0)
      :else
      (recur tail count)
      )))
