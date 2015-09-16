;;
;; Author José Albert Cruz Almaguer <jalbertcruz@gmail.com>
;; Copyright 2015 by José Albert Cruz Almaguer.
;;
;; This program is licensed to you under the terms of version 3 of the
;; GNU Affero General Public License. This program is distributed WITHOUT
;; ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
;; MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
;; AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
;;

(ns tools.compiler.grammar
  (:require [clojure.set :as cset])
  (:require [clojure.core.match :refer [match]])
  )


(declare compute-first-sets)

(defn setGrammar [g]
  (def grammar g)
  (def sigma (grammar 'sigma))
  (def S (grammar 'S))
  (def P (grammar 'P))
  (def N (cset/difference
           (set (flatten [(keys P) (vals P)]))
           (grammar 'sigma)))
  (def
    ^{:doc "All symbols"}
    alls (cset/union sigma N))
  (def

    ^{:doc "For each non-terminal symbol, it's FIRST set"}
    first-sets (into {}
                 ; init
                 (for [e alls] [e (if (sigma e)
                                    (atom #{e})
                                    (atom #{}))
                                ])))

  (def
    ^{:doc "For each non-terminal symbol, it's FOLLOW set"}
    follow-sets (into {}
                  ; init
                  (for [e N] [e (if (= S e)
                                  ; Regla 1
                                  (atom #{'eot})
                                  (atom #{}))
                              ])))
  (def flag-first false)
  (def flag-follow false)
  )

(defn
  ^{:doc "Total size of FIRST (or FOLLOW) sets"
    :arglists '([d])}
  count-dict [d] (reduce + (for [[_ ys] d] (count @ys))))

(defn
  ^{:doc "Compute the FIRST set from a string of symbols:
          FIRST(X_1 X_2 ... X_n)"
    :arglists '([lstr])}
  first-set-str [lstr]
  (when-not flag-first
    (compute-first-sets))
  (let [res (atom #{})]
    (loop [l lstr]
      (match [l]
        [[]] (swap! res
               #(cset/union % %2)
                 #{'e})
        [[h & t]] (let [hfirst-set @(first-sets h)]
                    (swap! res
                      #(cset/union % %2)
                      (cset/difference hfirst-set #{'e})
                      )
                    (when (hfirst-set 'e)
                      (recur t)
                      ))))
    @res)
  )

(defn LL1-parser-firsts []
  (into {} (for [[k v] P] [k
                           (for [l v] (if (= l [])
                                        {}
                                        (first-set-str l)
                                        ))
                           ])))

(defn
  ^{:doc "Calculate the first sets"
    :arglists '([])}
  compute-first-sets []
  (def flag-first true)
  (loop [cant (count-dict first-sets)]
    ; x -> partes izquierdas (no terminal)
    ; ys -> secuencia de partes derechas (pd),
    ; pd -> secuencia de símbolos de la parte derecha correspondiente
    (doseq [[x ys] P]
      (doseq [yss ys] ; Para cada parte derecha (yss) de un no terminal dado
        (loop [l yss]
          (match [l]
            ; $\epsilon$ es parte de FRIST(X) cuando X derive $\epsilon$
            ; o cuando todos los Y_i lo hayan hecho
            [[]] (swap! (first-sets x)
                   #(cset/union % %2)
                     #{'e})
            ; En cada Y_i (h) cuando sus anteriores derivan $\epsilon$
            [[h & t]] (let [hfirst-set @(first-sets h)]
                        (swap! (first-sets x)
                          ; Añadir a FIRST(X) el resultado de: (FIRST(Y_i) - $\epsilon$)
                          #(cset/union % %2)
                          (cset/difference hfirst-set #{'e}))
                        ; Si FIRST(Y_i) incluye a $\epsilon$ entonces
                        ; se siguen buscando símbolos
                        (when (hfirst-set 'e)
                          (recur t)
                          ))))))
    (let [fcant (count-dict first-sets)]
      (when-not (= cant fcant)
        (recur fcant)))))

(defn
  ^{:doc "Calculate the follow sets"
    :arglists '([])}
  compute-follow-sets []
  (def flag-follow true)
  (loop [cant (count-dict follow-sets)]
    (doseq [[x ys] P]
      (doseq [yss ys :when (not= yss [])]
        (loop [l yss]
          (if (N (first l))
            (match [l]
              ; Regla 3
              [[h & []]] (swap! (follow-sets h)
                           #(cset/union % %2)
                           @(follow-sets x))

              [[h & t]] (let [first-set-t (first-set-str t)]
                          ; Regla 2
                          (swap! (follow-sets h)
                            #(cset/union % %2)
                            (cset/difference first-set-t #{'e}))
                          ; Regla 3
                          (when (first-set-t 'e)
                            (swap! (follow-sets h)
                              #(cset/union % %2)
                              @(follow-sets x)))
                          (recur t)))
            (when (seq l) ; Solamente para no terminales
              (recur (subvec l 1)))))))

    (let [fcant (count-dict follow-sets)]
      (when-not (= cant fcant)
        (recur fcant)))))

(defn get-first-sets []
  (when-not flag-first
    (compute-first-sets))
  (into {} (for [[x ys] first-sets] [x @ys]))
  )

(defn get-follow-sets []
  (when-not flag-first
    (compute-first-sets))
  (when-not flag-follow
    (compute-follow-sets))
  (into {} (for [[x ys] follow-sets] [x @ys]))
  )
