(ns g-utility.core
  (:require [clojure.data.json :as json])
  (:require [clojure.string :as str])
  (:gen-class)
  )

(use '[clojure.java.io :only (reader file)])


(def N #{"A" "B"})
(def T #{"a" "b" "c"})
(def S "A")

(def P {
        "A" { 1 ["a" "A"] 2 ["B" "b" "A"] 3 ["c" "A" "c" "B"] 4 ["B" "c" "a" "A"] 5 [] }
        "B" { 6 ["c" "B"] 7 ["c"] }
        }
  )

(def P1 {
         "A" [ [ [5 1 2 3 4] [5 2 1 4 3] [5 1 4 2 3] ] (atom 2) ]
         "B" [ [7 6] ]
         }
  )

(defn productions [NT]
  (let [ [values & pos] (P1 NT)]
    (if pos
      (let [ppos ((vec pos) 0)]
        (swap! ppos #(identity %2) (mod (inc @ppos) (count values)))
        (values @ppos)
        )
      values
      )
    )
  )

(defn decompose [v]
  "Dado un vector v de símbolos terminales y no terminales devuelve una 3-tupla
  en el que el primer componente es el mayor prefijo de terminales, el segundo es
  el primer no terminal de izquierda a derecha y el tercero es el sufijo a partir
  del terminal."
  (let [
        f-N (take-while #(some #{%} T) v)
        [N & R] (subvec v (count f-N))
        ]
    [(vec f-N) N (vec R)]
    )
  )

(defn left-derivation [v nx];TODO: actualizar doc
  "Dado un vector v de símbolos terminales y no terminales, y el número nx de una producción,
  devuelve un nuevo vector resultado de derivar por la izquierda a v utilizando la producción nx
  del primer no terminal."
  (let [
        [f-N N R] (decompose v)
        new-N ((P N) nx)
        ]
    [(into (into f-N new-N) R) new-N]
    )
  )

(defn gen-garphviz-tree-model[sol]
  (let [
        first-node (atom 1)
        nodes (atom [[1 [S first-node]]])
        arcs (atom [])
        i (atom 1)
        f (fn[[v postponed] nx]
            (let [
                  top (peek postponed)
                  rest-postponed (pop postponed)
                  [nder new-symbols] (left-derivation v nx)
                  n-atoms (atom [])
                  current-parent-node @top
                  ]

              (swap! top #(identity %2) nx)
              (doseq [s new-symbols]
                (swap! i inc)
                (swap! arcs #(conj %1 %2) [current-parent-node @i])
                (if (some #{s} T)
                  (swap! nodes #(conj %1 %2) [@i s])
                  (do
                    (let [ n-ref (atom @i) ]
                      (swap! nodes #(conj %1 %2) [@i [s n-ref]])
                      (swap! n-atoms #(conj %1 %2) n-ref)
                      )
                    )
                  )
                )
              [nder (->> @n-atoms
                         reverse
                         vec
                         (into rest-postponed)
                         )]; return function f.
              )
            )

        cad (reduce f [[S] [first-node]] sol)
        ]

    [@arcs @nodes]
    )
  )

(defn gen-garphviz-tree[sol]
  (let[
       [arcs nodes] (gen-garphviz-tree-model sol)
       arcs-strs (str/join "\n" (map (fn[[a b]] (format "    %s -- %d;" a b)) arcs))
       nodes-strs (str/join "\n" (map (fn[[a b]]
                                        (if (vector? b)
                                          (let[[s i] b]
                                            (format "    %d [shape=record, label=\"<f0> %s | <f1> %d \"]" a s @i)
                                            )
                                          (format "    %d [label=\"%s\" , shape=plaintext]" a b)
                                          )
                                        ) nodes))
       ]
    (str/join "\n" [ "graph G {" arcs-strs nodes-strs "}"])
    )
  )

(defn isSol-N [x]
  "Devuelve una 2-tupla [res (cad | N)] en el que el primer componente
  dice si una vez aplicadas todas las producciones se obtiene una cadena de
  terminales, el segundo devuelve, para el caso true del primero: el símbolo
  no terminal más a la izquierda, para el otro caso: la solución."
  (let [
        cad (reduce (fn[v nx] (let [[nder _] (left-derivation v nx)] nder)) [S] x)
        res (every? #(some #{%} T) cad)
        ]
    (if res
      [true cad]
      (let [[ _ N _ ] (decompose cad)]
        [false N]
        )
      )
    )
  )

(def result (atom []))
(def search-active (atom true))

(defn processSol [sol]
  (when @search-active
    (if (< (count @result) 20)
      (swap! result #(conj %1 %2) sol)
      (swap! search-active #(identity %2) false)
      )
    )
  )

(defn mif [is-sol x process-sol continue]
  (let [[res TokensOrN] (is-sol x)]
    (if res
      (process-sol x)
      (continue TokensOrN)
      )
    )
  )

(defn bt [x]
  (mif isSol-N x processSol
       (fn[NT]
         (doseq [nx (productions NT)]
           (when @search-active
             (bt (conj x nx))
             )
           )
         )
       )
  )

(defn test5[]
  (println (gen-garphviz-tree [1 2 6 7 5]))
  )


(defn test4[]
  (bt [])
  (doseq [a @result]
    (println a)
    )
  )

(defn test2 []
  (println (isSol-N [1 2 6 7]))
  (println (isSol-N [1 2 6 7 5]))
  )

(defn -main
  [& args]
  (test5)
  )


(defn test3[]
  (println (productions "A"))
  (println (productions "A"))
  (println (productions "A"))
  (println (productions "A"))

  (println (productions "B"))
  (println (productions "B"))
  )


(defn test1 []
  (let [data (with-open [r (reader (file "treeExample.json"))]
               (json/read r :key-fn keyword)
               )
        ]
    (doseq [a (:producciones data)]
      (doseq [b (:pderechas a)]
        (doseq [c b]
          (println c (class c))
          )
        )
      )
    )
  )