(ns parser
  (:require [grammars :as wae]))

(defn parser-AE
  "Convierte una expresión aritmética en una instancia de WAE."
  [exp]
  (cond
    (number? exp) (wae/numG exp)
    (symbol? exp) (wae/idG exp)
    (list? exp)
    (let [op (first exp)]
      (cond
        (= op '+) (wae/addG (parser-AE (second exp)) (parser-AE (nth exp 2)))
        (= op '-) (wae/subG (parser-AE (second exp)) (parser-AE (nth exp 2)))
        :else (throw (IllegalArgumentException. (str "Operación no soportada: " op)))))
    :else (throw (IllegalArgumentException. (str "Expresión inválida: " exp)))))

(defn parser-WAE
  "Convierte una expresión con variables (WAE) en una instancia de WAE."
  [exp]
  (cond
    (number? exp) (wae/numG exp)
    (symbol? exp) (wae/idG exp)
    (list? exp)
    (let [op (first exp)]
      (cond
        (= op '+) (wae/addG (parser-WAE (second exp)) (parser-WAE (nth exp 2)))
        (= op '-) (wae/subG (parser-WAE (second exp)) (parser-WAE (nth exp 2)))
        (= op 'with) (let [[id val] (second exp)
                           body (nth exp 2)]
                       (wae/withG (wae/bindings id (parser-WAE val)) (parser-WAE body)))
        :else (throw (IllegalArgumentException. (str "Operación no soportada: " op)))))
    :else (throw (IllegalArgumentException. (str "Expresión inválida: " exp)))))
