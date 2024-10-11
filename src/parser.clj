(ns parser
  (:require [grammars :refer :all]))

(defn parser-AE
  "Convierte una expresión aritmética en una instancia de WAE."
  [exp]
  (cond
    (number? exp) (numG exp)
    (symbol? exp) (idG exp)
    (list? exp)
    (let [op (first exp)]
      (cond
        (= op '+) (addG (parser-AE (second exp)) (parser-AE (nth exp 2)))
        (= op '-) (subG (parser-AE (second exp)) (parser-AE (nth exp 2)))
        :else (throw (IllegalArgumentException. (str "Operación no soportada: " op)))))
    :else (throw (IllegalArgumentException. (str "Expresión inválida: " exp)))))

(defn parser-WAE
  "Convierte una expresión con variables (WAE) en una instancia de WAE."
  [exp]
  (cond
    (number? exp) (numG exp)
    (symbol? exp) (idG exp)
    (list? exp)
    (let [op (first exp)]
      (cond
        (= op '+) (addG (parser-WAE (second exp)) (parser-WAE (nth exp 2)))
        (= op '-) (subG (parser-WAE (second exp)) (parser-WAE (nth exp 2)))
        (= op 'with) (let [[id val] (second exp)
                           body (nth exp 2)]
                       (withG (bindings id (parser-WAE val)) (parser-WAE body)))
        :else (throw (IllegalArgumentException. (str "Operación no soportada: " op)))))
    :else (throw (IllegalArgumentException. (str "Expresión inválida: " exp)))))
