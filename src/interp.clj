; Continuara...
(ns interp
  (:require [grammars :refer :all]))

(defn lookup [id bindings]
  (let [found (first (filter #(= (:id %) id) bindings))]
    (if found
      (:value found)
      (throw (IllegalArgumentException. (str "Variable no definida: " id))))))

(defn interp [exp env]
  (cond
    (instance? NumG exp) (:n exp)
    (instance? IdG exp) (lookup (:i exp) env)
    (instance? AddG exp) (+ (interp (:izq exp) env) (interp (:der exp) env))
    (instance? SubG exp) (- (interp (:izq exp) env) (interp (:der exp) env))
    (instance? WithG exp) 
    (let [binding (:assign exp)]
      (interp (:body exp) (cons (bindings (:id binding) (interp (:value binding) env)) env)))
    :else (throw (IllegalArgumentException. (str "Expresión desconocida: " exp)))))