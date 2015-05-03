(ns metacircular-evaluator.core)

(defn self-evaluating? [exp]
  (number? exp))

(defn lookup [symbol env]
  (let [r (get env symbol )]
    (println "lookup" symbol env r)
    r))

(defn bind [params values env]
  (merge env (zipmap params values)))

(defn primitive? [op]
  ( #{'+ '- '* '/} op) )

(defn mapply-primitive [op arglist]
  (println op arglist)
  (let [r (apply @(resolve op) arglist)]
    (println r)
    r))

(declare meval)

(defn mapply [proc arglist]
  (println proc arglist)
  (cond
    (primitive? proc) (mapply-primitive proc arglist)
    (= 'closure (first proc)) (meval (nth (nth proc 1) 1)
                                     (bind (first (nth proc 1))
                                           arglist
                                           (last proc)))))

(defn meval-if [exp env]
  (if (meval (first exp) env)
    (meval (second exp) env)
    (meval (last exp) env)))

(defn meval-list [exp env]
  (println "meval-list" exp env)
  (map (fn [e] (meval e env)) exp))

(defn meval [exp env]
  (cond
    (self-evaluating? exp) exp
    (symbol? exp) (lookup exp env)
    (= 'lambda (first exp)) (list 'closure (rest exp) env)
    (= 'if (first exp)) (meval-if (rest exp) env)
    :else (mapply (meval (first exp) env)
                  (meval-list (rest exp) env)))
  
  )

(def stdenv {'+ '+
             '- '-
             '/ '/
             '* '*})

(defn mrepl []
  (let [env stdenv]
    (loop []
      (print "kathi> ")
      (println (meval (read) env))
      (recur))))
