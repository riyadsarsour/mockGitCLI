(ns explore
  (:require
   [ring.adapter.jetty :refer [run-jetty]]
   [hiccup.page :refer [html5]]
   [clojure.java.io :as io]))

(def default-port 3000)

(defn handler [_ dir database]
  {:status 200
   :headers {"content-type" "text/html"}
   :body (eval (html5
                :head :title "ResponderHeader"
                :ul (map #(html5 [:li [:a {:href (str "/branch/" %)} %]]) (sort (seq (.list (io/file (str dir "/" database "/" "refs" "/" "heads"))))))))})

(defn launch [port dir database]
  (println (str "Starting server on port"  " "  port "."))
  (run-jetty #(handler % dir database) {:port port}))

(defn explore [args dir database]
  (let [[switch port] args]
    (cond
      ;print help statement
      (or (= "-h" (first args)) (= "--help" (first args)))
      (println "idiot explore: start a web server to explore the database\n\nUsage: idiot explore [-p <port>]\n\nArguments:\n   -p <port>   listen on the given port (default: 3000)")
      ; if database not found
      (not (.exists (io/file (str dir "/" database))))
      (println "Error: could not find database. (Did you run `idiot init`?)")
      ;no p or help
      ;(and (not= switch "-p") (not= switch "-h") (not= switch "--help"))
      ;p w no port
      (and (= switch "-p") (= nil port))
      (println "Error: you must specify a numeric port with '-p'.")
      ; p switch w wrong port form
      (and (= switch "-p") (nil? (re-matches #"[0-9]+" port)))
      (println "Error: the argument for '-p' must be a non-negative integer.")
      ;other check for non negative numbers
      (and (= "-p" switch) (< (Integer/parseInt port) 0))
      (println "Error: the argument for '-p' must be a non-negative integer.")

      :else
      (if (= nil port) (launch default-port dir database)
                       ;else
          (launch (Integer/parseInt port) dir database)))))

;check all switch conditions
;(= switch "-p") (cond
;                  ;see if switch is valid but no port
;                  (= nil (rest args)) (println "Error: you must specify a numeric port with '-p'.")
;                  ;switch given but not a proper port #
;                  (nil? (re-matches #"[0-9]+" (rest args)))(println "Error: the argument for '-p' must be a non-negative integer.")
;                  ;otherwise start server
;                  :else (launch (second args) dir database))
;else launch server w/ default port