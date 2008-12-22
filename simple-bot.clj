(in-ns 'clobot)
(clojure.core/refer 'clojure.core)

;(load "network")
;(def *stop* 1)
;(def *stop* nil)


(def *server* "127.0.0.1")
(def *port* 6667)

(def *nick* "clobot")
(def *name* "clobot")
(def *domain* "clobot")
(def *serv* "clobot")
(def *realname* "clobot")

(def *chans* '("#test"))

; Tools
       

; Connection 
(def *connection* (ref nil))

(defn connect []
  (dosync (ref-set *connection* (new-connection *server* *port*))))

(defn auth []
  (write @*connection*
	 (str "NICK " *nick* " \n"
	      "USER " *name* " " *domain* " " *serv* " " *realname* " \n")))
; Hooks
(defn create-hooks []
  (let [simple-hook
	(struct hook '(#(ref-set *stop* 1)) #"lol" #(list %))]
    (dosync (ref-set *connection* 
		     (add-recv-hook @*connection* simple-hook)))))
			     
			     

; Main loop
(defn start []
  (connect)
  (create-hooks)
  (auth)
  (main-loop @*connection*))