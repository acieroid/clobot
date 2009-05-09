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

(defn send-cmd [cmd]
  (write @*connection* (str cmd "\n")))
; Fonctions IRC

(defn auth []
  (send-cmd
   (str "NICK " *nick* " \n"
	"USER " *name* " " *domain* " " 
	*serv* " " *realname* "\n")))

(defn join-chan 
  ([chan]
     (join-chan chan ""))
  ([chan key]
     (send-cmd
      (str "JOIN " chan " " key))))

(defn say [nick-chan message]
  (send-cmd 
   (str "PRIVMSG " nick-chan " :" message)))


; Hooks
(defn add-hook [hook]
  (dosync (ref-set *connection*
		   (add-recv-hook @*connection* hook))))
(defn add-function [hook-name f]
  (loop [hooks (:recv-hooks @*connection*)]
    (if (= (:name (first hooks)) hook-name)
      (dosync 
       (ref-set *connection*
		(replace-hook @*connection*
			      (first hooks)
			      (add-func (first hooks)
					f))))
      (recur (rest hooks)))))

(defn create-hooks []
  ; Pour arrêter la connection
  (add-hook (struct hook 'stop (list (fn [x] (dosync (ref-set *stop*
							      1)))) 
		    #"clobot/stop-listening"  #(list %)))
  ; Pour le PING
  (add-hook (struct hook 'pong (list (fn [serv] 
				       (send-cmd (str "PONG :"
						      serv))))
		    #"PING :(\w*)" 
		    #(list (second %))))
  )
(comment  (let [simple-hook
	(struct hook 'lol (list (fn [x] (dosync (ref-set (debug *stop*) 1))))
		#"lol" #(list %))]
    (dosync (ref-set *connection* 
		     (add-recv-hook @*connection* simple-hook)))))


; Main loop
(defn start []
  (connect)
  (create-hooks)
  (auth)
  (doseq [c *chans*]
    (join-chan c))
  (main-loop @*connection*))