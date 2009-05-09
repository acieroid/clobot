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
  (println "Command sent : " cmd)
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

(defn quit []
  (dosync (ref-set *stop* 1))
  (send-cmd
   (str "QUIT")))


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

; TODO simplifier les hooks des PRIVMSG
(defn create-hooks []
  ; Pour arrêter la connection
  (add-hook (struct hook 'stop (list (fn [x] (quit))) 
		    #":.* PRIVMSG (#?\w+) :!quit"  #(list %)))
  ; Pour le PING
  (add-hook (struct hook 'pong (list (fn [serv] 
				       (send-cmd (str "PONG :"
						      serv))))
		    #"PING :(\w*)" 
		    #(list (second %))))
  (add-hook (struct hook 'say (list (fn [chan what] 
				      (send-cmd (str "PRIVMSG "
						     chan " :"
						     what))))
		    #":.* PRIVMSG (#\w+) :!say (.*)"
		    #(list (second %) (second (rest %)))))
  (add-hook (struct hook 'say-chan (list (fn [chan what]
					   (send-cmd (str "PRIVMSG "
							  chan " :"
							  what))))
		    #":.* PRIVMSG .* :!saychan (#\w+) (.*)"
		    #(list (second %) (second (rest %)))))
;  (add-hook (struct hook 'reload (list (fn [] (load "network")
;					 (load "simple-bot")))
;		    #":.* PRIVMSG #?.* :!reload"
;		    #(list)))
  )

(comment  (let [simple-hook
	(struct hook 'lol (list (fn [x] (dosync (ref-set (debug *stop*) 1))))
		#"lol" #(list %))]
    (dosync (ref-set *connection* 
		     (add-recv-hook @*connection* simple-hook)))))


; Main loop
(defn start []
  (dosync (ref-set *stop* 0))
  (connect)
  (create-hooks)
  (auth)
  (doseq [c *chans*]
    (join-chan c))
  (main-loop @*connection*))