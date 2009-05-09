(in-ns 'clobot)
(clojure.core/refer 'clojure.core)

(load "network")

(def *server* "localhost")
(def *port* 6667)

(def *nick* "clobot")
(def *name* "clobot")
(def *domain* "clobot")
(def *serv* "clobot")
(def *realname* "clobot")

(def *admins* '("acieroid"))

(def *chans* '("#test"))

; Useful functions
(defn member [x l] 
  (loop [acc l] 
    (if acc 
      (or (= (first acc) x) 
	  (recur (rest acc)))
      'false)))

; Connection 
(def *connection* (ref nil))

(defn connect []
  (dosync (ref-set *connection* (new-connection *server* *port*))))

(defn send-cmd [cmd]
  (println "Command sent : " cmd)
  (write @*connection* (str cmd "\n")))

; IRC Commands
(defn auth []
  (send-cmd
   (str "NICK " *nick* " \n"
	"USER " *name* " " *domain* " " 
	*serv* " " *realname* "\n")))

(defn join
  ([chan]
     (join chan ""))
  ([chan key]
     (send-cmd
      (str "JOIN " chan " " key))))

(defn say [nick-or-chan message]
  (send-cmd 
   (str "PRIVMSG " nick-or-chan " :" message)))

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

(defn create-privmsg-hook [name fns regex indexes]
  (let [matcher (re-pattern (str ":(\\w+)!.* PRIVMSG (#?\\w+) :"
				 regex))
	indexer (fn [m] 
		  (loop [acc (list (nth 1 m) ; The nick
			     (nth 2 m) ; The chan (if on a chan)
			      )
			 i indexes]
		    (if i 
		      (recur (cons (nth (+ (first i) 2)) acc)
			(rest i)))))]
    (struct hook name fns matcher
		      indexer)))


(defn create-hooks []
  ; Stop the bot
  (add-hook (struct hook 'stop (list (fn [x] (quit))) 
		    #":.* PRIVMSG (#?\w+) :!quit"  #(list %)))
  ; Respond to pings
  (add-hook (struct hook 'pong (list (fn [serv] 
				       (send-cmd (str "PONG :"
						      serv))))
		    #"PING :(.*)" 
		    #(list (second %))))

  ; Join a chan
  (add-hook (struct hook 'join (list (fn [chan]
				       (join chan)))
		    #":.* PRIVMSG .* :!join (#\w+)"
		    #(list (second %))))
  ; Say something
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
  ; Eval clojure code
  ; Not secure at all, be carefull with that
  (add-hook (struct hook 'eval 
		    (list (fn [who chan code]
			    (if (member who *admins*) 
			      (say chan
				   (try (str (eval 
					      (read-string
					       code)))
					(catch Exception e (str e)))))))
		    #":(\w+).* PRIVMSG (#\w+) :!eval (.*)"
		    #(list (second %) (second (rest %)) (second (rest
  (rest %))))))

;  (add-hook (struct hook 'reload (list (fn [] (load "network")
;					 (load "simple-bot")))
;		    #":.* PRIVMSG #?.* :!reload"
;		    #(list)))
  )


; Main loop
(defn start []
  (dosync (ref-set *stop* 0))
  (connect)
  (create-hooks)
  (auth)
  (try (.start (Thread. #(main-loop @*connection*)))
       (catch Exception e (do
			    (println 
			     (str "Caught error :\n" e "\nQuitting"))
			    (quit)))))