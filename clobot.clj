(in-ns 'clobot)
(clojure.core/refer 'clojure.core)

(load "network")
(declare start)

(def *server* "localhost")
(def *port* 6667)

(def *nick* "clobot")
(def *name* "clobot")
(def *domain* "clobot")
(def *serv* "clobot")
(def *realname* "clobot")
(def *leave-msg* "Bye :)")
(def *max-output* 100)

(def *admins* '("acieroid"))

(def *chans* '("#test"))

; Useful functions
(defn member [x l] 
  (loop [acc l] 
    (if acc 
      (or (= (first acc) x) 
	  (recur (rest acc)))
      'false)))
(defn length [l]
  (loop [n 0
	 acc l]
    (if (rest acc)
      (recur (inc n) (rest acc))
      (inc n))))

; connection 
(def *connection* (ref nil))

(defn connect []
  (dosync (ref-set *connection* (new-connection *server* *port*))))

(defn send-cmd [cmd]
  (println "Command sent : " cmd)
  (write @*connection* (str cmd "\n")))

; irc commands
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
(defn part
  ([chan]
     (part chan *leave-msg*))
  ([chan msg]
     (send-cmd
      (str "PART " chan " " msg))))
     

(defn say [nick-or-chan message]
  (send-cmd 
   (str "PRIVMSG " nick-or-chan " :" message)))

(defn quit []
  (dosync (ref-set *stop* 1))
  (send-cmd
   (str "QUIT")))

; hooks
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
	indexer 
	(fn [m] 
	  (loop [acc (list (nth m 2) ; The chan (if on a chan)
			   (nth m 1) ; The nick
			   )
		 i indexes]
	    (if i 
	      (recur (cons (nth m (+ (first i) 2)) acc)
		     (rest i))
	      (reverse acc))))]
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
  (add-hook (create-privmsg-hook 'join 
				 (list (fn [who chan new-chan]
					 (join new-chan)))
				 "!join (#\\w+)"
				 '(1)))
  ; Leave a chan
  (add-hook (create-privmsg-hook 'leave
				 (list (fn [who chan]
					 (part chan)))
				 "!leave (\\w+)"
				 '(1)))
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
				   (try 
				    (let [val  
					    (str (eval 
						  (read-string
						   code)))]
				      (if (< (length val)
					     *max-output*)
					val
					(str (apply str (take
							 *max-output*
							 val))
					     "... (too long)")))
					(catch Exception e (str e)))))))
		    #":(\w+).* PRIVMSG (#\w+) :!eval (.*)"
		    #(list (second %) (second (rest %)) (second (rest
  (rest %))))))

  ; Reload the bot
  ; Don't work
  (add-hook (struct hook 'reload (list (fn [] (do 
						(quit)
						(load "clobot")
						(start)
						)))
		    #":.* PRIVMSG #?.* :!reload"
		    #(list)))
  ; A simple privmsg hook
  (add-hook (create-privmsg-hook 'hello
				 (list (fn [who chan what]
					 (say chan (str "Hello, "
							what
							" from "
							who))))
				 "!hello (\\w+)"
				 '(1)))

  )


; Main loop
(defn start []
  (dosync (ref-set *stop* 0))
  (connect)
  (create-hooks)
  (auth)
  (try (.start (Thread. #(main-loop *connection*)))
   ;(main-loop *connection*)
       (catch Exception e (do
			    (println 
			     (str "Caught error :\n" e "\nQuitting"))
			    (quit)))))