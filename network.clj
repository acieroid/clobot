;; @file:network.clj
;; Commandes relatives aux connections, envoi de données dans les
;; sockets, tout ce qui touche au réseau.
(in-ns 'clobot)
(clojure.core/refer 'clojure.core)

(import '(java.net Socket SocketException)
	'(java.io OutputStreamWriter InputStreamReader)
	'(clojure.lang LineNumberingPushbackReader))

(def *stop* (ref nil))
;; TODO:
;; créer une connection
;; écouter sur une connection
;; envoyer du texte sur une connection
;; attendre la réponse
(defmacro debug
  [expr]
  `(let [value# ~expr]
     (println '~expr "=>" value#)
     (flush)
     value#))

;; Les hooks : 
;; Crée un hook via create-recv-hook, qui retourne un hook
;; On ajoute des fonctions au hook
;; Le hook possède une regex, qui quand elle est matchée, appelle
;; toute ses fonctions avec certains arguments (dépend du hook, 
;; nick, chan, message la plupart du temps)
;; La fonction fait ce qu'elle souhaite avec ces arguments
(defstruct hook :name :functions :regexp :parse-args)

(defn add-recv-hook [connection hook]
  "Crée un hook"
  (assoc connection :recv-hooks
	 (cons 
	  hook
	  (:recv-hooks connection))))

(defn replace-hook [connection hook1 hook2]
  "Remplace hook1 par hook2"
  (loop [hooks (:recv-hooks connection) 
	 acc '()]
    (if (= (first hooks) hook1)
      (assoc connection :recv-hooks 
	     (concat acc (list hook2) (rest hooks)))
      (recur (rest hooks) (cons (first hooks) acc)))))

(defn add-func [hook function]
  "Ajoute une fonction à appeler à un hook"
  (assoc hook :functions 
	 (cons function (:functions hook))))

(defn call-functions [hook string]
  "Appelle toutes les fonctions du hook"
  (let [args ((:parse-args hook) (re-find (:regexp hook) string))]
    (doseq [f (:functions hook)]
      (apply f args))))


;; Structure "connection" contenant :
;;   -> Des hooks à appeler quand on reçoit du texte
;;   -> La socket
;; Fonctions associées
;;   -> Créer un hook
;;   -> Envoyer du texte dans la socket
;;   -> Dernier texte reçu dans la socket
(defstruct connection :socket :recv-hooks :input :output)

(defn new-connection 
  "Crée une nouvelle connection, retourne un structure connection"
  [address port]
  (let [s (new Socket address port)]
    (struct connection s '() 
	    (new LineNumberingPushbackReader
		 (new InputStreamReader (. s getInputStream)))
	    (new OutputStreamWriter (. s getOutputStream)))))

(defn write [connection & text]
  "Écrit des données dans une connection, via print"
  (binding [*out* (:output connection)]
    (apply print text)
    (flush)))
  
(defn recv [connection]
  "Lit des données d'une connection, via read"
  (let [cb #(. (:input connection) read)]
    (loop [s nil
	   current-byte (cb)]
      (if (== current-byte 10)
	s
	(recur (str s (char current-byte)) (cb))))))
		 
(defn call-hooks-if-match [connection data]
  (doseq [h (:recv-hooks connection)]
    (if (re-seq (:regexp h) data)
      (do (println "Hook matched" (:name h)) (call-functions h data)))))

; TODO: defmacro main-loop [refresh-time]
;TODO error handling

; Boucle principale
;Thread.currentThread().sleep(1000);//sleep for 1000 ms

(defn main-loop [connection]
  (let [data (recv connection)]
     (call-hooks-if-match connection (debug data)))
  (when (== @*stop* 0)
    (recur connection)))

;(def privmsg-hook (create-recv-hook c #"PRIVMSG :#[\w\-]+ :.+"))
;(create-recv-hook privmsg-hook)
;(add-recv-hook privmsg-hook)
;(add-hook privmsg-hook (fn [nick 



;; Protocole IRC : 
;; Authentification : 
;; NICK <nick>
;; USER nom domaine serveur nomréel

; test
;(def c (new-connection "127.0.0.1" 6667))
