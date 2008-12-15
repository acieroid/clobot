;; @file:network.clj
;; Commandes relatives aux connections, envoi de données dans les
;; sockets, tout ce qui touche au réseau.
(import '(java.net Socket SocketException)
	'(java.io OutputStreamWriter InputStreamReader)
	'(clojure.lang LineNumberingPushbackReader))
(in-ns 'clobot)
(clojure.core/refer 'clojure.core)
;; TODO:
;; créer une connection
;; écouter sur une connection
;; envoyer du texte sur une connection
;; attendre la réponse

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
    (print text)
    (flush)))
  
(defn read [connection]
  "Lit des données d'une connection, via read"
  (read (:input connection) false 'eof))

;; Les hooks : 
;; Crée un hook via create-recv-hook, qui retourne un hook
;; On ajoute des fonctions au hook
;; Le hook possède une regex, qui quand elle est matchée, appelle
;; toute ses fonctions avec certains arguments (dépend du hook, 
;; nick, chan, message la plupart du temps)
;; La fonction fait ce qu'elle souhaite avec ces arguments
(defstruct hook :functions :regexp :parse-args)

(defn create-recv-hook [connection regexp parse-args-func]
  "Crée un hook"
  (assoc connection :recv-hooks
	 (cons 
	  (struct hook '() regexp parse-args-func)
	  (:recv-hooks connection))))

(defn add-func [hook function]
  "Ajoute une fonction à appeler à un hook"
  (assoc hook :functions 
	 (cons function (:functions hook))))

(defn call-functions [hook string]
  "Appelle toutes les fonctions du hook"
  (let [args ((:parse-args hook) string)]
    (doseq [f (:functions hook)]
      (apply f args))))



;(def privmsg-hook (create-recv-hook c #"PRIVMSG :#[\w\-]+ :.+"))
;(create-recv-hook privmsg-hook)
;(add-recv-hook privmsg-hook)
;(add-hook privmsg-hook (fn [nick 



;; Protocole IRC : 
;; Authentification : 
;; NICK <nick>
;; USER nom domaine serveur nomréel