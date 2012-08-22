(ns overtone.osc
  (:use [overtone.osc.util]
        [overtone.osc.peer]))

;; We use binding to *osc-msg-bundle* to bundle messages
;; and send combined with an OSC timestamp.
(def ^{:dynamic true} *osc-msg-bundle* nil)

(defn- osc-send-msg
  "Send OSC msg to peer."
  [peer msg]
  (if *osc-msg-bundle*
    (swap! *osc-msg-bundle* #(conj %1 msg))
    (peer-send-msg peer msg)))

(defn- osc-reply-msg
  "Send OSC msg to peer. as a reply"
  [peer msg msg-to-reply-to]
  (peer-reply-msg peer msg msg-to-reply-to))

(defn osc-reply
  "Similar to osc-send except ignores the peer's target address and instead
  sends the OSC message to the sender of msg-to-reply-to. It is not currently
  possible to implicitly build OSC bundles as a reply to an OSC msg."
  [peer msg-to-reply-to path & args]
  (osc-reply-msg peer (apply mk-osc-msg path (osc-type-tag args) args) msg-to-reply-to))

(defn osc-send
  "Creates an OSC message and either sends it to the server immediately
  or if a bundle is currently being formed it adds it to the list of messages."
  [client path & args]
  (osc-send-msg client (apply mk-osc-msg path (osc-type-tag args) args)))

(defn osc-msg
  "Returns a map representing an OSC message with the specified path and args."
  [path & args]
  (apply mk-osc-msg path (osc-type-tag args) args))

(defn osc-bundle
  "Returns an OSC bundle, which is a timestamped set of OSC messages and/or bundles."
  [timestamp & items]
  (mk-osc-bundle timestamp items))

(defn osc-send-bundle
  "Send OSC bundle to client."
  [client bundle]
  (peer-send-bundle client bundle))

(defmacro in-osc-bundle
  "Send a bundle with associated timestamp enclosing the messages in body. Can
  be used to wrap around an arbtrary form. All osc-sends within will be
  automatically added to the bundle."
  [client timestamp & body]
  `(binding [*osc-msg-bundle* (atom [])]
     (let [res# (do ~@body)]
       (osc-send-bundle ~client (mk-osc-bundle ~timestamp @*osc-msg-bundle*))
       res#)))

(defn osc-client
 "Returns an OSC client ready to communicate with a host on a given port via UDP"
  [host port]
  (client-peer host port))

(defn osc-peer
  "Returns a generic OSC peer. You will need to configure it to make
  it act either as a server or client."
  []
  (peer))

(defn osc-target
  "Update the target address of an OSC client so future calls to osc-send
  will go to a new destination. Automatically updates zeroconf if necessary."
  [client host port]
  (update-peer-target client host port)
  client)

(defn osc-close
  "Close an osc-peer, works for both clients and servers. If peer has been
  registered with zeroconf, it will automatically remove it."
  [peer & wait]
  (apply close-peer peer wait)
  peer)

(defn osc-debug
  [& [on-off]]
  (let [on-off (if (= on-off false) false true)]
    (dosync (ref-set osc-debug* on-off))))

(defn osc-now
  "Return the current time in milliseconds"
  []
  (System/currentTimeMillis))
