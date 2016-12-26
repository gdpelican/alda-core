(ns alda.parser
  (:require [clojure.core.async       :refer (chan thread <!! >!! close!)]
            [clojure.string           :as    str]
            [clojure.java.io          :as    io]
            [taoensso.timbre          :as    log]
            [alda.lisp.attributes     :as    attrs]
            [alda.lisp.events         :as    evts]
            [alda.lisp.model.duration :as    dur]
            [alda.lisp.model.pitch    :as    pitch]
            [alda.lisp.score          :as    score]))

(def initial-parser-state
  {:state   :parsing ; parsing, done, or error
   :line    1
   :column  1
   :buffer  ""  ; what we have so far of the token being parsed
   :parsing nil ; the type of token being parsed
   })

(defn parser
  [tokens-ch]
  (assoc initial-parser-state :tokens-ch tokens-ch))

(def token-names
  {:comment    "comment"
   :clj-sexp   "Clojure S-expression"
   :clj-vector "Clojure vector"
   :clj-map    "Clojure map"
   :clj-string "Clojure string"
   :clj-symbol "Clojure symbol"})

(defn emit-token!
  [{:keys [buffer tokens-ch] :as parser} token & [content]]
  (>!! tokens-ch [token (or content buffer)])
  (-> parser (assoc :buffer "" :parsing nil)))

(defn unexpected-char-error
  [character {:keys [line column parsing]}]
  (format "Unexpected '%s' in %s at line %s, column %s."
          character
          (get token-names parsing parsing)
          line
          column))

(defn next-line
  [parser]
  (-> parser (update :line inc) (assoc :column 1)))

(defn next-column
  [parser]
  (-> parser (update :column inc)))

(defn read-to-buffer
  [parser x & [size]]
  (let [advance (if (#{\newline "\n"} x)
                  next-line
                  #(-> % (update :column + (or size 1))))]
    (-> parser (update :buffer str x) advance)))

(defn ignore-return-character
  [parser character]
  (when (= \return character)
    parser))

(defn parse-comment
  [{:keys [parsing] :as parser} character]
  (when (= :comment parsing)
    (if (= \newline character)
      (-> parser (emit-token! :comment) next-line)
      (-> parser (read-to-buffer character)))))

(defn parse-clj-sexp
  [{:keys [parsing balance] :as parser} character]
  (when (= :clj-sexp parsing)
    (cond
      (= \newline character)
      (-> parser next-line (update :buffer str \newline))

      (= \space character)
      (-> parser (read-to-buffer \space))

      (= \, character)
      (-> parser (read-to-buffer \,))

      (= \( character)
      (-> parser (read-to-buffer \() (update :balance conj \())

      (= \[ character)
      (-> parser (read-to-buffer \[)
                 (update :balance conj \[)
                 (assoc :parsing :clj-vector))

      (= \{ character)
      (-> parser (read-to-buffer \{)
                 (update :balance conj \{)
                 (assoc :parsing :clj-map))

      (= \" character)
      (-> parser (read-to-buffer \")
                 (update :balance conj \")
                 (assoc :parsing :clj-string))

      (= \' character)
      (-> parser (read-to-buffer \') (assoc :parsing :clj-symbol))

      (= \) character)
      (cond
        (= [\(] balance)
        (-> parser (read-to-buffer \))
                   (emit-token! :clj-expr)
                   (assoc :balance nil))

        (= \( (last balance))
        (let [balance (subvec balance 0 (dec (count balance)))]
          (-> parser (read-to-buffer \))
                     (assoc :balance balance
                            :parsing (case (last balance)
                                       \( :clj-sexp
                                       \[ :clj-vector
                                       \{ :clj-map))))

        :else
        (-> parser
            (emit-token! :error (unexpected-char-error character parser))
            (assoc :state :error)))

      :else ; FIXME
      (-> parser (read-to-buffer character)))))

(defn start-parsing-clj-sexp
  [parser character]
  (when (= \( character)
    (-> parser (read-to-buffer \()
               (assoc :balance [\(]
                      :parsing :clj-sexp))))

(defn start-parsing-comment
  [parser character]
  (when (= \# character)
    (-> parser (assoc :parsing :comment :buffer "") next-column)))

(defn finish-parsing
  [parser character]
  (when (= :EOF character)
    (assoc parser :state :done)))

; temp
(defn skip-chars
  [parser character]
  (if (= \newline character)
    (-> parser next-line)
    (-> parser next-column)))

(defn read-character!
  [p c]
  (or (ignore-return-character p c)
      (parse-comment p c)
      (parse-clj-sexp p c)
      (start-parsing-clj-sexp p c)
      (start-parsing-comment p c)
      (finish-parsing p c)
      ; TODO: parse more stuff
      (skip-chars p c)))

(defn parse-input
  [input]
  (let [chars-ch  (chan)
        tokens-ch (chan)]
    ; feed each character of input to chars-ch
    (thread
      (doseq [character input] (>!! chars-ch character))
      (>!! chars-ch :EOF)
      (close! chars-ch))

    ; parse tokens from chars-ch and feed them to tokens-ch
    (thread
      (loop [parser (parser tokens-ch)]
        (if-let [character (<!! chars-ch)]
          (recur (read-character! parser character))
          (do
            (>!! tokens-ch parser)
            (close! tokens-ch)))))

    ; temp: print out tokens as they are parsed
    (thread
      (loop []
        (when-let [token (<!! tokens-ch)]
          (prn token)
          (recur))))))

