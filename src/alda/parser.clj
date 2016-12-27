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
   :stack   []
   :parsing nil ; the type of token being parsed
   })

(defn parser
  [tokens-ch]
  (assoc initial-parser-state :tokens-ch tokens-ch))

(def token-names
  {:comment    "comment"
   :clj-sexp   "Clojure S-expression"
   :clj-string "Clojure string"
   :clj-char   "Clojure character"
   :event-seq  "event sequence"})

(defn determine-current-token
  [{:keys [stack] :as parser}]
  (let [token (case (first (peek stack))
                nil nil
                \( :clj-sexp
                \" :clj-string
                \[ :event-seq
                (throw
                  (Exception.
                    (format "Can't determine token type of buffer: %s"
                            (peek stack)))))]
    (-> parser (assoc :parsing token))))

(defn emit-token!
  [{:keys [stack tokens-ch] :as parser} token & [content]]
  (>!! tokens-ch [token (or content (peek stack))])
  (-> parser (update :stack pop)
             determine-current-token))

(defn unexpected-char-error
  [{:keys [parsing line column] :as parser} character]
  (let [error-msg (format "Unexpected %s in %s at line %s, column %s."
                          (if (= :EOF character)
                            "EOF"
                            (format "'%s'" character))
                          (get token-names parsing parsing)
                          line
                          column)]
    (-> parser (emit-token! :error error-msg) (assoc :state :error))))

(defn reject-chars
  [parser character blacklist]
  (when (contains? blacklist character)
    (unexpected-char-error parser character)))

(defn next-line
  [parser]
  (-> parser (update :line inc) (assoc :column 1)))

(defn next-column
  [parser]
  (-> parser (update :column inc)))

(defn append-to-current-buffer
  [{:keys [stack] :as parser} x]
  (let [buffer (peek stack)]
    (update parser :stack #(-> % pop (conj (str buffer x))))))

(defn add-current-buffer-to-last
  [{:keys [stack] :as parser}]
  (let [current-buffer (peek stack)
        popped-stack   (pop stack)
        last-buffer    (peek popped-stack)
        last-buffer+   (str last-buffer current-buffer)]
    (-> parser
        (assoc :stack (-> popped-stack pop (conj last-buffer+)))
        determine-current-token)))

(defn read-to-buffer
  [parser x & [size]]
  (let [advance (if (#{\newline "\n"} x)
                  next-line
                  #(-> % (update :column + (or size 1))))]
    (-> parser (append-to-current-buffer x) advance)))

(defn read-to-new-buffer
  [parser x & [size]]
  (-> parser (update :stack conj "")
             (read-to-buffer x size)))

(defn read-whitespace
  [parser character]
  (when (#{\newline \space \,} character)
    (-> parser (read-to-buffer character))))

(defn ensure-parsing
  [{:keys [state] :as parser}]
  (when (not= :parsing state)
    parser))

(defn ignore-return-character
  [parser character]
  (when (= \return character)
    parser))

(defn start-parsing-clj-sexp
  [parser character]
  (when (= \( character)
    (-> parser (read-to-new-buffer \()
               (assoc :parsing :clj-sexp))))

(defn start-parsing-clj-string
  [parser character]
  (when (= \" character)
    (-> parser (read-to-new-buffer \")
               (assoc :parsing :clj-string))))

(defn start-parsing-clj-char
  [parser character]
  (when (= \\ character)
    (-> parser (read-to-new-buffer \\)
               (assoc :parsing :clj-char))))

(defn start-parsing-comment
  [parser character]
  (when (= \# character)
    (-> parser
        (update :stack conj "")
        (assoc :parsing :comment)
        next-column)))

(defn parse-comment
  [{:keys [parsing] :as parser} character]
  (when (= :comment parsing)
    (if (= \newline character)
      (-> parser (emit-token! :comment) next-line)
      (-> parser (read-to-buffer character)))))

(defn parse-clj-string
  [{:keys [parsing stack] :as parser} character]
  (when (= :clj-string parsing)
    (cond
      (= \\ (last (peek stack)))
      (-> parser (read-to-buffer character))

      (= \" character)
      (-> parser (read-to-buffer character) add-current-buffer-to-last)

      :else
      (-> parser (read-to-buffer character)))))


(defn parse-clj-char
  [{:keys [parsing stack] :as parser} character]
  (when (= :clj-char parsing)
    (cond
      (empty? (peek stack))
      (-> parser (read-to-buffer character))

      ((set "0123456789abcdefghijklmnopqrstuvwxyz") character)
      (-> parser (read-to-buffer character))

      :else
      (-> parser (read-to-buffer character) add-current-buffer-to-last))))

(defn finish-parsing-clj-sexp
  [{:keys [stack] :as parser} character]
  (when (= \) character)
    (let [emit-or-continue-parsing-parent (if (= \( (-> stack pop peek first))
                                            add-current-buffer-to-last
                                            #(emit-token! % :clj-expr))]
      (-> parser (read-to-buffer \)) emit-or-continue-parsing-parent))))

(defn parse-clj-sexp
  [{:keys [parsing stack] :as p} c]
  (when (= :clj-sexp parsing)
    (or (reject-chars p c #{:EOF})
        (read-whitespace p c)
        (parse-clj-string p c)
        (parse-clj-char p c)
        (start-parsing-clj-sexp p c)
        (start-parsing-clj-string p c)
        (start-parsing-clj-char p c)
        (finish-parsing-clj-sexp p c)
        (read-to-buffer p c))))

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
  (or (ensure-parsing p)
      (ignore-return-character p c)
      (parse-comment p c)
      (parse-clj-sexp p c)
      (parse-clj-char p c)
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
      (loop [{:keys [state] :as parser} (parser tokens-ch)]
        (if-let [character (<!! chars-ch)]
          (recur (read-character! parser character))
          (do
            (>!! tokens-ch (dissoc parser :tokens-ch))
            (close! tokens-ch)))))

    ; temp: print out tokens as they are parsed
    (thread
      (loop []
        (when-let [token (<!! tokens-ch)]
          (prn token)
          (recur))))))

