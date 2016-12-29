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
  {:state          :parsing ; parsing, done, or error
   :line           1
   :column         1
   :stack          []       ; context for nesting tokens
   :pending-tokens []       ; context for consecutive tokens
   :parsing        nil      ; the type of token being parsed
   })

(defn parser
  [tokens-ch]
  (assoc initial-parser-state :tokens-ch tokens-ch))

(def token-names
  {:comment           "comment"
   :clj-sexp          "Clojure S-expression"
   :clj-string        "Clojure string"
   :clj-char          "Clojure character"
   :event-seq         "event sequence"
   :note              "note"
   :rest              "rest"
   :note-or-rest      "note or rest"
   :note-rest-or-name "note, rest, or name"})

(defn determine-current-token
  [{:keys [stack] :as parser}]
  (let [buffer (peek stack)
        token  (case (first buffer)
                 nil nil
                 \( :clj-sexp
                 \" :clj-string
                 \[ :event-seq
                 (throw
                   (Exception.
                     (format "Can't determine token type of buffer: %s"
                             buffer))))]
    (-> parser (assoc :parsing token))))

(defn emit-token!
  [{:keys [stack tokens-ch] :as parser} token & [content]]
  (>!! tokens-ch [token (or content (peek stack))])
  (-> parser (update :stack #(if (empty? %) % (pop %)))
             determine-current-token))

(defn add-to-pending-tokens
  [parser token]
  (-> parser (update :pending-tokens conj token)
             (update :stack #(if (empty? %) % (pop %)))
             determine-current-token))

(defn flush-pending-tokens
  [{:keys [pending-tokens] :as parser}]
  ; TODO: handle chords, variables
  (doseq [[token content] pending-tokens]
    (emit-token! parser token content))
  (-> parser (update :pending-tokens empty)))

(defn unexpected-char-error
  [{:keys [parsing line column] :as parser} character]
  (let [error-msg (format "Unexpected %s%s at line %s, column %s."
                          (if (= :EOF character)
                            "EOF"
                            (format "'%s'" character))
                          (if (#{nil :???} parsing)
                            ""
                            (str " in " (get token-names parsing parsing)))
                          line
                          column)]
    (-> parser (emit-token! :error error-msg) (assoc :state :error))))

(defn caught-error
  [{:keys [parsing line column] :as parser} e]
  (-> parser (emit-token! :error e) (assoc :state :error)))

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

(defn advance
  [parser x & [size]]
  (if (#{\newline "\n"} x)
    (-> parser next-line)
    (-> parser (update :column + (or size 1)))))

(defn append-to-current-buffer
  [{:keys [stack] :as parser} x]
  (if-let [buffer (peek stack)]
    (update parser :stack #(-> % pop (conj (str buffer x))))
    parser))

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
  (-> parser (append-to-current-buffer x) (advance x size)))

(defn read-to-new-buffer
  [parser x & [size]]
  (-> parser (update :stack conj "")
             (read-to-buffer x size)))

(defn read-chars
  [parser character whitelist]
  (when (contains? whitelist character)
    (-> parser (read-to-buffer character))))

(defn discard-buffer
  [parser]
  (-> parser (update :stack pop)
             determine-current-token))

(defn ensure-parsing
  [{:keys [state] :as parser}]
  (when (not= :parsing state)
    parser))

(defn ignore-return-character
  [parser character]
  (when (= \return character)
    parser))

(defn skip-whitespace
  [parser character]
  (when (#{\newline \space} character)
    (advance parser character)))

(defn start-parsing
  [parser character token & [{:keys [start-char ignore-first-char]}]]
  (when (or (nil? start-char)
            (= start-char character)
            (and (set? start-char) (contains? start-char character)))
    (let [init-buffer #(if ignore-first-char
                         (-> % (advance character) (update :stack conj ""))
                         (-> % (read-to-new-buffer character)))]
      (-> parser init-buffer (assoc :parsing token)))))

(defn start-parsing-clj-sexp
  [p c]
  (start-parsing p c :clj-sexp {:start-char \(}))

(defn start-parsing-clj-string
  [p c]
  (start-parsing p c :clj-string {:start-char \"}))

(defn start-parsing-clj-char
  [p c]
  (start-parsing p c :clj-char {:start-char \\}))

(defn start-parsing-comment
  [p c]
  (start-parsing p c :comment {:start-char \# :ignore-first-char true}))

(defn start-parsing-note-or-rest
  [p c & [opts]]
  (start-parsing p c :note-or-rest opts))

(defn start-parsing-name
  [p c & [opts]]
  (start-parsing p c :name opts))

(defn start-parsing-note-rest-or-name
  [p c]
  (start-parsing p c :note-rest-or-name
                 {:start-char (set (str "abcdefghijklmnopqrstuvwxyz"
                                        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))}))

(defn start-parsing-chord-or-instrument-call
  [{:keys [pending-tokens] :as p} c]
  (case (ffirst pending-tokens)
    :note (start-parsing-note-or-rest p c {:start-char \/ :ignore-first-char true})
    :rest (start-parsing-note-or-rest p c {:start-char \/ :ignore-first-char true})
    :name (start-parsing-name p c {:start-char \/ :ignore-first-char true})
    (when (= \/ c)
      (unexpected-char-error p c))))

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
        (read-chars p c #{\newline \space \,})
        (parse-clj-string p c)
        (parse-clj-char p c)
        (start-parsing-clj-sexp p c)
        (start-parsing-clj-string p c)
        (start-parsing-clj-char p c)
        (finish-parsing-clj-sexp p c)
        (read-to-buffer p c))))

(declare read-character!)

(defn parse-note-or-rest
  [{:keys [parsing stack] :as parser} character]
  (when (= :note-or-rest parsing)
    (let [buffer (peek stack)]
      (cond
        (empty? buffer)
        (cond
          (#{\newline \space} character)
          (advance parser character)

          (#{\a \b \c \d \e \f \g \r} character)
          (-> parser (read-to-buffer character))

          :else
          (-> parser (unexpected-char-error character)))

        (#{\+ \-} character)
        (if (re-matches #"[abcdefg][+-]*" buffer)
          (-> parser (read-to-buffer character))
          (-> parser (unexpected-char-error character)))

        (#{\space \newline \~ \| \0 \1 \2 \3 \4 \5 \6 \7 \8 \9} character)
        (-> parser (read-to-buffer character))

        :else
        (-> parser (add-to-pending-tokens (if (= \r (first buffer))
                                            [:rest buffer]
                                            [:note buffer]))
                   (read-character! character))))))

(defn parse-name
  [{:keys [parsing stack] :as parser} character]
  (when (= :name parsing)
    (let [buffer (peek stack)]
      (cond
        (and (empty? buffer) (#{\newline \space} character))
        (advance parser character)

        ((set (str "abcdefghijklmnopqrstuvwxyz"
                   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   "0123456789_-"))
         character)
        (-> parser (read-to-buffer character))

        :else
        (-> parser (add-to-pending-tokens [:name buffer])
                   (read-character! character))))))

(defn parse-note-rest-or-name
  "Parse a character that could be part of:
   - a variable name
   - a note
   - a rest
   - an instrument call"
  [{:keys [parsing stack pending-tokens] :as parser} character]
  (when (= :note-rest-or-name parsing)
    (let [buffer (peek stack)]
      (cond
        (= 1 (count buffer))
        (-> parser (read-to-buffer character))

        (re-matches #"[abcdefgr][\d\|\~\s]" buffer)
        (-> parser (assoc :parsing :note-or-rest) (read-character! character))

        (re-matches #"[a-zA-Z]{2}" buffer)
        (-> parser (assoc :parsing :name) (read-character! character))

        :else
        (-> parser (unexpected-char-error character))))))

(defn finish-parsing
  [parser character]
  (when (= :EOF character)
    (-> parser flush-pending-tokens (assoc :state :done))))

(defn read-character!
  [p c]
  (try
    (or (ensure-parsing p)
        (finish-parsing p c)
        (ignore-return-character p c)
        (parse-comment p c)
        (parse-clj-sexp p c)
        (parse-clj-string p c)
        (parse-clj-char p c)
        (parse-note-or-rest p c)
        (parse-name p c)
        (parse-note-rest-or-name p c)
        (start-parsing-clj-sexp p c)
        (start-parsing-comment p c)
        (start-parsing-note-rest-or-name p c)
        (start-parsing-chord-or-instrument-call p c)
        (skip-whitespace p c)
        (unexpected-char-error p c))
    (catch Throwable e
      (caught-error p e))))

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

