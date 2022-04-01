(ns io.randomseed.utils.fs

  ^{:doc    "Random utils, filesystem utilities."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.java.io                 :as       io]
            [clojure.java.classpath          :as       cp]
            [clojure.edn                     :as      edn]
            [clojure.string                  :as      str]
            [puget.printer                   :as    puget]
            [io.randomseed.utils             :refer  :all])

  (:import [java.nio.file Path Paths]
           [java.io File]
           [java.net URL]))

(def ^"[Ljava.lang.String;"
  empty-str-ary
  (into-array [""]))

(defn absolute-path?
  ^Boolean [pathname]
  (.isAbsolute ^Path (Paths/get ^String (str pathname)
                                ^"[Ljava.lang.String;" empty-str-ary)))

(def ^Boolean relative-path?
  (complement absolute-path?))

(defn get-java-classpath-folders
  "Lists all directories the exist in Java classpath as a sequence of strings. Returns
  nil if there are none."
  []
  (seq (map str (filter #(.isDirectory (io/file %)) (cp/classpath)))))

(defn prop-pathname
  "For the given Java property name and optional path names creates a path name
  expressed as a string by prefixing them with the directory obtained from a
  property with all parts joined using pathname separator."
  ([prop]       (some-> (System/getProperty (str prop)) str))
  ([prop paths] (some->> paths (remove nil?) seq
                         (apply io/file (System/getProperty (str prop)))
                         str)))

(defn user-dir-pathname
  "For the given pathnames creates a pathname expressed as a string by prefixing them
  with user's directory (typically a project directory) obtained from the Java
  property `user.dir` with all parts joined using current path name separator."
  ([]        (prop-pathname "user.dir"))
  ([& paths] (prop-pathname "user.dir" paths)))


(defn home-dir-pathname
  "For the given pathnames creates a pathname expressed as a string by prefixing them
  with user's home directory obtained from the Java property `user.dir` with all parts
  joined using current path name separator."
  ([]        (prop-pathname "user.home"))
  ([& paths] (prop-pathname "user.home" paths)))

(defn resource-pathname
  "For the given pathnames creates a pathname expressed as a string which resides
  within one of the Java resource directories. The path must exist to be returned."
  ([]        (some-> (io/resource "") io/file str))
  ([& paths] (some->> paths (filter identity) seq
                      (apply io/file) str
                      io/resource
                      io/file
                      str)))

(defn resource-file
  "Returns a java.io.File object for the existing resource of the given name."
  ^File [resource]
  (when resource
    (io/as-file ^URL (io/resource resource))))

(defn resource-exists?
  [r]
  (some? (io/resource r)))

(defn file
  ^File [fname]
  (when fname
    (io/as-file fname)))

(defn basename
  ^String [f]
  (when f
    (.getName ^File (io/as-file f))))

(defn extension
  ^String [f]
  (when-some [^String n (basename f)]
    (let [^"long" idx (str/last-index-of n \.)]
      (when (and idx (< idx (unchecked-dec (count n))))
        (subs ^String n (inc ^"long" idx))))))

(defn abs-pathname
  [path]
  (when (some? path)
    (if (relative-path? path)
      (home-dir-pathname path)
      path)))

(defn with-ns-loading
  "Executes the given function op and passes it a key or a sequence given as k and
  additional, optional arguments. Before that happens it will get all identifiers
  from the sequence (or check if the given single k is an identifier) and try to load
  their namespaces if the identifier(s) is/are fully qualified."
  ([op k & more]
   (if (sequential? k)
     (run! with-ns-loading k)
     (if-not (ident? k)
       k
       (let [n (namespace k)]
         (when (some? n) (try (require (symbol n)) (catch java.io.FileNotFoundException _)))
         (apply op k more)))))
  ([k]
   (with-ns-loading identity k)))

(defn read-preferences
  "Reads the given preferences file. If the path is relative it will be relative to
  user's home directory."
  ([^String filename]
   (when-some [abs-filename (abs-pathname filename)]
     (edn/read-string (slurp abs-filename)))))

(defn write-preferences
  "Writes the given preference file. If the path is relative it will be relative to
  user's home directory."
  [^String filename data]
  (when (some? filename)
    (when-some [abs-filename (abs-pathname filename)]
      (spit abs-filename (puget/pprint-str data)))))

(defn read-lines
  "Read first n lines from a file. If the pathname is relative it will be relative to
  user's home directory."
  ([filename]
   (read-lines 1 filename))
  ([n filename]
   (when (some? filename)
     (when-some [abs-filename (abs-pathname filename)]
       (with-open [rdr (io/reader filename)]
         (doall (take n (line-seq rdr))))))))

(defn exists?
  "Returns true if the file exists."
  [filename]
  (.exists ^java.io.File (io/file filename)))

(defn get-java-property
  [s]
  (when (some? s)
    (System/getProperty (str s))))

(def ^:const prop-regex (re-pattern "\\$\\{[\\_\\-\\.a-zA-Z0-9]+\\}"))

(defn parse-java-property
  [s]
  (let [c (count s)]
    (if (< c 4) s
        (str (when (some? s) (System/getProperty (subs s 2 (dec (count s)))))))))

(defn parse-java-properties
  [s]
  (if-not (string? s) s (str/replace s prop-regex parse-java-property)))

(defn mapv-java-properties
  [v]
  (if-not v v (mapv parse-java-properties v)))
