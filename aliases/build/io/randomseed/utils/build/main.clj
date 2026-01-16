(ns io.randomseed.utils.build.main

  (:require [io.randomseed.utils.build.pom-sync :as pom-sync]
            [clojure.string                     :as      str]
            [clojure.edn                        :as      edn]
            [clojure.java.io                    :as       io]
            [clojure.tools.build.api            :as        b]))

(def repo-root ".")

(defn- first-nonblank-line
  "Returns the first non-blank line (trimmed) from a text file.
   Throws if file has no non-blank lines."
  [path]
  (with-open [r (io/reader path)]
    (or (some (fn [^String line]
                (let [s (str/trim line)]
                  (when-not (str/blank? s) s)))
              (line-seq r))
        (throw (ex-info "VERSION file contains no non-blank lines"
                        {:path path})))))

(defn- kw->name
  ^String [x]
  (cond
    (ident?   x) (name x)
    (string?  x) x
    (nil?     x) nil
    :else        (str x)))

(defn- module-dir ^String [module]
  (str "modules/" (kw->name module)))

(defn- slurp-edn [^String path]
  (when (.exists (io/file path))
    (edn/read-string (slurp path))))

(defn- module-root
  [m]
  (module-dir m))

(defn- file-exists? [^String path]
  (.exists (io/file path)))

(defn- module-files
  "Returns key module paths (deps/pom/src/resources) for a module."
  [module]
  (let [dir       (module-dir module)
        deps      (str dir "/deps.edn")
        pom       (str dir "/pom.xml")
        src       (str dir "/src")
        res       (str dir "/resources")
        class-dir (str dir "/target/classes")]
    {:module    (keyword module)
     :dir       dir
     :class-dir class-dir
     :deps      deps
     :pom       pom
     :src       src
     :res       res}))

(defn- module-deps
  "Reads `deps.edn`."
  [m]
  (slurp-edn (:deps (module-files m))))

(defn- ensure-module!
  "Ensures module exists on disk (deps.edn + pom.xml). Returns module keyword."
  [module]
  (let [m                      (keyword module)
        {:keys [dir deps pom]} (module-files m)]
    (when-not (file-exists? deps)
      (throw (ex-info "Unknown module (missing deps.edn)"
                      {:module m :dir dir :missing deps})))
    (when-not (file-exists? pom)
      (throw (ex-info "Module missing pom.xml"
                      {:module m :dir dir :missing pom})))
    m))

(defn- module-paths
  "List of relative directories to be packed taken from `deps.edn` keys `:paths`.
   Defaults to [\"src\" \"resources\"]."
  [m]
  (let [paths (or (:paths (module-deps m)) ["src" "resources"])]
    (->> paths
         (map #(str (module-root m) "/" %))
         (filter #(-> % io/file .isDirectory)))))

(defn- module-lib
  "io.randomseed/utils-<module> ; module is a keyword."
  [m]
  (symbol (str "io.randomseed/utils-" (name m))))

(defn- module-name
  [m]
  (symbol (str "io.randomseed/utils-" (name m))))

(defn- module-jar-path
  "target/utils-<module>-<version>.jar"
  [m ^String v]
  (format "target/utils-%s-%s.jar" (name m) v))

(defn- pom-stream
  [m]
  (let [{:keys [pom]} (module-files m)]
    (io/input-stream (io/file pom))))

(defn- basis-for
  "Basis created from module's deps.edn."
  [m]
  (let [{:keys [deps dir aliases] :or {aliases []}} (module-files m)]
    (b/create-basis {:project deps :dir dir :aliases aliases})))

(defn- discovered-modules
  "Discovers module names by scanning modules/* with deps.edn and pom.xml."
  []
  (let [mods-dir (io/file "modules")]
    (when-not (.isDirectory mods-dir)
      (throw (ex-info "Missing modules directory" {:dir "modules"})))
    (->> (.listFiles mods-dir)
         (filter #(.isDirectory ^java.io.File %))
         (map #(.getName ^java.io.File %))
         (filter (fn [nm]
                   (and (file-exists? (str "modules/" nm "/deps.edn"))
                        (file-exists? (str "modules/" nm "/pom.xml")))))
         (map keyword)
         (sort))))

(defn- module-class-dir
  [m]
  (let [cd (:class-dir (module-files m))]
    (when (or (not cd) (< (count cd) 18))
      (throw (ex-info "Class directory is not long enough" {:data cd})))
    cd))

(defn- module-pom-file
  [m]
  (:pom (module-files m)))

(defn sync-pom
  [{:keys [module local-root-version] :or {local-root-version "${project.version}"}}]
  (let [{:keys [deps pom]} (module-files module)
        modname            (module-name module)]
    (pom-sync/sync-pom-deps! deps pom {:name modname :local-root-version local-root-version})))

(def version
  (first-nonblank-line (str repo-root "/VERSION")))

(declare jars)

(defn jar
  "Updates POM with dependencies and builds ONE module jar,
   unless :module is missing or :all.
   - clojure -T:build jar :module :core
   - clojure -T:build jar                  ;; builds all
   - clojure -T:build jar :module :all     ;; builds all"
  [{:keys [module] :as opts}]
  (let [mname (kw->name module)]
    (if (or (nil? mname) (= "all" mname))
      (jars opts)
      (let [m (ensure-module! mname)]
        ;; (sync-pom {:module m})
        (let [jar-path  (module-jar-path m version)
              class-dir (module-class-dir m)
              src-dirs  (module-paths     m)
              basis     (basis-for        m)]
          (io/make-parents (io/file jar-path))
          (b/delete      {:path class-dir})
          (b/copy-dir    {:src-dirs src-dirs :target-dir class-dir})
          (b/jar         {:class-dir class-dir
                          :jar-file  jar-path
                          :basis     basis})
          ;; (pack/library {:basis basis
          ;;                :path  jar-path
          ;;                :lib   (module-lib m)
          ;;                :pom   (pom-stream m)})
          )))))

(defn jars
  "Build all discovered module jars."
  [_]
  (doseq [m (discovered-modules)]
    (jar {:module m})))

(defn -main [& _]
  (jars nil))
