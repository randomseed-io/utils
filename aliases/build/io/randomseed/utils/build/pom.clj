(ns io.randomseed.utils.build.pom-sync

  (:require [clojure.edn      :as edn]
            [clojure.java.io  :as  io]
            [clojure.string   :as str]
            [clojure.data.xml :as xml]))

(defn- read-edn-file [path]
  (with-open [r (io/reader path)]
    (edn/read {:readers *data-readers*} r)))

(defn- lib->ga
  "deps.edn lib symbol (group/artifact) -> [groupId artifactId]."
  [lib]
  (let [s (str lib)
        i (.indexOf ^String s "/")]
    (if (neg? i)
      [s s]
      [(subs s 0 i) (subs s (inc i))])))

(defn- artifact-base
  "Given a lib symbol like io.randomseed/utils-log returns the artifact base,
   i.e. the part after '/' up to the first '-' (or whole artifact if no '-')."
  [lib]
  (let [s   (str lib)
        i   (.indexOf ^String s "/")
        art (if (neg? i) s (subs s (inc i)))
        j   (.indexOf ^String art "-")]
    (if (neg? j) art (subs art 0 j))))

(defn- coord->version
  "Returns Maven-representable version string, or nil (skip).

   Rules:
   - {:mvn/version \"x\"} -> \"x\"
   - {:local/root \"./...\" or \"../...\"} -> local-root-version ONLY when:
       * :name option was provided, AND
       * artifact-base(lib) == artifact-base(:name)
     Otherwise for relative local/root: nil (skipped).
   - other {:local/root ...} -> local-root-version (backward-compatible)
   - git deps etc -> nil"
  [lib coord {:keys [local-root-version name]
              :or   {local-root-version "${project.version}"}}]
  (cond
    (contains? coord :mvn/version)
    (str (:mvn/version coord))

    (contains? coord :local/root)
    (let [root (str (:local/root coord))
          rel? (or (str/starts-with? root "./")
                   (str/starts-with? root "../"))]
      (cond
        (and rel?
             (some? name)
             (= (artifact-base lib) (artifact-base name)))
        (str local-root-version)

        rel?
        nil

        :else
        (str local-root-version)))

    :else
    nil))

(defn deps->maven-deps
  "Reads deps.edn and returns normalized Maven deps:
   [{:groupId .. :artifactId .. :version ..} ...] sorted deterministically.

   Options:
     :local-root-version (default \"${project.version}\")"
  ([deps-edn-path] (deps->maven-deps deps-edn-path nil))
  ([deps-edn-path opts]
   (let [m    (read-edn-file deps-edn-path)
         deps (:deps m)]
     (->> deps
          (keep (fn [[lib coord]]
                  (when-let [v (coord->version lib coord (or opts {}))]
                    (let [[g a] (lib->ga lib)]
                      {:groupId g :artifactId a :version v}))))
          (sort-by (juxt :groupId :artifactId))
          vec))))

(defn- dependency-node [{:keys [groupId artifactId version scope optional]}]
  {:tag     :dependency
   :attrs   nil
   :content (cond-> [{:tag :groupId    :attrs nil :content [groupId]}
                     {:tag :artifactId :attrs nil :content [artifactId]}
                     {:tag :version    :attrs nil :content [version]}]
              scope            (conj {:tag :scope :attrs nil :content [scope]})
              (some? optional) (conj {:tag :optional :attrs nil :content [(str optional)]}))})

(defn render-dependencies-xml
  "Renders a <dependencies>...</dependencies> XML string for given dep specs.

   dep-specs: vector of maps {:groupId :artifactId :version ...}
   Returns string WITHOUT leading indentation (you indent when splicing)."
  [dep-specs]
  (xml/emit-str
   {:tag     :dependencies
    :attrs   nil
    :content (mapv dependency-node dep-specs)}))

(defn- read-text [path]
  (slurp path :encoding "UTF-8"))

(defn- write-text! [path s]
  (spit path s :encoding "UTF-8"))

(defn- indent-block
  "Prefixes every non-empty line in s with indent."
  [indent s]
  (let [lines (str/split s #"\r?\n" -1)]
    (->> lines
         (map (fn [line]
                (if (str/blank? line) line (str indent line))))
         (str/join "\n"))))

(defn splice-dependencies-section
  "Pure function: returns updated pom.xml text with <dependencies> replaced or inserted.

   Strategy:
   - If existing <dependencies>...</dependencies> present: replace first occurrence.
     Preserve indentation by taking leading whitespace of the opening <dependencies>.
   - If not present: insert before closing </project>, indented like that line.

   Throws if </project> not found."
  [pom-text deps-xml]
  (let [deps-re   #"(?s)(^[ \t]*)<dependencies\b.*?</dependencies>[ \t]*\r?\n?"
        deps-re-m #"(?s)(?m)(^[ \t]*)<dependencies\b.*?</dependencies>[ \t]*\r?\n?"
        m (re-find deps-re-m pom-text)]
    (if m
      (let [indent (nth m 1)
            repl   (str (indent-block indent deps-xml) "\n")]
        (str/replace-first pom-text deps-re-m repl))
      ;; no dependencies section: insert before </project>
      (let [proj-close-re #"(?m)^([ \t]*)</project>[ \t]*\r?$"
            m2 (re-find proj-close-re pom-text)]
        (when-not m2
          (throw (ex-info "Cannot insert <dependencies>: missing </project> in pom.xml"
                          {})))
        (let [indent-close (nth m2 1)
              indent-section (str indent-close "  ")
              insert (str (indent-block indent-section deps-xml) "\n" indent-close "</project>")]
          (str/replace-first pom-text proj-close-re insert))))))

(defn splice-dependencies-section!
  "Reads pom.xml, splices dependencies, writes back.
   Returns {:pom pom-path :changed? boolean}."
  [pom-path deps-xml]
  (let [old (read-text pom-path)
        new (splice-dependencies-section old deps-xml)]
    (when-not (= old new)
      (write-text! pom-path new))
    {:pom pom-path :changed? (not= old new)}))

(defn sync-pom-deps!
  "Reads deps.edn :deps, renders <dependencies> XML, and splices it into pom.xml.

   Options:
     :name (module name)
     :local-root-version (default \"${project.version}\")"
  ([deps-edn-path pom-path]
   (sync-pom-deps! deps-edn-path pom-path nil))
  ([deps-edn-path pom-path opts]
   (let [dep-specs (deps->maven-deps deps-edn-path opts)
         deps-xml  (render-dependencies-xml dep-specs)
         res       (splice-dependencies-section! pom-path deps-xml)]
     (assoc res :deps (count dep-specs)))))
