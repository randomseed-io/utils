(ns

    ^{:doc    "Random utils, Reitit support functions."
      :author "Paweł Wilk"
      :added  "1.0.5"}

    io.randomseed.utils.reitit.http

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [reitit.core              :as        r]
            [reitit.ring              :as     ring]
            [ring.util.http-response  :as     resp]
            [io.randomseed.utils      :refer  :all])

  (:import  [reitit.core Match Router]))

;; Class-based single method dispatch for routing operations.

(defprotocol Routable
  "The Routable protocol describes operations on route-related data."

  (^Boolean router? [r]
   "Returns true if the given argument is a Router object.")

  (^Router router [r]
   "Converts or coerces the given argument to a Router."))

(defprotocol Matchable
  "The Identifiable protocol describes operations on path or route ID/name data."

  (^Boolean match? [r]
   "Returns true if the given argument is a Match object.")

  (^Match match [r] [id r] [id params r]
   "Converts or coerces the given argument to a Match."))

;; Protocol implementations.

(extend-protocol Routable

  Match

  (^Boolean router? [r] false)
  (^Router  router  [r] nil)

  Router

  (^Boolean router? [r] true)
  (^Router  router  [r] r)

  nil

  (^Boolean router? [r] false)
  (^Router  router  [r] nil)

  Boolean

  (^Boolean router? [r] false)
  (^Router  router  [r] nil)

  clojure.lang.IPersistentMap

  (^Boolean router? [r] false)
  (^Router  router  [r] (get r ::r/router))

  clojure.lang.Associative

  (^Boolean router? [r] false)
  (^Router  router  [r] (get r ::r/router))

  Object

  ;; (satisfies? Router r)
  (^Boolean router? [r] false))

(extend-protocol Matchable

  Match

  (^Boolean match? [r] true)
  (^Match match
   (^Match [m]     m)
   (^Match [m _]   m)
   (^Match [m _ _] m))

  Router

  (^Boolean match? [r] false)
  (^Match match
   (^Match [r] nil)
   (^Match [r id]          (match id r))
   (^Match [r id params]   (match id params r)))

  clojure.lang.Named

  (^Boolean match? [r] false)
  (^Match match
   (^Match [id] nil)
   (^Match [id r]          (r/match-by-name (router r) id))
   (^Match [id params r]   (r/match-by-name (router r) id params)))

  String

  (^Boolean match? [r] false)
  (^Match match
   (^Match [id] nil)
   (^Match [id r]          (r/match-by-path (router r) id))
   (^Match [id _ r]        (r/match-by-path (router r) id)))

  clojure.lang.IPersistentMap

  (^Boolean match? [r] false)
  (^Match match
   (^Match [req]           (ring/get-match req))
   (^Match [req id]        (match id req))
   (^Match [req id params] (match id params req)))

  clojure.lang.Associative

  (^Boolean match? [r] false)
  (^Match match
   (^Match [req]           (ring/get-match req))
   (^Match [req id]        (match id req))
   (^Match [req id params] (match id params req)))

  nil

  (^Boolean match? [r] false)
  (match
    ([id]          nil)
    ([id r]        nil)
    ([id params r] nil))

  Object
  (^Boolean match? [r] false))

(defn route-data
  "For the given request map, a Reitit match object, or a Reitit router, returns a
  route data map for the current path or for the path specified by its unique
  identifier or a string. Optionally path params map can be given if the path
  supports path params."
  ([req-or-match]
   (get (match req-or-match) :data))
  ([req-or-router name-or-path]
   (get (match req-or-router name-or-path) :data))
  ([req-or-router name-or-path path-params]
   (get (match req-or-router name-or-path path-params) :data)))

(defn route-data-param
  "For the given request map, a Reitit match object, or a Reitit router, returns the
  requested parameter of a route data map for the current path or for the path
  specified by its unique identifier or a string. Optionally path params map can be
  given if the path supports path params."
  ([req-or-match param]
   (get (route-data req-or-match) param))
  ([req-or-router param name-or-path]
   (get (route-data req-or-router name-or-path) param))
  ([req-or-router param name-or-path path-params]
   (get (route-data req-or-router name-or-path path-params) param)))

(defn route-name
  "For the given request map, a Reitit match object, or a Reitit router, returns a
  unique name of a route for the path specified by its unique identifier or a
  string. Optionally path params map can be given if the path supports path params."
  ([req-or-match]
   (route-data-param req-or-match :name))
  ([req-or-router name-or-path]
   (route-data-param req-or-router :name name-or-path))
  ([req-or-router name-or-path path-params]
   (route-data-param req-or-router :name name-or-path path-params)))

(defn route-middleware
  "For the given request map, a Reitit match object, or a Reitit router, returns a
  middleware chain of a route for the path specified by its unique identifier or a
  string. Optionally path params map can be given if the path supports path params."
  ([req-or-match]
   (route-data-param req-or-match :middleware))
  ([req-or-router name-or-path]
   (route-data-param req-or-router :middleware name-or-path))
  ([req-or-router name-or-path path-params]
   (route-data-param req-or-router :middleware name-or-path path-params)))

(defn route-handler
  "For the given request map, a Reitit match object, or a Reitit router, returns a
  handler of a route for the path specified by its unique identifier or a
  string. Optionally path params map can be given if the path supports path params."
  ([req-or-match]
   (route-data-param req-or-match :handler))
  ([req-or-router name-or-path]
   (route-data-param req-or-router :handler name-or-path))
  ([req-or-router name-or-path path-params]
   (route-data-param req-or-router :handler name-or-path path-params)))

(defn route-conflicting?
  "For the given request map, a Reitit match object, or a Reitit router, returns true
  if a route (for the path specified by its unique identifier or a string) is marked
  as conflicting. Optionally path params map can be given if the path supports path
  params."
  ([req-or-match]
   (boolean (route-data-param req-or-match :conflicting)))
  ([req-or-router name-or-path]
   (boolean (route-data-param req-or-router :conflicting name-or-path)))
  ([req-or-router name-or-path path-params]
   (boolean (route-data-param req-or-router :conflicting name-or-path path-params))))

(defn path
  "For the given request map, a Reitit match object, or a Reitit router, returns the
  path of a route for the current path or for a path specified by its unique
  identifier or a string. Optionally path params map can be given if the path
  supports path params."
  ([req-or-match]
   (r/match->path (match req-or-match)))
  ([req-or-router name-or-path]
   (r/match->path (match req-or-router name-or-path)))
  ([req-or-router name-or-path path-params]
   (r/match->path (match req-or-router name-or-path path-params))))

(defn req-or-route-param
  "Works like io.randomseed.utils.reitit.http/route-data-param but first tries to get
  the parameter from a request map, if the first argument is a request map."
  ([req-or-match param]
   (if (and (map? req-or-match) (contains? req-or-match param))
     (get req-or-match param)
     (route-data-param req-or-match param)))
  ([req-or-router param default]
   (if (and (map? req-or-router) (contains? req-or-router param))
     (get req-or-router param)
     (get (route-data req-or-router) param default))))
