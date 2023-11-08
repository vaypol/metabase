(ns metabase.query-processor.schema
  (:require
   [metabase.util.malli.registry :as mr]))

;; this schema is not very strict because we need to handle different types of queries (legacy MBQL, pMBQL, super-legacy
;; MBQL, internal audit app queries, etc.) and it might not be normalized yet.
(mr/def ::query
  [:and
   :map
   [:fn
    {:error/message "Query with a :type or :lib/type key"}
    (some-fn :type :lib/type)]])

;; TODO -- fill this out a bit.
(mr/def ::metadata :any)

(mr/def ::rff
  [:=>
   [:cat ::metadata]
   [:function
    [:=> [:cat]           :any]
    [:=> [:cat :any]      :any]
    [:=> [:cat :any :any] :any]]])

(mr/def ::qp
  [:=>
   [:cat ::query ::rff ::context]
   :some])
