(ns metabase.search.filter
  "Namespace that defines the filters that are applied to the search results.

  There are required filters and optional filters.
  Archived is an required filters and is always applied, the reason because by default we want to hide archived/inactive entities.

  But there are OPTIONAL FILTERS like :created-by, :created-at, when these filters are provided, the results will return only
  results of models that have these filters.

  The multi method for optional filters should have the default implementation to throw for unsupported models, and then each model
  that supports the filter should define its own method for the filter."
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [honey.sql.helpers :as sql.helpers]
   [metabase.public-settings.premium-features :as premium-features]
   [metabase.search.config :as search.config :refer [SearchableModel SearchContext]]
   [metabase.search.util :as search.util]
   [metabase.util.malli :as mu]))

(def ^:private true-clause [:inline [:= 1 1]])
(def ^:private false-clause [:inline [:= 0 1]])

;; ------------------------------------------------------------------------------------------------;;
;;                                         Required Filters                                         ;
;; ------------------------------------------------------------------------------------------------;;

(defmulti ^:private archived-clause
  "Clause to filter by the archived status of the entity."
  {:arglists '([model archived?])}
  (fn [model _] model))

(defmethod archived-clause :default
  [model archived?]
  [:= (search.config/column-with-model-alias model :archived) archived?])

;; Databases can't be archived
(defmethod archived-clause "database"
  [_model archived?]
  (if archived?
    false-clause
    true-clause))

(defmethod archived-clause "indexed-entity"
  [_model archived?]
  (if-not archived?
    true-clause
    false-clause))

;; Table has an `:active` flag, but no `:archived` flag; never return inactive Tables
(defmethod archived-clause "table"
  [model archived?]
  (if archived?
    false-clause                        ; No tables should appear in archive searches
    [:and
     [:= (search.config/column-with-model-alias model :active) true]
     [:= (search.config/column-with-model-alias model :visibility_type) nil]]))

(defn- search-string-clause
  [model query searchable-columns]
  (when query
    (into [:or]
          (for [column searchable-columns
                token (search.util/tokenize (search.util/normalize query))]
            (if (and (= model "indexed-entity") (premium-features/sandboxed-or-impersonated-user?))
              [:= 0 1]

              [:like
               [:lower column]
               (search.util/wildcard-match token)])))))

(mu/defn ^:private search-string-clause-for-model
  [model          :- SearchableModel
   search-context :- SearchContext]
  (search-string-clause model (:search-string search-context)
                        (map #(search.config/column-with-model-alias model %)
                             (search.config/searchable-columns-for-model model))))


;; ------------------------------------------------------------------------------------------------;;
;;                                         Optional filters                                        ;;
;; ------------------------------------------------------------------------------------------------;;

(defmulti ^:private build-optional-filter-query
  "Build the query to filter by `filter`.
  Dispath with an array of [filter model-name]."
  {:arglists '([model fitler query filter-value])}
  (fn [filter model _query _filter-value]
    [filter model]))

(defmethod build-optional-filter-query :default
  [filter model _query _creator-id]
  (throw (ex-info (format "%s filter for %s is not supported" filter model) {:filter filter :model model})))

;; Created by filters
(defn- default-created-by-fitler-clause
  [model creator-id]
  [:= (search.config/column-with-model-alias model :creator_id) creator-id])

(defmethod build-optional-filter-query [:created-by "card"]
  [_filter model query creator-id]
  (sql.helpers/where query (default-created-by-fitler-clause model creator-id)))

(defmethod build-optional-filter-query [:created-by "dataset"]
  [_filter model query creator-id]
  (sql.helpers/where query (default-created-by-fitler-clause model creator-id)))

(defmethod build-optional-filter-query [:created-by "dashboard"]
  [_filter model query creator-id]
  (sql.helpers/where query (default-created-by-fitler-clause model creator-id)))

(defmethod build-optional-filter-query [:created-by "action"]
  [_filter model query creator-id]
  (sql.helpers/where query (default-created-by-fitler-clause model creator-id)))

;; Verified filters

(defmethod build-optional-filter-query [:verified "card"]
  [_filter model query verified]
  (assert (true? verified) "filter for non-verified cards is not supported")
  (if (premium-features/has-feature? :content-verification)
    (-> query
        (sql.helpers/join :moderation_review
                          [:= :moderation_review.moderated_item_id
                           (search.config/column-with-model-alias model :id)])
        (sql.helpers/where [:= :moderation_review.status "verified"]
                           [:= :moderation_review.moderated_item_type "card"]
                           [:= :moderation_review.most_recent true]))
    (sql.helpers/where query false-clause)))

(defmethod build-optional-filter-query [:verified "collection"]
  [_filter model query verified]
  (assert (true? verified) "filter for non-verified collections is not supported")
  (if (premium-features/has-feature? :official-collections)
    (sql.helpers/where query [:= (search.config/column-with-model-alias model :authority_level) "official"])
    (sql.helpers/where query false-clause)))

(defn- feature->supported-models
  "Return A map of filter to its support models.

  E.g: {:created-by #{\"card\" \"dataset\" \"dashboard\" \"action\"}}

  This is function instead of a def so that optional-filter-clause can be defined anywhere in the codebase."
  []
  (->> (dissoc (methods build-optional-filter-query) :default)
       keys
       (reduce (fn [acc [filter model]]
                 (update acc filter set/union #{model}))
               {})))

;; ------------------------------------------------------------------------------------------------;;
;;                                        Public functions                                         ;;
;; ------------------------------------------------------------------------------------------------;;

(mu/defn search-context->applicable-models :- [:set SearchableModel]
  "Returns a set of models that are applicable given the search context.

  If the context has optional filters, the models will be restricted for the set of supported models only."
  [search-context :- SearchContext]
  (let [{:keys [created-by
                models
                verified]} search-context]
    (cond-> models
      (some? created-by) (set/intersection (:created-by (feature->supported-models)))
      (some? verified)   (set/intersection (:verified (feature->supported-models))))))

(mu/defn build-filters :- map?
  "Build the search filters for a model."
  [honeysql-query :- :map
   model          :- SearchableModel
   search-context :- SearchContext]
  (let [{:keys [archived?
                created-by
                search-string
                verified]}    search-context]
    (cond-> honeysql-query
      (not (str/blank? search-string))
      (sql.helpers/where (search-string-clause-for-model model search-context))

      (some? archived?)
      (sql.helpers/where (archived-clause model archived?))

      ;; build optional filters
      (int? created-by)
      (#(build-optional-filter-query :created-by model % created-by))

      (some? verified)
      (#(build-optional-filter-query :verified model % verified)))))