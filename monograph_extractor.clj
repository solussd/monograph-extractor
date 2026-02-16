#!/usr/bin/env bb

(require '[babashka.process :as p]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[babashka.fs :as fs])

(def base-url "https://app.monograph.com")
(def cookie-jar "/tmp/monograph-cookies.txt")

;; ---------------------------------------------------------------------------
;; HTTP / Auth
;; ---------------------------------------------------------------------------

(defn curl! [& args]
      (let [cmd (into ["curl.exe" "-s" "-b" cookie-jar "-c" cookie-jar] args)
            result (apply p/process {:out :string :err :string} cmd)]
        (:out @result)))

(defn login! [email password]
      (when (fs/exists? cookie-jar) (fs/delete cookie-jar)
        (let [page (curl! (str base-url "/login"))
              csrf (second (re-find #"name=\"authenticity_token\"\s+value=\"([^\"]+)\"" page))
              _ (when-not csrf (throw (ex-info "No CSRF token" {})))

              body (str "authenticity_token=" (java.net.URLEncoder/encode csrf "UTF-8")
                        "&user%5Bemail%5D=" (java.net.URLEncoder/encode email "UTF-8")
                        "&user%5Bpassword%5D=" (java.net.URLEncoder/encode password "UTF-8")
                        "&user%5Bremember_me%5D=0"
                        "&user%5Bremember_me%5D=1"
                        "&commit=Log+in+with+password")

              resp (curl! "-D" "-" "-X" "POST"
                          "-H" "Content-Type: application/x-www-form-urlencoded"
                          "-H" (str "Origin: " base-url)
                          "-H" (str "Referer: " base-url "/login")
                          "--data-raw" body
                          "-L" (str base-url "/login"))]

             (if (and (str/includes? resp "<!DOCTYPE html>")
                      (not (str/includes? resp "Log in to Monograph"))))
             (println "✓ Login successful")
             (throw (ex-info "Login failed" {:resp (subs resp 0 (min 500 (count resp)))})))))

(defn query! [payload]
  (-> (curl! "-X" "POST"
             "-H" "Content-Type: application/json"
             "-d" (json/generate-string payload)
             (str base-url "/graphql"))
      (json/parse-string true)))

;; ---------------------------------------------------------------------------
;; CSV helpers
;; ---------------------------------------------------------------------------

(defn escape-csv [v]
  (let [s (str (if (nil? v) "" v))]
    (if (or (str/includes? s ",") (str/includes? s "\"") (str/includes? s "\n"))
      (str "\"" (str/replace s "\"" "\"\"") "\"")
      s)))

(defn rows->csv [headers row-fn rows]
  (let [header-line (str/join "," (map name headers))
        data-lines  (map (fn [row]
                           (->> headers
                                (map #(escape-csv (row-fn row %)))
                                (str/join ",")))
                         rows)]
    (str/join "\n" (cons header-line data-lines))))

(defn write-csv! [query-name headers row-fn rows]
  (let [date-str (.format (java.time.LocalDate/now)
                          (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd"))
        filename (str "monograph-" query-name "-" date-str ".csv")]
    (spit filename (rows->csv headers row-fn rows))
    (println (str "✓ Wrote " (count rows) " rows to " filename))))

;; ---------------------------------------------------------------------------
;; Query: gantt-projects (one row per project, phases summarized)
;; ---------------------------------------------------------------------------

(def gantt-query-str
  "query ganttChart($isTemplate: Boolean, $filters: GanttChartFiltersInput, $first: Int, $offset: Int) {
     ganttChart(filters: $filters, isTemplate: $isTemplate, first: $first, offset: $offset) {
       totalProjectsCount totalFilteredCount hasNextPage earliestDate latestDate
       rows {
         ...GanttProjectRow
       }
     }
   }
   fragment GanttProjectRow on GanttChartRow {
     assigned hoursConsumed hoursPlanned href id number status color name slug
     startDate endDate showMoneyGantt showMoneyGanttHoverCard showInvoicedData
     showStayOnBudgetAlerts clientName isCollaborative isActingAsLead
     address { id line1 line2 city state zipcode country }
     phases {
       id startDate endDate rolesCount hoursPlanned hoursConsumed
       planned logged invoiced paid allocated isCollaborative
       type { id name abbr feeType status budget }
       milestones { name date completed isCollaborative completedTasksCount tasksCount }
     }
   }")

(def gantt-projects-headers
  [:id :number :name :clientName :status :startDate :endDate
   :hoursPlanned :hoursConsumed :assigned
   :address_line1 :address_city :address_state :address_zipcode
   :phaseCount :totalPhaseBudget :totalPhasePlanned :totalPhaseLogged
   :totalPhaseInvoiced :totalPhasePaid])

(defn gantt-project-row [row field]
  (case field
    :address_line1 (get-in row [:address :line1])
    :address_city (get-in row [:address :city])
    :address_state (get-in row [:address :state])
    :address_zipcode (get-in row [:address :zipcode])
    :phaseCount (count (:phases row))
    :totalPhaseBudget (->> (:phases row) (keep #(get-in % [:type :budget])) (reduce + 0))
    :totalPhasePlanned (->> (:phases row) (keep :planned) (reduce + 0))
    :totalPhaseLogged (->> (:phases row) (keep :logged) (reduce + 0))
    :totalPhaseInvoiced (->> (:phases row) (keep :invoiced) (reduce + 0))
    :totalPhasePaid (->> (:phases row) (keep :paid) (reduce + 0))
    (get row field)))

(defn fetch-gantt-projects! []
  (loop [offset 0, all-rows []]
    (let [result   (query! {:operationName "ganttChart"
                            :variables     {:filters {:projectStatuses ["ACTIVE"]
                                                      :sortBy          "alphabetical"}
                                            :first   25
                                            :offset  offset}
                            :query         gantt-query-str})
          gantt    (get-in result [:data :ganttChart])
          all-rows (into all-rows (:rows gantt))]
      (println (str "  Fetched " (count all-rows) "/" (:totalFilteredCount gantt)))
      (if (:hasNextPage gantt)
        (recur (+ offset 25) all-rows)
        all-rows))))

;; ---------------------------------------------------------------------------
;; Query: gantt-phases (one row per phase, denormalized with project info)
;; ---------------------------------------------------------------------------
(def gantt-phases-headers
  [;; Project fields
   :projectId :projectNumber :projectName :projectSlug :clientName
   :projectStatus :projectColor :projectStart :projectEnd
   :projectHoursPlanned :projectHoursConsumed :projectAssigned
   :projectHref :isCollaborative :isActingAsLead
   :showMoneyGantt :showMoneyGanttHoverCard :showInvoicedData :showStayOnBudgetAlerts
   ;; Address
   :addressLine1 :addressLine2 :addressCity :addressState :addressZipcode :addressCountry
   ;; Phase type
   :phaseId :phaseName :phaseAbbr :feeType :phaseBudget :phaseTypeStatus
   ;; Phase dates & staffing
   :phaseStart :phaseEnd :phaseIsCollaborative
   :rolesCount :rolesWithoutHoursCount :rolesWithAssignedProfilesCount :assignedProfilesCount
   ;; Phase financials
   :hoursPlanned :hoursConsumed :planned :logged :invoiced :paid :allocated
   ;; Milestone summary
   :milestoneCount :milestonesCompleted :totalMilestoneTasks :completedMilestoneTasks
   :nextMilestoneName :nextMilestoneDate])

(defn next-incomplete-milestone [milestones]
  (->> milestones
       (remove :completed)
       (sort-by :date)
       first))

(defn explode-phases [projects]
  (mapcat
   (fn [proj]
     (map (fn [phase]
            (let [next-ms (next-incomplete-milestone (:milestones phase))]
              {:projectId                      (:id proj)
               :projectNumber                  (:number proj)
               :projectName                    (:name proj)
               :projectSlug                    (:slug proj)
               :clientName                     (:clientName proj)
               :projectStatus                  (:status proj)
               :projectColor                   (:color proj)
               :projectStart                   (:startDate proj)
               :projectEnd                     (:endDate proj)
               :projectHoursPlanned            (:hoursPlanned proj)
               :projectHoursConsumed           (:hoursConsumed proj)
               :projectAssigned                (:assigned proj)
               :projectHref                    (:href proj)
               :isCollaborative                (:isCollaborative proj)
               :isActingAsLead                 (:isActingAsLead proj)
               :showMoneyGantt                 (:showMoneyGantt proj)
               :showMoneyGanttHoverCard        (:showMoneyGanttHoverCard proj)
               :showInvoicedData               (:showInvoicedData proj)
               :showStayOnBudgetAlerts         (:showStayOnBudgetAlerts proj)
               ;; Address
               :addressLine1                   (get-in proj [:address :line1])
               :addressLine2                   (get-in proj [:address :line2])
               :addressCity                    (get-in proj [:address :city])
               :addressState                   (get-in proj [:address :state])
               :addressZipcode                 (get-in proj [:address :zipcode])
               :addressCountry                 (get-in proj [:address :country])
               ;; Phase type info
               :phaseId                        (:id phase)
               :phaseName                      (get-in phase [:type :name])
               :phaseAbbr                      (get-in phase [:type :abbr])
               :feeType                        (get-in phase [:type :feeType])
               :phaseBudget                    (get-in phase [:type :budget])
               :phaseTypeStatus                (get-in phase [:type :status])
               ;; Phase dates & staffing
               :phaseStart                     (:startDate phase)
               :phaseEnd                       (:endDate phase)
               :phaseIsCollaborative           (:isCollaborative phase)
               :rolesCount                     (:rolesCount phase)
               :rolesWithoutHoursCount         (:rolesWithoutHoursCount phase)
               :rolesWithAssignedProfilesCount (:rolesWithAssignedProfilesCount phase)
               :assignedProfilesCount          (:assignedProfilesCount phase)
               ;; Phase financials
               :hoursPlanned                   (:hoursPlanned phase)
               :hoursConsumed                  (:hoursConsumed phase)
               :planned                        (:planned phase)
               :logged                         (:logged phase)
               :invoiced                       (:invoiced phase)
               :paid                           (:paid phase)
               :allocated                      (:allocated phase)
               ;; Milestone summary
               :milestoneCount                 (count (:milestones phase))
               :milestonesCompleted            (->> (:milestones phase) (filter :completed) count)
               :totalMilestoneTasks            (->> (:milestones phase) (keep :tasksCount) (reduce + 0))
               :completedMilestoneTasks        (->> (:milestones phase) (keep :completedTasksCount) (reduce + 0))
               :nextMilestoneName              (:name next-ms)
               :nextMilestoneDate              (:date next-ms)}))
          (:phases proj)))
   projects))


;; ---------------------------------------------------------------------------
;; Query: project-billings (unbilled report, one row per phase)
;; ---------------------------------------------------------------------------

(def unbilled-query-str
  "query ProjectsUnbilledV3($filters: ProjectsUnbilledV3FiltersInput!, $sortBy: ProjectsUnbilledV3SortInput, $last: Int, $first: Int, $before: String, $after: String) {
     reports {
       projectsUnbilledV3(sortBy: $sortBy, filters: $filters, last: $last, first: $first, before: $before, after: $after) {
         totalCount
         pageInfo { endCursor hasNextPage }
         nodes {
           projectColor projectId projectName projectNumber projectActingAsLead
           clientName totalInvoiced historicalBilledAmount lastInvoiceTotal
           totalPlannedServices percentComplete totalUnbilled totalWriteOffs projectLink
           phases {
             id name totalPlannedServices percentComplete totalInvoiced
             historicalBilledAmount totalUnbilled feeType projectId
             phaseTemplateId writeOffAmount
           }
         }
         totals { invoicedServices plannedServices totalUnbilled }
       }
     }
   }")

(def billing-phases-headers
  [:projectId :projectNumber :projectName :clientName :projectActingAsLead :projectColor
   :projectTotalInvoiced :projectHistoricalBilled :projectLastInvoiceTotal
   :projectTotalPlanned :projectPercentComplete :projectTotalUnbilled :projectTotalWriteOffs
   :projectLink
   :phaseId :phaseName :feeType :phaseTemplateId
   :phaseTotalPlanned :phasePercentComplete :phaseTotalInvoiced
   :phaseHistoricalBilled :phaseTotalUnbilled :phaseWriteOff])

(defn explode-billing-phases [nodes]
  (mapcat
   (fn [proj]
     (map (fn [phase]
            {:projectId               (:projectId proj)
             :projectNumber           (:projectNumber proj)
             :projectName             (:projectName proj)
             :clientName              (:clientName proj)
             :projectActingAsLead     (:projectActingAsLead proj)
             :projectColor            (:projectColor proj)
             :projectTotalInvoiced    (:totalInvoiced proj)
             :projectHistoricalBilled (:historicalBilledAmount proj)
             :projectLastInvoiceTotal (:lastInvoiceTotal proj)
             :projectTotalPlanned     (:totalPlannedServices proj)
             :projectPercentComplete  (:percentComplete proj)
             :projectTotalUnbilled    (:totalUnbilled proj)
             :projectTotalWriteOffs   (:totalWriteOffs proj)
             :projectLink             (:projectLink proj)
             :phaseId                 (:id phase)
             :phaseName               (:name phase)
             :feeType                 (:feeType phase)
             :phaseTemplateId         (:phaseTemplateId phase)
             :phaseTotalPlanned       (:totalPlannedServices phase)
             :phasePercentComplete    (:percentComplete phase)
             :phaseTotalInvoiced      (:totalInvoiced phase)
             :phaseHistoricalBilled   (:historicalBilledAmount phase)
             :phaseTotalUnbilled      (:totalUnbilled phase)
             :phaseWriteOff           (:writeOffAmount phase)})
          (:phases proj)))
   nodes))

(defn fetch-project-billings! []
  (let [today (.format (java.time.LocalDate/now)
                       (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd"))]
    (loop [cursor nil, all-nodes []]
      (let [vars      (cond-> {:sortBy  {:field "PROJECT_NUMBER" :direction "ASC"}
                               :filters {:projectType "ACTIVE_WITH_TOTAL_UNBILLED"
                                         :endDate     today}
                               :first   20}
                              cursor (assoc :after cursor))
            result    (query! {:operationName "ProjectsUnbilledV3"
                               :variables     vars
                               :query         unbilled-query-str})
            report    (get-in result [:data :reports :projectsUnbilledV3])
            all-nodes (into all-nodes (:nodes report))]
        (println (str "  Fetched " (count all-nodes) "/" (:totalCount report)))
        (if (get-in report [:pageInfo :hasNextPage])
          (recur (get-in report [:pageInfo :endCursor]) all-nodes)
          (explode-billing-phases all-nodes))))))



;; ---------------------------------------------------------------------------
;; Query registry — add new queries here
;; ---------------------------------------------------------------------------

(def queries
  {"gantt-projects" {:desc    "Active projects (one row per project)"
                     :fetch   fetch-gantt-projects!
                     :headers gantt-projects-headers
                     :row-fn  gantt-project-row}

   "gantt-phases"   {:desc    "Active project phases (one row per phase)"
                     :fetch   (fn [] (explode-phases (fetch-gantt-projects!)))
                     :headers gantt-phases-headers
                     :row-fn  (fn [row field] (get row field))}

   "project-billings" {:desc    "Unbilled report by phase (invoiced, planned, unbilled)"
                       :fetch   fetch-project-billings!
                       :headers billing-phases-headers
                       :row-fn  (fn [row field] (get row field))}})

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [email    (or (System/getenv "MONOGRAPH_EMAIL")
                   (do (print "Email: ") (flush) (read-line)))
      password (or (System/getenv "MONOGRAPH_PASSWORD")
                   (do (print "Password: ") (flush) (read-line)))]

  (login! email password)

  (println "\nAvailable queries:")
  (doseq [[i [k v]] (map-indexed vector (sort queries))]
    (println (str "  " (inc i) ") " k " — " (:desc v))))
  (print "\nSelect query (number or name): ")
  (flush)

  (let [input  (str/trim (read-line))
        sorted (sort (keys queries))
        qname  (if (re-matches #"\d+" input)
                 (nth sorted (dec (parse-long input)) nil)
                 input)
        q      (get queries qname)]
    (if-not q
      (println "Unknown query:" input)
      (let [rows ((:fetch q))]
        (write-csv! qname (:headers q) (:row-fn q) rows)))))
