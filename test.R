library(fuzzymineR)

elog <- create_eventlog(artificial_loan_process,
                case_id = "case",
                activity_id = "event",
                timestamp = "completeTime")

fuzzy_model <- mine_fuzzy_model(elog)

viz_fuzzy_model(fuzzy_model,node_sig_threshold = 0.3)
