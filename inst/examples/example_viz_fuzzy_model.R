path <- paste0(path.package("fuzzymineR"),.Platform$file.sep,"data",.Platform$file.sep,"artificial_loan_process.csv")
log <- create_eventlog(path,
                       case_id = "case",
                       activity_id = "event",
                       timestamp = "completeTime")
metrics <- mine_fuzzy_model(log)
viz_fuzzy_model(metrics = metrics,node_sig_threshold = 0, edge_sig_threshold = 0.3,edge_sig_to_corr_ratio = 0.75)
