#' @title Mines eventlog data using Fuzzy Algorithm
#'
#' @description Takes eventlog as input, applies fuzzy mining algorithm and returns a list of metrics which specifies significance for different activities and their precedence relations.
#'
#' @param eventlog An eventlog object
#'
#'
#' @details \strong{\emph{mine_fuzzy_model}} uses \strong{fuzzy mining algorithm} to mine eventlog data
#' and generates a list of metrics. This list of metrics is then used for
#' creating process models. Fuzzy mining algorithm is widely used to create
#' process models because of its adaptiveness.
#'
#' It generates a fixed number of metrics. There are total of 9 metrics generated
#' by this algorithm. Out of this, 2 metrics are for the activities which will be
#' nodes in our process models. Other 7 would represent precedence relation between
#' them and would form edges in the process model. These precedence relation are
#' of two types - significance and correlation. Out of 7, 2 would be significance
#' relations and 5 would be correlation metrics.
#'
#' \strong{Frequency Unary Significance :} The more often an activity is observed
#' in event log more the value of frequency significance of that activity.
#'
#' \strong{Routing Unary Significance :} The higher the number and significance
#' of predecessors for a node (Activity) differs from the number and significance
#' of its successors the more important that node is for routing in the process.
#'
#' \strong{Frequency Binary Significance :} The more often two activities
#' are observed after one another, the more significant their
#' precedence relation
#'
#' \strong{Distance Binary Significance :} This metric calculates the significance of
#' two activity that occur in a trace but not after one another. More the
#' distance, less is the weightage of that significance value. Hence it takes
#' into account the significance of all the global relations.
#'
#' \strong{Proximity Binary Correlation :} This metric captures the activities
#' that occur shortly after one another and measures there correlation. This
#' metric is important for identifying the clusters of activites that occur in
#' short time frame and hence can be seen as one logical activity.
#'
#' \strong{Originator Binary Correlation :} Correlation between two activities that is
#' determined by the name/ID of the person who have triggered the event. Similar
#' the names, the higher the correlation between two activities. Eg: Resource
#' person names : (sales_x, sales_y) and (technical_x, technical_y)
#'
#' \strong{Endpoint Binary Correlation :} Correlation between two activities is
#' determined by the name of the two events. More similar the names are more is
#' the correlation between activites. Eg : (Check_x, Check_y)
#'
#' \strong{Data Type Correlation :} Correlation bewteen two activities is higher
#' when sub-sequent activities share large amount of data types(attributes keys).
#'
#'
#' \strong{Data Value Correlation :} Corrrelation between two activities is determined
#' by the values of these shared data types or common attributes. More similar the
#' value higher is the correlation.
#'
#' For getting more insights about these metrics and what is their importance, please
#' go to the reference website.
#'
#' @seealso \link[fuzzymineR]{create_eventlog}, \link[fuzzymineR]{viz_fuzzy_model}
#'
#' @examples
#' \dontrun{
#' #' library(fuzzymineR)
#' data("artificial_loan_process")
#' log <- create_eventlog(artificial_loan_process,
#'                        case_id = "case",
#'                        activity_id = "event",
#'                        timestamp = "completeTime")
#' metrics <- mine_fuzzy_model(log)
#' }
#' @references \href{http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.81.1207&rep=rep1&type=pdf}{Fuzzy Mining - Adaptive Process Simplification
#' Based on Multi-Perspective Metrics}
#'
#'
#' @return A list of metrics which contains significance of each activities
#' and their precedence relations with each other for several parameters
#'
#' @import bupaR
#' @import readr
#' @import dplyr
#' @import stringr
#' @import xesreadR
#'
#' @export
mine_fuzzy_model <- function(eventlog) {

  pkg_dir <- path.package("fuzzymineR")
  temp_dir <- tempdir()
  user_dir <- getwd()

  #checking if timestamps of any event is missing
  if (any(is.na(eventlog %>% pull(timestamp(eventlog))))) {
    warning("Some of the timestamps in the supplied event log are missing (NA values). This may result in a invalid process map!")
  }
  # Switching the wd to temp dir to save the temporary files
  setwd(tempdir())

  # Saving the eventlog as a .xes file and also saving the current directory path and temp_dir where all the temporary files will be saved
  filename_xes <- "fuzzy_miner_eventlog.xes"
  write_xes(eventlog, filename_xes)

  # Creating all the paths that are required to replace place holders in template scripts
  path_xes <- file.path(getwd(), filename_xes)
  path_names <- file.path(getwd(), "names.csv")
  path_UM_Out1 <- file.path(getwd(), "UM_Out1.csv")
  path_UM_Out2 <- file.path(getwd(), "UM_Out2.csv")
  path_BM_Out1 <- file.path(getwd(), "BM_Out1.csv")
  path_BM_Out2 <- file.path(getwd(), "BM_Out2.csv")
  path_BM_Out3 <- file.path(getwd(), "BM_Out3.csv")
  path_BM_Out4 <- file.path(getwd(), "BM_Out4.csv")
  path_BM_Out5 <- file.path(getwd(), "BM_Out5.csv")
  path_BM_Out6 <- file.path(getwd(), "BM_Out6.csv")
  path_BM_Out7 <- file.path(getwd(), "BM_Out7.csv")

  #list of replacements in template script-1 file
  cli_template_rvars <- list(
    FNAME = path_xes,
    OUTNAMES = path_names,
    UMOUT1 = path_UM_Out1,
    UMOUT2 = path_UM_Out2,
    BMOUT1 = path_BM_Out1,
    BMOUT2 = path_BM_Out2,
    BMOUT3 = path_BM_Out3,
    BMOUT4 = path_BM_Out4,
    BMOUT5 = path_BM_Out5,
    BMOUT6 = path_BM_Out6,
    BMOUT7 = path_BM_Out7
  )

  # Path to my template scripts
  cli_template_fpath <- file.path(pkg_dir, "prom", "prom_cli_template_fuzzy_miner.txt")

  # Changing the placeholders in template script-1 with the actual values
  cli_template <- read_lines(cli_template_fpath)

  cli_script <- sapply(cli_template, function(x) {

    for (varnm in names(cli_template_rvars)) {
      x <- str_replace_all(x, paste0("__", varnm, "__"), cli_template_rvars[[varnm]])
    }

    x
  }, USE.NAMES = FALSE)

  # Writing the result object to a text file. We will run this script from cmd to mine using fuzzy algorithm and extract data of mining output.
  cli_script_fpath <- file.path(temp_dir,"prom_cli_fuzzy_miner.txt")
  write_lines(cli_script, cli_script_fpath)

  # Switching the wd to package dir to run the command
  setwd(file.path(pkg_dir, "prom"))

  # Calling java executable script from cmd
  command <- "java"
  arguments <- c("-da",
            "-Xmx1G",
            "-XX:MaxPermSize=256m",
            "-classpath",
            "ProM641.jar",
            "-Djava.util.Arrays.useLegacyMergeSort=true",
            "org.processmining.contexts.cli.CLI",
            "-f", shQuote(cli_script_fpath))
  system2(command, args = arguments, stdout = NULL, stderr = NULL)

  # Going back to the user dir
  setwd(user_dir)


  #Getting all the data(Metrics that are mined) into R and properly formatting it
  act_names <- read_lines(path_names)

  freq_sig_unary_log <- read_lines(path_UM_Out1)
  freq_sig_unary_log <- as.vector(freq_sig_unary_log)
  names(freq_sig_unary_log) <- act_names
  freq_sig_unary_log <- sapply(freq_sig_unary_log , as.numeric)

  routing_sig_unary_der <- read_lines(path_UM_Out2)
  routing_sig_unary_der <- as.vector(routing_sig_unary_der)
  names(routing_sig_unary_der) <- act_names
  routing_sig_unary_der <- sapply(routing_sig_unary_der , as.numeric)

  freq_sig_binary_log <- suppressMessages(read_csv(path_BM_Out1, col_names = FALSE))
  freq_sig_binary_log <- as.matrix(freq_sig_binary_log)
  colnames(freq_sig_binary_log) <- act_names
  rownames(freq_sig_binary_log) <- act_names

  prox_corr_binary_log <- suppressMessages(read_csv(path_BM_Out2, col_names = FALSE))
  prox_corr_binary_log <- as.matrix(prox_corr_binary_log)
  colnames(prox_corr_binary_log) <- act_names
  rownames(prox_corr_binary_log) <- act_names

  end_point_corr_binary_log <- suppressMessages(read_csv(path_BM_Out3, col_names = FALSE))
  end_point_corr_binary_log <- as.matrix(end_point_corr_binary_log)
  colnames(end_point_corr_binary_log) <- act_names
  rownames(end_point_corr_binary_log) <- act_names

  org_corr_binary_log <- suppressMessages(read_csv(path_BM_Out4, col_names = FALSE))
  org_corr_binary_log <- as.matrix(org_corr_binary_log)
  colnames(org_corr_binary_log) <- act_names
  rownames(org_corr_binary_log) <- act_names

  data_type_corr_binary_log <- suppressMessages(read_csv(path_BM_Out5, col_names = FALSE))
  data_type_corr_binary_log <- as.matrix(data_type_corr_binary_log)
  colnames(data_type_corr_binary_log) <- act_names
  rownames(data_type_corr_binary_log) <- act_names

  data_value_corr_binary_log <- suppressMessages(read_csv(path_BM_Out6, col_names = FALSE))
  data_value_corr_binary_log <- as.matrix(data_value_corr_binary_log)
  colnames(data_value_corr_binary_log) <- act_names
  rownames(data_value_corr_binary_log) <- act_names

  distance_sig_binary_der <- suppressMessages(read_csv(path_BM_Out7, col_names = FALSE))
  distance_sig_binary_der <- as.matrix(distance_sig_binary_der)
  colnames(distance_sig_binary_der) <- act_names
  rownames(distance_sig_binary_der) <- act_names

  #If resource_id was not present in the original event data then org_corr_binary_log is of no use
  if (all(is.na(eventlog %>% pull(resource_id(eventlog))))) {
    org_corr_binary_log <- matrix(0,nrow = length(act_names),ncol = length(act_names))
    rownames(org_corr_binary_log) <- act_names
    colnames(org_corr_binary_log) <- act_names
  }

  #If any additional attributes are not present then data_value_binary_log and data_type_binary_log are also of no use
  general_attributes <- c("case_id","activity_id","activity_instance_id","timestamp","lifecycle_id","resource_id",".order")
  additional_attributes <- !(all(colnames(eventlog) %in% general_attributes))
  if(!additional_attributes){
    data_value_corr_binary_log <- matrix(0,nrow = length(act_names),ncol = length(act_names))
    rownames(data_value_corr_binary_log) <- act_names
    colnames(data_value_corr_binary_log) <- act_names

    data_type_corr_binary_log <- matrix(0,nrow = length(act_names),ncol = length(act_names))
    rownames(data_type_corr_binary_log) <- act_names
    colnames(data_type_corr_binary_log) <- act_names
  }

  #Cleaning data which was stored in temporary file
  unlink(path_BM_Out1)
  unlink(path_BM_Out2)
  unlink(path_BM_Out3)
  unlink(path_BM_Out4)
  unlink(path_BM_Out5)
  unlink(path_BM_Out6)
  unlink(path_BM_Out7)
  unlink(path_UM_Out1)
  unlink(path_UM_Out2)
  unlink(path_names)
  unlink(path_xes)

  #Return a list of metrics
  metrics <- list()
  metrics$freq_sig_unary_log = freq_sig_unary_log
  metrics$routing_sig_unary_der = routing_sig_unary_der
  metrics$freq_sig_binary_log = freq_sig_binary_log
  metrics$distance_sig_binary_der = distance_sig_binary_der
  metrics$prox_corr_binary_log = prox_corr_binary_log
  metrics$end_point_corr_binary_log = end_point_corr_binary_log
  metrics$data_type_corr_binary_log = data_type_corr_binary_log
  metrics$data_value_corr_binary_log = data_value_corr_binary_log
  metrics$org_corr_binary_log = org_corr_binary_log

  #getting back to the users directory
  setwd(user_dir)

  #returning the metrics(a list of 9 elements)
  return(metrics)
}
