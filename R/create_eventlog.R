#' @title Create an eventlog object
#'
#' @description  Create an event log object from \code{data.frame} on the disk.
#'
#' @param df a \code{data.frame} consisting of event log data
#' @param case_id character vector of length 1. It contains column/attribute name which has unique cases in event data. (Mandatory)
#' @param activity_id character vector of length 1. It contains column/attribute name which has unique activities in event data. (Mandatory)
#' @param activity_instance_id character vector of length 1. It contains column/attribute name which has enumeration of activity occurances. An specific activity performed by a specific case at specific point in time is called an activity instance. For all lifecycle instances of a single activity has same activity instance Id. (Optional)
#' @param lifecycle_id character vector of length 1.It contains column/attribute name which specifies lifecycle of an activity in event data. (Optional)
#' @param timestamp character vector of length 1.It contains column/attribute name which specifies the timestamp when the activity was performed in event data. (Mandatory)
#' @param resource_id character vector of length 1. It conatins column/attribute name which specifies the handler/resource of an activity in event data. (Optional)
#'
#' @details This function takes a \code{data.frame} as input and gives an eventlog object.
#' The function is only applicable for eventlog data. Eventlog data must contain
#' Case, Activity and Timestamp attributes.
#'
#' @seealso \link[bupaR]{eventlog}, \link[fuzzymineR]{mine_fuzzy_model}, \link[fuzzymineR]{viz_fuzzy_model}
#'
#' @return An \code{\link[bupaR]{eventlog}} created from the CSV file on the disk.
#'
#'
#' @examples
#' \dontrun{
#' library(fuzzymineR)
#' data("artificial_loan_process")
#' log <- create_eventlog(artificial_loan_process,
#'                        case_id = "case",
#'                        activity_id = "event",
#'                        timestamp = "completeTime")
#'
#' }
#' @import bupaR
#' @import dplyr
#'
#' @export
create_eventlog <- function(df,
                            case_id = NULL,
                            activity_id = NULL,
                            activity_instance_id = NULL,
                            lifecycle_id = NULL,
                            timestamp = NULL,
                            resource_id = NULL) {



  #Error handling : Mandatory fields should be mapped

  if(!is.data.frame(df)){
    stop("First argument should be a data.frame")
  }


  if(is.null(case_id)) {
    stop("case_id cannot be NULL. Must be mapped to some field in data.frame")
  }

  if(is.null(activity_id)) {
    stop("activity_id cannot be NULL. Must be mapped to some field in data.frame.")
  }

  if(is.null(timestamp)) {
    stop("timestamp cannot be NULL. Must be mapped to some field in data.frame")
  }

  #loading and pre-processing the file
  df[[timestamp]] <- as.POSIXct(df[[timestamp]])

  #adding required fields to the data.frame if not present
  if(is.null(activity_instance_id)) {
    df <- df %>%
      mutate(activity_instance_id = row_number())

    activity_instance_id = "activity_instance_id"
  }

  if(is.null(lifecycle_id)) {
    df <- df %>%
      mutate(lifecycle_id = "Complete")

    lifecycle_id = "lifecycle_id"
  }

  if(is.null(resource_id)) {
    df <- df %>%
      mutate(resource_id = NA)

    resource_id = "resource_id"
  }
  #converting the names of the attributes to standard names
  colnames(df)[colnames(df) == case_id] <- "case_id"
  colnames(df)[colnames(df) == activity_id] <- "activity_id"
  colnames(df)[colnames(df) == activity_instance_id] <- "activity_instance_id"
  colnames(df)[colnames(df) == timestamp] <- "timestamp"
  colnames(df)[colnames(df) == lifecycle_id] <- "lifecycle_id"
  colnames(df)[colnames(df) == resource_id] <- "resource_id"

  #converting data.frame/tibble to an eventlog object
  elog <- df %>%
    eventlog(case_id = "case_id",
             activity_id = "activity_id",
             activity_instance_id = "activity_instance_id",
             lifecycle_id = "lifecycle_id",
             timestamp  = "timestamp",
             resource_id = "resource_id")



  #Returns the eventlog object
  elog
}
