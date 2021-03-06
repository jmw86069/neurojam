% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neurojam-filedb.R
\name{get_event_table_db}
\alias{get_event_table_db}
\title{Retrieve event data by filename from the database}
\usage{
get_event_table_db(con, filename, default_event_diff = 40,
  event_start_name = "EVT02", event_stop_name = "EVT03",
  file_event_table = "file_event", rename_columns = TRUE, ...)
}
\description{
Retrieve event data by filename from the database
}
