% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neurojam-db.R
\name{get_animal_event_data}
\alias{get_animal_event_data}
\title{Get animal event data}
\usage{
get_animal_event_data(con, animal = NULL, project = NULL,
  phase = NULL, channel = NULL, event = NULL, step = NULL,
  event_prestart = NULL, event_poststop = NULL, event_type = NULL,
  animal_event_data_table = "animal_event_data", return_signal = FALSE,
  verbose = FALSE, ...)
}
\description{
Get animal event data
}
\seealso{
Other jam database functions: \code{\link{ephys_short_name}},
  \code{\link{extract_event_data}},
  \code{\link{get_animal_event_derived}},
  \code{\link{get_animal_raw_data}},
  \code{\link{save_animal_event_data}},
  \code{\link{save_animal_event_derived}},
  \code{\link{save_animal_raw_data}},
  \code{\link{sqlite_desc}}, \code{\link{sqlite_indices}},
  \code{\link{sqlite_view}}
}
\concept{jam database functions}
