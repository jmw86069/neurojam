% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neurojam-db.R
\name{extract_event_data}
\alias{extract_event_data}
\title{Extract animal event data from raw data}
\usage{
extract_event_data(con, animal = NULL, project = NULL, phase = NULL,
  source_filename = NULL, source_dirname = NULL,
  rdata_filename = NULL, rdata_dirname = NULL,
  animal_raw_data_table = "animal_raw_data", event_prestart = 4,
  event_poststop = 4, return_baseline = FALSE,
  base_max_duration = NULL, verbose = FALSE, ...)
}
\arguments{
\item{con}{database connection with class \code{DBIConnection},
for example produced from \code{DBI::dbConnect()}. The database
can be of any type compatible with \code{DBI}, for example
\code{dbConnect(RMySQL::MySQL())} produces an object of
class \code{MySQLConnection} which extends \code{DBIConnection}.}

\item{animal, project, phase}{arguments used to identify the unique entry from which
the event data was derived, equivalent to parameters
passed to \code{get_animal_event_data()}.}

\item{source_filename, source_dirname, rdata_filename, rdata_dirname}{arguments passed to \code{get_animal_raw_data()} used
to filter input data.}

\item{animal_raw_data_table}{character name of the database
table where raw signal results are stored by
\code{save_animal_raw_data()}.}

\item{event_prestart, event_poststop}{numeric value representing
the time in seconds to include before and after each
event, respectively. For example \code{event_prestart=4}
will represent 4 seconds, using the \code{step} information
included in the raw signal for each channel. The units
here are intended to allow each channel to have
independent time step.}

\item{return_baseline}{logical indicating whether to
define a \code{"baseline"} event which contains the signal
prior to the first event. Note that the baseline is
not extended using \code{event_prestart,event_poststop}.}

\item{verbose}{logical indicating whether to print verbose output.}

\item{...}{additional arguments are passed to downstream
functions \code{get_animal_raw_data()}, \code{get_ephys_event_data()}.}

\item{baseline_max_duration}{numeric value in seconds,
for the maximum duration of baseline signal to return,
intended to help baseline signals be consistent
duration. Some signals may have shorter signal, in
which case only the available signal duration is
returned.}
}
\value{
\code{data.frame} with colnames include animal,
project, phase, step, event_prestart, event_poststop,
event, channel, mat_name, and events_m. The \code{events_m}
column is a list of event matrices, whose
columns contain \code{channel} names, rownames
contain \code{event} numbers, and each cell contains
the numeric matrix of signal data for each event.
}
\description{
Extract animal event data from raw data
}
\details{
This function uses \code{get_animal_raw_data()} to
query the database, retrieve the associated
animal raw data, then extracts the available
event data.

In cases where event data for one animal is split across
two files, the event names are re-numbered in order
that the events appear across files for the same
animal. For example, for two files that contain two
events each for the same animal, events for the
first file will be labeled \code{c("1", "2")} and the second
file with be labeled \code{c("3", "4")}. The output is
therefore suitable to send to \code{save_animal_event_data()}
so the events can be stored without conflict.
}
\seealso{
Other jam database functions: \code{\link{ephys_short_name}},
  \code{\link{get_animal_event_data}},
  \code{\link{get_animal_event_derived}},
  \code{\link{get_animal_raw_data}},
  \code{\link{save_animal_event_data}},
  \code{\link{save_animal_event_derived}},
  \code{\link{save_animal_raw_data}},
  \code{\link{sqlite_desc}}, \code{\link{sqlite_indices}},
  \code{\link{sqlite_view}}
}
\concept{jam database functions}
