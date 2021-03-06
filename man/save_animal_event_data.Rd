% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neurojam-db.R
\name{save_animal_event_data}
\alias{save_animal_event_data}
\title{Save event signal data per animal}
\usage{
save_animal_event_data(con, events_m, animal = NULL, project = NULL,
  phase = NULL, step = NULL, event_prestart = NULL,
  event_poststop = NULL, event_type = "event",
  animal_event_data_table = "animal_event_data", overwrite = FALSE,
  dryrun = FALSE, verbose = FALSE, ...)
}
\arguments{
\item{con}{database connection with class \code{DBIConnection},
for example produced from \code{DBI::dbConnect()}. The database
can be of any type compatible with \code{DBI}, for example
\code{dbConnect(RMySQL::MySQL())} produces an object of
class \code{MySQLConnection} which extends \code{DBIConnection}.}

\item{events_m}{matrix containing lists of numeric vectors,
where each cell contains a numeric vector of raw signal.
The \code{colnames(events_m)} are channel names, and
\code{rownames(events_m)} are event names. Alternatively,
\code{events_m} can be a list of matrices, in which case
each element in the list is handled independently.
When \code{events_m} is supplied as a list, other
arguments are taken from the attributes including
\code{animal}, \code{step}, \code{event_prestart},
\code{event_poststop}. The values for \code{project}, \code{phase}
are passed on, but if they are empty, the corresponding
value in attributes is used if possible.}

\item{animal}{character string with the animal identifier.}

\item{project, phase}{character string with project name,
and the project phase, respectively.}

\item{step}{numeric value indicating the time step between
each numeric measurement, assumed to be in units of seconds.
When \code{step} is \code{NULL} the attribute \code{"step"} is retrieved
if possible, otherwise an error is thrown.}

\item{event_prestart, event_poststop}{integer values indicating
the number of time steps before and after the event start
and event stop, which are included in the signal being stored.
If either value is \code{NULL} the attribute of the same name
is retrieved if possible, otherwise an error is thrown.}

\item{event_type}{character string stored in the database table
field \code{"event_type"}, intended to indicate whether a
particular stored event signal represents \code{"baseline"} or
\code{"event"} data. When \code{"baseline"} data is stored, it may
be processed differently, for example in \code{event_freq_profile()}
which divides the event signal into time bins, and which
may not be relevant when working with baseline signal.}

\item{animal_event_data_table}{character string with the name
of the database table in which to store event data.}

\item{dryrun}{logical indicating whether to perform database
operations. Otherwise when \code{dryrun} is \code{FALSE}, no database
operations are performed, including not creating the table
if it does not exist.}

\item{verbose}{logical indicating whether to print verbose output.}

\item{...}{additional arguments are passed to \code{get_animal_event_data()},
typically used for optional arguments relevant to event data.}
}
\description{
Save event signal data per animal
}
\details{
This function saves the subset of raw signal per
per animal, per channel, and per annotated event.
}
\seealso{
Other jam database functions: \code{\link{ephys_short_name}},
  \code{\link{extract_event_data}},
  \code{\link{get_animal_event_data}},
  \code{\link{get_animal_event_derived}},
  \code{\link{get_animal_raw_data}},
  \code{\link{save_animal_event_derived}},
  \code{\link{save_animal_raw_data}},
  \code{\link{sqlite_desc}}, \code{\link{sqlite_indices}},
  \code{\link{sqlite_view}}
}
\concept{jam database functions}
