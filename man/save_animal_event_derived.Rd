% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neurojam-db.R
\name{save_animal_event_derived}
\alias{save_animal_event_derived}
\title{Save animal event derived results}
\usage{
save_animal_event_derived(con, animal, project, phase, step, channel,
  event, event_prestart, event_poststop, derived_data, derived_type,
  derived_extra, animal_event_derived_table = "animal_event_derived",
  dryrun = FALSE, verbose = FALSE, ...)
}
\arguments{
\item{con}{database connection with class \code{DBIConnection},
for example produced from \code{DBI::dbConnect()}. The database
can be of any type compatible with \code{DBI}, for example
\code{dbConnect(RMySQL::MySQL())} produces an object of
class \code{MySQLConnection} which extends \code{DBIConnection}.}

\item{animal, project, phase, step, channel, event, event_prestart, event_poststop}{arguments used to identify the unique entry from which
the event data was derived, equivalent to parameters
passed to \code{get_animal_event_data()}.}

\item{derived_data}{R object to be stored as a \code{"blob"} in
the database table.}

\item{derived_type}{character string describing the type
of data being stored, used in subsequent queries.}

\item{derived_extra}{character string intended to describe
any additional arguments or parameters related to the
method used to derive results.}

\item{animal_event_derived_table}{character string with
the database table in which to store the derived result
data.}

\item{dryrun}{logical indicating whether to perform database
operations. Otherwise when \code{dryrun} is \code{FALSE}, no database
operations are performed, including not creating the table
if it does not exist.}

\item{verbose}{logical indicating whether to print verbose output.}

\item{...}{additional arguments are passed to \code{get_animal_event_data()},
typically used for optional arguments relevant to event data.}
}
\description{
Save animal event derived results
}
\details{
This function is intended to store result data derived
from animal event data. For example, data returned by
\code{get_animal_event_data()} is used for some analysis,
after which the results are stored in a database table
for later use.
}
\seealso{
Other jam database functions: \code{\link{ephys_short_name}},
  \code{\link{extract_event_data}},
  \code{\link{get_animal_event_data}},
  \code{\link{get_animal_event_derived}},
  \code{\link{get_animal_raw_data}},
  \code{\link{save_animal_event_data}},
  \code{\link{save_animal_raw_data}},
  \code{\link{sqlite_desc}}, \code{\link{sqlite_indices}},
  \code{\link{sqlite_view}}
}
\concept{jam database functions}
