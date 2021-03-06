% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neurojam-db.R
\name{ephys_short_name}
\alias{ephys_short_name}
\title{Make Ephys short name}
\usage{
ephys_short_name(x, max_nchar_word = 4, word_pattern = "[a-zA-Z0-9]",
  remove_extension = TRUE, ...)
}
\arguments{
\item{x}{character vector to be modified to a short name.}

\item{max_nchar_word}{integer indicating the maximum
characters retained per word}

\item{word_pattern}{character pattern used to define characters
which are considered "word" characters.}

\item{remove_extension}{logical indicating whether to remove
file extensions, defined by "." followed by alphanumeric
characters.}

\item{...}{additional arguments are ignored.}
}
\description{
Make Ephys short name
}
\details{
This function simply creates a short name given
a label, using some logic specific to Ephys naming
strategies.
}
\examples{
x <- c("FearConditioning",
   "Fear Conditioning",
   "Fear_Conditioning",
   "Fear-Conditioning");
ephys_short_name(x);

}
\seealso{
Other jam database functions: \code{\link{extract_event_data}},
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
