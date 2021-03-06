% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neurojam-import.R
\name{import_ephys_mat_1}
\alias{import_ephys_mat_1}
\title{Import single MatLab .mat file}
\usage{
import_ephys_mat_1(mat_file, animal, channels_per_animal = 5,
  channels_to_retain = seq_len(channels_per_animal - 1),
  channel_grep = c("^FP[0-9]+$", "^AI[0-9]+$"), channel_grep_n = 1,
  min_channel_rows = 750000, drop_unused_channels = TRUE,
  drop_grep = "^SPKC|^WB", verbose = FALSE, ...)
}
\arguments{
\item{mat_file}{one of the recognized inputs:
\enumerate{
\item character path to a \code{.mat} Matlab file,
\item character path to a \code{.RData} file converted
from a \code{.mat} file by the \code{"reach"} R package.
\item \code{list} object representing data equivalent
to data already imported from a \code{.mat} file.
}}

\item{animal}{character vector representing the animal
or multiple animals whose signal data is contained
in the data.}

\item{channels_per_animal}{integer number of channels
allocated to each animal. The vector of recognized
channels is annotated with each \code{animal} value
repeated this many times. For example a file with
\code{10} channels, and \code{2} animals, would have
\code{channels_per_animal=5} which would assign the
first \code{5} channels to the first animal,
and the next \code{5} channels to the second animal.
When \code{channel_grep} contains multiple values,
\code{channels_per_animal} is applied to each set
of matched channels, in order, so that the
\code{channels_per_animal} value can be custom for
each matched set of channels.}

\item{channels_to_retain}{integer vector representing
the channels to retain for each animal, with values
starting at \code{1} and values as high as \code{channels_per_animal}.
For example if \code{channels_per_animal=5}, to retain the
first \code{4} channels per animal, use
\code{channels_to_retain=c(1,2,3,4)}.
When \code{channel_grep} contains multiple values,
\code{channels_to_retain} is applied to each set
of matched channels, in order, so that the
\code{channels_to_retain} value can be custom for
each matched set of channels.}

\item{channel_grep}{character vector of regular expression
patterns used to recognize channels to retain. This
vector is passed to \code{jamba::provigrep()} which applies
each pattern in order, and returns a list of recognized
channels.}

\item{channel_grep_n}{integer number of matched channel
patterns to include. For example, \code{channel_grep} may
contain \code{2} patterns, so to retain only the channels
from the first matched pattern, use \code{channel_grep_n=1}.
This argument allows flexibiliity in recognizing the
best available channel for each file. To return
all channels for all matched patterns, use
\code{channel_grep_n=Inf}.}

\item{min_channel_rows}{integer representing the minimum
number of values in a channel in order to consider that
channel valid. Only valid channels are used when
annotating the \code{animal}. This argument is intended to
help when a file containing \code{10} channels has only \code{1}
animal whose data is contained in channels \code{6:10}, which
assumes the data in channels \code{1:5} have fewer than
\code{min_channel_rows} values.
When \code{channel_grep} contains multiple values,
\code{min_channel_rows} is applied to each set
of matched channels, in order, so that the
\code{min_channel_rows} value can be custom for
each matched set of channels.}

\item{drop_unused_channels}{logical indicating whether to
drop channels which are not annotated for each
specific \code{animal}. Typically \code{drop_unused_channels=TRUE}
is default, which returns only the data relevant to
each \code{animal}.}

\item{drop_grep}{character pattern used to define other
names to exclude from the final returned data object.
This pattern will not override any channels already
annotated to each specific \code{animal}. This argument is
intended to help remove extraneous channels or signals
which are not required for downstream processing, in
order to reduce the size of the returned data.}

\item{verbose}{logical indicating whether to print verbose output.}

\item{...}{additional arguments are ignored.}
}
\value{
\code{list} containing the relevant data for
each animal in the source \code{mat_file} file.
}
\description{
Import single MatLab .mat file
}
\details{
This file is intended for a specific scenario, importing
a Matlab \code{.mat} file that contains E-physiology
neural measurements, stored as a list of numeric
matrix objects.

The measurements are expected to be stored in channels
whose names match the \code{channel_grep} argument, by default
channels like \code{"AI01"} that begin with \code{"AI"} and end with
numeric digits.

Channels are required to have at least \code{min_channel_rows}
rows of measurements. This argument is intended to remove
channels which were not active in a given experiment,
and were included in the output file.

Multiple channels are stored per animal in each file,
with argument \code{channels_per_animal} describing how many
measurement channels there are for each animal. By default,
there are five channels per animal, and the first four
channels are retained, defined with argument
\code{channels_to_retain}.
}
\seealso{
Other jam import functions: \code{\link{import_ephys_mat}}
}
\concept{jam import functions}
