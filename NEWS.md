# version 0.0.9.900

## Coming changes

* There is currently unrest when condensing a numeric matrix
to smaller size, specifically how to label the resulting bins.
Previously the labels of each bin were used to determine the
output -- either taking min, mean, or max. Rounding errors
are a problem in each case, because of one shortcoming:
the data does not encode the "step" or "range" covered between
adjacent labels. One can infer the step by taking average
distance between adjacent labels, but only when there are
2 or more labels. A single value is not self-described, for
example you couldn't answer the question:
"What span of time is represented by `time=1`"?
* Bins can be 1-based or 0-based, but must somehow be self-described
to the underlying range can be known. Once the range is known,
labeling becomes a secondary, and much easier issue.

## new functions

* `signal_freq_heatmap()` queries the database by animal,
channel, and optionally the time_step_sec (time step in seconds),
freq_step_hz (frequency step in Hertz), to retrieve the
corresponding signal matrix, then produces a heatmap.
It calls `freq_heatmap()` which provides custom options.
* `freq_heatmap()` takes a numeric matric, expected to have
frequency rows, and time columns, and produces a heatmap.
It has additional customizations:

    * It displays a subset of row and column labels, configurable
    by `row_label_n` and `column_label_n`.
    * `row_range`,`column_range` allows selecting a subsection of
    columns and rows.
    * `row_split_width` optionally divides rows by fixed number of rows.
    * `row_split_at` optionally divides rows at fixed positions, using the
    numeric value of the rowname (in Hertz units).
    * `row_split_names` allows custom names for each row split, for example
    `c("low theta", "high theta")`.
    * `column_split_width`,`column_split_at`,`column_split_names` same
    as above, for column values.
    * applies color gradient based upon signal quantile, by default
    the color range is mapped to 1% through 99%, which usually clips off
    enough outlier signal.
    

## changes to existing functions

* `cutIntoChunks()` new argument `fixed_size` will bin the input
vector into a list of vectors, each of which has length
`fixed_size`. Optional argument `offset` is used to adjust the
starting position, so break position can occur at integer
breaks, for example.
* `condense_freq_matrix()` new arguments `column_fixed_size` and
`row_fixed_size` will bin columns and rows by exactly this many
columns, which is the preferred method to condense a frequency
matrix, since it provides a specific repeating unit output.
This method may supercede using `column_pad` and `row_pad`
which was a previous workaround to have fixed-unit output.
By default, bins are named by the first value in the bin, and
include all value up to the next bin label.
`calc_ephys_wavelet()` new argument `column_fixed_size` passes
this argument to `condense_freq_matrix()`.

# version 0.0.7.900

## new functions

* `signal_freq_matrix()` queries previously stored frequency-time
matrix data from the database, as defined by animal, channel, project,
phase, etc. It then creates a heatmap using `freq_heatmap()`.
When argument `type="Heatmap"` by default, it creates a heatmap
using ComplexHeatmap::Heatmap().
When argument `type="matrix"` it returns the numeric matrix itself,
which can be further analyzed.
When the input query is not specific enough to return only one row,
a `data.frame` of query results is returned, which can be helpful
to see the available data.


# version 0.0.7.900

## new functions

* `bin_biwavelets()` is a wrapper function that divides
very large signal into subset time bins, processes each
time bin by biwavelet transformation, then concatenates
the results. It cleverly extends the bins so each bin
overlaps the previous and next bin, then trims the overlap
away, in order to avoid edge artifacts.
* `freq_heatmap()` takes a frequency-time matrix (power
spectral density, psd) as output from `biwavelet::wt()`,
and creates a heatmap using `ComplexHeatmap::Heatmap()`.
It creates discretized labels, using `discretize_labels()`
(obviously). It optionally allows splitting rows and columns
by defined intervals, or at specific points.

## Bug fix / enhancement

**Note:** `condense_freq_matrix()` may change in near future to
allow distinct time step sizes, and frequency step sizes, instead
of condensing the columns and rows into a fixed number of output
columns. The problem occurs when some signal data spans 99.7%
to 100.5% the intended time duration, causing small rounding errors
in the output time bins, when dividing the data into equal size
time bins.


* `condense_freq_matrix()` has two new arguments
`column_pad` and `row_pad`, which are intended to help maintain
consistent step widths when condensing a frequency-time
matrix into distinct time bins. For example, when the intent
is to use 1-second time bins, and the data contains 14.97 seconds,
instead of dividing into 15 equal-sized bins (as previous) and labeling
each bin accordingly, the new process allows "padding" the last
0.03 second, then creating 15 bins of exactly 1.00-second duration.
Note that this function cannot determine the padding, it
must be determined by the upstream function.
* `bin_biwavelets()` uses `new_step` to determine the `column_pad`
value, which is sent to `calculate_wavelet_matrix()`,
and then passed to `condense_freq_matrix()`.
* `bin_biwavelets()` adds another argument `min_fraction` which
is a threshold below which a time bin is dropped instead of being
padded. For example, if the data spans 15.02 seconds, and the
intent is to create 1.0-second time bins, the final 0.02 seconds
will be dropped instead of padded. By default a time bin must have
at least 0.4 the requested time bin to be kept.

# version 0.0.6.900

## New functions specific to SQLite databases:

* `sqlite_desc()` is a describe function, similar to `desc`.
* `sqlite_view()` gives deeper information about database views,
including the source tables, and whether each column in the
source table is indexed.
* `sqlite_indices()` lists fields of each table that have
an associated index.


# version 0.0.5.900

* More updates to use the filedb, and some bug fixes.

# version 0.0.4.900

* Another refactor, this time to change primary key for
Ephys signal data to be the filename, from which animal
associations are secondary and largely not necessary
for most of the workflow. Each file is associated with
the event tables, and channel data. From this information,
each channel series of events can be extracted, processed
by biwavelet and power spectral density, and stored in
the database. No steps require animal association, which
can therefore be done separately, for example after manual
review of specific lab notes. Lastly, this workflow allows
storing all relevant channels, then later using the
appropriate subset for downstream analyses.

# version 0.0.3.900

* Refactored `import_ephys_mat_1()` to allow multiple
`channel_grep` patterns, in order to select the best
available matching channel name where possible.
* Several new functions for relational database interaction.
* `matrix_period2hz()` converts a frequency matrix whose
row units are period (time per occurrence) to Hertz
frequency (occurrences per second). It provides
linear frequency ranges by using `approx()` for
interpolation between values in the original data
which are in period units.
* `save_animal_event_derived()` stores derived results
from analysis on event data. It is intended to be used
with `event_freq_profile()` which extracts event data,
then performs biwavelet transformation, then derived
downstream data.
* `event_freq_profile()` extracts event data using
`get_animal_event_data()`, performs `biwavelet::wt()`,
condenses the frequency matrix (by default `"power.corr"`
which is power-corrected) to time units of 0.1 seconds,
then runs `summarize_event_bins()` to aggregate the
power spectral density into bins within the overall
event.
* `summarize_event_bins()` takes event signal data,
determines pre-event, and post-event time ranges,
then subdivides the pre-, during-, and post-event
time ranges into bins. These `time_bin` values are
aggregated by taking the mean signal across the range.


# version 0.0.2.900

* Began transitioning all methods to use `RDBI` to store
most data in a relational database, with the exception that
raw signal is stored in `RData` objects per animal until
there is a driving reason not to do so.
* The new workflow concept:

1. Matlab files are parsed to retrieve data per animal
for each Matlab file. Data for each animal is stored 
via `save_animal_raw_data()` in relational database table
`"animal_raw_data"` for query and retrieval, with pointer
to the `RData` file that contains only the data relevant
per animal. This step ensures the logic of separating
Matlab data channels by animal is separate from steps
that consume the data. The consumers therefore can assume
consistent input data.
2. Animal raw data is retrieved with `get_animal_raw_data()`,
and event signal data is extracted with `extract_event_data()`.
This step contains logic to quality-check the content,
including verifying the event time stamps are valid.
Some animal data is split across files, this step enables
combining the event data after the fact.
3. Event data is stored in relational database table
`"animal_event_data"` using `save_animal_event_data()`.
The signal data for the event is stored as a `blob` in
the database table for convenient retrieval.
4. Event data is queried using `get_animal_event_data()`
which by default returns a table without the event signal,
for summary purpose.
5. Event signal is also queried with
`get_animal_event_data(..., return_signal=TRUE)` where
the `data.frame` returned includes colname `"event_signal"`
which is a list of numeric matrices.
6. Animal Ephys annotations are stored in the same
relational database, with `"animal"` as the only required
column key, used to join with the tables
`"animal_raw_data"` and `"animal_event_data"`.

## new functions

* `import_ephys_mat_1()` imports one Matlab `.mat` file,
for one animal. Its parameters allow flexible number of
channels per animal, with multiple animals per file.
* `save_animal_raw_data()` saves the data for one animal
from one Matlab `.mat` file into a relational database.
It stores an object named `"mat_l"` in an `RData` file,
with only the channel data relevant to that animal.
It also stores `"project"` and `"phase"`, which is
maintained throughout the subsequent workflow.
* `get_animal_raw_data()` retrieves the `"mat_l"` data
for one or more animals, for the given `"project"` and
`"phase"`.
* `extract_event_data()` calls `get_animal_raw_data()` to
retrieve raw channel data, then parses and extracts
event signal data for each channel. It applies the
timestamps, and verifies correct time stamps in the
source data. It returns a `data.frame` with colname
`"events_m"` which contains a list of numeric matrices,
with channels as columns and events as rows.
* `save_animal_event_data()` saves the data from 
`extract_event_data()` in a relational database.
* `get_animal_event_data()` retrieves event data
from the relational database.


# version 0.0.1.900

## new functions

* `event_freq_profile()` converts a signal for one event
into a frequency matrix, and corresponding binned frequency
power profile.

## changes

* Updated `get_ephys_event_data()` to be more robust when
checking for time step "ts.step", and the index "ind"
values in the original Matlab `.mat` data.

