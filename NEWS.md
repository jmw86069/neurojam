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

