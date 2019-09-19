
#' Import Plexon MatLab data
#'
#' Import MatLab .mat files specifically exported from Plexon
#' NeuroExplorer software.
#'
#' The `mat_files_use` data.frame should contain the following colnames:
#'
#' * path: character column containing the full file path to each .mat file
#' * filename: character column containing the base filename,
#'    without folder or path information.
#' * animal: character column with animal identifier, where multiple
#'    animals are separated by a space.
#' * phase: character column containing phase, used to delineate multiple
#'    experimental phases.
#'
#' @param mat_files_use data.frame containing columns described
#'    above, used to select the appropriate MatLab .mat file
#'    to import.
#' @param animal character vector with one animal identifier
#' @param phase character vector with one phase
#' @param channels_per_animal integer number of channels to assign for
#'    each animal.
#'    If 5 channels were assigned per animal,
#'    `channels_per_animal=5` which
#'    will assign blocks of 5 channels to each animal identifier
#'    associated with the .mat file.
#' @param channels_to_retain integer vector of channels to retain for
#'    each animal. For example, there may be 5 channels assigned
#'    to each animal, and `channels_to_retain` defines which of
#'    those channels should be used for downstream analysis.
#'    If 5 channels were assigned per animal, `channels_per_animal=5` which
#'    will assign channels to each animal in blocks of 5. However,
#'    if only 4 channels were enabled during recording,
#'    `channels_to_retain=c(1,2,3,4)` will cause the first 4 channels to
#'    be retained.
#' @param drop_unused_channels logical indicating whether to drop
#'    all channels except those assigned to the animal, and
#'    channels to be retained as defined by `channels_to_retain`.
#' @param do_plot logical indicating whether to call
#'    `plot_ephys_event_data()` to plot an overview of the channel
#'    signal, and the signal defined for each event.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional arguments are passed to `plot_ephys_event_data()`
#'    when `do_plot=TRUE`.
#'
#' @family jam import functions
#'
#'
#' @export
import_ephys_mat <- function
(mat_files_use,
 animal=NULL,
 phase=NULL,
 channels_per_animal=5,
 channels_to_retain=seq_len(channels_per_animal-1),
 drop_unused_channels=TRUE,
 do_plot=FALSE,
 verbose=FALSE,
 ...)
{
   ##
   if (verbose) {
      printDebug("import_ephys_mat(): ",
         "Searching for animal:",
         animal);
   }
   if (length(animal) > 0) {
      animal_grep <- paste0("(^|\\s)", animal, "($|\\s)");
      animal_row <- grepl(animal_grep,
         perl=TRUE,
         mat_files_use[,"animal"]);
   } else {
      animal_row <- rep(TRUE, nrow(mat_files_use));
   }
   if (length(phase) > 0) {
      phase_row <- (mat_files_use[,"phase"] %in% phase);
   } else {
      phase_row <- rep(TRUE, nrow(mat_files_use));
   }
   use_row <- (animal_row & phase_row);
   if (length(rownames(mat_files_use)) == 0) {
      rownames(mat_files_use) <- makeNames(mat_files_use$filename);
   }
   mat_file_rows <- mat_files_use[use_row,,drop=FALSE];
   mat_lll <- lapply(nameVector(rownames(mat_file_rows)), function(i_row){
      mat_file_row <- mat_file_rows[i_row,,drop=FALSE];

      animals <- unlist(strsplit(mat_file_row$animal, "[ ]+"));
      if (verbose) {
         printDebug("import_ephys_mat(): ",
            "File contains data for animals:",
            animals);
      }

      mat_file <- pasteByRow(
         mat_file_row[,c("path", "filename")],
         sep="/");
      if (verbose) {
         printDebug("import_ephys_mat(): ",
            "Importing file:",
            mat_file);
         print(mat_file_row);
      }

      ## Import data for one .mat file
      mat_lll <- import_ephys_mat_1(mat_file,
         channels_per_animal=channels_per_animal,
         channels_to_retain=channels_to_retain,
         drop_unused_channels=drop_unused_channels,
         verbose=verbose);
      mat_l <- readMat(mat_file);

      ## Detect channels
      if (verbose) {
         printDebug("import_ephys_mat(): ",
            "Detecting channels.");
      }
      sdim_mat <- sdim(mat_l);
      mat_chs <- vigrep("^AI[0-9A-Za-z]+$", names(mat_l));
      use_chs <- rownames(
         subset(sdim_mat,
            grepl("^AI[0-9A-Za-z]+$", rownames(sdim_mat)) &
               rows > 10000)
      );
      if (verbose) {
         printDebug("import_ephys_mat(): ",
            "use_chs:",
            use_chs);
      }
      if (!length(use_chs) == length(animals)*5) {
         printDebug("Detected channels have the wrong length, compared to animals:",
            animals);
      }
      animal_v <- rep(rep(c(animals, "blank"), each=5), length.out=length(use_chs));
      use_chs_v <- split(use_chs, animal_v)[[animal]];

      ## Drop any channels not relevant to the animal requested
      if (drop_unused_channels) {
         drop_chs1 <- setdiff(use_chs, use_chs_v);
         drop_chs <- provigrep(paste0("^", drop_chs1),
            names(mat_l));
         keep_chs <- setdiff(names(mat_l), drop_chs);
         mat_l <- mat_l[keep_chs];
      }

      if (verbose) {
         printDebug("import_ephys_mat(): ",
            "use_chs_v:",
            use_chs_v);
      }
      if (length(use_chs_v) == 0) {
         stop("use_chs_v is empty");
      }
      mat_l$animal <- animal;

      ##########################################
      ## Update channel data
      ##
      ## The ".ts" suffix is "time start"
      ## The ".ind" suffix is the index of the start time
      ## The ".ts.step" suffix is the time step, duration between measurements
      ##
      ## Store this information as attribute for each list element
      suffix_set <- c("ind", "ts", "ts.step");
      for (i in seq_along(use_chs_v)) {
         i_debug <- list(i);
         channel <- use_chs_v[i];
         attr(mat_l[[channel]], "channelnum") <- i;
         attr(mat_l[[channel]], "channel") <- channel;
         attr(mat_l[[channel]], "animal") <- animal;
         for (suffix in suffix_set) {
            if (1 == 2 && verbose) {
               printDebug("import_ephys_mat(): ",
                  "suffix:",
                  suffix);
            }
            suffix_name <- paste(channel, suffix, sep=".");
            if (suffix_name %in% names(mat_l)) {
               i_val <- mat_l[[suffix_name]];
               attr(mat_l[[channel]], suffix) <- i_val;
               i_debug <- c(i_debug,
                  list(c(paste0(suffix, ":"), i_val)))
            } else {
               if (1 == 2 && verbose) {
                  printDebug("import_ephys_mat(): ",
                     "suffix_name not found:",
                     suffix_name);
               }
            }
         }
         if (1 == 2 && verbose) {
            printDebug(unlist(i_debug, recursive=FALSE),
               list(c("orange","lightblue")), sep=" ");
         }
      }
      ## prepare a list to return
      ret_l <- list(mat_l=mat_l,
         use_chs_v=use_chs_v);

      ##########################################
      ## Get event_data
      if (do_plot) {
         #event_data <- get_ephys_event_data(mat_l, channel=use_chs_v[1]);
         plot_ephys_event_data(mat_l,
            channel=use_chs_v[1],
            ...);
      }

      return(ret_l);
   });
   return(mat_lll);
}

#' Import single MatLab .mat file
#'
#' Import single MatLab .mat file
#'
#' This file is intended for a specific scenario, importing
#' a Matlab `.mat` file that contains E-physiology
#' neural measurements, stored as a list of numeric
#' matrix objects.
#'
#' The measurements are expected to be stored in channels
#' whose names match the `channel_grep` argument, by default
#' channels like `"AI01"` that begin with `"AI"` and end with
#' numeric digits.
#'
#' Channels are required to have at least `min_channel_rows`
#' rows of measurements. This argument is intended to remove
#' channels which were not active in a given experiment,
#' and were included in the output file.
#'
#' Multiple channels are stored per animal in each file,
#' with argument `channels_per_animal` describing how many
#' measurement channels there are for each animal. By default,
#' there are five channels per animal, and the first four
#' channels are retained, defined with argument
#' `channels_to_retain`.
#'
#' @return `list` containing the relevant data for
#'    each animal in the source `mat_file` file.
#'
#' @family jam import functions
#'
#' @param mat_file one of the recognized inputs:
#'    1. character path to a `.mat` Matlab file,
#'    2. character path to a `.RData` file converted
#'    from a `.mat` file by the `"reach"` R package.
#'    3. `list` object representing data equivalent
#'    to data already imported from a `.mat` file.
#' @param animal character vector representing the animal
#'    or multiple animals whose signal data is contained
#'    in the data.
#' @param channels_per_animal integer number of channels
#'    allocated to each animal. The vector of recognized
#'    channels is annotated with each `animal` value
#'    repeated this many times. For example a file with
#'    `10` channels, and `2` animals, would have
#'    `channels_per_animal=5` which would assign the
#'    first `5` channels to the first animal,
#'    and the next `5` channels to the second animal.
#'    When `channel_grep` contains multiple values,
#'    `channels_per_animal` is applied to each set
#'    of matched channels, in order, so that the
#'    `channels_per_animal` value can be custom for
#'    each matched set of channels.
#' @param channels_to_retain integer vector representing
#'    the channels to retain for each animal, with values
#'    starting at `1` and values as high as `channels_per_animal`.
#'    For example if `channels_per_animal=5`, to retain the
#'    first `4` channels per animal, use
#'    `channels_to_retain=c(1,2,3,4)`.
#'    When `channel_grep` contains multiple values,
#'    `channels_to_retain` is applied to each set
#'    of matched channels, in order, so that the
#'    `channels_to_retain` value can be custom for
#'    each matched set of channels.
#' @param channel_grep character vector of regular expression
#'    patterns used to recognize channels to retain. This
#'    vector is passed to `jamba::provigrep()` which applies
#'    each pattern in order, and returns a list of recognized
#'    channels.
#' @param channel_grep_n integer number of matched channel
#'    patterns to include. For example, `channel_grep` may
#'    contain `2` patterns, so to retain only the channels
#'    from the first matched pattern, use `channel_grep_n=1`.
#'    This argument allows flexibiliity in recognizing the
#'    best available channel for each file. To return
#'    all channels for all matched patterns, use
#'    `channel_grep_n=Inf`.
#' @param min_channel_rows integer representing the minimum
#'    number of values in a channel in order to consider that
#'    channel valid. Only valid channels are used when
#'    annotating the `animal`. This argument is intended to
#'    help when a file containing `10` channels has only `1`
#'    animal whose data is contained in channels `6:10`, which
#'    assumes the data in channels `1:5` have fewer than
#'    `min_channel_rows` values.
#'    When `channel_grep` contains multiple values,
#'    `min_channel_rows` is applied to each set
#'    of matched channels, in order, so that the
#'    `min_channel_rows` value can be custom for
#'    each matched set of channels.
#' @param drop_unused_channels logical indicating whether to
#'    drop channels which are not annotated for each
#'    specific `animal`. Typically `drop_unused_channels=TRUE`
#'    is default, which returns only the data relevant to
#'    each `animal`.
#' @param drop_grep character pattern used to define other
#'    names to exclude from the final returned data object.
#'    This pattern will not override any channels already
#'    annotated to each specific `animal`. This argument is
#'    intended to help remove extraneous channels or signals
#'    which are not required for downstream processing, in
#'    order to reduce the size of the returned data.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#'
#' @export
import_ephys_mat_1 <- function
(mat_file,
 animal,
 channels_per_animal=5,
 channels_to_retain=seq_len(channels_per_animal-1),
 channel_grep=c("^FP[0-9]+$", "^AI[0-9]+$"),
 channel_grep_n=1,
 min_channel_rows=750000,
 drop_unused_channels=TRUE,
 drop_grep="^SPKC|^WB",
 verbose=FALSE,
 ...)
{
   ## Purpose is a simple wrapper to import one .mat Plexon file

   ## Wrapper to load RData file previously converted by R package reach
   import_reach <- function
   (rdatafile,
      ...)
   {
      ## Purpose is to import RData and return mat_l list
      rdataloaded <- load(rdatafile);
      lapply(nameVector(rdataloaded), function(i){
         get(i);
      });
   }

   if (is.list(mat_file)) {
      ## Assume input is already mat_l format
      if (verbose) {
         printDebug("import_ephys_mat_1(): ",
            "Using ",
            "mat_file",
            " directly.");
      }
      mat_l <- mat_file;
      rm(mat_file);
   } else if (igrepHas("([.]rda|[.]rdata)$", mat_file)) {
      ## Import RData file
      if (verbose) {
         printDebug("import_ephys_mat_1(): ",
            "Importing from RData file.");
      }
      mat_l <- import_reach(mat_file);
   } else {
      ## Import data for one .mat file
      if (verbose) {
         printDebug("import_ephys_mat_1(): ",
            "Importing from Matlab .mat file.");
      }
      mat_l <- R.matlab::readMat(mat_file);
   }

   ## Extend arguments to length(channel_grep)
   min_channel_rows <- rep(min_channel_rows,
      length.out=length(channel_grep));
   channels_per_animal <- rep(channels_per_animal,
      length.out=length(channel_grep));
   if (!is.list(channels_to_retain)) {
      channels_to_retain <- rep(list(channels_to_retain),
         length.out=length(channel_grep));
   } else {
      channels_to_retain <- rep(channels_to_retain,
         length.out=length(channel_grep));
   }

   ## Detect channels
   if (verbose) {
      printDebug("import_ephys_mat_1(): ",
         "Detecting channels.");
   }

   ## Get dimensions of each element in mat_l
   sdim_mat <- jamba::sdim(mat_l);

   ## Find channel names matching channel_grep
   mat_chs_l <- provigrep(channel_grep,
      rownames(sdim_mat),
      returnType="list");
   ## Enforce minimum rows per matched channel
   mat_chs_l <- lapply(seq_along(mat_chs_l), function(i){
      mat_chs <- mat_chs_l[[i]];
      rownames(
         subset(sdim_mat,
            rownames(sdim_mat) %in% mat_chs &
               rows >= min_channel_rows[i]));
   });

   mat_chs_nonzero <- which(lengths(mat_chs_l) > 0);
   if (length(mat_chs_nonzero) == 0) {
      stop("No channels match the given channel_grep values.");
   }
   mat_chs_l <- mat_chs_l[mat_chs_nonzero];
   min_channel_rows <- min_channel_rows[mat_chs_nonzero];
   channels_per_animal <- channels_per_animal[mat_chs_nonzero];
   channels_to_retain <- channels_to_retain[mat_chs_nonzero];

   if (length(channel_grep_n) > 0) {
      mat_chs_l <- head(mat_chs_l, channel_grep_n);
   }
   #mat_chs <- vigrep(channel_grep, names(mat_l));
   use_chs_l <- lapply(seq_along(mat_chs_l), function(use_num){
      use_chs <- mat_chs_l[[use_num]];
      use_chs <- rownames(
         subset(sdim_mat, rownames(sdim_mat) %in% use_chs & rows >= min_channel_rows[use_num])
      );
      if (length(use_chs) == 0) {
         return(NULL);
      }
      if (verbose) {
         printDebug("import_ephys_mat_1(): ",
            "use_chs:",
            use_chs);
      }
      if ((length(use_chs) %% channels_per_animal) != 0) {
         printDebug("import_ephys_mat_1(): ",
            "Detected channels have the wrong length, compared to channels_per_animal:",
            channels_per_animal);
         printDebug("import_ephys_mat_1(): ",
            "use_chs:",
            use_chs);
      }
      ch_multiple <- length(use_chs) / channels_per_animal[use_num];
      blank_v <- makeNames(rep("blank", ch_multiple));
      num_v <- rep(
         seq_len(channels_per_animal[use_num]),
         length.out=length(use_chs));
      keep_v <- (num_v %in% channels_to_retain[[use_num]]);
      animal_v <- rep(
         rep(c(unlist(animal), blank_v),
            each=channels_per_animal[use_num]),
         length.out=length(use_chs));
      names(animal_v) <- use_chs;
      if (verbose) {
         printDebug("import_ephys_mat_1(): ",
            "animal_v:");
         print(animal_v);
         printDebug("import_ephys_mat_1(): ",
            "use_chs:", use_chs);
      }
      ch_df <- data.frame(channel=names(animal_v),
         animal=animal_v,
         channel_num=num_v,
         keep=keep_v);
      ch_df;
   });
   use_chs_df <- rbindList(use_chs_l);
   use_chs_df_l <- split(use_chs_df, use_chs_df$animal);
   if (verbose) {
      printDebug("import_ephys_mat_1(): ",
         "use_chs_df_l:");
      print(use_chs_df_l);
   }
   #use_chs_v <- split(use_chs, animal_v)[[animal]];

   ## Drop any channels not relevant to the animal requested
   if (drop_unused_channels) {
      ## Split
      if (verbose) {
         printDebug("import_ephys_mat_1(): ",
            "channels_to_retain:",
            channels_to_retain);
      }
      mat_lll <- lapply(use_chs_df_l[animal], function(use_chs_df_1){
         use_chs_v <- subset(use_chs_df_1, keep)$channel;
         ## Extend channel name to match any suffix
         use_chs_v <- provigrep(paste0("^", use_chs_v),
            rownames(sdim_mat));

         if (verbose) {
            printDebug("import_ephys_mat_1(): ",
               "use_chs_v:",
               use_chs_v);
         }
         ## Drop all channels (mat_chs_l)
         drop_chs <- setdiff(unlist(mat_chs_l),
            use_chs_v);

         ## Extend channel name to match any suffix
         drop_chs <- provigrep(paste0("^", drop_chs),
            rownames(sdim_mat));
         if (length(drop_grep) > 0 && any(nchar(drop_grep) > 0)) {
            drop_grep <- drop_grep[nchar(drop_grep) > 0];
            drop_chs2 <- setdiff(
               provigrep(drop_grep, rownames(sdim_mat)),
               use_chs_v);
            drop_chs <- unique(c(drop_chs, drop_chs2));
         }
         keep_chs <- setdiff(rownames(sdim_mat), drop_chs);
         if (verbose) {
            printDebug("import_ephys_mat_1(): ",
               "keep_chs:",
               keep_chs);
            printDebug("import_ephys_mat_1(): ",
               "drop_chs:",
               drop_chs);
         }
         mat_l_i <- mat_l[keep_chs];
         mat_l_i$animal <- use_chs_df_1$animal[[1]];
         attr(mat_l_i, "animal") <- mat_l_i$animal;
         attr(mat_l_i, "channels") <- subset(use_chs_df_1, keep)$channel;
         attr(mat_l_i, "drop_channels") <- intersect(rownames(sdim_mat), drop_chs);
         attr(mat_l_i, "sdim_mat") <- sdim_mat;
         if (verbose) {
            printDebug("import_ephys_mat_1(): ",
               "sdim(mat_l_i):");
            print(sdim(mat_l_i));
         }
         ## Add some attributes
         for (channel in subset(use_chs_df_1, keep)$channel) {
            attr(mat_l_i[[channel]], "channelnum") <- use_chs_df_1[channel,"channel_num"];
            attr(mat_l_i[[channel]], "channel") <- channel;
            attr(mat_l_i[[channel]], "animal") <- mat_l_i$animal;
         }
         mat_l_i;
      });
      return(mat_lll);
   }
   return(mat_l);
}
