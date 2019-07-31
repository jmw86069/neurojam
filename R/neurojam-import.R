
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
#' @family jam import functions
#'
#' @export
import_ephys_mat_1 <- function
(mat_file,
 animal,
 channels_per_animal=5,
 channels_to_retain=seq_len(channels_per_animal-1),
 channel_grep="^AI[0-9a-zA-Z]+$",
 drop_unused_channels=TRUE,
 verbose=FALSE,
 ...)
{
   ## Purpose is a simple wrapper to import one .mat Plexon file

   ## Import data for one .mat file
   mat_l <- readMat(mat_file);

   ## Detect channels
   if (verbose) {
      printDebug("import_ephys_mat_1(): ",
         "Detecting channels.");
   }
   sdim_mat <- sdim(mat_l);
   mat_chs <- vigrep(channel_grep, names(mat_l));
   use_chs <- rownames(
      subset(sdim_mat,
         grepl(channel_grep, rownames(sdim_mat)) &
            rows > 10000)
   );
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
   ch_multiple <- length(use_chs) / channels_per_animal;
   blank_v <- makeNames(rep("blank", ch_multiple));
   animal_v <- rep(
      rep(c(unlist(animal), blank_v),
         each=channels_per_animal),
      length.out=length(use_chs));
   names(animal_v) <- use_chs;
   if (verbose) {
      printDebug("import_ephys_mat_1(): ",
         "animal_v:");
      print(animal_v);
      printDebug("import_ephys_mat_1(): ",
         "use_chs:", use_chs);
   }
   use_chs_l <- split(use_chs, animal_v);
   #use_chs_v <- split(use_chs, animal_v)[[animal]];


   ## Drop any channels not relevant to the animal requested
   if (drop_unused_channels) {
      ## Split
      if (verbose) {
         printDebug("import_ephys_mat_1(): ",
            "channels_to_retain:",
            channels_to_retain);
      }
      mat_lll <- lapply(use_chs_l, function(use_chs_v){
         use_chs_v <- use_chs_v[channels_to_retain];
         if (verbose) {
            printDebug("import_ephys_mat_1(): ",
               "use_chs_v:",
               use_chs_v);
         }
         drop_chs1 <- setdiff(use_chs, use_chs_v);
         drop_chs <- provigrep(paste0("^", drop_chs1),
            names(mat_l));
         keep_chs <- setdiff(names(mat_l), drop_chs);
         if (verbose) {
            printDebug("import_ephys_mat_1(): ",
               "keep_chs:",
               keep_chs);
         }
         mat_l$animal <- animal_v[use_chs_v];
         mat_l_i <- mat_l[keep_chs];
         if (verbose) {
            printDebug("import_ephys_mat_1(): ",
               "sdim(mat_l_i):");
            print(sdim(mat_l_i));
         }
         ## Add some attributes
         for (i in seq_along(use_chs_v)) {
            channel <- use_chs_v[i];
            attr(mat_l_i[[channel]], "channelnum") <- channels_to_retain[i];
            attr(mat_l_i[[channel]], "channel") <- channel;
            attr(mat_l_i[[channel]], "animal") <- animal_v[use_chs_v[i]];
         }
         mat_l_i;
      });
      return(mat_lll);
   }
   return(mat_l);
}
