#' Plot Ephys event data
#'
#' @family jam event functions
#' @family jam plot functions
#'
#' @export
plot_ephys_event_data <- function
(mat_l,
 channel,
 plot_which=c("all", "events", "total"),
 nbin=512,
 bandwidthN=5000,
 do_par=TRUE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to plot data from one ephys channel
   plot_which <- match.arg(plot_which);
   if (any(!channel %in% names(mat_l))) {
      x_channel <- setdiff(channel, names(mat_l));
      stop(paste0("Channel '",
         x_channel,
         "' not found in names(mat_l) ",
         cPaste(names(mat_l))));
   }
   ## Transform eventData into milliseconds
   event_data_l <- get_ephys_event_data(mat_l,
      channels=channel,
      ...);
   i_step <- event_data_l$step;
   i_ind <- event_data_l$ind;
   i_starts <- event_data_l$starts;
   i_stops <- event_data_l$stops;
   i_labels <- event_data_l$labels;
   if (verbose) {
      printDebug("plot_ephys_event_data(): ",
         "length(i_starts):",
         length(i_starts));
   }

   plot_nrow <- (plot_which %in% c("all", "total")) +
      length(i_starts) * (plot_which %in% c("all", "events"));
   plot_ncol <- length(channel);
   if (do_par) {
      opar <- par();
      on.exit(par(opar));
      par("mfrow"=c(plot_nrow, plot_ncol),
         "mar"=c(2,4,2,2),
         "xpd"=FALSE);
   }

   ## Iterate each channel
   channels <- channel;
   ## Get data from each signal
   ret_l <- list();
   if (plot_which %in% c("all", "total")) {
      for (channel in channels) {
         i_v <- mat_l[[channel]][,1];
         animal <- attr(mat_l[[channel]], "animal");
         plotSmoothScatter(x=seq_along(i_v),
            nbin=nbin,
            bandwidthN=bandwidthN,
            y=i_v,
            main=paste0("Animal ", animal, " Channel ", channel),
            xaxt="n",
            #xaxs="i",
            ...);
         abline(v=c(i_ind, i_starts, i_stops),
            col=alpha2col(alpha=0.7,
               rep(c("#009900", "#777700", "#770000"),
                  lengths(list(i_ind, i_starts, i_stops)))),
            lwd=1,
            lty="dotted"
         );
         axis(1,
            las=2,
            cex=0.7,
            at=c(i_ind, i_starts, i_stops),
            line=1.2,
            lty=0,
            labels=i_labels,
            cex=0.7);
         at1 <- pretty(par("usr")[1:2]);
         axis(1, las=2, at=at1,
            labels=format(big.mark=",", trim=TRUE, at1/1000));
      }
   }
   if (plot_which %in% c("all", "events")) {
      i_v_name <- rownames(event_data_l$events_m);
      #i_v_l <- event_data_l$i_v_l;
      if (verbose) {
         printDebug("plot_ephys_event_data(): ",
            "Plotting events:",
            i_v_name);
      }
      ## Iterate each event
      par_mar <- par("mar");
      for (k in i_v_name) {
         ## Iterate each channel
         if (k %in% tail(i_v_name, 1)) {
            par("mar"=par_mar);
         } else {
            k_par_mar <- par("mar");
            k_par_mar[1] <- min(c(1, k_par_mar[1]));
            par("mar"=k_par_mar);
         }
         for (channel in channels) {
            j_v <- event_data_l$events_m[k,channel][[1]];
            label_start <- attr(j_v, "label_start");
            label_stop <- attr(j_v, "label_stop");
            animal <- attr(j_v, "animal");
            plotSmoothScatter(x=seq_along(j_v)-label_start,
               nbin=nbin,
               bandwidthN=bandwidthN,
               y=j_v,
               xaxt="n",
               ...);
            title(main=paste0(
               "Animal ", animal,
               " Channel ", channel,
               " Event ", k),
               line=0.2)
            abline(v=c(label_start, label_stop)-label_start,
               col=alpha2col(alpha=0.7,
                  c("#777700", "#770000")),
               lty="dotted",
               lwd=1);
            k1 <- match(k, i_v_name);
            if (k %in% tail(i_v_name, 1)) {
               axis(1,
                  las=1,
                  line=1.2,
                  lty=0,
                  at=c(label_start, label_stop)-label_start,
                  labels=i_labels[k1 + c(1, 6)],
                  cex=0.7);
            }
            at1 <- pretty(par("usr")[1:2]);
            #at1 <- at1[abs(at1-(label_stop-label_start)) > 100 & abs(at1) > 100];
            axis(1, las=2, at=at1, labels=at1/1000);
            #j_v;
         }
      }
   }
   invisible(ret_l);
}

#' Plot Ephys spectrum
#'
#' @export
plot_ephys_spec <- function
(x,
   do_plot=TRUE,
   verbose=TRUE,
   ...)
{
   ##
   if (!suppressPackageStartupMessages(require(psd))) {
      stop("The psd package is required.");
   }
   #event_data_l <- get_ephys_event_data(mat_l,
   #   channel=channel);
   if (ncol(x) > 1) {
      x <- x[,1];
   }
   i_psd <- pspectrum(x,
      plot=do_plot,
      verbose=verbose);
   invisible(i_psd);
}

