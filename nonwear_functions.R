# Helper Functions --------------------------------------------------------


  .check_assumptions <- function(sdvm, wear_value, nonwear_value) {

    if (!isTRUE(requireNamespace("magrittr", quietly = TRUE))) stop(
      "Please install the magrittr package to run the nonwear algorithm.",
      "\nUse the following:",
      "\n\ninstall.packages(\"magrittr\")", call. = FALSE
    )

    library(magrittr)

    stopifnot(
      is.numeric(sdvm),
      identical(class(wear_value), class(nonwear_value)),
      !isTRUE(all.equal(wear_value, nonwear_value))
    )

    invisible(sdvm)

  }


  .screen_short_wear <- function(raw_wear, window) {

    ## Get set up

      runs <-
        rle(raw_wear) %>%
        do.call(data.frame, .) %>%
        rev(.) %>%
        stats::setNames(c("is_wear", "duration"))


    ## create a table with row length equal to wear periods

      wear_runs <-
        matrix(data = 0, nrow = sum(runs$is_wear), ncol = 3) %>%
        as.data.frame(.) %>%
        stats::setNames(c("previous", "current", "subsequent"))


    ## Check via loop

      run_i <- 1
      wear_i <- 1

      while ( run_i <= nrow(runs) ) {

        if ( runs$is_wear[run_i] ) {

          #Record duration of prior nonwear period (if applicable)
          if( run_i > 1 ) wear_runs$previous[wear_i] <- runs$duration[run_i-1]

          #Record duration of current wear period
          wear_runs$current[wear_i] <- runs$duration[run_i]

          #Record duration of subsequent nonwear period (if applicable)
          if ( run_i < nrow(runs) ) wear_runs$subsequent[wear_i] <- runs$duration[run_i+1]

          #Re-label wear time as nonwear if needed
          if (
            wear_runs$current[wear_i] < 60/window*30 &
            wear_runs$current[wear_i]/(wear_runs$previous[wear_i] + wear_runs$subsequent[wear_i]) < 0.3
          ) {
            runs$is_wear[run_i] <- FALSE
          }

          wear_i <- wear_i+1

        }

        run_i <- run_i+1

      }

    ## Return

      rep(runs$is_wear, runs$duration)

  }


# Main function -----------------------------------------------------------


  analyze_wear <- function(
    sdvm, window = 1, wear_value = TRUE, nonwear_value = FALSE
  ) {

    .check_assumptions(sdvm, wear_value, nonwear_value)

    ifelse(sdvm < 0.013, "nonwear", "wear") %>%
    {data.frame(
      raw_status = .,
      duration = rle(.) %>% {rep(.$lengths, .$lengths)}
    )} %>%
    {(.$raw_status == "wear") | (.$duration < 60/window*30)} %>%
    .screen_short_wear(window) %>%
    ifelse(wear_value, nonwear_value)

  }
