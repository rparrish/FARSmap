
# fars_functions.R
#
# Peer-assignment: documenting code
#
# Author: Rollie Parrish
# Coursera - Building R packages
# 2017-01-17
#

#' Reads a FARS file
#'
#' This function creates a tbl_df object from the specified filename. Prints the resulting tbl_df a side-effect.
#'   An error message is returned if the file does not exist.
#'
#' @param filename The desired filename to be read.
#' @return object of class tbl_df
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2015.csv.bz2")
#' }
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}



#' Creates a FARS file name
#'
#' This function returns a properly-formatted file name using the year as a function argument.
#'   An error will occur if the year parameter cannot be coerced into an integer.
#'
#' @param year The year
#' @return string in the form of "accident_YYYY.csv.bz2"
#'
#' @examples
#' \dontrun{
#' make_filename(2016)
#' }
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}



#' Read multiple FARS files
#'
#' This function creates a tbl_df object from multiple files using a list of years as the function argument.
#'   Prints the resulting tbl_df a side-effect. An error message Invalid Year: <value> is returned if
#'   the provided year is not between 2013 and 2015.
#'
#' @param years A vector of years.
#' @return A tbl_df with the month and year for each of the years that were specified in the function argument.
#'   Prints the tbl_df as a side-effect.
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013)
#' fars_read_years(c(2013, 2014, 2015))
#' }
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}




#' Summarizes fars data by year
#'
#' This function creates a tbl_df object with the
#' number of records for each month by year. Returns
#' an error message of Invalid year: <value> if the
#' year value is not between 2013-2015.
#'
#' @param years A vector of years.
#' @return A tbl_df of the the number of records for each month by year. Prints the tbl_df as a side-effect.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013)
#' fars_summarize_years(c(2013, 2014, 2015))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}




#' Map with accident locations
#'
#' This function creates a map of the accident locations for a given state and year.
#'   Returns an error message of Invalid year: <value> if the year value
#'   is not between 2013-2015. Returns an error message of Invalid STATE number:
#'   if the state number is not one of the States included in the data.
#'   Returns an error of no accidents to plot if there are no accidents for the
#'   specified state number and year.
#'
#' @param state.num Integer value between 0 and 50.
#' @param year The year
#' @return a map object. Prints the map as a side-effect.
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, 2014)
#' fars_map_state(20, 2015)
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
