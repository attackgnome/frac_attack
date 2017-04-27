#'FARS reader
#'
#'\code{fars_read} returns a data frame tbl sourced from the file in its arguments
#'
#'@note requires readr and dplyr packages
#'@note If the files does not exist will return an error message "file '", filename, "' does not exist"
#'
#'@param filename A string.
#'@return data A data frame tbl
#'@examples
#'fars_read("accident.2014.csv.bz2")
#'fars_read("xyz.csv")
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#'Filename maker
#'
#'\code{make_filename} returns a string containg the file name for the year specified in the arguments
#'
#'@note year must be compatible with as.integer
#'
#'@param year A numeric.
#'@return sprintf A string
#'@examples
#'fars_read(2014)
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#'FARS reader by year
#'
#'\code{fars_read_years} reads files into a data.frame with two columns MONTH and year
#'
#'@note requires dplyr package
#'
#'@param years A vector of numbers representing years
#'@return a data.frame with with the month and year as columns
#'@examples
#'fars_read(2014)
#'#'fars_read(2014:2015)
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


#'FARS summarize by year and month
#'
#'\code{fars_read_years} returns a data.frame containing the number of accidents with years as columns and months as rows
#'
#'@note requires dplyr package
#'
#'@param years A vector of numbers representing years
#'@return A data.frame containing the number of accidents with years as columns and months as rows
#'@examples
#'fars_summarize_years(c(2013,2014))
#'fars_summarize_years(2014:2015)
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#'FARS mapper
#'
#'\code{fars_map_state} plots the number of accidents per state per year specified in the arguments
#'
#'@note requires maps and graphics packages
#'
#'@param year A integer specifying the year.
#'@param state.num A integer representing a state in the FARS data
#'
#'@return If the data for the year is available and the state.num is valid
#'  the output will be a plot with an outline of the state with points showing the
#'  locations of accidents during that year
#'  If an invalid state.num is passed a an argument the following message will be returned
#'  "invalid STATE number: state.num"
#'  If there are no accidents for the year and state specified the following message will
#'  be returned "no accidents to plot"
#'
#'@examples
#'fars_map_state('10',2013)
#'fars_map_state('01',2015)
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
