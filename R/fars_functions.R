#' Read csv files into a data.frame.
#'
#' This function takes a csv file path and reads the file
#' using \code{readr::read_csv()} and \code{dplyr::tbl_df()}.
#' Example data is only available for 2013-2015.
#' Requires readr and dplyr to run.
#'
#' @param filename Path to a csv file.
#'
#' @return Returns an object of the class data.frame.
#'
#' @examples
#' \dontrun{
#'   df <- fars_read("data/accident_2013.csv.bz2")
#'   print(class(df))
#'   head(df)
#' }
#'
#' @import dplyr readr
#'
#'@export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
#' Get Fatality-Analysis-Reporting-System (FARS) csv filename
#'
#' This function returns the filename for FARS csv data given
#' a certain year. For more info, see \href{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}{the FARS website}.
#' Example data is only available for 2013-2015.
#'
#' @param year The year for the file name you want to return.
#'
#' @return Returns a character string giving the name of a csv file.
#'
#' @examples
#'   print(make_filename("2015"))
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
#' Collect Month / Year columns from FARS csv data
#'
#' This function takes a vector of years, reads the FARS data
#' from these years and returns a list of data frame results
#' containing the month and year column for each year's data frame.
#' The FARS csv files you want to read must be in your working
#' directory otherwise the function will return a warning: invalid year.
#' Requires the dplyr and readr package to be loaded.
#' Example data is only available for 2013:2015.
#'
#' @param years A vector of years that you wish to collect data for
#'
#' @return Returns a list containing data frame objects with columns month and year.
#'
#' @examples
#' \dontrun{
#'   setwd("data")
#'   years <- 2013:2015
#'   fdat <- fars_read_years(years)
#'   print(class(fdat))
#'   print(head(fdat[1]))
#' }
#'
#' @import dplyr readr
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
#' Summarize FARS data by month and year
#'
#' The functions loads multiple years of FARS csv data
#' and shows the total number of fatal road incidents
#' by month (rows) and years (columns).
#' The FARS csv files you wish to read must be in the
#' working directory otherwise the function will give you a \code{warning: invalid year}.
#' Requires the dplyr and tidyr packages to be loaded.
#' Example data is only available for 2013:2015.
#'
#' @param years A vector of the years you'd like to collect data for
#'
#' @return A summary data.frame showing the total number of fatal road incidents by month / year.
#'
#' @examples
#' \dontrun{
#'   setwd("data")
#'   years <- 2013:2015
#'   df <- fars_summarize_years(years)
#'   print(class(df))
#'   print(head(df))
#' }
#'
#' @import dplyr readr tidyr
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
#' Map FARS fatal road incidents by state
#'
#' This function will collect FARS csv data for a given year
#' and map the data for a given state number (\code{state.num}).
#' If the state number is not one contained in the data, the
#' function will give an error: \code{invalid STATE number: state.num}
#' Requires the dplyr, readr, maps, stats and graphics packages to be loaded.
#' Example data is only available for 2013:2015.
#'
#' @param state.num The unique state number
#' @param year The year to collect data for
#'
#' @return No value is returned. Will map the FARS data.
#'
#' @examples
#' \dontrun{
#'   setwd("data")
#'   year <- 2015
#'   state.num <- 15
#'   fars_map_state(state.num, year)
#' }
#'
#' @import dplyr readr tidyr maps graphics
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
