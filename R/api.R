#' Get last exercises for an athlete
#'
#' Retrieves the most recent exercises/workouts done by an athlete.
#'
#' @param session A treinus_session object from [treinus_auth()]
#' @param athlete_id Integer ID of the athlete
#'
#' @return A tibble with exercise data including columns like `id_exercise_done`,
#'   `genre_name`, `start`, `distance`, `total_elapsed_time`, etc.
#'
#' @examples
#' \dontrun{
#' session <- treinus_auth()
#' exercises <- treinus_get_exercises(session, athlete_id = 50)
#' }
#'
#' @export
treinus_get_exercises <- function(session, athlete_id) {
  if (!inherits(session, "treinus_session")) {
    cli::cli_abort("{.arg session} must be a treinus_session object from {.fn treinus_auth}")
  }

  base_url <- attr(session, "base_url")
  url <- paste0(base_url, "/Athlete/Exercise/GetLastExerciseDone")

  resp <- session |>
    httr2::req_url(url) |>
    httr2::req_url_query(idAthlete = athlete_id) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_perform()

  json <- httr2::resp_body_json(resp)

  if (is.null(json$data) || length(json$data) == 0) {
    return(tibble::tibble())
  }

  dplyr::bind_rows(json$data) |>
    janitor::clean_names()
}


#' Get exercises for multiple athletes
#'
#' Retrieves the most recent exercises for a vector of athlete IDs.
#'
#' @param session A treinus_session object from [treinus_auth()]
#' @param athlete_ids Integer vector of athlete IDs
#' @param .progress Show progress bar? Default TRUE.
#'
#' @return A tibble with exercise data for all athletes
#'
#' @examples
#' \dontrun{
#' session <- treinus_auth()
#' exercises <- treinus_get_exercises_batch(session, athlete_ids = 1:50)
#' }
#'
#' @export
treinus_get_exercises_batch <- function(session, athlete_ids, .progress = TRUE) {
  if (!inherits(session, "treinus_session")) {
    cli::cli_abort("{.arg session} must be a treinus_session object from {.fn treinus_auth}")
  }

  results <- purrr::map(
    athlete_ids,
    function(id) {
      tryCatch(
        treinus_get_exercises(session, athlete_id = id),
        error = function(e) {
          cli::cli_alert_warning("Failed to get exercises for athlete {id}: {e$message}")
          tibble::tibble()
        }
      )
    },
    .progress = .progress
  )

  dplyr::bind_rows(results)
}


#' Get detailed exercise analysis
#'
#' Retrieves detailed analysis data for a specific exercise, including
#' GPS records, heart rate, and other metrics.
#'
#' @param session A treinus_session object from [treinus_auth()]
#' @param exercise_id Integer ID of the exercise (id_exercise_done)
#' @param athlete_id Integer ID of the athlete
#' @param team_id Integer ID of the team
#'
#' @return A list with exercise analysis data including `Records` with
#'   detailed time-series data.
#'
#' @examples
#' \dontrun{
#' session <- treinus_auth()
#' analysis <- treinus_get_exercise_analysis(
#'   session,
#'   exercise_id = 57,
#'   athlete_id = 50,
#'   team_id = 2994
#' )
#' # Access records
#' records <- analysis$data$Analysis$Records
#' }
#'
#' @export
treinus_get_exercise_analysis <- function(session, exercise_id, athlete_id, team_id) {
  if (!inherits(session, "treinus_session")) {
    cli::cli_abort("{.arg session} must be a treinus_session object from {.fn treinus_auth}")
  }

  base_url <- attr(session, "base_url")
  url <- paste0(base_url, "/Athlete/ExerciseSheet/Done/ExerciseAnalysis")

  resp <- session |>
    httr2::req_url(url) |>
    httr2::req_url_query(
      idTeam = team_id,
      idAthlete = athlete_id,
      idExerciseDone = exercise_id
    ) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_perform()

  httr2::resp_body_json(resp)
}


#' Extract records from exercise analysis
#'
#' Extracts time-series records from exercise analysis. Includes unit metadata
#' as attributes and optionally standardizes values to common units.
#'
#' @param analysis Analysis data from [treinus_get_exercise_analysis()]
#' @param standardize Logical. Apply unit standardization? Default FALSE.
#' @param tz Timezone for timestamp conversion (used when `standardize=TRUE`).
#'   Default "America/Sao_Paulo".
#'
#' @return A tibble with time-series records. Unit metadata from the analysis
#'   is attached as attributes (e.g., `attr(result, "DistanceUnit")`).
#'
#'   If `standardize=TRUE`, additional columns are added:
#'   \itemize{
#'     \item `timestamp`: converted to POSIXct datetime
#'     \item `lat`, `lon`: coordinates in degrees (converted from semicircles)
#'     \item `distance_km`: distance in kilometers
#'     \item `speed_kmh`: speed in km/h
#'   }
#'
#' @examples
#' \dontrun{
#' session <- treinus_auth()
#' analysis <- treinus_get_exercise_analysis(session, 57, 50, 2994)
#'
#' # Raw records with unit attributes
#' records <- treinus_extract_records(analysis)
#' attr(records, "DistanceUnit")
#'
#' # Standardized records
#' records <- treinus_extract_records(analysis, standardize = TRUE)
#' }
#'
#' @export
treinus_extract_records <- function(analysis, standardize = FALSE, tz = "America/Sao_Paulo") {
  records <- analysis$data$Analysis$Records
  if (is.null(records) || length(records) == 0) {
    return(tibble::tibble())
  }

  df <- dplyr::bind_rows(records) |>
    janitor::clean_names()


  # Extract all unit metadata

  analysis_data <- analysis$data$Analysis
  unit_names <- grep("Unit$", names(analysis_data), value = TRUE)
  units <- stats::setNames(
    lapply(unit_names, function(u) analysis_data[[u]]),
    unit_names
  )

  # Attach units as attributes
  for (unit_name in names(units)) {
    attr(df, unit_name) <- units[[unit_name]]
  }

  if (!standardize) return(df)

  # Validate required units for standardization
  distance_unit <- units$DistanceUnit
  speed_unit <- units$SpeedUnit

  if (is.null(distance_unit)) {
    cli::cli_abort("Cannot standardize: {.field DistanceUnit} not found in analysis metadata")
  }
  if (is.null(speed_unit)) {
    cli::cli_abort("Cannot standardize: {.field SpeedUnit} not found in analysis metadata")
  }

 # Conversion factors
  semicircles_to_deg <- 180 / 2^31

  dist_to_km <- switch(
    distance_unit,
    "m" = 1 / 1000,
    "km" = 1,
    cli::cli_abort("Unknown {.field DistanceUnit}: {.val {distance_unit}}")
  )

  speed_to_kmh <- switch(
    speed_unit,
    "m/s" = 3.6,
    "km/h" = 1,
    cli::cli_abort("Unknown {.field SpeedUnit}: {.val {speed_unit}}")
  )

  df |>
    dplyr::mutate(
      timestamp = as.POSIXct(timestamp, tz = tz),
      lat = position_lat * semicircles_to_deg,
      lon = position_long * semicircles_to_deg,
      distance_km = distance * dist_to_km,
      speed_kmh = speed * speed_to_kmh
    )
}


#' Get dashboard data
#'
#' Retrieves dashboard summary data including recent workouts and statistics.
#'
#' @param session A treinus_session object from [treinus_auth()]
#'
#' @return A list with dashboard data
#'
#' @examples
#' \dontrun{
#' session <- treinus_auth()
#' dashboard <- treinus_get_dashboard(session)
#' }
#'
#' @export
treinus_get_dashboard <- function(session) {
  
  if (!inherits(session, "treinus_session")) {
    cli::cli_abort("{.arg session} must be a treinus_session object from {.fn treinus_auth}")
  }
  
  base_url <- attr(session, "base_url")
  url <- paste0(base_url, "/Global/DashBoard/Index")
  
  cli::cli_progress_step("Fetching dashboard data...")
  
  resp <- session |>
    httr2::req_url(url) |>
    httr2::req_perform()
  
  page <- httr2::resp_body_html(resp)
  
  # Extract key metrics from dashboard
  dashboard_data <- list(
    title = page |> rvest::html_element("title") |> rvest::html_text2(),
    # Add more extraction logic here based on actual dashboard structure
    status = "success"
  )
  
  cli::cli_progress_done()
  
  dashboard_data
}


#' Make a raw API request
#'
#' Low-level function to make authenticated requests to any Treinus endpoint.
#' Useful for exploring the API or accessing endpoints not yet wrapped.
#'
#' @param session A treinus_session object from [treinus_auth()]
#' @param endpoint Character string with the endpoint path (e.g., "/Athlete/Performance/Index")
#' @param method HTTP method ("GET", "POST", etc.)
#' @param body Optional request body for POST requests
#' @param ... Additional arguments passed to [httr2::req_perform()]
#'
#' @return httr2 response object
#'
#' @examples
#' \dontrun{
#' session <- treinus_auth()
#' 
#' # Make a custom request
#' resp <- treinus_request(
#'   session,
#'   endpoint = "/Athlete/Performance/Index",
#'   method = "GET"
#' )
#' }
#'
#' @export
treinus_request <- function(session, endpoint, method = "GET", body = NULL, ...) {
  
  if (!inherits(session, "treinus_session")) {
    cli::cli_abort("{.arg session} must be a treinus_session object from {.fn treinus_auth}")
  }
  
  base_url <- attr(session, "base_url")
  url <- paste0(base_url, endpoint)
  
  req <- session |>
    httr2::req_url(url) |>
    httr2::req_method(method)
  
  if (!is.null(body)) {
    req <- req |> httr2::req_body_json(body)
  }
  
  req |> httr2::req_perform(...)
}


#' Extract data from Treinus HTML tables
#'
#' Helper function to parse data from HTML tables commonly found in Treinus pages.
#'
#' @param html An html_document or html_node from rvest
#' @param selector CSS selector for the table (default: "table")
#'
#' @return A tibble with the table data
#'
#' @examples
#' \dontrun{
#' session <- treinus_auth()
#' resp <- treinus_request(session, "/Athlete/Performance/Index")
#' page <- httr2::resp_body_html(resp)
#' 
#' # Extract table data
#' data <- treinus_parse_table(page)
#' }
#'
#' @export
treinus_parse_table <- function(html, selector = "table") {
  
  table <- html |> rvest::html_element(selector)
  
  if (rvest::html_length(table) == 0) {
    cli::cli_warn("No table found with selector: {selector}")
    return(tibble::tibble())
  }
  
  table |>
    rvest::html_table() |>
    tibble::as_tibble()
}
