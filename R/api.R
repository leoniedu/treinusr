#' Get last exercises for an athlete
#'
#' Retrieves the most recent exercises/workouts done by an athlete.
#'
#' @param session A treinus_session object from [treinus_auth()]
#' @param athlete_id Integer ID of the athlete. If NULL, uses `TREINUS_ATHLETE_ID`
#'   environment variable.
#'
#' @return A tibble with exercise data including columns like `id_exercise`,
#'   `genre_name`, `start`, `distance`, `total_elapsed_time`, etc.
#'
#' @examples
#' \dontrun{
#' session <- treinus_auth()
#' exercises <- treinus_get_exercises(session, athlete_id = 50)
#'
#' # Or with environment variable set
#' Sys.setenv(TREINUS_ATHLETE_ID = "50")
#' exercises <- treinus_get_exercises(session)
#' }
#'
#' @export
treinus_get_exercises <- function(session, athlete_id = NULL) {
  if (!inherits(session, "treinus_session")) {
    cli::cli_abort("{.arg session} must be a treinus_session object from {.fn treinus_auth}")
  }

  athlete_id <- resolve_athlete_id(athlete_id)

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
#' @param .delay Numeric. Seconds to wait between API requests. Default 0.5.
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
treinus_get_exercises_batch <- function(session, athlete_ids, .progress = TRUE, .delay = 0.5) {
  if (!inherits(session, "treinus_session")) {
    cli::cli_abort("{.arg session} must be a treinus_session object from {.fn treinus_auth}")
  }

  n <- length(athlete_ids)
  results <- vector("list", n)

  if (.progress) cli::cli_progress_bar("Fetching athletes", total = n)

  for (i in seq_len(n)) {
    # Rate limit: delay between API calls (skip first)
    if (i > 1 && .delay > 0) {
      Sys.sleep(.delay)
    }

    results[[i]] <- tryCatch(
      treinus_get_exercises(session, athlete_id = athlete_ids[i]),
      error = function(e) {
        cli::cli_alert_warning("Failed to get exercises for athlete {athlete_ids[i]}: {e$message}")
        tibble::tibble()
      }
    )

    if (.progress) cli::cli_progress_update()
  }

  if (.progress) cli::cli_progress_done()

  dplyr::bind_rows(results)
}


#' Get detailed exercise analysis
#'
#' Retrieves detailed analysis data for one or more exercises, including
#' GPS records, heart rate, and other metrics.
#'
#' @param session A treinus_session object from [treinus_auth()]
#' @param exercise_id Integer ID(s) of the exercise(s). Can be a vector.
#' @param athlete_id Integer ID of the athlete (single value). If NULL, uses
#'   `TREINUS_ATHLETE_ID` environment variable.
#' @param team_id Integer ID of the team (single value). If NULL, uses the
#'   team_id from session (if set during auth) or `TREINUS_TEAM_ID` environment variable.
#' @param cache Logical. Cache results locally? Default TRUE.
#'   Cached data is stored in the user cache directory (see [treinus_cache_dir()]).
#' @param .progress Logical. Show progress bar for multiple exercises? Default TRUE.
#' @param .delay Numeric. Seconds to wait between API requests (for multiple exercises).
#'   Default 0.5. Set to 0 for no delay. Only applies to non-cached requests.
#'
#' @return For a single exercise_id, a list with exercise analysis data.
#'   For multiple exercise_ids, a named list of analysis results.
#'
#' @examples
#' \dontrun{
#' session <- treinus_auth()
#'
#' # Single exercise (with env vars set)
#' analysis <- treinus_get_exercise_analysis(session, exercise_id = 57)
#'
#' # Or explicit IDs
#' analysis <- treinus_get_exercise_analysis(
#'   session,
#'   exercise_id = 57,
#'   athlete_id = 50,
#'   team_id = 2994
#' )
#' records <- analysis$data$Analysis$Records
#'
#' # Multiple exercises
#' analyses <- treinus_get_exercise_analysis(session, exercise_id = c(57, 58, 59))
#' analyses[["57"]]$data$Analysis$Records
#' }
#'
#' @seealso [treinus_cache_dir()], [treinus_clear_cache()]
#' @export
treinus_get_exercise_analysis <- function(session, exercise_id, athlete_id = NULL, team_id = NULL,
                                          cache = TRUE, .progress = TRUE, .delay = 0.5) {
  if (!inherits(session, "treinus_session")) {
    cli::cli_abort("{.arg session} must be a treinus_session object from {.fn treinus_auth}")
  }

  athlete_id <- resolve_athlete_id(athlete_id)
  team_id <- resolve_team_id(team_id, session)

  if (length(athlete_id) != 1) {
    cli::cli_abort("{.arg athlete_id} must be a single value, not a vector.")
  }
  if (length(team_id) != 1) {
    cli::cli_abort("{.arg team_id} must be a single value, not a vector.")
  }

  # Single exercise - return directly
  if (length(exercise_id) == 1) {
    return(fetch_single_analysis(session, exercise_id, athlete_id, team_id, cache))
  }

  # Multiple exercises - return named list
  n <- length(exercise_id)
  results <- vector("list", n)
  names(results) <- as.character(exercise_id)

  if (.progress) cli::cli_progress_bar("Fetching exercises", total = n)

  last_was_api_call <- FALSE

  for (i in seq_len(n)) {
    ex_id <- exercise_id[i]

    # Rate limit: delay after previous API call
    if (last_was_api_call && .delay > 0) {
      Sys.sleep(.delay)
    }

    # Check cache first
    if (cache) {
      cache_file <- treinus_cache_path(team_id, athlete_id, ex_id)
      if (file.exists(cache_file)) {
        results[[i]] <- readRDS(cache_file)
        last_was_api_call <- FALSE
        if (.progress) cli::cli_progress_update()
        next
      }
    }

    # Fetch from API
    last_was_api_call <- TRUE
    results[[i]] <- tryCatch(
      fetch_single_analysis(session, ex_id, athlete_id, team_id, cache),
      error = function(e) {
        cli::cli_alert_warning("Failed exercise {ex_id}: {conditionMessage(e)}")
        NULL
      }
    )

    if (.progress) cli::cli_progress_update()
  }

  if (.progress) cli::cli_progress_done()

  results
}


#' Fetch a single exercise analysis (internal)
#' @keywords internal
fetch_single_analysis <- function(session, exercise_id, athlete_id, team_id, cache) {
  # Check cache first
  if (cache) {
    cache_file <- treinus_cache_path(team_id, athlete_id, exercise_id)
    if (file.exists(cache_file)) {
      return(readRDS(cache_file))
    }
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

  result <- httr2::resp_body_json(resp)

  # Check for API error
  if (isFALSE(result$success)) {
    cli::cli_abort(c(
      "x" = "Failed to get exercise analysis.",
      "i" = "API message: {result$message}",
      "i" = "exercise_id={exercise_id}, athlete_id={athlete_id}, team_id={team_id}"
    ))
  }

  # Save to cache only on success
  if (cache) {
    cache_file <- treinus_cache_path(team_id, athlete_id, exercise_id)
    dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, cache_file)
  }

  result
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
      timestamp = as.POSIXct(.data$timestamp, tz = tz),
      lat = .data$position_lat * semicircles_to_deg,
      lon = .data$position_long * semicircles_to_deg,
      distance_km = .data$distance * dist_to_km,
      speed_kmh = .data$speed * speed_to_kmh
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
  
  if (inherits(table, "xml_missing") || length(table) == 0) {
    cli::cli_warn("No table found with selector: {selector}")
    return(tibble::tibble())
  }
  
  table |>
    rvest::html_table() |>
    tibble::as_tibble()
}


# ID resolution helpers ---------------------------------------------------

#' Resolve athlete_id from argument or environment
#' @keywords internal
resolve_athlete_id <- function(athlete_id) {
  if (!is.null(athlete_id)) {
    return(as.integer(athlete_id))
  }

  env_id <- Sys.getenv("TREINUS_ATHLETE_ID", unset = "")
  if (env_id != "") {
    return(as.integer(env_id))
  }

  cli::cli_abort(c(
    "x" = "{.arg athlete_id} not provided.",
    "i" = "Set {.envvar TREINUS_ATHLETE_ID} environment variable or pass explicitly."
  ))
}


#' Resolve team_id from argument, session, or environment
#' @keywords internal
resolve_team_id <- function(team_id, session) {
  if (!is.null(team_id)) {
    return(as.integer(team_id))
  }

  # Try from session
  session_team <- attr(session, "team_id")
  if (!is.null(session_team)) {
    return(as.integer(session_team))
  }

  # Try from environment
  env_id <- Sys.getenv("TREINUS_TEAM_ID", unset = "")
  if (env_id != "") {
    return(as.integer(env_id))
  }

  cli::cli_abort(c(
    "x" = "{.arg team_id} not provided.",
    "i" = "Set {.envvar TREINUS_TEAM_ID} environment variable, or pass explicitly,",
    "i" = "or authenticate with team selection to store team_id in session."
  ))
}


# Cache functions ---------------------------------------------------------

#' Get the cache directory path
#'
#' Returns the path to the treinusr cache directory, following
#' platform-specific conventions via [tools::R_user_dir()].
#'
#' @return Character string with the cache directory path.
#'
#' @examples
#' treinus_cache_dir()
#'
#' @seealso [treinus_clear_cache()], [treinus_cache_info()]
#' @export
treinus_cache_dir <- function() {
  tools::R_user_dir("treinusr", "cache")
}


#' Get cache file path for an exercise
#'
#' @param team_id Team ID
#' @param athlete_id Athlete ID
#' @param exercise_id Exercise ID
#' @return Character string with cache file path
#' @keywords internal
treinus_cache_path <- function(team_id, athlete_id, exercise_id) {
  file.path(
    treinus_cache_dir(),
    sprintf("%s_%s_%s.rds", team_id, athlete_id, exercise_id)
  )
}


#' Clear the exercise analysis cache
#'
#' Removes cached exercise analysis data. Can clear all cached data or
#' specific exercises.
#'
#' @param team_id Optional team ID to filter
#' @param athlete_id Optional athlete ID to filter
#' @param exercise_id Optional exercise ID to clear specific exercise
#' @param all Logical. If TRUE, clear entire cache. Default FALSE.
#'
#' @return Invisibly returns the number of files deleted.
#'
#' @examples
#' \dontrun{
#' # Clear all cache
#' treinus_clear_cache(all = TRUE)
#'
#' # Clear specific exercise
#' treinus_clear_cache(team_id = 2994, athlete_id = 50, exercise_id = 151)
#'
#' # Clear all exercises for an athlete
#' treinus_clear_cache(team_id = 2994, athlete_id = 50)
#' }
#'
#' @seealso [treinus_cache_dir()], [treinus_cache_info()]
#' @export
treinus_clear_cache <- function(team_id = NULL, athlete_id = NULL, exercise_id = NULL, all = FALSE) {
  cache_dir <- treinus_cache_dir()

  if (!dir.exists(cache_dir)) {
    cli::cli_alert_info("Cache directory does not exist.")
    return(invisible(0L))
  }

  if (all) {
    files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  } else if (!is.null(exercise_id) && !is.null(athlete_id) && !is.null(team_id)) {
    # Specific exercise
    files <- treinus_cache_path(team_id, athlete_id, exercise_id)
    files <- files[file.exists(files)]
  } else {
    # Pattern match
    pattern <- paste0(
      if (!is.null(team_id)) team_id else "\\d+",
      "_",
      if (!is.null(athlete_id)) athlete_id else "\\d+",
      "_",
      if (!is.null(exercise_id)) exercise_id else "\\d+",
      "\\.rds$"
    )
    files <- list.files(cache_dir, pattern = pattern, full.names = TRUE)
  }

  if (length(files) == 0) {
    cli::cli_alert_info("No cached files match the criteria.")
    return(invisible(0L))
  }

  unlink(files)
  cli::cli_alert_success("Deleted {length(files)} cached file{?s}.")
  invisible(length(files))
}


#' Get cache information
#'
#' Returns information about cached exercise analysis data.
#'
#' @return A tibble with columns: team_id, athlete_id, exercise_id, size_kb, modified
#'
#' @examples
#' \dontrun{
#' treinus_cache_info()
#' }
#'
#' @seealso [treinus_cache_dir()], [treinus_clear_cache()]
#' @export
treinus_cache_info <- function() {
  cache_dir <- treinus_cache_dir()

  if (!dir.exists(cache_dir)) {
    return(tibble::tibble(
      team_id = integer(),
      athlete_id = integer(),
      exercise_id = integer(),
      size_kb = numeric(),
      modified = as.POSIXct(character())
    ))
  }

  files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)

  if (length(files) == 0) {
    return(tibble::tibble(
      team_id = integer(),
      athlete_id = integer(),
      exercise_id = integer(),
      size_kb = numeric(),
      modified = as.POSIXct(character())
    ))
  }

  info <- file.info(files)
  basenames <- basename(files)

  # Parse filenames: team_athlete_exercise.rds
  parts <- strsplit(sub("\\.rds$", "", basenames), "_")

  tibble::tibble(
    team_id = as.integer(vapply(parts, `[`, character(1), 1)),
    athlete_id = as.integer(vapply(parts, `[`, character(1), 2)),
    exercise_id = as.integer(vapply(parts, `[`, character(1), 3)),
    size_kb = round(info$size / 1024, 1),
    modified = info$mtime
  )
}
