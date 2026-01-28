#' Authenticate with Treinus
#'
#' Creates an authenticated session with the Treinus webapp. This function
#' handles the ASP.NET WebForms authentication flow, including extracting
#' ViewState tokens and managing session cookies.
#'
#' @param email Character string with your Treinus email/username.
#'   Can also be set via `TREINUS_EMAIL` environment variable.
#' @param password Character string with your Treinus password.
#'   Can also be set via `TREINUS_PASSWORD` environment variable.
#' @param team Team ID (numeric) or team name (character) to select if the user
#'   belongs to multiple teams. Can also be set via `TREINUS_TEAM` environment
#'   variable. If NULL and user has multiple teams, uses the first (default) team.
#' @param remember_me Logical. Keep session alive for future requests.
#'   Default is TRUE.
#' @param base_url Character string with the base URL for Treinus.
#'   Default is "https://webapp.treinus.com.br".
#'
#' @return An httr2 request object with authenticated session cookies that
#'   can be used for subsequent API calls.
#'
#' @examples
#' \dontrun{
#' # Authenticate using environment variables
#' session <- treinus_auth()
#'
#' # Or provide credentials directly (not recommended for scripts)
#' session <- treinus_auth(
#'   email = "your.email@example.com",
#'   password = "your-password"
#' )
#'
#' # If you belong to multiple teams, specify which one to use
#' session <- treinus_auth(team = "My Team Name")
#' # Or by team ID
#' session <- treinus_auth(team = 600)
#' }
#'
#' @export
treinus_auth <- function(
  email = NULL,
  password = NULL,
  team = NULL,
  remember_me = TRUE,
  base_url = "https://webapp.treinus.com.br"
) {
  # Get credentials from environment if not provided
  email <- email %||% Sys.getenv("TREINUS_EMAIL")
  password <- password %||% Sys.getenv("TREINUS_PASSWORD")
  team <- team %||% Sys.getenv("TREINUS_TEAM", unset = NA)

  # Validate credentials
  if (email == "" || password == "") {
    cli::cli_abort(c(
      "!" = "Credentials not provided.",
      "i" = "Set {.envvar TREINUS_EMAIL} and {.envvar TREINUS_PASSWORD} environment variables,",
      "i" = "or provide {.arg email} and {.arg password} arguments."
    ))
  }

  cli::cli_progress_step("Authenticating with Treinus...")

  # Step 1: Get the login page to extract ViewState tokens
  login_url <- paste0(base_url, "/Default.aspx")

  initial_req <- httr2::request(login_url) |>
    httr2::req_user_agent("treinusr R package (httr2)")

  initial_resp <- initial_req |>
    httr2::req_perform()

  # Extract ViewState tokens using rvest
  login_page <- httr2::resp_body_html(initial_resp)

  viewstate <- login_page |>
    rvest::html_element("input[name='__VIEWSTATE']") |>
    rvest::html_attr("value")

  viewstate_generator <- login_page |>
    rvest::html_element("input[name='__VIEWSTATEGENERATOR']") |>
    rvest::html_attr("value")

  if (is.na(viewstate) || is.na(viewstate_generator)) {
    cli::cli_abort(c(
      "x" = "Failed to extract ViewState tokens from login page.",
      "i" = "The page structure may have changed."
    ))
  }

  # Step 2: Build the login POST request
  login_body <- list(
    `__EVENTTARGET` = "",
    `__EVENTARGUMENT` = "",
    `__VIEWSTATE` = viewstate,
    `__VIEWSTATEGENERATOR` = viewstate_generator,
    `ctl00$ctl00$ctl00$FormContent$FormContent$body$LoginControl1$LoginUser$UserName` = email,
    `ctl00$ctl00$ctl00$FormContent$FormContent$body$LoginControl1$LoginUser$Password` = password,
    `ctl00$ctl00$ctl00$FormContent$FormContent$body$LoginControl1$LoginUser$RememberMe` = if (
      remember_me
    ) {
      "C"
    } else {
      "U"
    },
    `ctl00$ctl00$ctl00$FormContent$FormContent$body$LoginControl1$LoginUser$LoginButton` = "Entrar"
  )

  # Step 3: Perform login (don't follow redirects to capture cookies)
  login_resp <- httr2::request(login_url) |>
    httr2::req_body_form(!!!login_body) |>
    httr2::req_method("POST") |>
    httr2::req_options(followlocation = FALSE) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  # Check if login was successful (expect 302 redirect on success)
  status <- httr2::resp_status(login_resp)
  if (status >= 400) {
    cli::cli_abort(c(
      "x" = "Login failed with status {status}.",
      "i" = "Check your credentials."
    ))
  }

  # Extract cookies from the login response (set on 302 redirect)
  set_cookie_headers <- httr2::resp_headers(login_resp, "set-cookie")

  if (is.null(set_cookie_headers) || length(set_cookie_headers) == 0) {
    cli::cli_abort(c(
      "x" = "No session cookies received from login.",
      "i" = "Authentication may have failed silently."
    ))
  }

  # Parse cookies into name=value format for the Cookie header
  cookie_values <- vapply(
    set_cookie_headers,
    function(cookie) {
      strsplit(cookie, ";")[[1]][1]
    },
    character(1),
    USE.NAMES = FALSE
  )
  cookie_string <- paste(cookie_values, collapse = "; ")

  # Get redirect location and follow it
  redirect_url <- httr2::resp_header(login_resp, "location")
  if (is.null(redirect_url)) {
    cli::cli_abort("Login did not redirect. Check credentials.")
  }

  # Handle relative URLs
 if (!grepl("^https?://", redirect_url)) {
    redirect_url <- paste0(base_url, "/", sub("^/", "", redirect_url))
  }

  # Follow the redirect with cookies
  redirect_resp <- httr2::request(redirect_url) |>
    httr2::req_headers(Cookie = cookie_string) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  final_url <- redirect_resp$url

  # Check if we were redirected to team selection page
  if (grepl("LoginSelectTeam", final_url, ignore.case = TRUE)) {
    team_result <- handle_team_selection(
      redirect_resp,
      team,
      base_url,
      cookie_string
    )
    redirect_resp <- team_result$response
    cookie_string <- team_result$cookies
    final_url <- redirect_resp$url
  }

  # Check if we were redirected to dashboard (successful login)
  if (!grepl("Dashboard|Private", final_url, ignore.case = TRUE)) {
    # Try to extract error message from the response
    error_page <- httr2::resp_body_html(redirect_resp)
    error_msg <- error_page |>
      rvest::html_element(".error, .validation-summary-errors") |>
      rvest::html_text2()

    if (!is.na(error_msg)) {
      cli::cli_abort(c(
        "x" = "Login failed: {error_msg}"
      ))
    } else {
      cli::cli_abort(c(
        "x" = "Login failed.",
        "i" = "Check your credentials."
      ))
    }
  }

  cli::cli_progress_done()

  cli::cli_alert_success("Successfully authenticated with Treinus!")

  # Create a base request object with session cookies

  authenticated_req <- httr2::request(base_url) |>
    httr2::req_user_agent("treinusr R package (httr2)") |>
    httr2::req_headers(Cookie = cookie_string)

  structure(
    authenticated_req,
    class = c("treinus_session", class(authenticated_req)),
    login_response = login_resp,
    base_url = base_url,
    cookies = cookie_string
  )
}


#' Check if a Treinus session is still valid
#'
#' @param session A treinus_session object from [treinus_auth()]
#'
#' @return Logical indicating if the session is still valid
#' @export
treinus_session_valid <- function(session) {
  if (!inherits(session, "treinus_session")) {
    return(FALSE)
  }

  # Try to access a protected page
  test_url <- paste0(attr(session, "base_url"), "/Global/DashBoard/Index")

  result <- tryCatch(
    {
      resp <- session |>
        httr2::req_url(test_url) |>
        httr2::req_perform()

      # Check if we got redirected to login
      !grepl("Default\\.aspx", resp$url, ignore.case = TRUE)
    },
    error = function(e) FALSE
  )

  result
}


#' Handle team selection page
#'
#' @param login_resp The response from the login page that redirected to team selection
#' @param team Team ID or name to select (NULL for default/first team)
#' @param base_url Base URL for Treinus
#' @param cookies Cookie string from the login response
#' @return A list with `response` and `cookies`
#' @keywords internal
handle_team_selection <- function(login_resp, team, base_url, cookies) {
  cli::cli_progress_step("Selecting team...")

  team_page <- httr2::resp_body_html(login_resp)

  # Extract ViewState tokens

  viewstate <- team_page |>
    rvest::html_element("input[name='__VIEWSTATE']") |>
    rvest::html_attr("value")

  viewstate_generator <- team_page |>
    rvest::html_element("input[name='__VIEWSTATEGENERATOR']") |>
    rvest::html_attr("value")

  # Extract available teams (radio buttons)
  team_radios <- team_page |>
    rvest::html_elements(
      "input[name='ctl00$ctl00$ctl00$FormContent$FormContent$body$rblTeam']"
    )

  team_ids <- team_radios |> rvest::html_attr("value")
  team_labels <- team_page |>
    rvest::html_elements(
      "label[for^='FormContent_FormContent_body_rblTeam']"
    ) |>
    rvest::html_text2()

  if (length(team_ids) == 0) {
    cli::cli_abort("No teams found on team selection page.")
  }

  # Determine which team to select
  selected_team_id <- NULL

  if (is.null(team) || is.na(team) || team == "") {
    # Use the first (default) team
    selected_team_id <- team_ids[1]
    cli::cli_alert_info("Using default team: {team_labels[1]}")
  } else if (is.numeric(team) || grepl("^[0-9]+$", team)) {
    # Team specified by ID
    team_id_str <- as.character(team)
    if (team_id_str %in% team_ids) {
      selected_team_id <- team_id_str
      idx <- which(team_ids == team_id_str)
      cli::cli_alert_info("Selecting team: {team_labels[idx]}")
    } else {
      cli::cli_abort(c(
        "x" = "Team ID {team} not found.",
        "i" = "Available teams: {paste(team_ids, team_labels, sep = ' - ', collapse = ', ')}"
      ))
    }
  } else {
    # Team specified by name (partial match, case-insensitive)
    matches <- grep(team, team_labels, ignore.case = TRUE)
    if (length(matches) == 1) {
      selected_team_id <- team_ids[matches]
      cli::cli_alert_info("Selecting team: {team_labels[matches]}")
    } else if (length(matches) > 1) {
      cli::cli_abort(c(
        "x" = "Multiple teams match '{team}'.",
        "i" = "Matching teams: {paste(team_labels[matches], collapse = ', ')}",
        "i" = "Use the team ID for exact selection."
      ))
    } else {
      cli::cli_abort(c(
        "x" = "No team matches '{team}'.",
        "i" = "Available teams: {paste(team_ids, team_labels, sep = ' - ', collapse = ', ')}"
      ))
    }
  }

  # Build team selection POST request
  team_select_url <- paste0(base_url, "/LoginSelectTeam.aspx")

  team_body <- list(
    `__EVENTTARGET` = "",
    `__EVENTARGUMENT` = "",
    `__VIEWSTATE` = viewstate,
    `ctl00$ctl00$ctl00$FormContent$FormContent$body$rblTeam` = selected_team_id,
    `ctl00$ctl00$ctl00$FormContent$FormContent$body$LoginButton` = "Entrar",
    `__VIEWSTATEGENERATOR` = viewstate_generator
  )

  team_resp <- httr2::request(team_select_url) |>
    httr2::req_headers(
      Cookie = cookies,
      Referer = team_select_url,
      Origin = base_url
    ) |>
    httr2::req_body_form(!!!team_body) |>
    httr2::req_method("POST") |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  # Merge cookies: start with original cookies, then add/update with new ones
  new_cookie_headers <- httr2::resp_headers(team_resp, "set-cookie")
  if (!is.null(new_cookie_headers) && length(new_cookie_headers) > 0) {
    # Parse existing cookies into a named list
    existing_pairs <- strsplit(cookies, "; ")[[1]]
    cookie_list <- stats::setNames(
      vapply(existing_pairs, function(p) sub("^[^=]+=", "", p), character(1)),
      vapply(existing_pairs, function(p) sub("=.*", "", p), character(1))
    )
    # Add/update with new cookies
    for (cookie in new_cookie_headers) {
      pair <- strsplit(cookie, ";")[[1]][1]
      name <- sub("=.*", "", pair)
      value <- sub("^[^=]+=", "", pair)
      cookie_list[[name]] <- value
    }
    # Rebuild cookie string
    cookies <- paste(
      names(cookie_list),
      cookie_list,
      sep = "=",
      collapse = "; "
    )
  }

  cli::cli_progress_done()
  list(response = team_resp, cookies = cookies)
}

