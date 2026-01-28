# treinusr <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/yourusername/treinusr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yourusername/treinusr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**treinusr** provides authenticated access to the [Treinus](https://webapp.treinus.com.br) workout tracking platform API. Retrieve your training data, performance metrics, and workout history programmatically in R.

## Installation

You can install the development version of treinusr from GitHub:

```r
# install.packages("pak")
pak::pak("yourusername/treinusr")
```

## Setup

### 1. Configure Credentials

The recommended way to authenticate is by storing your credentials in environment variables:

```r
library(treinusr)

# Interactive setup (recommended - prompts for password securely)
treinus_set_credentials()

# Or provide directly
treinus_set_credentials(
  email = "your.email@example.com",
  password = "your-password"
)
```

This will save your credentials to `~/.Renviron`. Restart R or run `readRenviron("~/.Renviron")` to load them.

Alternatively, set environment variables manually in your `.Renviron`:

```
TREINUS_EMAIL=your.email@example.com
TREINUS_PASSWORD=your-password
```

### 2. Authenticate

Create an authenticated session:

```r
library(treinusr)

# Authenticate using credentials from environment variables
session <- treinus_auth()
```

## Usage

### Get Dashboard Data

```r
# Retrieve dashboard summary
dashboard <- treinus_get_dashboard(session)
```

### Get Workout Data

```r
# Get all workouts
workouts <- treinus_get_workouts(session)

# Get workouts for a specific date range
workouts <- treinus_get_workouts(
  session,
  start_date = "2024-01-01",
  end_date = "2024-01-31"
)
```

### Make Custom API Requests

For endpoints not yet wrapped, use the low-level request function:

```r
# Make a custom GET request
resp <- treinus_request(
  session,
  endpoint = "/Athlete/Performance/Index",
  method = "GET"
)

# Parse the response
page <- httr2::resp_body_html(resp)
```

### Session Management

```r
# Check if session is still valid
treinus_session_valid(session)

# Check current configuration
treinus_config()
```

## Authentication Details

Treinus uses ASP.NET WebForms authentication. The package handles:

- Extracting ViewState and ViewStateGenerator tokens
- Managing session cookies
- Handling ASP.NET-specific form postback requirements

All of this is managed automatically by `treinus_auth()`.

## Development Status

This package is in active development. Current features:

- ✅ Authentication with Treinus
- ✅ Session management
- ✅ Basic data retrieval
- 🚧 Parsing workout data (in progress)
- 🚧 Performance metrics extraction (planned)
- 🚧 Training plan management (planned)

## Workflow Integration

Combine with the tidyverse for data analysis:

```r
library(treinusr)
library(tidyverse)

# Authenticate
session <- treinus_auth()

# Get and analyze workout data
workouts <- treinus_get_workouts(session) |>
  # Add your analysis here
  filter(date >= "2024-01-01") |>
  mutate(distance_km = distance / 1000)
```

## Security

**Never commit credentials to version control!**

- Use environment variables via `.Renviron`
- Add `.Renviron` to your `.gitignore`
- Use `treinus_set_credentials()` for secure setup

## Related Projects

- [httr2](https://httr2.r-lib.org/) - Modern HTTP client for R
- [rvest](https://rvest.tidyverse.org/) - Web scraping tools

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.

## License

MIT © Eduardo Leoni
