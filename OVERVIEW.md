# treinusr Package Overview

## What is treinusr?

treinusr is an R package that provides authenticated access to the Treinus workout tracking platform API (https://webapp.treinus.com.br). It handles the complex ASP.NET WebForms authentication and provides a clean, modern R interface for retrieving workout data.

## Key Features

### Authentication
- Automatic handling of ASP.NET WebForms ViewState tokens
- Secure credential storage using environment variables
- Session cookie management
- Session validation and re-authentication

### API Access
- High-level functions for common tasks (workouts, dashboard)
- Low-level request function for custom endpoints
- HTML parsing utilities
- Error handling and informative messages

### Modern R Package Design
- Built with httr2 for HTTP requests
- rvest for HTML parsing
- cli for beautiful console output
- Follows tidyverse design principles
- Works seamlessly with both tidyverse and data.table

## Package Structure

```
treinusr/
├── R/                          # R source code
│   ├── auth.R                  # Authentication functions
│   ├── api.R                   # API wrapper functions
│   ├── utils.R                 # Utility functions
│   └── treinusr-package.R      # Package documentation
├── man/                        # Documentation (generated)
├── tests/                      # Unit tests
│   └── testthat/
│       └── test-auth.R
├── vignettes/                  # Long-form documentation
│   └── getting-started.Rmd
├── inst/                       # Additional files
│   └── examples/               # Example scripts
│       ├── basic-usage.R
│       └── data-table-analysis.R
├── DESCRIPTION                 # Package metadata
├── NAMESPACE                   # Exported functions
├── README.md                   # Package README
├── NEWS.md                     # Change log
├── LICENSE                     # MIT license
├── .Rbuildignore              # Build exclusions
├── .gitignore                 # Git exclusions
├── _pkgdown.yml               # Documentation site config
├── dev-setup.R                # Development helper
└── DEVELOPMENT.md             # Development guide
```

## Core Functions

### Authentication
- `treinus_auth()` - Create authenticated session
- `treinus_session_valid()` - Check session validity
- `treinus_set_credentials()` - Configure credentials
- `treinus_has_credentials()` - Check if credentials are set
- `treinus_config()` - View configuration

### Data Retrieval
- `treinus_get_workouts()` - Get workout data
- `treinus_get_dashboard()` - Get dashboard summary
- `treinus_request()` - Make custom API requests
- `treinus_parse_table()` - Parse HTML tables

## Technical Implementation

### ASP.NET WebForms Authentication Flow

1. **GET /Default.aspx**
   - Retrieve login page
   - Extract `__VIEWSTATE` and `__VIEWSTATEGENERATOR` tokens using rvest
   
2. **POST /Default.aspx**
   - Submit credentials with tokens
   - Form fields use ASP.NET naming convention:
     - `ctl00$ctl00$ctl00$FormContent$FormContent$body$LoginControl1$LoginUser$UserName`
     - `ctl00$ctl00$ctl00$FormContent$FormContent$body$LoginControl1$LoginUser$Password`
   - Handle session cookies automatically via httr2
   
3. **Verify Success**
   - Check for redirect to `/Global/DashBoard/Index`
   - Store authenticated session for reuse

### Session Management

- httr2 automatically handles session cookies
- Session object can be reused for multiple requests
- `treinus_session_valid()` checks if session is still active
- Re-authentication is transparent if session expires

### HTML Parsing

- Uses rvest to extract data from HTML pages
- Provides helper functions for common patterns
- Extensible for adding new parsers

## Usage Examples

### Basic Authentication

```r
library(treinusr)

# Set credentials (one-time setup)
treinus_set_credentials()

# Authenticate
session <- treinus_auth()
```

### Get Data

```r
# Dashboard summary
dashboard <- treinus_get_dashboard(session)

# Workout data
workouts <- treinus_get_workouts(session)

# Date-filtered workouts
jan_workouts <- treinus_get_workouts(
  session,
  start_date = "2024-01-01",
  end_date = "2024-01-31"
)
```

### Custom Requests

```r
# Access any endpoint
resp <- treinus_request(
  session,
  endpoint = "/Athlete/Performance/Index",
  method = "GET"
)

# Parse response
page <- httr2::resp_body_html(resp)
data <- treinus_parse_table(page)
```

### Tidyverse Integration

```r
library(tidyverse)

workouts |>
  mutate(distance_km = distance / 1000) |>
  filter(date >= "2024-01-01") |>
  group_by(sport_type) |>
  summarise(total_distance = sum(distance_km))
```

### data.table Integration

```r
library(data.table)

dt <- as.data.table(workouts)
dt[, .(total_distance = sum(distance)), by = sport_type]
```

## Development

### Setup Development Environment

```r
# Install development dependencies
source("dev-setup.R")

# Or manually
install.packages(c("devtools", "roxygen2", "testthat", "pkgdown"))
```

### Build and Check

```r
# Generate documentation
devtools::document()

# Load for testing
devtools::load_all()

# Run tests
devtools::test()

# Full package check
devtools::check()
```

### Add New Functions

1. Write function in appropriate R/ file
2. Add roxygen2 documentation
3. Export with `@export`
4. Run `devtools::document()`
5. Add tests
6. Update NEWS.md

## Best Practices

### Security
- Never commit credentials to version control
- Always use environment variables
- Use `treinus_set_credentials()` for setup
- Add `.Renviron` to `.gitignore`

### Performance
- Reuse session objects
- Don't re-authenticate for each request
- Check session validity before long operations

### Error Handling
- Wrap API calls in `tryCatch()` for production
- Check return values
- Validate data after retrieval

### Testing
- Set test credentials in environment
- Use `skip_if_not()` for tests requiring auth
- Test error conditions

## Future Enhancements

Potential additions for future versions:

1. **Data Parsing**
   - Complete workout data parser
   - Performance metrics extraction
   - Event/race data retrieval

2. **Write Operations**
   - Upload workouts
   - Update training plans
   - Manage equipment

3. **Analytics**
   - Built-in analysis functions
   - Performance trend analysis
   - Training load calculations

4. **Visualization**
   - ggplot2 visualization functions
   - Interactive dashboards
   - Training calendar views

5. **Caching**
   - Local data caching
   - Incremental updates
   - Offline mode

## Resources

- [R Packages Book](https://r-pkgs.org/)
- [httr2 Documentation](https://httr2.r-lib.org/)
- [rvest Documentation](https://rvest.tidyverse.org/)
- [Tidyverse Style Guide](https://style.tidyverse.org/)

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Update documentation
5. Submit a pull request

## License

MIT © Eduardo Leoni

## Acknowledgments

- Built with [httr2](https://httr2.r-lib.org/) by Hadley Wickham
- HTML parsing by [rvest](https://rvest.tidyverse.org/)
- Inspired by modern R package development practices
