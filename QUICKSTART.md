# Getting Started with treinusr

## Quick Start (5 minutes)

### 1. Install Dependencies

Open R and run:

```r
install.packages(c(
  "devtools",
  "httr2",
  "rvest",
  "xml2",
  "cli",
  "rlang",
  "jsonlite",
  "glue"
))
```

### 2. Load and Build the Package

```r
# Set working directory to package root
setwd("path/to/treinusr")

# Generate documentation
devtools::document()

# Load package
devtools::load_all()
```

### 3. Configure Credentials

```r
# Interactive setup (recommended)
treinus_set_credentials()

# Or manually edit ~/.Renviron:
# TREINUS_EMAIL=your.email@example.com
# TREINUS_PASSWORD=your-password
```

### 4. Test Authentication

```r
# Authenticate
session <- treinus_auth()

# Should see: "✓ Successfully authenticated with Treinus!"

# Verify session
treinus_session_valid(session)
# Should return: TRUE
```

### 5. Get Data

```r
# Get dashboard data
dashboard <- treinus_get_dashboard(session)

# Get workouts
workouts <- treinus_get_workouts(session)
```

## What's Included

### Core Files
- `R/auth.R` - Authentication with ASP.NET WebForms
- `R/api.R` - API wrapper functions  
- `R/utils.R` - Credential management
- `R/treinusr-package.R` - Package documentation

### Documentation
- `README.md` - Package overview
- `OVERVIEW.md` - Technical details
- `DEVELOPMENT.md` - Development guide
- `vignettes/getting-started.Rmd` - Getting started guide

### Examples
- `inst/examples/basic-usage.R` - Basic workflow
- `inst/examples/data-table-analysis.R` - Advanced analysis with data.table

### Development Tools
- `dev-setup.R` - Automated setup script
- `tests/testthat/` - Unit tests
- `_pkgdown.yml` - Documentation website config

## Key Features

### ✅ Automated Authentication
- Handles ASP.NET ViewState extraction
- Manages session cookies automatically
- Validates sessions

### ✅ Modern R Design
- Uses httr2 for HTTP requests
- rvest for HTML parsing
- Follows tidyverse conventions
- Works with data.table

### ✅ Secure Credentials
- Environment variable storage
- Helper function for setup
- Never commits secrets

### ✅ Comprehensive Documentation
- Roxygen2 documentation
- Vignettes
- Working examples
- Development guides

## Next Steps

1. **Test the package**:
   ```r
   devtools::test()
   ```

2. **Build documentation site**:
   ```r
   pkgdown::build_site()
   ```

3. **Install locally**:
   ```r
   devtools::install()
   ```

4. **Explore examples**:
   ```r
   source("inst/examples/basic-usage.R")
   ```

## Common Commands

```r
# Development workflow
devtools::load_all()    # Load package
devtools::document()    # Update documentation
devtools::test()        # Run tests
devtools::check()       # Full package check

# Using the package
session <- treinus_auth()
workouts <- treinus_get_workouts(session)
dashboard <- treinus_get_dashboard(session)

# Custom requests
resp <- treinus_request(session, "/Athlete/Performance/Index")
```

## Need Help?

See these files for more information:
- `README.md` - Usage examples
- `DEVELOPMENT.md` - Development workflow
- `OVERVIEW.md` - Technical implementation details
- `vignettes/getting-started.Rmd` - Complete tutorial

## Architecture

```
User Request
     ↓
treinus_auth() - Extracts ViewState tokens using rvest
     ↓           Posts credentials with tokens using httr2
Session Object - Contains authenticated cookies
     ↓
API Functions - Use session for authenticated requests
     ↓
Data Parsing - Extract and structure data with rvest
     ↓
Return Data - Clean tibbles/lists ready for analysis
```

Enjoy using treinusr! 🏃‍♂️📊
