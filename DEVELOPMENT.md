# treinusr Package Development Quick Start

This guide will help you set up, build, and test the treinusr package.

## Prerequisites

Install required R packages:

```r
install.packages(c(
  "devtools",
  "roxygen2", 
  "testthat",
  "pkgdown",
  "usethis",
  "httr2",
  "rvest",
  "xml2",
  "cli",
  "rlang",
  "jsonlite",
  "glue"
))
```

## Initial Setup

1. **Generate Documentation**

```r
# From the package root directory
devtools::document()
```

This will:
- Generate NAMESPACE from roxygen2 comments
- Create .Rd files in man/ directory

2. **Load Package for Development**

```r
# Load package without installing
devtools::load_all()

# Now you can test functions
treinus_config()
```

## Building and Checking

### Quick Build

```r
# Generate documentation
devtools::document()

# Run tests
devtools::test()

# Load package
devtools::load_all()
```

### Full Package Check

```r
# Complete R CMD check
devtools::check()
```

This checks for:
- Documentation completeness
- Code syntax and style
- Example code execution
- Test coverage
- Package structure

### Install Package Locally

```r
# Install from source
devtools::install()

# Or build and install
devtools::build()
devtools::install_local("treinusr_0.1.0.tar.gz")
```

## Testing

### Run All Tests

```r
devtools::test()
```

### Run Specific Test File

```r
testthat::test_file("tests/testthat/test-auth.R")
```

### Test with Coverage

```r
covr::package_coverage()
```

## Documentation

### Build Documentation Website

```r
pkgdown::build_site()
```

This creates a documentation website in `docs/` that you can:
- View locally by opening `docs/index.html`
- Deploy to GitHub Pages
- Host anywhere

### Preview Single Help Page

```r
?treinus_auth
```

## Development Workflow

### Typical Development Cycle

```r
# 1. Make changes to R files in R/

# 2. Generate documentation
devtools::document()

# 3. Load package
devtools::load_all()

# 4. Test interactively
session <- treinus_auth()

# 5. Run formal tests
devtools::test()

# 6. Check package
devtools::check()
```

### Adding New Functions

1. Create function in appropriate R/ file
2. Add roxygen2 documentation above function:

```r
#' Function title
#'
#' Function description
#'
#' @param arg1 Description of arg1
#' @return Description of return value
#' @export
#' @examples
#' \dontrun{
#' example_code()
#' }
my_function <- function(arg1) {
  # implementation
}
```

3. Run `devtools::document()` to update NAMESPACE
4. Add tests in tests/testthat/test-*.R

## Common Tasks

### Update Documentation

```r
# After modifying roxygen comments
devtools::document()
```

### Run Specific Examples

```r
# Run example from documentation
example(treinus_auth)
```

### Check Code Style

```r
library(styler)
styler::style_pkg()

library(lintr)
lintr::lint_package()
```

### Update NEWS.md

After making changes, document them in NEWS.md:

```markdown
## treinusr 0.1.1

* Fixed bug in authentication
* Added new function `treinus_get_events()`
```

## Debugging

### Debug Functions

```r
# Set breakpoint in function
debugonce(treinus_auth)

# Call function - execution will pause at breakpoint
session <- treinus_auth()

# Use debugger commands:
# n - next line
# s - step into function
# c - continue
# Q - quit debugger
```

### View Function Source

```r
# View function source
View(treinus_auth)

# Or in console
treinus_auth
```

### Check Package State

```r
# Check loaded package version
packageVersion("treinusr")

# View package search path
search()

# Detach package
detach("package:treinusr", unload = TRUE)
```

## Troubleshooting

### "object not found" after devtools::load_all()

Run `devtools::document()` first to regenerate NAMESPACE

### Tests failing

Make sure credentials are set:

```r
Sys.setenv(
  TREINUS_EMAIL = "your.email@example.com",
  TREINUS_PASSWORD = "your-password"
)
```

### Documentation not updating

Clear cache and rebuild:

```r
devtools::clean_dll()
devtools::document()
devtools::load_all()
```

## Publishing

### GitHub

```bash
git init
git add .
git commit -m "Initial commit"
git remote add origin https://github.com/yourusername/treinusr.git
git push -u origin main
```

### Install from GitHub

```r
# Others can install with:
pak::pak("yourusername/treinusr")

# Or
devtools::install_github("yourusername/treinusr")
```

## Resources

- [R Packages Book](https://r-pkgs.org/) by Hadley Wickham & Jenny Bryan
- [httr2 Documentation](https://httr2.r-lib.org/)
- [rvest Documentation](https://rvest.tidyverse.org/)
- [roxygen2 Documentation](https://roxygen2.r-lib.org/)
- [testthat Documentation](https://testthat.r-lib.org/)
