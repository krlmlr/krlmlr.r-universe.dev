#!/usr/bin/env Rscript

library(jsonlite)

get_all_dependencies <- function(package_name) {
  deps1 <- tools::package_dependencies(package_name, recursive = FALSE, which = c("Depends", "Imports", "Suggests"))
  deps <- tools::package_dependencies(unique(unlist(deps1)), recursive = TRUE, which = c("Depends", "Imports"))
  sort(unique(unlist(deps)))
}

get_github_url <- function(package_name) {
  tryCatch({
    db <- packageDescription(package_name)

    if (inherits(db, "try-error") || is.null(db)) {
      return(NA)
    }

    if (identical(db[["Priority"]], "base")) {
      return(NA)
    }

    url_field <- db[["URL"]]
    bug_reports_field <- db[["BugReports"]]

    github_url <- NA

    if (!is.null(url_field) && !is.na(url_field) && url_field != "") {
      urls <- strsplit(url_field, "[,\n]")[[1]]
      urls <- trimws(urls)

      github_urls <- grep("github\\.com", urls, value = TRUE)
      if (length(github_urls) > 0) {
        github_url <- github_urls[1]
        github_url <- gsub("/$", "", github_url)
        github_url <- gsub("#.*$", "", github_url)
        return(github_url)
      }
    }

    if (!is.null(bug_reports_field) && !is.na(bug_reports_field) && bug_reports_field != "") {
      if (grepl("github\\.com", bug_reports_field)) {
        github_url <- gsub("/issues.*$", "", bug_reports_field)
        github_url <- gsub("/$", "", github_url)
        return(github_url)
      }
    }

    return(paste0("https://github.com/cran/", package_name))
  }, error = function(e) {
    cat("Error processing package", package_name, ":", conditionMessage(e), "\n")
    return(NA)
  })
}

target_package <- c("hms", "tibble", "blob", "RSQLite", "duckdb", "RMariaDB", "RPostgres")
json_file <- "packages.json"

cat("Finding all dependencies (strong + suggested) of package:", target_package, "\n")

dependencies <- get_all_dependencies(target_package)
cat("Found", length(dependencies), "dependencies:\n")
cat(paste(dependencies, collapse = ", "), "\n\n")

packages_data <- list()

for (pkg in dependencies) {
  cat("Processing package:", pkg, "\n")
  github_url <- get_github_url(pkg)

  stopifnot(pkg != "duckdb")

  if (!is.na(github_url)) {
    packages_data[[length(packages_data) + 1]] <- list(
      package = pkg,
      url = github_url
    )
    cat("  Found GitHub URL:", github_url, "\n")
  } else {
    cat("  No GitHub URL found\n")
  }
}

json_content <- toJSON(packages_data, pretty = TRUE, auto_unbox = TRUE)
writeLines(json_content, json_file)
cat("\nUpdated", json_file, "with", length(packages_data), "packages\n")
