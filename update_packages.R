#!/usr/bin/env Rscript

library(tools)
library(jsonlite)

get_all_dependencies <- function(package_name) {
  av_pkgs <- available.packages()

  if (!package_name %in% rownames(av_pkgs)) {
    stop("Package '", package_name, "' not found in available packages")
  }

  pkg_info <- av_pkgs[package_name, ]

  depends <- pkg_info["Depends"]
  imports <- pkg_info["Imports"]
  linking_to <- pkg_info["LinkingTo"]
  suggests <- pkg_info["Suggests"]

  all_deps <- c()

  if (!is.na(depends) && depends != "") {
    deps_parsed <- parse_dep_string(depends)
    all_deps <- c(all_deps, deps_parsed)
  }

  if (!is.na(imports) && imports != "") {
    imports_parsed <- parse_dep_string(imports)
    all_deps <- c(all_deps, imports_parsed)
  }

  if (!is.na(linking_to) && linking_to != "") {
    linking_parsed <- parse_dep_string(linking_to)
    all_deps <- c(all_deps, linking_parsed)
  }

  if (!is.na(suggests) && suggests != "") {
    suggests_parsed <- parse_dep_string(suggests)
    all_deps <- c(all_deps, suggests_parsed)
  }

  all_deps <- unique(all_deps)
  all_deps <- all_deps[all_deps != "R"]

  return(all_deps)
}

parse_dep_string <- function(dep_string) {
  if (is.na(dep_string) || dep_string == "") {
    return(character(0))
  }

  deps <- strsplit(dep_string, ",")[[1]]
  deps <- trimws(deps)

  pkg_names <- character(length(deps))
  for (i in seq_along(deps)) {
    pkg_name <- gsub("\\s*\\([^)]*\\)", "", deps[i])
    pkg_name <- trimws(pkg_name)
    pkg_names[i] <- pkg_name
  }

  return(pkg_names[pkg_names != ""])
}

get_github_url <- function(package_name) {
  tryCatch({
    db <- packageDescription(package_name)

    if (inherits(db, "try-error") || is.null(db)) {
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

    return(NA)
  }, error = function(e) {
    cat("Error processing package", package_name, ":", conditionMessage(e), "\n")
    return(NA)
  })
}

main <- function() {
  target_package <- "hms"
  json_file <- "packages.json"

  cat("Finding all dependencies (strong + suggested) of package:", target_package, "\n")

  dependencies <- get_all_dependencies(target_package)
  dependencies <- sort(dependencies)
  cat("Found", length(dependencies), "dependencies:\n")
  cat(paste(dependencies, collapse = ", "), "\n\n")

  packages_data <- list()

  for (pkg in dependencies) {
    cat("Processing package:", pkg, "\n")
    github_url <- get_github_url(pkg)

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

  if (length(packages_data) > 0) {
    json_content <- toJSON(packages_data, pretty = TRUE, auto_unbox = TRUE)
    writeLines(json_content, json_file)
    cat("\nUpdated", json_file, "with", length(packages_data), "packages\n")
  } else {
    cat("\nNo packages with GitHub URLs found\n")
  }
}

if (!interactive()) {
  main()
}