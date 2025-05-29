options(repos = c(CRAN = "https://cloud.r-project.org"))

library(R6)

PackageManager <- R6Class("PackageManager",
  public = list(
    packages = NULL,
    initialize = function(packages) {
      self$packages <- packages
    },
    load_packages = function(update = FALSE) {
      if (update) {
        update.packages(ask = FALSE, oldPkgs = self$packages)
      }
      for (pkg in self$packages) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
          install.packages(pkg)
        }
        library(pkg, character.only = TRUE)
      }
    }
  )
)
