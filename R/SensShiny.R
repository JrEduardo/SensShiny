#' @name SensShiny
#' @title Shiny solutions for Sensory Analysis and Consumer Tests
#' @docType package
#' @description The SensShiny package contains wrappers functions for
#'     building web interfaces, implemented in shiny, to collect data
#'     from sensory and consumer experiments.
#'
NULL

## @name .onAttach
## @title Message on Attach Package
## @description Function to show welcome message when package is
##     attached.
## @author Eduardo E. R. Jr <edujrrib@gmail.com>
.onAttach <- function(libname, pkgname) {
    pkg.info <- drop(read.dcf(
        file = system.file("DESCRIPTION", package = "SensShiny"),
        fields = c("Package", "Title", "Version", "Date", "URL")
    ))
    dashes <- paste0(rep("----------", times = 7), collapse = "")
    packageStartupMessage(
        paste0(dashes, "\n  ",
               pkg.info["Package"], ": ", pkg.info["Title"], "\n  ",
               "version ", pkg.info["Version"], " (build on ",
               pkg.info["Date"], ") is now loaded.\n\n  ",
               "For support, collaboration or bug report, visit: \n    ",
               pkg.info["URL"], "\n",
               dashes)
    )
}
