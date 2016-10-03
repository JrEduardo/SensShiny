#' @name tdsApp
#' @title Web Interface for a Temporal Dominance of Sensations
#'     Experiment
#' @author Eduardo E. R. Junior <edujrrib@gmail.com>.
#' @description This function builds a consumer web interface for
#'     Temporal Dominance of Sensations experiment.
#' @return Open the web browser for show the shiny interface.
#'
tdsApp <- function() {
    ##-------------------------------------------
    ## Paths
    pcpath <- system.file("ShinyApps", "tdsApp", package = "SensShiny")
    shpath <- tempdir()
    wdpath <- getwd()
    if (pcpath == "") {
        stop("Application not found. Try reinstall package.")
    }
    ##-------------------------------------------
    ## Read the shiny components
    server <- readLines(paste0(pcpath, "/server.R"))
    ui     <- readLines(paste0(pcpath, "/ui.R"))
    ##-------------------------------------------
    ## Write the shiny components
    writeLines(server, paste0(shpath, "/server.R"))
    writeLines(    ui, paste0(shpath, "/ui.R"))
    ##-------------------------------------------
    ## Run the application
    shiny::runApp(shpath)
}
