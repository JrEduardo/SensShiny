#' @name tdsApp
#' @title Web Interface for a Temporal Dominance of Sensations
#'     Experiment
#' @param attributes A vector names representing the attributes that
#'     compose a sensory analysis.
#' @param max_time A numeric representing a max time, in seconds,
#'     allowed for each consumer.
#' @author Eduardo E. R. Junior <edujrrib@gmail.com>.
#' @description This function builds a consumer web interface for
#'     Temporal Dominance of Sensations experiment.
#' @return Open the web browser for show the shiny interface.
#' @importFrom utils capture.output
#' @export
tdsApp <- function(attributes, max_time = 60) {
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
    ## Create components that depend of the arguments
    REACTIVE <- lapply(1:length(attributes), function(i) {
        paste0(
            "observeEvent(input$attr", i, ", {
            time <- as.numeric(
                difftime(Sys.time(), isolate(va$time),
                         units = \"secs\"))
            da$sample <- c(isolate(da$sample), input$SAMPLE)
            da$consumer <- c(isolate(da$consumer), input$CONSUMER)
            da$attribute <- c(isolate(da$attribute),
                              attributes[", i, "])
            da$time <- c(isolate(da$time), time)
        })")
    })
    BUTTONSATTR <- lapply(1:length(attributes), function(i) {
        paste0(
            "column(width = 3,
                actionButton(
                    inputId = va$shortattr[", i, "],
                    label = va$attributes[", i, "])
               )")
    })
    REACTIVE <- do.call(paste, list(REACTIVE, collapse = "\n"))
    BUTTONSATTR <- do.call(paste, list(BUTTONSATTR, collapse = ",\n"))
    ##-------------------------------------------
    ## Replace this components into server
    attnames <- paste0(capture.output(dput(attributes)), collapse = "")
    server <- gsub("## INSERT-NAMES-ATTR ##", attnames, server)
    server <- gsub("## INSERT-MAX-TIME ##", max_time, server)
    server <- gsub("## INSERT-REACTIVE-HERE ##", REACTIVE, server)
    server <- gsub("## INSERT-BUTTONS-ATTR-HERE ##", BUTTONSATTR, server)
    ##-------------------------------------------
    ## Write the shiny components
    writeLines(server, paste0(shpath, "/server.R"))
    writeLines(    ui, paste0(shpath, "/ui.R"))
    ##-------------------------------------------
    ## Run the application
    shiny::runApp(shpath)
}
