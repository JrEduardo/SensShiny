##-------------------------------------------
## ui.R

path <- system.file("ShinyApps", package = "SensShiny")
shinyUI(
    fluidPage(
        title = "Sensometrics",
        includeCSS(paste0(path, "/_css/general.css")),
        h1("Temporal Dominance of Sensations Interface",
           class = "title"),
        h3(paste("SensShiny Package - Implemented",
                 "by Eduardo E. R. Junior"),
           class = "author"),
        hr(),
        hr(class = "white"),
        verbatimTextOutput("test"),
        uiOutput("START"),
        htmlOutput("FOOTER")
    )
)
