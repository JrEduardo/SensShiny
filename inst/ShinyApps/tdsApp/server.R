##-------------------------------------------
## server.R
path <- system.file("ShinyApps", package = "SensShiny")

##======================================================================
## Elementos estáticos da interface

## For build the fields about consumer and sample for fill
InformationField0 <- fluidRow(
    column(
        width = 4,
        textInput(
            inputId = "CONSUMER",
            label = "Identificação do Consumidor",
            placeholder = "Insira sua identificação")
    ),
    column(
        width = 4,
        textInput(
            inputId = "SAMPLE",
            label = "Identificação da Amostra",
            placeholder = "Insira a identificação da amostra")
    ),
    column(
        width = 2,
        HTML('<label class="control-label"> Tempo</label><br>'),
        verbatimTextOutput("TNAME")
    ),
    column(
        width = 2,
        tagList(
            HTML('<label class="control-label"> Iniciar</label><br>'),
            actionButton(
                inputId = "DO",
                label = "",
                class = "btn btn-success",
                icon = icon("play")))
    )
)

InformationField1 <- fluidRow(
    column(
        width = 4,
        HTML(paste0('<label class="control-label">',
                    'Identificação do Consumidor</label><br>')),
        verbatimTextOutput("CNAME")),
    column(
        width = 4,
        HTML(paste0('<label class="control-label">',
                    'Identificação da Amostra</label><br>')),
        verbatimTextOutput("SNAME")),
    column(
        width = 2,
        HTML('<label class="control-label"> Tempo</label><br>'),
        verbatimTextOutput("TNAME")),
    column(
        width = 2,
        tagList(
            HTML('<label class="control-label"> Finalizar</label><br>'),
            actionButton(
                inputId = "DONE",
                label = "",
                class = "btn btn-danger",
                icon = icon("stop")))
    )
)

## For show time
TimeField <- column(
    width = 2,
    HTML('<label class="control-label"> Tempo</label><br>'),
    verbatimTextOutput("TNAME")
)

## Build help experiment
HelpField <- tagList(
    includeMarkdown(paste0(path, "/_mds/TDS.md"))
)

## AttributesField
atributos <- paste("attr", LETTERS[1:4])
shortattr <- paste0("attr", 1:length(atributos))

## Finish Page
FisinhField <- tagList(
    HTML("THANKS"),
    br(),
    fluidRow(
        column(
            width = 4,
            actionButton(
                inputId = "NEXT",
                label = "Próxima avaliação",
                icon = icon("arrow-right"))
        ),
        column(
            width = 4,
            downloadButton(
                outputId = "DOWNLOAD",
                label = "Download dados")
        ),
        column(
            width = 4,
            actionButton(
                inputId = "STOP",
                label = "Stop interface")
        )
    )
)

##======================================================================
## Instruções shiny para interface reativa

shinyServer(
    function(input, output, session) {

        ##-------------------------------------------
        ## Test area
        output$test <- renderPrint({
            ## print(autoInvalidate())
            print(reactiveValuesToList(va))
            cat("==============================\n", sep = "\n")
            print(reactiveValuesToList(da))
            cat("==============================\n", sep = "\n")
            va$shortattr[1]
            ## va$attributes[1]
        })

        ##-------------------------------------------
        ## Controle de exibição
        va <- reactiveValues(
            init = 0,
            time = Sys.time(),
            attributes = atributos,
            shortattr = shortattr
        )

        ##-------------------------------------------
        ## Salvar dados
        da <- reactiveValues(
            consumer = NULL,
            sample = NULL,
            time = NULL,
            attribute = NULL
        )

        ##-------------------------------------------
        ## Inicia o experimento ao pressionar 'Iniciar'
        observeEvent(input$DO, {
            va$init <- 1
            va$time <- Sys.time()
        })

        ##-------------------------------------------
        ## Finaliza o experimento ao pressionar 'Finalizar'
        observeEvent(input$DONE, {
            va$init <- 2
        })

        ##-------------------------------------------
        ## Reinicia avaliação
        observeEvent(input$NEXT, {
            va$init <- 0
            index <- sample(1:length(va$attributes))
            va$shortattr <- va$shortattr[index]
            va$attributes <- va$attributes[index]
        })

        ##-------------------------------------------
        ## Controle do tempo de experimento
        autoInvalidate <- reactiveTimer(1000)
        observe({
            dt <- difftime(autoInvalidate(), va$time, units = "secs")
            dt <- as.numeric(dt)
            if (dt > 180 & va$init == 1) {
                va$init <- 2
            }
        })

        ##-------------------------------------------
        ## Exibe as informações e controla tempo do experimento
        ##     - Nome/identificação do consumidor
        ##     - Nome/identificação da amostra
        ##     - Tempo passado do experimento
        output$CNAME <- renderText({
           input$CONSUMER
        })
        output$SNAME <- renderText({
            input$SAMPLE
        })
        output$TNAME <- renderText({
            ## Experiment time
            timer <- autoInvalidate()
            dt <- difftime(timer, isolate(va$time), units = "secs")
            ## Finish experiment if diff time is bigger than 180
            if (as.numeric(dt) > 180 & va$init == 1) {
                va$init <- 2
            }
            ## Show the experiment time
            off <- ifelse(isolate(va$init) == 0, 0, dt)
            format(.POSIXct(180 - off, tz = "GMT"), "%Mmin %Ssecs")
        })

        ##-------------------------------------------
        ## Salva os dados de tempos e atributos sentidos
        ## INSERT-REACTIVE-HERE ##
        observeEvent(input$attr1, {
            time <- as.numeric(
                difftime(Sys.time(), isolate(va$time), units = "secs"))
            da$sample <- c(isolate(da$sample), input$SAMPLE)
            da$consumer <- c(isolate(da$assesor), input$CONSUMER)
            da$attribute <- c(isolate(da$attribute), atributos[1])
            da$time <- c(isolate(da$time), time)
        })
        observeEvent(input$attr2, {
            time <- as.numeric(
                difftime(Sys.time(), isolate(va$time), units = "secs"))
            da$sample <- c(isolate(da$sample), input$SAMPLE)
            da$consumer <- c(isolate(da$assesor), input$CONSUMER)
            da$attribute <- c(isolate(da$attribute), atributos[2])
            da$time <- c(isolate(da$time), time)
        })
        observeEvent(input$attr3, {
            time <- as.numeric(
                difftime(Sys.time(), isolate(va$time), units = "secs"))
            da$sample <- c(isolate(da$sample), input$SAMPLE)
            da$consumer <- c(isolate(da$assesor), input$CONSUMER)
            da$attribute <- c(isolate(da$attribute), atributos[3])
            da$time <- c(isolate(da$time), time)
        })
        observeEvent(input$attr4, {
            time <- as.numeric(
                difftime(Sys.time(), isolate(va$time), units = "secs"))
            da$sample <- c(isolate(da$sample), input$SAMPLE)
            da$consumer <- c(isolate(da$assesor), input$CONSUMER)
            da$attribute <- c(isolate(da$attribute), atributos[4])
            da$time <- c(isolate(da$time), time)
        })

        ##-------------------------------------------
        ## Contrói toda a interface
        output$START <- renderUI({
            if (va$init == 0) {
                tagList(
                    HelpField,
                    hr(class = "white"),
                    InformationField0
                )
            } else if (va$init == 1) {
                tagList(
                    InformationField1,
                    HTML("<center>Lista de atributos</center>"),
                    br(),
                    ## INSERT-BUTTONS-ATTR-HERE ##
                    tagList(
                        column(width = 3,
                               actionButton(
                                   inputId = va$shortattr[1],
                                   label = va$attributes[1])
                               ),
                        column(width = 3,
                               actionButton(
                                   inputId = va$shortattr[2],
                                   label = va$attributes[2])
                               ),
                        column(width = 3,
                               actionButton(
                                   inputId = va$shortattr[3],
                                   label = va$attributes[3])
                               ),
                        column(width = 3,
                               actionButton(
                                   inputId = va$shortattr[4],
                                   label = va$attributes[4])
                               )
                    ),
                    br(),
                    hr(class = "white")
                )
            } else {
                FisinhField
            }
        })

        ##-------------------------------------------
        ## Rodapé
        output$FOOTER <- renderPrint({
            l1 <- paste("<code>SensShiny</code>: R Package version",
                        packageVersion("lattice"))
            l2 <- paste("URL: <a href=\"https://github.com/",
                        "JrEduardo/SensShiny\">",
                        "https://github.com/JrEduardo/SensShiny</a>")
            l3 <- paste("Contact: <a ",
                        "href=\"mailto:edujrrib@gmail.com\">",
                        "edujrrib@gmail.com</a>")
            cat("<div class=\"footer\">\n", l1, l2, l3, "<br/></div>",
                sep = "<br/>")
        })
    })
