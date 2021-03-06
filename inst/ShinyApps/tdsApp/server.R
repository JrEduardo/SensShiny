##-------------------------------------------
## server.R
path <- system.file("ShinyApps", package = "SensShiny")

## Arguments
attributes <- ## INSERT-NAMES-ATTR ##
attributes <- sample(attributes)
shortattr <- paste0("attr", 1:length(attributes))
max_time <- ## INSERT-MAX-TIME ##

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
mdhelp <- includeMarkdown(paste0(path, "/_mds/TDS.md"))
HelpField <- gsub("## MAX-TIME ##", max_time, mdhelp)

## Finish Page
FisinhField <- tagList(
    h2("OBRIGADO PELA SUA CONTRIBUIÇÃO!"),
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

        ## ##-------------------------------------------
        ## ## Test area
        ## output$test <- renderPrint({
        ##     ## print(autoInvalidate())
        ##     print(reactiveValuesToList(va))
        ##     cat("==============================\n", sep = "\n")
        ##     print(reactiveValuesToList(da))
        ##     cat("==============================\n", sep = "\n")
        ##     as.data.frame(reactiveValuesToList(da))
        ## })

        ##-------------------------------------------
        ## Controle de exibição
        va <- reactiveValues(
            init = 0,
            time = Sys.time(),
            attributes = attributes,
            shortattr = shortattr
        )

        ##-------------------------------------------
        ## Salvar dados
        da <- reactiveValues(
            consumer = NULL,
            sample = NULL,
            time = NULL,
            attribute = NULL,
            general = NULL
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
        ## Interrompe o processo R da interface
        observeEvent(input$STOP, {
            stopApp()
        })

        ##-------------------------------------------
        ## Controle do tempo de experimento
        autoInvalidate <- reactiveTimer(1000)
        observe({
            dt <- difftime(autoInvalidate(), va$time, units = "secs")
            dt <- as.numeric(dt)
            if (dt > max_time & va$init == 1) {
                va$init <- 2
            }
        })

        ##-------------------------------------------
        ## Nota final
        observeEvent(input$CONFIRM, {
            index <- is.na(da$general)
            da$general[index] <- input$GRADE
            va$init <- 3
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
            ## Finish experiment if diff time is bigger than max_time
            if (as.numeric(dt) > max_time & va$init == 1) {
                va$init <- 2
            }
            ## Show the experiment time
            off <- ifelse(isolate(va$init) == 0, 0, dt)
            format(.POSIXct(max_time - off, tz = "GMT"), "%Mmin %Ssecs")
        })

        ##-------------------------------------------
        ## Salva os dados de tempos e atributos sentidos
        ## INSERT-REACTIVE-HERE ##

        ##-------------------------------------------
        ## Transforma para data.frame para salvar em arquivo csv
        output$DOWNLOAD <- downloadHandler(
            filename = function() {
                sufix <- format(Sys.time(), "%M%S")
                paste0("data-", sufix, ".csv")
            },
            content = function(file) {
                write.table(
                    as.data.frame(reactiveValuesToList(da)),
                    file = file,
                    sep = ";",
                    row.names = FALSE,
                    quote = FALSE)
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
                    fluidRow(
                        ## INSERT-BUTTONS-ATTR-HERE ##
                    ),
                    hr(class = "white")
                )
            } else if (va$init == 2) {
                fluidRow(
                    column(width = 8,
                           sliderInput(
                               inputId = "GRADE",
                               label = "Nota geral",
                               min = 1,
                               max = 9,
                               value = 5,
                               width = "100%")
                           ),
                    column(width = 3, offset = 1,
                           HTML(paste0("<label class='control-label'",
                                      ">Confirma nota:</label><br>")),
                           actionButton(
                               inputId = "CONFIRM",
                               label = "Confirm",
                               class = "btn btn-primary")
                           )
                )
            } else {
                FisinhField
            }
        })
        ##-------------------------------------------
        ## Rodapé
        ## INSERT-FOOTER ##
    })
