library(shiny)
library(shinyjs)
library(openVA)
library(CrossVA)

options(shiny.maxRequestSize=30*1024^2)

ui <- fluidPage(

    titlePanel("Probabilistic Cause-of-death Assignment using Verbal Autopsies"),
    p("Developed by Tyler McCormick ", (a(href="mailto:tylermc@uw.edu", "(tylermc@uw.edu)")),
      "Zehang Richard Li ", (a(href="mailto:lizehang@uw.edu", "(lizehang@uw.edu)")),
      "and Samuel Clark ", a(href="mailto:work@samclark.net ", "(work@samclark.net)")),
    p("The complete study can be viewed ", a(href="http://arxiv.org/abs/1411.3042", "here")),
    hr(),
    shinyjs::useShinyjs(),

    ## Inputs
    ## tags$head(tags$script(src = "message-handler.js")),

    sidebarLayout(
        sidebarPanel(
            fileInput("readIn",
                      "Upload your own data here",
                      multiple = FALSE,
                      accept = NULL),
            ## fileInput("customProbbase",
            ##           "Upload a customized conditional probability table",
            ##           multiple = FALSE,
            ##           accept = NULL),
            ## checkboxInput("defaultCondProb", "or click here to use the default", FALSE),
            h3("Choose your preferences"),
            br(),
            selectInput(inputId="algorithm", label="Select Algorithm:",
                        choices=c("InSilico"="InSilicoVA", "InterVA4"="InterVA"),
                        ## choices=c("InSilico"="InSilicoVA", "InterVA4"="InterVA",
                        ##           "Naive Bayes"="NBC", "Tariff"="Tariff"),
                        width="150px"),
            conditionalPanel(condition="input.algorithm=='InSilicoVA'",
                             sliderInput(inputId="simLength", label="Number of iterations in the simulation", min=300, max=7000, value=5000)),
            ## conditionalPanel(condition="input.algorithm=='InSilicoVA'",
            ##                  checkboxInput("autoLength", "Automatically increase iterations if needed?", FALSE)),
            conditionalPanel(condition="input.algorithm=='InterVA'",
                             selectInput(inputId="HIV", label="Level of HIV prevalence",
                                         choices=c("v (very low: < 0.01% of all deaths)" = "v",
                                                   "l (low:  ~ 0.1% of all deaths)" = "l",
                                                   "h (high: >= 1% of all deaths)" = "h"), width="300px")),
            ## conditionalPanel(condition="input.algorithm=='InSilicoVA' || input.algorithm=='InterVA'",
            conditionalPanel(condition="input.algorithm=='InterVA'",
                             selectInput(inputId="Malaria", label="Level of malaria prevalence",
                                         choices=c("v (very low: < 0.01% of all deaths)" = "v",
                                                   "l (low:  ~ 0.1% of all deaths)" = "l",
                                                   "h (high: >= 1% of all deaths)" = "h"), width="300px")),

            br(),
            h4("Data Checks"),
            checkboxInput("odkBC", "Are the data from an ODKBriefcase export?", TRUE),
            conditionalPanel(condition="input.algorithm=='InSilicoVA'"
                             ## checkboxInput("isNumeric", "Data already in numeric form?", FALSE)## ,
                             ## checkboxInput("useProbbase", "Use InterVA conditional probability without reestimating?", FALSE),
                             ## checkboxInput("keepProbbase", "Estimating Probbase levels only?", TRUE),
                             ## checkboxInput("datacheck", "Check data consistency?", TRUE),
                             ## checkboxInput("externalSep", "Separate out external clauses (suggested)?", TRUE),
                             ## numericInput("seed", "Select Seed Value", 1, min = "1")
                             ),
            ## h6("(Please note this connection is not encrypted.  Do not upload data that require a secure connection.)"),
            conditionalPanel("output.fileUploaded", actionButton("processMe", "Analyze my data!")),
            hr(),
            helpText("Downloads will be available once the data have been analyzed"),
            downloadButton("downloadData1", "Download Summary for All Records as .csv"),
            downloadButton("downloadData2", "Download Summary for Males as .csv"),
            downloadButton("downloadData3", "Download Summary for Females as .csv"),
            downloadButton("downloadData4", "Download Summary for Infants as .csv"),
            downloadButton("downloadData5", "Download Summary for Children as .csv"),
            downloadButton("downloadData6", "Download Summary for Adults as .csv")
            ),

        ## Outputs
        mainPanel(
            ## verbatimTextOutput("mainWarnings"),
            ## verbatimTextOutput("titleDescriptiveStats"),
            h4(textOutput("titleDescriptiveStats")),
            tableOutput("descriptiveStats"),
            h4(textOutput("titleSummaryAll")),
            verbatimTextOutput("summaryAll"),
            h4(textOutput("titlePlotAll")),
            plotOutput("plotAll"),
            h4(textOutput("titleSummaryMale")),
            h4(textOutput("emptySummaryMale")),
            verbatimTextOutput("summaryMale"),
            h4(textOutput("titlePlotMale")),
            h4(textOutput("emptyPlotMale")),
            plotOutput("plotMale"),
            h4(textOutput("titleSummaryFemale")),
            h4(textOutput("emptySummaryFemale")),
            verbatimTextOutput("summaryFemale"),
            h4(textOutput("titlePlotFemale")),
            h4(textOutput("emptyPlotFemale")),
            plotOutput("plotFemale"),
            h4(textOutput("titleSummaryInfant")),
            h4(textOutput("emptySummaryInfant")),
            verbatimTextOutput("summaryInfant"),
            h4(textOutput("titlePlotInfant")),
            h4(textOutput("emptyPlotInfant")),
            plotOutput("plotInfant"),
            h4(textOutput("titleSummaryChild")),
            h4(textOutput("emptySummaryChild")),
            verbatimTextOutput("summaryChild"),
            h4(textOutput("titlePlotChild")),
            h4(textOutput("emptyPlotChild")),
            plotOutput("plotChild"),
            h4(textOutput("titleSummaryAdult")),
            h4(textOutput("emptySummaryAdult")),
            verbatimTextOutput("summaryAdult"),
            h4(textOutput("titlePlotAdult")),
            h4(textOutput("emptyPlotAdult")),
            plotOutput("plotAdult")
        )
    )
)


server <- function(input, output, session){

    ## Read in data
    getData <- reactive({

        ## vaData <- isolate(input$readIn)
        vaData <- input$readIn

        if(is.null(vaData)){
            return(NULL)
        }
        read.csv(vaData$datapath)
    })
    output$fileUploaded <- reactive({
        return(!is.null(getData()))
    })
    outputOptions(output, "fileUploaded", suspendWhenHidden=FALSE)

    ## Run model
    rv <- reactiveValues()
    rv$male   <- TRUE
    rv$female <- TRUE
    rv$infant <- TRUE
    rv$child  <- TRUE
    rv$adult  <- TRUE
    rv$mess = ""

    observeEvent(input$processMe, {

        withProgress(value=0,{

            setProgress(message = "Starting analysis of data \n (this may take a while)...")

            if(input$algorithm=="InSilicoVA"){

                #get the optional, user-provided cond prob
                ## userBase <- isolate(input$customProbbase)
                ## if (is.null(userBase) | isolate(input$defaultCondProb)) {
                ##     userBase = NULL
                ## } else {
                ##     userBase = read.csv(userBase$datapath)
                ## }

                if( input$odkBC) records <- map_records2(getData(), mapping="insilicova")
                if(!input$odkBC) records <- getData()

                male <- rep(FALSE, length(records$male))
                male[records$male=="y"] <- TRUE
                female <- rep(FALSE, length(records$female))
                female[records$female=="y"] <- TRUE
                infant <- rep(FALSE, length(records$neonate))
                infant[records$neonate=="y"] <- TRUE
                infant[records$infant =="y"] <- TRUE
                child  <- rep(FALSE, length(records$under5))
                child[records$under5 =="y"] <- TRUE
                adult <- rep(FALSE, length(records$child))
                adult[records$child  =="y"] <- TRUE
                adult[records$adult  =="y"] <- TRUE
                adult[records$midage =="y"] <- TRUE
                adult[records$elder  =="y"] <- TRUE

                burn <- round(input$simLength / 2)

                incProgress(.01, detail=paste("Analysis with all cases"))
                rv$fitAll     <- insilico(records, Nsim = isolate(input$simLength), burnin = burn)
                shinyjs::enable("downloadData1")

                incProgress(.15, detail=paste("Analysis with Males"))
                if(length(male[male])==0) rv$male <- NULL
                if(length(male[male])>0){
                    rv$fitMale <- insilico(records[male,], Nsim = isolate(input$simLength), burnin = burn)
                    shinyjs::enable("downloadData2")
                }

                incProgress(.15, detail=paste("Analysis with Females"))
                if(length(female[female])==0) rv$female <- NULL
                if(length(female[female])>0){
                    rv$fitFemale <- insilico(records[female,], Nsim = isolate(input$simLength), burnin = burn)
                    shinyjs::enable("downloadData3")
                }

                incProgress(.15, detail=paste("Analysis with Infants"))
                if(length(infant[infant])==0) rv$infant <- NULL
                if(length(infant[infant])>0){
                    rv$fitInfant <- insilico(records[infant,], Nsim = isolate(input$simLength), burnin = burn)
                    shinyjs::enable("downloadData4")
                }

                incProgress(.15, detail=paste("Analysis with Children"))
                if(length(child[child])==0) rv$child <- NULL
                if(length(child[child])>0){
                    rv$fitChild <- insilico(records[child,], Nsim = isolate(input$simLength), burnin = burn)
                    shinyjs::enable("downloadData5")
                }

                incProgress(.15, detail=paste("Analysis with Adults"))
                if(length(adult[adult])==0) rv$adult <- NULL
                if(length(adult[adult])>0){
                    rv$fitAdult <- insilico(records[adult,], Nsim = isolate(input$simLength), burnin = burn)
                    shinyjs::enable("downloadData6")
                }

                rv$counts <- c(length(male[male]), length(female[female]),
                               length(infant[infant]), length(child[child]),
                               length(adult[adult]),
                               length(records$id[records$neonate=="" & records$infant=="" & records$under5=="" &
                                                 records$child=="" & records$adult=="" & records$midage=="" &
                                                 records$elder==""]),
                               nrow(records))
            }

            if(input$algorithm=="InterVA"){

                if( input$odkBC) records <- map_records2(getData(), mapping="interva4")
                if(!input$odkBC) records <- getData()

                male <- rep(FALSE, length(records$MALE))
                male[records$MALE=="y"] <- TRUE
                female <- rep(FALSE, length(records$FEMALE))
                female[records$FEMALE=="y"] <- TRUE
                infant <- rep(FALSE, length(records$NEONATE))
                infant[records$NEONATE=="y"] <- TRUE
                infant[records$INFANT =="y"] <- TRUE
                child  <- rep(FALSE, length(records$UNDER5))
                child[records$UNDER5 =="y"] <- TRUE
                adult <- rep(FALSE, length(records$CHILD))
                adult[records$CHILD  =="y"] <- TRUE
                adult[records$ADULT  =="y"] <- TRUE
                adult[records$MIDAGE =="y"] <- TRUE
                adult[records$ELDER  =="y"] <- TRUE

                incProgress(.01, detail=paste("Analysis with all cases"))
                rv$fitAll     <- InterVA(Input=records, HIV=input$HIV, Malaria=input$Malaria)
                rv$resultsAll <- read.csv("VA_result.csv")
                shinyjs::enable("downloadData1")

                incProgress(.15, detail=paste("Analysis with Males"))
                if(length(male[male])==0) rv$male <- NULL
                if(length(male[male])>0){
                    rv$fitMale     <- InterVA(Input=records[male,], HIV=input$HIV, Malaria=input$Malaria)
                    rv$resultsMale <- read.csv("VA_result.csv")
                    shinyjs::enable("downloadData2")
                }

                incProgress(.15, detail=paste("Analysis with Females"))
                if(length(female[female])==0) rv$female <- NULL
                if(length(female[female])>0){
                    ## rv$female <- TRUE
                    rv$fitFemale     <- InterVA(Input=records[female,], HIV=input$HIV, Malaria=input$Malaria)
                    rv$resultsFemale <- read.csv("VA_result.csv")
                    shinyjs::enable("downloadData3")
                }

                incProgress(.15, detail=paste("Analysis with Infants"))
                if(length(infant[infant])==0) rv$infant <- NULL
                if(length(infant[infant])>0){
                    ## rv$infant <- TRUE
                    rv$fitInfant     <- InterVA(Input=records[infant,], HIV=input$HIV, Malaria=input$Malaria)
                    rv$resultsInfant <- read.csv("VA_result.csv")
                    shinyjs::enable("downloadData4")
                }

                incProgress(.15, detail=paste("Analysis with Children"))
                if(length(child[child])==0) rv$child <- NULL
                if(length(child[child])>0){
                    ## rv$child <- TRUE
                    rv$fitChild     <- InterVA(Input=records[child,], HIV=input$HIV, Malaria=input$Malaria)
                    rv$resultsChild <- read.csv("VA_result.csv")
                    shinyjs::enable("downloadData5")
                }

                incProgress(.15, detail=paste("Analysis with Adults"))
                if(length(adult[adult])==0) rv$adult <- NULL
                if(length(adult[adult])>0){
                    ## rv$adult <- TRUE
                    rv$fitAdult     <- InterVA(Input=records[adult,], HIV=input$HIV, Malaria=input$Malaria)
                    rv$resultsAdult <- read.csv("VA_result.csv")
                    shinyjs::enable("downloadData6")
                }

                file.remove("errorlog.txt")
                file.remove("warnings.txt")
                file.remove("VA_result.csv")

                rv$counts <- c(length(male[male]), length(female[female]),
                               length(infant[infant]), length(child[child]),
                               length(adult[adult]),
                               length(records$ID[records$NEONATE=="" & records$INFANT=="" & records$UNDER5=="" &
                                                 records$CHILD=="" & records$ADULT=="" & records$MIDAGE=="" &
                                                 records$ELDER==""]),
                               nrow(records))
            }
        })
    })

    ## Print warning messages -- HERE

    ## Output table with descriptive statistics
    output$titleDescriptiveStats <- renderText({
        if(!is.null(rv$counts)){
            "Counts of Deaths by Sex & Age"
        }
    })

    output$descriptiveStats <- renderTable({
        if(!is.null(rv$counts)){
            ## matrix(rv$counts, nrow=1, ncol=11, dimnames = list(c("# of Deaths"),
            ##                                                    c("Male", "Female", "Neonate", "Infant",
            ##                                                      "Age 1-4", "Age 5-14", "Age 14-49",
            ##                                                      "Age 50-64", "Age 65+", "Age is Missing", "Total")))
            matrix(rv$counts, nrow=1, ncol=7, dimnames = list(c("# of Deaths"),
                                                               c("Male", "Female", "Infant (0-1)", "Child (1-4)", "Ages 5+",
                                                                 "Age is Missing", "Total")))
        }
    })

    ## All
    #### Summarize and print output (all)
    output$titleSummaryAll <- renderText({
        if(!is.null(rv$fitAll)){
            "Summary of Results using All Records"
        }
    })
    output$summaryAll <- renderPrint({
        if(!is.null(rv$fitAll)){
            ## if(input$algorithm=="InSilicoVA" & is.null(rv$fit$HIV)){
                summary(rv$fitAll)
            ## }
            ## if(input$algorithm=="InterVA" & !is.null(rv$fit$HIV)){
            ##     summary(rv$fit)
            ## }
        }
    })

    #### Create plot (all)
    output$titlePlotAll <- renderText({
        if(!is.null(rv$fitAll)){
            "CSMF for Total Population"
        }
    })
    output$plotAll <- renderPlot({
        if(!is.null(rv$fitAll)){
            if(input$algorithm=="InterVA" & !is.null(rv$fitAll$HIV)){
                CSMF(rv$fitAll)
            }
            if(input$algorithm=="InSilicoVA" & is.null(rv$fitAll$HIV)){
                plot(rv$fitAll)
            }
        }
    })

    ## Male
    #### Summarize and print output (male)
    output$titleSummaryMale <- renderText({
        if(!is.null(rv$fitMale)){
            "Summary of Results for Males"
        }
    })
    output$emptySummaryMale <- renderText({
        if(is.null(rv$male)){
            "No Summary (no males in data set)"
        }
    })
    output$summaryMale <- renderPrint({
        if(!is.null(rv$fitMale)){
            ## if(input$algorithm=="InSilicoVA" & is.null(rv$fit$HIV)){
                summary(rv$fitMale)
            ## }
            ## if(input$algorithm=="InterVA" & !is.null(rv$fit$HIV)){
            ##     summary(rv$fit)
            ## }
        }
    })

    #### Create plot (male)
    output$titlePlotMale <- renderText({
        if(!is.null(rv$fitMale)){
            "CSMF for Males"
        }
    })
    output$emptyPlotMale <- renderText({
        if(is.null(rv$male)){
            "No Plot (no males in data set)"
        }
    })
    output$plotMale <- renderPlot({
        if(!is.null(rv$fitMale)){
            if(input$algorithm=="InterVA" & !is.null(rv$fitMale$HIV)){
                CSMF(rv$fitMale)
            }
            if(input$algorithm=="InSilicoVA" & is.null(rv$fitMale$HIV)){
                plot(rv$fitMale)
            }
        }
    })
    ## Female
    #### Summarize and print output (female)
    output$titleSummaryFemale <- renderText({
        if(!is.null(rv$fitFemale)){
            "Summary of Results for Females"
        }
    })
    output$emptySummaryFemale <- renderText({
        if(is.null(rv$female)){
            "No Summary (no females in data set)"
        }
    })
    output$summaryFemale <- renderPrint({
        if(!is.null(rv$fitFemale)){
            ## if(input$algorithm=="InSilicoVA" & is.null(rv$fit$HIV)){
                summary(rv$fitFemale)
            ## }
            ## if(input$algorithm=="InterVA" & !is.null(rv$fit$HIV)){
            ##     summary(rv$fit)
            ## }
        }
    })

    #### Create plot (female)
    output$titlePlotFemale <- renderText({
        if(!is.null(rv$fitFemale)){
            "CSMF for Females"
        }
    })
    output$emptyPlotFemale <- renderText({
        if(is.null(rv$female)){
            "No Plot (no females in data set)"
        }
    })
    output$plotFemale <- renderPlot({
        if(!is.null(rv$fitFemale)){
            if(input$algorithm=="InterVA" & !is.null(rv$fitFemale$HIV)){
                CSMF(rv$fitFemale)
            }
            if(input$algorithm=="InSilicoVA" & is.null(rv$fitFemale$HIV)){
                plot(rv$fitFemale)
            }
        }
    })
    ## Infant
    #### Summarize and print output
    output$titleSummaryInfant <- renderText({
        if(!is.null(rv$fitInfant)){
            "Summary of Results for Infants"
        }
    })
    output$emptySummaryInfant <- renderText({
        if(is.null(rv$infant)){
            "No Summary (no infants in data set)"
        }
    })
    output$summaryInfant <- renderPrint({
        if(!is.null(rv$fitInfant)){
            ## if(input$algorithm=="InSilicoVA" & is.null(rv$fit$HIV)){
                summary(rv$fitInfant)
            ## }
            ## if(input$algorithm=="InterVA" & !is.null(rv$fit$HIV)){
            ##     summary(rv$fit)
            ## }
        }
    })

    #### Create plot (infants)
    output$titlePlotInfant <- renderText({
        if(!is.null(rv$fitInfant)){
            "CSMF for Infants"
        }
    })
    output$emptyPlotInfant <- renderText({
        if(is.null(rv$infant)){
            "No Plot (no infants in data set)"
        }
    })
    output$plotInfant <- renderPlot({
        if(!is.null(rv$fitInfant)){
            if(input$algorithm=="InterVA" & !is.null(rv$fitInfant$HIV)){
                CSMF(rv$fitInfant)
            }
            if(input$algorithm=="InSilicoVA" & is.null(rv$fitInfant$HIV)){
                plot(rv$fitInfant)
            }
        }
    })
    ## Child
    #### Summarize and print output
    output$titleSummaryChild <- renderText({
        if(!is.null(rv$fitChild)){
            "Summary of Results for Children"
        }
    })
    output$emptySummaryChild <- renderText({
        if(is.null(rv$child)){
            "No Summary (no children in data set)"
        }
    })
    output$summaryChild <- renderPrint({
        if(!is.null(rv$fitChild)){
            ## if(input$algorithm=="InSilicoVA" & is.null(rv$fit$HIV)){
                summary(rv$fitChild)
            ## }
            ## if(input$algorithm=="InterVA" & !is.null(rv$fit$HIV)){
            ##     summary(rv$fit)
            ## }
        }
    })

    #### Create plot (children)
    output$titlePlotChild <- renderText({
        if(!is.null(rv$fitChild)){
            "CSMF for Children"
        }
    })
    output$emptyPlotChild <- renderText({
        if(is.null(rv$child)){
            "No Plot (no children in data set)"
        }
    })
    output$plotChild <- renderPlot({
        if(!is.null(rv$fitChild)){
            if(input$algorithm=="InterVA" & !is.null(rv$fitInfant$HIV)){
                CSMF(rv$fitChild)
            }
            if(input$algorithm=="InSilicoVA" & is.null(rv$fitInfant$HIV)){
                plot(rv$fitChild)
            }
        }
    })
    ## Adult
    #### Summarize and print output
    output$titleSummaryAdult <- renderText({
        if(!is.null(rv$fitAdult)){
            "Summary of Results for Adults"
        }
    })
    output$emptySummaryAdult <- renderText({
        if(is.null(rv$adult)){
            "No Summary (no adults in data set)"
        }
    })
    output$summaryAdult <- renderPrint({
        if(!is.null(rv$fitAdult)){
            ## if(input$algorithm=="InSilicoVA" & is.null(rv$fit$HIV)){
                summary(rv$fitAdult)
            ## }
            ## if(input$algorithm=="InterVA" & !is.null(rv$fit$HIV)){
            ##     summary(rv$fit)
            ## }
        }
    })

    #### Create plot
    output$titlePlotAdult <- renderText({
        if(!is.null(rv$fitAdult)){
            "CSMF for Adults"
        }
    })
    output$emptyPlotAdult <- renderText({
        if(is.null(rv$adult)){
            "No Plot (no adults in data set)"
        }
    })
    output$plotAdult <- renderPlot({
        if(!is.null(rv$fitAdult)){
            if(input$algorithm=="InterVA" & !is.null(rv$fitAdult$HIV)){
                CSMF(rv$fitAdult)
            }
            if(input$algorithm=="InSilicoVA" & is.null(rv$fitAdult$HIV)){
                plot(rv$fitAdult)
            }
        }
    })

    ## Set up file for downloading
    output$downloadData1 <- downloadHandler(
        filename = "resultsAll.csv",
        content = function(file) {
            if (!is.null(rv$fitAll)){
                if(input$algorithm=="InterVA"){
                    write.csv(rv$resultsAll, file = file)
                }
                if(input$algorithm=="InSilicoVA"){
                    summary(rv$fitAll, file = file)
                }
            }
        }
    )
    output$downloadData2 <- downloadHandler(
        filename = "resultsMale.csv",
        content = function(file) {
            if (!is.null(rv$fitMale)){
                if(input$algorithm=="InterVA"){
                    write.csv(rv$resultsMale, file = file)
                }
                if(input$algorithm=="InSilicoVA"){
                    summary(rv$fitMale, file = file)
                }
            }
        }
    )
    output$downloadData3 <- downloadHandler(
        filename = "resultsFemale.csv",
        content = function(file) {
            if (!is.null(rv$fitFemale)){
                if(input$algorithm=="InterVA"){
                    write.csv(rv$resultsFemale, file = file)
                }
                if(input$algorithm=="InSilicoVA"){
                    summary(rv$fitFemale, file = file)
                }
            }
        }
    )
    output$downloadData4 <- downloadHandler(
        filename = "resultsInfant.csv",
        content = function(file) {
            if (!is.null(rv$fitInfant)){
                if(input$algorithm=="InterVA"){
                    write.csv(rv$resultsInfant, file = file)
                }
                if(input$algorithm=="InSilicoVA"){
                    summary(rv$fitInfant, file = file)
                }
            }
        }
    )
    output$downloadData5 <- downloadHandler(
        filename = "resultsChild.csv",
        content = function(file) {
            if (!is.null(rv$fitChild)){
                if(input$algorithm=="InterVA"){
                    write.csv(rv$resultsChild, file = file)
                }
                if(input$algorithm=="InSilicoVA"){
                    summary(rv$fitChild, file = file)
                }
            }
        }
    )
    output$downloadData6 <- downloadHandler(
        filename = "resultsAdult.csv",
        content = function(file) {
            if (!is.null(rv$fitAdult)){
                if(input$algorithm=="InterVA"){
                    write.csv(rv$resultsAdult, file = file)
                }
                if(input$algorithm=="InSilicoVA"){
                    summary(rv$fitAdult, file = file)
                }
            }
        }
    )

    # disable download button on page load
    shinyjs::disable("downloadData1")
    shinyjs::disable("downloadData2")
    shinyjs::disable("downloadData3")
    shinyjs::disable("downloadData4")
    shinyjs::disable("downloadData5")
    shinyjs::disable("downloadData6")

}

shinyApp(ui=ui, server=server)
