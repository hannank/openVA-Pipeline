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
            checkboxInput("byAll", "Include an analysis of all records?", TRUE),
            checkboxInput("bySex", "Include sex-specific results?", FALSE),
            checkboxInput("byAge", "Include age-specific results (infant, child, adult)?", FALSE),
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
            downloadButton("downloadData1", "Download Summary for All Records as .csv"), br(),
            downloadButton("downloadPlot1", "Download Plot for All Records as .pdf"), br(),
            downloadButton("downloadData2", "Download Summary for Males as .csv"),
            downloadButton("downloadPlot2", "Download Plot for Males as .pdf"), br(),
            downloadButton("downloadData3", "Download Summary for Females as .csv"),
            downloadButton("downloadPlot3", "Download Plot for Females as .pdf"), br(),
            downloadButton("downloadData4", "Download Summary for Infants as .csv"),
            downloadButton("downloadPlot4", "Download Plot for Infants as .pdf"), br(),
            downloadButton("downloadData5", "Download Summary for Children as .csv"),
            downloadButton("downloadPlot5", "Download Plot for Children as .pdf"), br(),
            downloadButton("downloadData6", "Download Summary for Adults as .csv"),
            downloadButton("downloadPlot6", "Download Plot for Adults as .pdf"),
            downloadButton("downloadWarnings", "Download warnings as .txt")
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

        shinyjs::disable("processMe")
        shinyjs::disable("downloadData1")
        shinyjs::disable("downloadData2")
        shinyjs::disable("downloadData3")
        shinyjs::disable("downloadData4")
        shinyjs::disable("downloadData5")
        shinyjs::disable("downloadData6")
        shinyjs::disable("downloadPlot1")
        shinyjs::disable("downloadPlot2")
        shinyjs::disable("downloadPlot3")
        shinyjs::disable("downloadPlot4")
        shinyjs::disable("downloadPlot5")
        shinyjs::disable("downloadPlot6")
        shinyjs::disable("downloadWarnings")

        withProgress(value=0,{

           setProgress(message = paste("Starting analysis of data (this may take a while)"))

            if(input$algorithm=="InSilicoVA"){

                ## if( input$odkBC) records <- map_records2(getData(), mapping="insilicova")
                if( input$odkBC) records <- map_records2(getData(), mapping="insilicova", cores=2)
                if(!input$odkBC) records <- getData()
                names(records) <- tolower(names(records))

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
                if(file.exists("warning_insilico.txt")) file.remove("warning_insilico.txt")
                if(file.exists("InSilico-warnings.txt")) file.remove("InSilico-warnings.txt")
                file.create("InSilico-warnings.txt")
                cat("Warnings and Errors from InSilico \t", date(), "\n", file="InSilico-warnings.txt")

                if(input$byAll){
                    incProgress(.01, detail=paste("Analysis with all cases"))
                    ## rv$fitAll     <- insilico(records, Nsim = isolate(input$simLength), burnin = burn)
                    rv$fitAll <- do.call("codeVA", list(data=records, model="InSilicoVA",
                                                        Nsim=input$simLength, burnin=burn, warning.write=TRUE))
                    file.append("InSilico-warnings.txt", "warning_insilico.txt")
                    file.remove("warning_insilico.txt")

                    shinyjs::enable("downloadData1")
                    if(file.exists("plotAll.pdf")) file.remove("plotAll.pdf")
                    ## plot(rv$fitAll, top=20)
                    rv$agg.csmf <- get.indiv(rv$fitAll, data=records, CI = 0.95, is.aggregate=TRUE)
                    indivplot(rv$agg.csmf, top = 20, title = "Aggregated COD distribution")
                    ggsave("plotAll.pdf", device="pdf")
                    shinyjs::enable("downloadPlot1")
                }

                if(input$bySex){
                    incProgress(.15, detail=paste("Analysis with Males"))
                    if(length(male[male])==0) rv$male <- NULL
                    if(length(male[male])>0){
                        ## rv$fitMale <- insilico(records[male,], Nsim = isolate(input$simLength), burnin = burn)
                        try(rv$fitMale <- do.call("codeVA", list(data=records[male,], model="InSilicoVA",
                                                                 Nsim=isolate(input$simLength), burnin=burn, warning.write=TRUE)))
                        if(!is.null(rv$fitMale)){

                            cat("\n", "Warnings and Errors from Analysis for Males", date(), "\n", file="InSilico-warnings.txt", append=TRUE)
                            file.append("InSilico-warnings.txt", "warning_insilico.txt")
                            file.remove("warning_insilico.txt")

                            shinyjs::enable("downloadData2")
                            if(file.exists("plotMale.pdf")) file.remove("plotMale.pdf")
                            ## plot(rv$fitMale, top=20); ggsave("plotMale.pdf", device="pdf")
                            rv$agg.csmfMale <- get.indiv(rv$fitMale, data=records[male,], CI = 0.95, is.aggregate=TRUE)
                            indivplot(rv$agg.csmfMale, top = 20, title = "Aggregated COD distribution")
                            ggsave("plotMale.pdf", device="pdf")
                            shinyjs::enable("downloadPlot2")
                        }
                    }
                    if(is.null(rv$fitMale)) rv$male <- NULL

                    incProgress(.15, detail=paste("Analysis with Females"))
                    if(length(female[female])==0) rv$female <- NULL
                    if(length(female[female])>0){
                        ## rv$fitFemale <- insilico(records[female,], Nsim = isolate(input$simLength), burnin = burn)
                        try(rv$fitFemale <- do.call("codeVA", list(data=records[female,], model="InSilicoVA",
                                                                   Nsim=isolate(input$simLength), burnin=burn, warning.write=TRUE)))
                        if(!is.null(rv$fitFemale)){

                            cat("\n", "Warnings and Errors from Analysis for Females", date(), "\n", file="InSilico-warnings.txt", append=TRUE)
                            file.append("InSilico-warnings.txt", "warning_insilico.txt")
                            file.remove("warning_insilico.txt")

                            shinyjs::enable("downloadData3")
                            if(file.exists("plotFemale.pdf")) file.remove("plotFemale.pdf")
                            ## plot(rv$fitFemale, top=20); ggsave("plotFemale.pdf", device="pdf")
                            rv$agg.csmfFemale <- get.indiv(rv$fitFemale, data=records[female,], CI = 0.95, is.aggregate=TRUE)
                            indivplot(rv$agg.csmfFemale, top = 20, title = "Aggregated COD distribution")
                            ggsave("plotFemale.pdf", device="pdf")
                            shinyjs::enable("downloadPlot3")
                        }
                    }
                    if(is.null(rv$fitFemale)) rv$female <- NULL
                }
                if(input$byAge){
                    incProgress(.15, detail=paste("Analysis with Infants"))
                    if(length(infant[infant])==0) rv$infant <- NULL
                    if(length(infant[infant])>0){
                        ## rv$fitInfant <- insilico(records[infant,], Nsim = isolate(input$simLength), burnin = burn)
                        try(rv$fitInfant <- do.call("codeVA", list(data=records[infant,], model="InSilicoVA",
                                                                   Nsim=isolate(input$simLength), burnin=burn, warning.write=TRUE)))
                        if(!is.null(rv$fitInfant)){

                            cat("\n", "Warnings and Errors from Analysis for Infants", date(), "\n", file="InSilico-warnings.txt", append=TRUE)
                            file.append("InSilico-warnings.txt", "warning_insilico.txt")
                            file.remove("warning_insilico.txt")

                            shinyjs::enable("downloadData4")
                            if(file.exists("plotInfant.pdf")) file.remove("plotInfant.pdf")
                            ## plot(rv$fitInfant, top=20); ggsave("plotInfant.pdf", device="pdf")
                            rv$agg.csmfInfant <- get.indiv(rv$fitInfant, data=records[infant,], CI = 0.95, is.aggregate=TRUE)
                            indivplot(rv$agg.csmfInfant, top = 20, title = "Aggregated COD distribution")
                            ggsave("plotInfant.pdf", device="pdf")
                            shinyjs::enable("downloadPlot4")
                        }
                    }
                    if(is.null(rv$fitInfant)) rv$infant <- NULL

                    incProgress(.15, detail=paste("Analysis with Children"))
                    if(length(child[child])==0) rv$child <- NULL
                    if(length(child[child])>0){
                        ## rv$fitChild <- insilico(records[child,], Nsim = isolate(input$simLength), burnin = burn)
                        try(rv$fitChild <- do.call("codeVA", list(data=records[child,], model="InSilicoVA",
                                                                  Nsim=isolate(input$simLength), burnin=burn, warning.write=TRUE)))
                        if(!is.null(rv$fitChild)){

                            cat("\n", "Warnings and Errors from Analysis for Children", date(), "\n", file="InSilico-warnings.txt", append=TRUE)
                            file.append("InSilico-warnings.txt", "warning_insilico.txt")
                            file.remove("warning_insilico.txt")

                            shinyjs::enable("downloadData5")
                            if(file.exists("plotChild.pdf")) file.remove("plotChild.pdf")
                            ## plot(rv$fitChild, top=20); ggsave("plotChild.pdf", device="pdf")
                            rv$agg.csmfChild <- get.indiv(rv$fitChild, data=records[child,], CI = 0.95, is.aggregate=TRUE)
                            indivplot(rv$agg.csmfChild, top = 20, title = "Aggregated COD distribution")
                            ggsave("plotChild.pdf", device="pdf")
                            shinyjs::enable("downloadPlot5")
                        }
                    }
                    if(is.null(rv$fitChild)) rv$child <- NULL

                    incProgress(.15, detail=paste("Analysis with Adults"))
                    if(length(adult[adult])==0) rv$adult <- NULL
                    if(length(adult[adult])>0){
                        ## rv$fitAdult <- insilico(records[adult,], Nsim = isolate(input$simLength), burnin = burn)
                        try(rv$fitAdult <- do.call("codeVA", list(data=records[adult,], model="InSilicoVA",
                                                                  Nsim=isolate(input$simLength), burnin=burn, warning.write=TRUE)))
                        if(!is.null(rv$fitAdult)){

                            cat("\n", "Warnings and Errors from Analysis for Adults", date(), "\n", file="InSilico-warnings.txt", append=TRUE)
                            file.append("InSilico-warnings.txt", "warning_insilico.txt")
                            file.remove("warning_insilico.txt")

                            shinyjs::enable("downloadData6")
                            if(file.exists("plotAdult.pdf")) file.remove("plotAdult.pdf")
                            ## plot(rv$fitAdult, top=20); ggsave("plotAdult.pdf", device="pdf")
                            rv$agg.csmfAdult <- get.indiv(rv$fitAdult, data=records[adult,], CI = 0.95, is.aggregate=TRUE)
                            indivplot(rv$agg.csmfAdult, top = 20, title = "Aggregated COD distribution")
                            ggsave("plotAdult.pdf", device="pdf")
                            shinyjs::enable("downloadPlot6")
                        }
                    }
                    if(is.null(rv$fitAdult)) rv$adult <- NULL
                }
                shinyjs::enable("downloadWarnings")
                rv$counts <- c(length(male[male]), length(female[female]),
                               length(infant[infant]), length(child[child]),
                               length(adult[adult]),
                               length(records$id[records$neonate=="" & records$infant=="" & records$under5=="" &
                                                 records$child=="" & records$adult=="" & records$midage=="" &
                                                 records$elder==""]),
                               nrow(records))
            }

            if(input$algorithm=="InterVA"){

                ## if( input$odkBC) records <- map_records2(getData(), mapping="interva4")
                if( input$odkBC) records <- map_records2(getData(), mapping="interva4", cores=2)
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

                if(file.exists("InterVA-warnings.txt")) file.remove("InterVA-warnings.txt")
                file.create("Interva-warnings.txt")
                cat("Warnings and Errors from Interva \t", date(), "\n", file="Interva-warnings.txt")

                if(input$byAll){
                    incProgress(.01, detail=paste("Analysis with all cases"))
                    rv$fitAll     <- InterVA(Input=records, HIV=input$HIV, Malaria=input$Malaria)
                    rv$resultsAll <- read.csv("VA_result.csv")
                    shinyjs::enable("downloadData1")
                    if(file.exists("plotAdult.pdf")) file.remove("plotAdult.pdf")
                    pdf("plotAll.pdf");CSMF(rv$fitAll, top.plot=20);dev.off()
                    shinyjs::enable("downloadPlot1")

                    cat("\n", "Warnings from Analysis for All Records", date(), "\n", file="InterVA-warnings.txt", append=TRUE)
                    file.append("InterVA-warnings.txt", "warnings.txt")
                    cat("\n", "Errors from Analysis for All Records", date(), "\n", file="InterVA-warnings.txt", append=TRUE)
                    file.append("InterVA-warnings.txt", "errorlog.txt")
                    file.remove("warnings.txt"); file.remove("errorlog.txt")
                    file.remove("VA_result.csv")
                }

                if(input$bySex){
                    incProgress(.15, detail=paste("Analysis with Males"))
                    if(length(male[male])==0) rv$male <- NULL
                    if(length(male[male])>0){
                        rv$fitMale     <- InterVA(Input=records[male,], HIV=input$HIV, Malaria=input$Malaria)
                        rv$resultsMale <- read.csv("VA_result.csv")
                        shinyjs::enable("downloadData2")
                        if(file.exists("plotMale.pdf")) file.remove("plotMale.pdf")
                        pdf("plotMale.pdf");CSMF(rv$fitMale, top.plot=20);dev.off()
                        shinyjs::enable("downloadPlot2")

                        cat("\n", "Warnings from Analysis for Males", date(), "\n", file="InterVA-warnings.txt", append=TRUE)
                        file.append("InterVA-warnings.txt", "warnings.txt")
                        cat("\n", "Errors from Analysis for Males", date(), "\n", file="InterVA-warnings.txt", append=TRUE)
                        file.append("InterVA-warnings.txt", "errorlog.txt")
                        file.remove("warnings.txt"); file.remove("errorlog.txt")
                        file.remove("VA_result.csv")
                    }
                    if(is.null(rv$fitMale)) rv$male <- NULL

                    incProgress(.15, detail=paste("Analysis with Females"))
                    if(length(female[female])==0) rv$female <- NULL
                    if(length(female[female])>0){
                        ## rv$female <- TRUE
                        rv$fitFemale     <- InterVA(Input=records[female,], HIV=input$HIV, Malaria=input$Malaria)
                        rv$resultsFemale <- read.csv("VA_result.csv")
                        shinyjs::enable("downloadData3")
                        if(file.exists("plotFemale.pdf")) file.remove("plotFemale.pdf")
                        pdf("plotFemale.pdf");CSMF(rv$fitFemale, top.plot=20);dev.off()
                        shinyjs::enable("downloadPlot3")

                        cat("\n", "Warnings from Analysis for Females", date(), "\n", file="InterVA-warnings.txt", append=TRUE)
                        file.append("InterVA-warnings.txt", "warnings.txt")
                        cat("\n", "Errors from Analysis for Females", date(), "\n", file="InterVA-warnings.txt", append=TRUE)
                        file.append("InterVA-warnings.txt", "errorlog.txt")
                        file.remove("warnings.txt"); file.remove("errorlog.txt")
                        file.remove("VA_result.csv")
                    }
                    if(is.null(rv$fitFemale)) rv$female <- NULL
                }

                if(input$byAge){
                    incProgress(.15, detail=paste("Analysis with Infants"))
                    if(length(infant[infant])==0) rv$infant <- NULL
                    if(length(infant[infant])>0){
                        ## rv$infant <- TRUE
                        rv$fitInfant     <- InterVA(Input=records[infant,], HIV=input$HIV, Malaria=input$Malaria)
                        rv$resultsInfant <- read.csv("VA_result.csv")
                        shinyjs::enable("downloadData4")
                        if(file.exists("plotInfant.pdf")) file.remove("plotInfant.pdf")
                        pdf("plotInfant.pdf");CSMF(rv$fitInfant, top.plot=20);dev.off()
                        shinyjs::enable("downloadPlot4")

                        cat("\n", "Warnings from Analysis for Infants", date(), "\n", file="InterVA-warnings.txt", append=TRUE)
                        file.append("InterVA-warnings.txt", "warnings.txt")
                        cat("\n", "Errors from Analysis for Infants", date(), "\n", file="InterVA-warnings.txt", append=TRUE)
                        file.append("InterVA-warnings.txt", "errorlog.txt")
                        file.remove("warnings.txt"); file.remove("errorlog.txt")
                        file.remove("VA_result.csv")
                    }
                    if(is.null(rv$fitInfant)) rv$infant <- NULL

                    incProgress(.15, detail=paste("Analysis with Children"))
                    if(length(child[child])==0) rv$child <- NULL
                    if(length(child[child])>0){
                        ## rv$child <- TRUE
                        rv$fitChild     <- InterVA(Input=records[child,], HIV=input$HIV, Malaria=input$Malaria)
                        rv$resultsChild <- read.csv("VA_result.csv")
                        shinyjs::enable("downloadData5")
                        if(file.exists("plotChild.pdf")) file.remove("plotChild.pdf")
                        pdf("plotChild.pdf");CSMF(rv$fitChild, top.plot=20);dev.off()
                        shinyjs::enable("downloadPlot5")

                        cat("\n", "Warnings from Analysis for Child", date(), "\n", file="InterVA-warnings.txt", append=TRUE)
                        file.append("InterVA-warnings.txt", "warnings.txt")
                        cat("\n", "Errors from Analysis for Child", date(), "\n", file="InterVA-warnings.txt", append=TRUE)
                        file.append("InterVA-warnings.txt", "errorlog.txt")
                        file.remove("warnings.txt"); file.remove("errorlog.txt")
                        file.remove("VA_result.csv")
                    }
                    if(is.null(rv$fitChild)) rv$child <- NULL

                    incProgress(.15, detail=paste("Analysis with Adults"))
                    if(length(adult[adult])==0) rv$adult <- NULL
                    if(length(adult[adult])>0){
                        ## rv$adult <- TRUE
                        rv$fitAdult     <- InterVA(Input=records[adult,], HIV=input$HIV, Malaria=input$Malaria)
                        rv$resultsAdult <- read.csv("VA_result.csv")
                        shinyjs::enable("downloadData6")
                        if(file.exists("plotAdult.pdf")) file.remove("plotAdult.pdf")
                        pdf("plotAdult.pdf");CSMF(rv$fitAdult, top.plot=20);dev.off()
                        shinyjs::enable("downloadPlot6")

                        cat("\n", "Warnings from Analysis for Adults", date(), "\n", file="InterVA-warnings.txt", append=TRUE)
                        file.append("InterVA-warnings.txt", "warnings.txt")
                        cat("\n", "Errors from Analysis for Adults", date(), "\n", file="InterVA-warnings.txt", append=TRUE)
                        file.append("InterVA-warnings.txt", "errorlog.txt")
                        file.remove("warnings.txt"); file.remove("errorlog.txt")
                        file.remove("VA_result.csv")
                    }
                    if(is.null(rv$fitAdult)) rv$adult <- NULL
                }

                rv$counts <- c(length(male[male]), length(female[female]),
                               length(infant[infant]), length(child[child]),
                               length(adult[adult]),
                               length(records$ID[records$NEONATE=="" & records$INFANT=="" & records$UNDER5=="" &
                                                 records$CHILD=="" & records$ADULT=="" & records$MIDAGE=="" &
                                                 records$ELDER==""]),
                               nrow(records))
            }
        })
        shinyjs::enable("processMe")
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
        ## if(!is.null(rv$fitAll)){
            "Summary of Results using All Records"
        ## }
    })
    output$summaryAll <- renderPrint({
        if(!is.null(rv$fitAll)) summary(rv$fitAll)
    })

    #### Create plot (all)
    output$titlePlotAll <- renderText({
        ## if(!is.null(rv$fitAll)){
            "CSMF Plot for Total Population"
        ## }
    })
    output$plotAll <- renderPlot({
        if(!is.null(rv$fitAll)){
            if(input$algorithm=="InterVA" & !is.null(rv$fitAll$HIV)){
                CSMF(rv$fitAll, top.plot=20)
            }
            if(input$algorithm=="InSilicoVA" & is.null(rv$fitAll$HIV)){
                plot(rv$fitAll, top=20)
            }
        }
    })

    ## Male
    #### Summarize and print output (male)
    output$titleSummaryMale <- renderText({
        ## if(!is.null(rv$fitMale)){
            "Summary of Results for Males"
        ## }
    })
    output$emptySummaryMale <- renderText({
        if(is.null(rv$male)){
            "No Summary for Males (not enough deaths for analysis)"
        }
    })
    output$summaryMale <- renderPrint({
        if(!is.null(rv$fitMale)) summary(rv$fitMale)
    })

    #### Create plot (male)
    output$titlePlotMale <- renderText({
        ## if(!is.null(rv$fitMale)){
            "CSMF Plot for Males"
        ## }
    })
    output$emptyPlotMale <- renderText({
        if(is.null(rv$male)){
            "No Plot for Males (not enough deaths for analysis)"
        }
    })
    output$plotMale <- renderPlot({
        if(!is.null(rv$fitMale)){
            if(input$algorithm=="InterVA" & !is.null(rv$fitMale$HIV)){
                CSMF(rv$fitMale, top.plot=20)
            }
            if(input$algorithm=="InSilicoVA" & is.null(rv$fitMale$HIV)){
                ## plot(rv$fitMale, top=20)
                indivplot(rv$agg.csmfMale, top = 20, title = "Aggregated COD distribution")
            }
        }
    })
    ## Female
    #### Summarize and print output (female)
    output$titleSummaryFemale <- renderText({
        ## if(!is.null(rv$fitFemale)){
            "Summary of Results for Females"
        ## }
    })
    output$emptySummaryFemale <- renderText({
        if(is.null(rv$female)){
            "No Summary for Females (not enough deaths for analysis)"
        }
    })
    output$summaryFemale <- renderPrint({
        if(!is.null(rv$fitFemale)) summary(rv$fitFemale)
    })

    #### Create plot (female)
    output$titlePlotFemale <- renderText({
        ## if(!is.null(rv$fitFemale)){
            "CSMF Plot for Females"
        ## }
    })
    output$emptyPlotFemale <- renderText({
        if(is.null(rv$female)){
            "No Plot for Females (not enough deaths for analysis)"
        }
    })
    output$plotFemale <- renderPlot({
        if(!is.null(rv$fitFemale)){
            if(input$algorithm=="InterVA" & !is.null(rv$fitFemale$HIV)){
                CSMF(rv$fitFemale, top.plot=20)
            }
            if(input$algorithm=="InSilicoVA" & is.null(rv$fitFemale$HIV)){
                ## plot(rv$fitFemale, top=20)
                indivplot(rv$agg.csmfFemale, top = 20, title = "Aggregated COD distribution")
            }
        }
    })
    ## Infant
    #### Summarize and print output
    output$titleSummaryInfant <- renderText({
        ## if(!is.null(rv$fitInfant)){
            "Summary of Results for Infants"
        ## }
    })
    output$emptySummaryInfant <- renderText({
        if(is.null(rv$infant)){
            "No Summary for Infants (not enough deaths for analysis)"
        }
    })
    output$summaryInfant <- renderPrint({
        if(!is.null(rv$fitInfant)) summary(rv$fitInfant)
    })

    #### Create plot (infants)
    output$titlePlotInfant <- renderText({
        ## if(!is.null(rv$fitInfant)){
            "CSMF Plot for Infants"
        ## }
    })
    output$emptyPlotInfant <- renderText({
        if(is.null(rv$infant)){
            "No Plot for Infants (not enough deaths for analysis)"
        }
    })
    output$plotInfant <- renderPlot({
        if(!is.null(rv$fitInfant)){
            if(input$algorithm=="InterVA" & !is.null(rv$fitInfant$HIV)){
                CSMF(rv$fitInfant, top.plot=20)
            }
            if(input$algorithm=="InSilicoVA" & is.null(rv$fitInfant$HIV)){
                ## plot(rv$fitInfant, top=20)
                indivplot(rv$agg.csmfInfant, top = 20, title = "Aggregated COD distribution")
            }
        }
    })
    ## Child
    #### Summarize and print output
    output$titleSummaryChild <- renderText({
        ## if(!is.null(rv$fitChild)){
            "Summary of Results for Children"
        ## }
    })
    output$emptySummaryChild <- renderText({
        if(is.null(rv$child)){
            "No Summary for Children (not enough deaths for analysis)"
        }
    })
    output$summaryChild <- renderPrint({
        if(!is.null(rv$fitChild)) summary(rv$fitChild)
    })

    #### Create plot (children)
    output$titlePlotChild <- renderText({
        ## if(!is.null(rv$fitChild)){
            "CSMF Plot for Children"
        ## }
    })
    output$emptyPlotChild <- renderText({
        if(is.null(rv$child)){
            "No Plot for Children (not enough deaths for analysis)"
        }
    })
    output$plotChild <- renderPlot({
        if(!is.null(rv$fitChild)){
            if(input$algorithm=="InterVA" & !is.null(rv$fitInfant$HIV)){
                CSMF(rv$fitChild, top.plot=20)
            }
            if(input$algorithm=="InSilicoVA" & is.null(rv$fitInfant$HIV)){
                ## plot(rv$fitChild, top=20)
                indivplot(rv$agg.csmfChild, top = 20, title = "Aggregated COD distribution")
            }
        }
    })
    ## Adult
    #### Summarize and print output
    output$titleSummaryAdult <- renderText({
        ## if(!is.null(rv$fitAdult)){
            "Summary of Results for Adults"
        ## }
    })
    output$emptySummaryAdult <- renderText({
        if(is.null(rv$adult)){
            "No Summary for Adults (not enough deaths for analysis)"
        }
    })
    output$summaryAdult <- renderPrint({
        if(!is.null(rv$fitAdult)) summary(rv$fitAdult)
    })

    #### Create plot
    output$titlePlotAdult <- renderText({
        ## if(!is.null(rv$fitAdult)){
            "CSMF Plot for Adults"
        ## }
    })
    output$emptyPlotAdult <- renderText({
        if(is.null(rv$adult)){
            "No Plot for Adults (not enough deaths for analysis)"
        }
    })
    output$plotAdult <- renderPlot({
        if(!is.null(rv$fitAdult)){
            if(input$algorithm=="InterVA" & !is.null(rv$fitAdult$HIV)){
                CSMF(rv$fitAdult, top.plot=20)
            }
            if(input$algorithm=="InSilicoVA" & is.null(rv$fitAdult$HIV)){
                ## plot(rv$fitAdult, top=20)
                indivplot(rv$agg.csmfAdult, top = 20, title = "Aggregated COD distribution")
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
    output$downloadPlot1 <- downloadHandler(
        filename = "plotAll.pdf",
        content = function(file) {
            if (!is.null(rv$fitAll)){
                file.copy("plotAll.pdf", file)
            }
        }
    )
    output$downloadData2 <- downloadHandler(
        filename = "resultsMale.csv",
        content = function(file) {
            ## if (!is.null(rv$fitMale)){
            if (!is.null(rv$male)){
                if(input$algorithm=="InterVA"){
                    write.csv(rv$resultsMale, file = file)
                }
                if(input$algorithm=="InSilicoVA"){
                    summary(rv$fitMale, file = file)
                }
            }
        }
    )
    output$downloadPlot2 <- downloadHandler(
        filename = "plotMale.pdf",
        content = function(file) {
            if (!is.null(rv$fitMale)){
                file.copy("plotMale.pdf", file)
            }
        }
    )
    output$downloadData3 <- downloadHandler(
        filename = "resultsFemale.csv",
        content = function(file) {
            ## if (!is.null(rv$fitFemale)){
            if (!is.null(rv$female)){
                if(input$algorithm=="InterVA"){
                    write.csv(rv$resultsFemale, file = file)
                }
                if(input$algorithm=="InSilicoVA"){
                    summary(rv$fitFemale, file = file)
                }
            }
        }
    )
    output$downloadPlot3 <- downloadHandler(
        filename = "plotFemale.pdf",
        content = function(file) {
            if (!is.null(rv$fitFemale)){
                file.copy("plotFemale.pdf", file)
            }
        }
    )
    output$downloadData4 <- downloadHandler(
        filename = "resultsInfant.csv",
        content = function(file) {
            ## if (!is.null(rv$fitInfant)){
            if (!is.null(rv$infant)){
                if(input$algorithm=="InterVA"){
                    write.csv(rv$resultsInfant, file = file)
                }
                if(input$algorithm=="InSilicoVA"){
                    summary(rv$fitInfant, file = file)
                }
            }
        }
    )
    output$downloadPlot4 <- downloadHandler(
        filename = "plotInfant.pdf",
        content = function(file) {
            if (!is.null(rv$fitInfant)){
                file.copy("plotInfant.pdf", file)
            }
        }
    )
    output$downloadData5 <- downloadHandler(
        filename = "resultsChild.csv",
        content = function(file) {
            ## if (!is.null(rv$fitChild)){
            if (!is.null(rv$child)){
                if(input$algorithm=="InterVA"){
                    write.csv(rv$resultsChild, file = file)
                }
                if(input$algorithm=="InSilicoVA"){
                    summary(rv$fitChild, file = file)
                }
            }
        }
    )
    output$downloadPlot5 <- downloadHandler(
        filename = "plotChild.pdf",
        content = function(file) {
            if (!is.null(rv$fitChild)){
                file.copy("plotChild.pdf", file)
            }
        }
    )
    output$downloadData6 <- downloadHandler(
        filename = "resultsAdult.csv",
        content = function(file) {
            ## if (!is.null(rv$fitAdult)){
            if (!is.null(rv$adult)){
                if(input$algorithm=="InterVA"){
                    write.csv(rv$resultsAdult, file = file)
                }
                if(input$algorithm=="InSilicoVA"){
                    summary(rv$fitAdult, file = file)
                }
            }
        }
    )
    output$downloadPlot6 <- downloadHandler(
        filename = "plotAdult.pdf",
        content = function(file) {
            if (!is.null(rv$fitAdult)){
                file.copy("plotAdult.pdf", file)
            }
        }
    )

    output$downloadWarnings <- downloadHandler(
        filename = "warnings-openVA.txt",
        content = function(file) {
            if(input$algorithm=="InterVA"){
                file.copy("InterVA-warnings.txt", file)
            }
            if(input$algorithm=="InSilicoVA"){
                file.copy("InSilico-warnings.txt", file)
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
    shinyjs::disable("downloadPlot1")
    shinyjs::disable("downloadPlot2")
    shinyjs::disable("downloadPlot3")
    shinyjs::disable("downloadPlot4")
    shinyjs::disable("downloadPlot5")
    shinyjs::disable("downloadPlot6")
    shinyjs::disable("downloadWarnings")
}

shinyApp(ui=ui, server=server)
## shinyApp(ui=ui, server=server, option=list(port=5567))
