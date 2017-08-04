library(shiny)
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


    ## Inputs
    ## tags$head(tags$script(src = "message-handler.js")),

    sidebarLayout(
        sidebarPanel(
            fileInput("readIn",
                      "Upload your own data here",
                      multiple = FALSE,
                      accept = NULL),
            fileInput("customProbbase",
                      "Upload a customized conditional probability table",
                      multiple = FALSE,
                      accept = NULL),
            checkboxInput("defaultCondProb", "or click here to use the default", FALSE),
            h3("Choose your preferences"),
            br(),
            selectInput(inputId="algorithm", label="Select Algorithm:",
                        choices=c("InSilico"="InSilicoVA", "InterVA4"="InterVA",
                                  "Naive Bayes"="NBC", "Tariff"="Tariff"), width="150px"),
            conditionalPanel(condition="input.algorithm=='InSilicoVA'",
                             sliderInput(inputId="simLength", label="Number of iterations in the simulation", min=300, max=7000, value=5000)),
            conditionalPanel(condition="input.algorithm=='InSilicoVA'",
                             checkboxInput("autoLength", "Automatically increase iterations if needed?", FALSE)),
            conditionalPanel(condition="input.algorithm=='InterVA'",
                             selectInput(inputId="HIV", label="Level of HIV prevalence",
                                         choices=c("h (high: >= 1% of all deaths)" = "h",
                                                   "l (low:  ~ 0.1% of all deaths)" = "l",
                                                   "v (very low: < 0.01% of all deaths)" = "v"), width="300px")),

            ## conditionalPanel(condition="input.algorithm=='InSilicoVA' || input.algorithm=='InterVA'",
            conditionalPanel(condition="input.algorithm=='InterVA'",
                             selectInput(inputId="Malaria", label="Level of malaria prevalence",
                                         choices=c("h (high: >= 1% of all deaths)" = "h",
                                                   "l (low:  ~ 0.1% of all deaths)" = "l",
                                                   "v (very low: < 0.01% of all deaths)" = "v"), width="300px")),

            br(),
            h4("Data Checks"),
            checkboxInput("odkBC", "Are the data from an ODKBriefcase export?", TRUE),
            conditionalPanel(condition="input.algorithm=='InSilicoVA'",
                             checkboxInput("isNumeric", "Data already in numeric form?", FALSE),
                             checkboxInput("useProbbase", "Use InterVA conditional probability without reestimating?", FALSE),
                             checkboxInput("keepProbbase", "Estimating Probbase levels only?", TRUE),
                             checkboxInput("datacheck", "Check data consistency?", TRUE),
                             checkboxInput("externalSep", "Separate out external clauses (suggested)?", TRUE),
                             numericInput("seed", "Select Seed Value", 1, min = "1")),
            h6("(Please note this connection is not encrypted.  Do not upload data that require a secure connection.)"),
            actionButton("processMe", "Analyze my data!"),
            hr(),
            downloadButton("downloadData", "Download Summary as .csv")
            ),

        ## Outputs
        mainPanel(
            ## verbatimTextOutput("mainWarnings"),
            verbatimTextOutput("mainSummary"),
            plotOutput("mainPlot")

        )
    )
)


server <- function(input, output, session){

    ## Read in data
    getData <- reactive({

        vaData <- isolate(input$readIn)

        if (is.null(vaData))
            return(NULL)

            read.csv(vaData$datapath)
    })

    ## Run model
    rv <- reactiveValues()
    rv$mess = ""

    observeEvent(input$processMe, {

        withProgress({

            setProgress(message = "Starting analysis of data (this may take a while)...")

            if(input$algorithm=="InSilicoVA"){

                #get the optional, user-provided cond prob
                userBase <- isolate(input$customProbbase)
                if (is.null(userBase) | isolate(input$defaultCondProb)) {
                    userBase = NULL
                } else {
                    userBase = read.csv(userBase$datapath)
                }

                burn <- round(input$simLength / 2)
                rv$fit <- insilico(getData(), subpop = NULL, isNumeric = isolate(input$isNumeric),
                                   Nsim = isolate(input$simLength), burnin = burn, thin = 10, auto.length = isolate(input$autoLength),
                                   conv.csmf = 0.02, external.sep = isolate(input$externalSep),
                                   useProbbase = isolate(input$useProbbase),
                                   keepProbbase.level = isolate(input$keepProbbase), cond.prob.touse = userBase,
                                   datacheck = isolate(input$datacheck), seed = isolate(input$seed))
            }

            if(input$algorithm=="InterVA"){
                rv$fit <- InterVA(Input=getData(), HIV=input$HIV, Malaria=input$Malaria)
                system("rm errorlog.txt")
                system("rm warnings.txt")
                rv$interVAResults <- read.csv("VA_result.csv")
                system("rm VA_result.csv")
            }
        })
    })

    ## Print warning messages -- HERE

    ## Summarize and print output
    output$mainSummary <- renderPrint({
        if(!is.null(rv$fit)){
            summary(rv$fit)
        }
    })

    ## Create plot
    output$mainPlot <- renderPlot({
        if(!is.null(rv$fit)){
            if(input$algorithm=="InterVA"){
                CSMF(rv$fit)
            }
            if(input$algorithm=="InSilicoVA"){
                plot(rv$fit)
            }
        }
    })

    ## Set up file for downloading
    output$downloadData <- downloadHandler(
        filename = "AnalysisResults.csv",
        content = function(file) {
            if (!is.null(rv$fit)){
                if(input$algorithm=="InterVA"){
                    write.csv(rv$interVAResults, file = file)
                }
                if(input$algorithm=="InSilicoVA"){
                    summary(rv$fit, file = file)
                }
            }
        }
    )
}

shinyApp(ui=ui, server=server)
