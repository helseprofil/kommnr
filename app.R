pkgs <- c("data.table", "collapsibleTree", "shiny", "DT", "magrittr", "stringr")
## nypkg <- pkgs[!(pkgs  %in% installed.packages()[, "Package"])]
## if(length(nypkg)) install.packages(nypkg, repos = "https://cran.rstudio.org/")
sapply(pkgs, require, character.only = TRUE)

## Read raw data
DTraw <- readRDS("geo2020.RDS")
nyvar <- c("geo_b4", "geo2020", "antall", "kommnr", "fylke", "endret")
DT <- subset(DTraw, select = c("gk_old", "gk2020", "grp", "kommid", "fylk", "ny"))
data.table::setnames(DT, names(DT), nyvar)

## kommunenr
DT[, kom_b4:= stringr::str_extract(geo_b4, ".{4}")]
komvar <- c("kom_b4", "kommnr", "antall", "fylke")

## Create vector of all new and old GEOs for selection input
## komid <- unique(DT$kommnr)
dtvec <- DT[!duplicated(kommnr),
            .(id = as.integer(kommnr), kommnr = as.character(kommnr))]

komid = dtvec[, .(kommnr), keyby = id][[2]]

dtvecOld <-  DT[!duplicated(kom_b4),
            .(id = as.integer(kom_b4), kommnr = as.character(kom_b4))]

komidOld = dtvecOld[, .(kommnr), keyby = id][[2]]


## SHINY
ui <- shiny::navbarPage(
  title = "Geo 2020",

  tabPanel("Grunnkrets",
           column(2,
                  selectInput("komm", "Kommunenummer:",
                              choices = komid,
                              selected = "3001",
                              multiple = FALSE),


                  tags$hr(),
                  tags$div(HTML("<strong>NB!</strong> <em>'geo_b4'</em> er alle endringer i geonumrene siden 1990, dvs. leses som <em>geo before</em>")),

                  #download all
                  tags$hr(),
                  tags$b("Laster ned datasettet:"),
                  downloadButton("downall", "Hele datasettet", class="butt"),
                  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #111;}")),
                  tags$p(""),
                  downloadButton("downsub", "Utvalgte data", class = "butt"),
                  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #111;}")),

),

column(8,
       DT::DTOutput("tbl"))
),

tabPanel("Visual",
         column(3,
                ## verbatimTextOutput("test"),
                h3("Kommunenummer: ", textOutput("txt")),

                uiOutput("komm_slid")

                ),

         column(9,
                collapsibleTree::collapsibleTreeOutput("sti")
                )),

tabPanel("Kommune",
         column(2,
                radioButtons("valgKomm", "Nye eller gamle GEO?",
                             choices = list("Nye" = 1, "Gamle" = 2),
                             selected = 1),
                conditionalPanel(
                  condition = "input.valgKomm == 1",
                  selectInput("komm1", "2020 GEO:",
                              choices = komid,
                              selected = "3001",
                              multiple = FALSE),
                ),
                conditionalPanel(
                condition = "input.valgKomm == 2",
                selectInput("komm2", "Gamle GEO:",
                            choices = komidOld,
                            selected = "1102",
                            multiple = FALSE)),
                 tags$hr(),
                  tags$div(HTML("<strong>NB!</strong> <em>'kom_b4'</em> er alle endringer i kommunenummer siden 1990, dvs. leses som <em>kommune geo before</em>")),
                ),
         column(8,
                DT::DTOutput("komtbl"))
         )

)

server <- function(input, output, session) {

  kommune <- eventReactive(input$komm, {
    komDT = subset(DT, kommnr == input$komm)
    komDT
  })

  output$txt <- renderText(
    input$komm
  )

  komDT <- reactive({
    kommune()[, ..nyvar]
  })

  output$tbl <- DT::renderDT(
    datatable(komDT(), options = list(
      pageLength = 20,
      searching = FALSE,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#0c52c9', 'color': '#fff'});",
        "}"))
      )
  )

  ##kommuner fane
  kommdt <- reactive({
    if (input$valgKomm == 1){
      komdt <-  subset(DT, kommnr == input$komm1)
      komUT <- komdt[!duplicated(kom_b4), ..komvar]
    } else {
      komdt = subset(DT, kom_b4 == input$komm2)
      komUT <- komdt[!duplicated(kommnr), ..komvar]
    }

    return(komUT)
  })

  output$komtbl <- DT::renderDT(
    datatable(kommdt(), options = list(
      dom = 't',
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#0c52c9', 'color': '#fff'});",
        "}"))
      )
  )

  ## min max for slider
  output$komm_slid <- renderUI({

    maxGK <- sum(!duplicated(kommune()$geo2020))
    sliderInput("gknr", "Antall grunnkrets å vise:",
                min = 1, max = maxGK, value = c(1, maxGK), step = 1)

  })


  ## Kommuner lister
  komVec <- reactive({
    komVec = unique(kommune()$geo2020)
    valgGK = komVec[input$gknr[1]:input$gknr[2]]

    valgGK
  })


  ## ## TEST
  ## output$test <- renderText({
  ##   paste(input$gknr, collapse = ":")
  ## })

  output$sti <- collapsibleTree::renderCollapsibleTree({


    subDT <- kommune()[geo2020 %in% komVec(), ]

    subDT[, .("Antall grunnskrets" = .N), keyby = list(kommnr, geo2020, geo_b4)] %>%
      collapsibleTree::collapsibleTreeSummary(
        hierarchy = c("geo2020", "geo_b4"),
        root = "kommnr",
        attribute = "Antall grunnskrets",
        zoomable = FALSE
      )
  })


  output$downall <- downloadHandler(
    filename = function() {
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      fwrite(DT, file, sep=";")
    })

  output$downsub <- downloadHandler(
    filename = function(){
      paste("kommnr-", input$komm, "-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      fwrite(kommune(), file, sep=";")
    }
  )

}



shinyApp(ui = ui, server = server) # this launches your app
