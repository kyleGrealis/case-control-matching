library(shiny)
library(bslib)

shinyApp(

  ui <- page_fixed(
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose any File", accept = NULL),
        uiOutput("idVariable"),
        conditionalPanel(
          # TODO: add the ability for the user to convert cases-control to 0/1
          # currently the test data is already coded 0=control, 1=case
          condition = "input.idVariable != ''",
          uiOutput("caseControl"),
        ),
        conditionalPanel(
          condition = "input.caseControl != ''",
          uiOutput("numericVariable")
        ),
        conditionalPanel(
          condition = "input.numericVariable != ''",
          uiOutput("numVarRange")
        ),
        conditionalPanel(
          condition = "input.numericVariable != ''",
          uiOutput("explanation")
        ),
        conditionalPanel(
          condition = "input.numericVariable != ''",
          uiOutput("categoricalVariable")
        ),
        conditionalPanel(
          condition = "input.categoricalVariable != ''",
          uiOutput("thirdVariable")
        ),
        conditionalPanel(
          condition = "input.categoricalVariable != ''",
          uiOutput("matchButton")
        )
      ),
      mainPanel(
        uiOutput("tableSummaryMessage"),
        tableOutput("contents"),
        uiOutput("tableFootnote")
      )
    )
  ),

  server <- function(input, output) {
    file <- reactive(input$file1)
    newFile <- reactive({
      if (is.null(file())) {
        return(NULL)
      }
      rio::import(file()$datapath)
    })

    output$contents <- renderTable({
      if (is.null(file())) {
        return(NULL)
      }
      head(newFile(), n = 20)
    })

    output$tableSummaryMessage <- renderUI({
      if (is.null(file())) {
        return(NULL)
      }
      glue::glue(
        "The table has {nrow(newFile())} rows and {ncol(newFile())} columns."

      )
    })

    output$tableFootnote <- renderUI({
      if (is.null(file())) {
        return(NULL)
      }
      glue::glue(
        "Displaying only the first 20 rows."
      )
    })

    output$idVariable <- renderUI({
      if (is.null(file())) {
        return(NULL)
      }
      list(
        tooltip(
          bsicons::bs_icon("info-circle", title = "About tooltips"),
          "Text shown in the tooltip."
        ),
        selectInput(
          "idVariable", "Choose ID variable.",
          choices = c("", names(newFile())),
          selected = ""
        )
      )
    })

    output$caseControl <- renderUI({
      if (is.null(file())) {
        return(NULL)
      }
      selectInput(
        "caseControl", "Choose case-control variable.",
        choices = c(
          "",
          setdiff(
            newFile() |> purrr::keep(is.numeric) |> names(),
            c(input$idVariable)
          )
        )
      )
    })

    output$numericVariable <- renderUI({
      if (is.null(file())) {
        return(NULL)
      }
      selectInput(
        "numericVariable", "Choose numeric matching variable.",
        choices = c(
          "",
          setdiff(
            newFile() |> purrr::keep(is.numeric) |> names(),
            c(
              input$idVariable,
              input$caseControl
            )
          )
        )
      )
    })

    output$numVarRange <- renderUI({
      if (is.null(file()) | is.null(input$numericVariable)) {
        return(NULL)
      }
      numericInput(
        "numVarRange", "Choose matching range of numeric variable.",
        min = 0, max = 100, step = 1, value = 1
      )
    })

    output$explanation <- renderUI({
      if (is.null(file()) | is.null(input$numericVariable)) {
        return(NULL)
      }
      renderText(
        "Choosing 0 will cause exact matching. If a case has an age of 30,
        then it will only be matched to controls that are also 30 years old.
        If you choose 1, then the case will be matched to controls aged 29 to 31."
      )
    })

    output$categoricalVariable <- renderUI({
      if (is.null(file())) {
        return(NULL)
      }
      selectInput(
        "categoricalVariable", "Choose categorical matching variable.",
        choices = c(
          "",
          setdiff(
            newFile() |> purrr::keep(is.character) |> names(),
            c(
              input$idVariable,
              input$caseControl,
              input$numericVariable
            )
          )
        )
      )
    })

    output$thirdVariable <- renderUI({
      if (is.null(file())) {
        return(NULL)
      }
      selectInput(
        "thirdVariable", "Choose categorical matching variable.",
        choices = c(
          "leave blank if not needed" = "",
          setdiff(
            newFile() |> names(),
            c(
              input$idVariable,
              input$caseControl,
              input$numericVariable,
              input$categoricalVariable
            )
          )
        )
      )
    })

    output$matchButton <- renderUI({
      if (is.null(file())) {
        return(NULL)
      }
      actionButton("matchButton", "Match cases to controls")
    })

  }

)
