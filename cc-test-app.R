library(dplyr)
library(shiny)
library(bslib)

shinyApp(

  ui <- page_fixed(
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose any File", accept = NULL),
        uiOutput("idVariable"),
        conditionalPanel(
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

    # Custom function to format numbers -- was originally adding decimals to
    # whole numbers. May need to be re-addressed
    format_numbers <- function(x) {
      if (all(x == floor(x))) {
        return(format(x, nsmall = 0, scientific = FALSE))
      } else {
        return(x)
      }
    }
    output$contents <- renderTable({
      if (is.null(file())) {
        return(NULL)
      }
      # Apply the formatting to the numeric columns
      df <- newFile() |>
        mutate(across(where(is.numeric), format_numbers))

      head(df, n = 20)
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
      selectInput(
        "idVariable", "Choose ID variable.",
        choices = c("", names(newFile())),
        selected = ""
      )
    })

    output$caseControl <- renderUI({
      if (is.null(file())) {
        return(NULL)
      }
      selectInput(
        "caseControl",
        span("Choose case-control variable.", bs_icon("info-circle-fill")),
        choices = c(
          "",
          setdiff(
            newFile() |> purrr::keep(is.numeric) |> names(),
            c(input$idVariable)
          )
        )
      ) |>
        tooltip(
          "Be sure that this is coded where 0=\"Control\" and 1=\"Case\"",
          placement = "top"
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
          ) # setdiff
        ) # choices
      )
    })

    output$numVarRange <- renderUI({
      if (is.null(file()) | is.null(input$numericVariable)) {
        return(NULL)
      }
      numericInput(
        "numVarRange",
        span(
          "Choose matching range of numeric variable.",
          bs_icon("info-circle-fill")
        ),
        min = 0, max = 100, step = 1, value = 1
      ) |>
        tooltip(
          "Choosing 0 will cause exact matching. If a case has an age of 30,
          then it will only be matched to controls that are also 30 years old.
          If you choose 1, then the case will be matched to controls aged 29 to 31.",
          placement = "top"
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
          ) # setdiff
        ) # choices
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
          ) # setdiff
        ) # choices
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
