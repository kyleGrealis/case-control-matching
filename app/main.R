box::use(
  bsicons[bs_icon],
  bslib[page_fixed, tooltip],
  glue[glue],
  rio[import],
  shiny[actionButton, conditionalPanel, div, fileInput, mainPanel, moduleServer,
        NS, numericInput, selectInput, sidebarLayout, sidebarPanel, reactive,
        req, renderTable, renderText, renderUI, tableOutput, tags, uiOutput],
  utils[head]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fixed(
    sidebarLayout(

      sidebarPanel(
        fileInput(ns("file1"), "Choose any file", accept = NULL),
        uiOutput(ns("idVariable")),
        conditionalPanel(
          condition = "input.idVariable != ''",
          uiOutput(ns("caseControl"))
        ),
        conditionalPanel(
          condition = "input.caseControl != ''",
          uiOutput(ns("numericVariable"))
        ),
        conditionalPanel(
          condition = "input.numericVariable != ''",
          uiOutput(ns("numVarRange")),
          uiOutput(ns("explanation")),
          uiOutput(ns("categoricalVariable"))
        ),
        # conditionalPanel(
        #   condition = "input.numericVariable != ''",
        # ),
        # conditionalPanel(
        #   condition = "input.numericVariable != ''",
        # ),
        conditionalPanel(
          condition = "input.categoricalVariable != ''",
          uiOutput(ns("thirdVariable"))
        ),
        conditionalPanel(
          condition = "input.categoricalVariable != ''",
          uiOutput(ns("matchButton"))
        )
      ), # sidebarPanel

      mainPanel(
        uiOutput(ns("tableSummaryMessage")),
        tableOutput(ns("contents")),
        uiOutput(ns("tableFootnote"))
      ) # mainPanel

    ) # sidebarLayout
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
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
      selectInput(
        ns("idVariable"), "Choose ID variable.",
        choices = c("", names(newFile())),
        selected = ""
      )
      # bslib::tooltip(
      #   bsicons::bs_icon("info-circle", title = "About tooltips"),
      #   "Text shown in the tooltip."
      # )
    })

    output$caseControl <- renderUI({
      if (is.null(file())) {
        return(NULL)
      }
      selectInput(
        ns("caseControl"), "Choose case-control variable.",
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
        ns("numericVariable"), "Choose numeric matching variable.",
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
        ns("numVarRange"), "Choose matching range of numeric variable.",
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
        ns("categoricalVariable"), "Choose categorical matching variable.",
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
        ns("thirdVariable"), "Choose categorical matching variable.",
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
  }) # moduleServer
}
