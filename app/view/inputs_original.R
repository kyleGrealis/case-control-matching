#' @concept Displaying sidebar user inputs to strategize the case-control matching. This will be a collection of conditionalPanels, selectInputs, and a numericInput. The available options will be updated as the user navigates through available variables.

box::use(
  bsicons[bs_icon],
  bslib[tooltip],
  shiny[actionButton, conditionalPanel, fileInput, moduleServer, NS,
        numericInput, reactive, renderText, renderUI, req, selectInput,
        span, tagList, uiOutput, observeEvent, textOutput],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("idVariable")),
    uiOutput(ns("caseControl")),
    uiOutput(ns("numericVariable")),
    uiOutput(ns("numRangeCat")),
    uiOutput(ns("thirdVariable")),
    uiOutput(ns("matchButton"))
  )
}

#' @export
server <- function(id, newFile) {
  moduleServer(id, function(input, output, server) {
    ns <- NS(id)

    output$idVariable <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      selectInput(
        ns("idVariable"), "Choose ID variable.",
        choices = c("", names(newFile())),
        selected = ""
      )
    })

    output$caseControl <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      selectInput(
        ns("caseControl"),
        span("Choose case-control variable.", bs_icon("info-circle-fill")),
        choices = c(
          "",
          setdiff(
            newFile() |> purrr::keep(is.numeric) |> names(),
            c(input$idVariable)
          )
        )
      )|>
        tooltip(
          "Be sure that this is coded where 0=\"Control\" and 1=\"Case\"",
          placement = "top"
        )
    })

    output$numericVariable <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      conditionalPanel(
        condition = "input.caseControl != ''",
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
      )
    })

    output$numRangeCat <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      conditionalPanel(
        condition = "input.numericVariable != ''",
        numericInput(
          ns("numRange"),
          span("Choose matching range of numeric variable.", bs_icon("info-circle-fill")),
          min = 0, max = 100, step = 1, value = 1
        ) |>
          tooltip(
            "Choosing 0 will cause exact matching. If a case has an age of 30,
          then it will only be matched to controls that are also 30 years old.
          If you choose 1, then the case will be matched to controls aged 29 to 31.",
          placement = "top"
          ),
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
            ) # setiff
          ) # choices
        )
      ) # conditionalPanel
    })

    output$thirdVariable <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      conditionalPanel(
        condition = "input.categoricalVariable != ''",
        selectInput(
          ns("thirdVariable"), "Choose categorical matching variable.",
          choices = c(
            "leave blank if not needed" = "blank",
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
        ) # selectInput
      )
    })

    output$matchButton <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      conditionalPanel(
        condition = "input.categoricalVariable != ''",
        actionButton("matchButton", "Match!")
      )
    })

    # collect all inputs as a reactive to be used in the matching algorithm
    # reactive({
    #   list(
    #     idVariable          = input$idVariable,
    #     caseControl         = input$caseControl,
    #     numericVariable     = input$numericVariable,
    #     numRange            = as.numeric(input$numRange),
    #     categoricalVariable = input$categoricalVariable,
    #     thirdVariable       = input$thirdVariable
    #   )
    # })
  })
}
