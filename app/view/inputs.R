#' @concept Displaying sidebar user inputs to strategize the case-control matching. This will be a collection of conditionalPanels, selectInputs, and a numericInput. The available options will be updated as the user navigates through available variables.

box::use(
  bsicons[bs_icon],
  bslib[tooltip],
  shiny[actionButton, conditionalPanel, fileInput, moduleServer, NS, numericInput, reactive, renderText, renderUI, req, selectInput, tagList, uiOutput],
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
      list(
        bslib::tooltip(
          bsicons::bs_icon("info-circle", title = "About tooltips"),
          "Text shown in the tooltip."
        ),
        selectInput(
          ns("idVariable"), "Choose ID variable.",
          choices = c("", names(newFile())),
          selected = ""
        )
      )
    })

    output$caseControl <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      conditionalPanel(
        condition = "input.idVariable != ''",
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
          ns("numVarRange"), "Choose matching range of numeric variable.",
          min = 0, max = 100, step = 1, value = 1
        ),
        renderText(
          "Choosing 0 will cause exact matching. If a case has an age of 30,
          then it will only be matched to controls that are also 30 years old.
          If you choose 1, then the case will be matched to controls aged 29 to 31."
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
  })
}
