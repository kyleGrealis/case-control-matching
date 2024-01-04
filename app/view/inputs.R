#' @concept Displaying sidebar user inputs to strategize the case-control matching. This will be a collection of conditionalPanels, selectInputs, and a numericInput. The available options will be updated as the user navigates through available variables.

box::use(
  bsicons[bs_icon],
  bslib[tooltip],
  shiny[actionButton, conditionalPanel, fileInput, isolate, moduleServer, NS,
        numericInput, reactive, reactiveVal, renderText, renderUI, selectInput,
        span, tagList, uiOutput, observeEvent],
  shinyjs[disable, useShinyjs],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    uiOutput(ns("idVariable")),
    uiOutput(ns("caseControl")),
    uiOutput(ns("numericVariable")),
    uiOutput(ns("numRange")),
    uiOutput(ns("categoricalVariable")),
    uiOutput(ns("ratio")),
    uiOutput(ns("thirdVariable")),
    uiOutput(ns("matchButton"))
  )
}

#' @export
server <- function(id, newFile) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
      conditionalPanel(
        ns = ns,
        condition = "input.idVariable !== ''",
        selectInput(
          ns("caseControl"),
          span("Choose case-control variable.", bs_icon("info-circle-fill")),
          choices = c(
            "",
            setdiff(
              newFile() |> purrr::keep(is.numeric) |> names(),
              c(input$idVariable)
            ) # setdiff
          ) # choices
        ) |> # selectInput
          tooltip(
            "Be sure that this is coded where 0=\"Control\" and 1=\"Case\"",
            placement = "top"
          )
      ) # conditionalPanel
    })

    output$numericVariable <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      conditionalPanel(
        ns = ns,
        condition = "input.caseControl !== ''",
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
            ) # setdiff
          ) # choices
        ) # selectInput
      ) # conditionalPanel
    })

    output$numRange <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      conditionalPanel(
        ns = ns,
        condition = "input.numericVariable !== ''",
        numericInput(
          ns("numRange"),
          span(
            "Choose matching range of numeric variable.",
            bs_icon("info-circle-fill")
          ),
          min = 0, max = 100, step = 1, value = NA
        ) |>
        tooltip(
          "A value of \"0\" will perform exact matching (example: the case &
          control(s) are the same age). A value of \"1\" will perform matching
          based on a range (case age 40 and eligible controls aged 39-41).",
          placement = "top"
        )
      ) # conditionalPanel
    })

    output$categoricalVariable <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      conditionalPanel(
        ns = ns,
        condition = "input.numRange !== null",
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
            ) # setdiff
          ) # choices
        ) # selectInput
      ) # conditionalPanel
    })

    output$ratio <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      conditionalPanel(
        ns = ns,
        condition = "input.categoricalVariable !== ''",
        numericInput(
          ns("ratio"),
          span(
            "Choose matching ratio.",
            bs_icon("info-circle-fill")
          ),
          min = 1, max = 10, step = 1, value = NA
        ) |>
        tooltip(
          "A value of \"1\" will attempt 1:1 matching (one case is
          matched to one control). A value of \"2\" will perform 1:2 matching
          (matching one case to two controls).",
          placement = "top"
        )
      ) # conditionalPanel
    })

    output$thirdVariable <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      conditionalPanel(
        ns = ns,
        condition = "input.ratio !== null",
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
      ) # conditionalPanel
    })

    output$matchButton <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      conditionalPanel(
        ns = ns,
        condition = "input.ratio !== null",
        actionButton(ns("matchButton"), "Match!")
      )
    })

    # # collect all inputs as a reactive to be used in the matching algorithm
    # reactive({
    #   list(
    #     idVariable          = input$idVariable,
    #     caseControl         = input$caseControl,
    #     numericVariable     = input$numericVariable,
    #     numRange            = as.numeric(input$numRange),
    #     categoricalVariable = input$categoricalVariable,
    #     ratio               = input$ratio,
    #     thirdVariable       = input$thirdVariable,
    #     matchButton         = input$matchButton
    #   )
    # })

    # Create a reactive value to store the button click status
    buttonClicked <- reactiveVal(FALSE)

    # Use conditionalPanel to hide the button after it's clicked
    conditionalPanel(
      condition = "output.buttonClicked == false",
      actionButton(ns("matchButton"), "Match")
    )

    observeEvent(input$matchButton, {
      # Update the reactive value when the button is clicked
      buttonClicked(TRUE)
    })

    output$buttonClicked <- reactive({ buttonClicked() })

    # Return the reactive value with the inputs
    reactive({
      list(
        idVariable          = input$idVariable,
        caseControl         = input$caseControl,
        numericVariable     = input$numericVariable,
        numRange            = as.numeric(input$numRange),
        categoricalVariable = input$categoricalVariable,
        ratio               = input$ratio,
        thirdVariable       = input$thirdVariable,
        matchButton         = buttonClicked()
      )
    })

  })
}
