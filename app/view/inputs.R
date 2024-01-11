#' @concept Displaying sidebar user inputs to strategize the case-control matching. This will be a collection of conditionalPanels, selectInputs, and a numericInput. The available options will be updated as the user navigates through available variables.

box::use(
  bslib[tooltip],
  purrr[keep],
  shiny[actionButton, conditionalPanel, div, moduleServer, NS, numericInput,
        reactive, renderUI, selectInput, tagList, uiOutput],
  shiny.fluent[Slider.shinyInput]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
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
        ns("idVariable"), "Participant ID variable",
        choices = c("", names(newFile())),
        selected = ""
      )
    })

    # NOTE: all conditionalPanels render once the previous selection
    # evaluates to not being NULL or an empty string. this walks the user
    # through the inputs and hopes to keep an interactive feel

    output$caseControl <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      conditionalPanel(
        ns = ns,
        condition = "input.idVariable !== ''",
        selectInput(
          ns("caseControl"),
          "Case-control variable",
          choices = c(
            "",
            setdiff(
              newFile() |> purrr::keep(is.numeric) |> names(),
              c(input$idVariable)
            ) # setdiff
          ) # choices
        ), # selectInput
        style = "display: none;"
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
          ns("numericVariable"), "Numeric variable",
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
        ), # selectInput
        style = "display: none;"
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
          "Numeric variable matching tolerance",
          min = 0, max = 100, step = 1, value = NA
        ) |>
        tooltip(
          "A value of \"0\" matches cases & controls having the same numeric
          value. A value of \"5\" will perform matching based on a range
          (case age=40, eligible controls aged 35-45).",
          placement = "top"
        ),
        style = "display: none;"
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
          ns("categoricalVariable"), "Categorical variable",
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
        ), # selectInput
        style = "display: none;"
      ) # conditionalPanel
    })

    output$ratio <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      conditionalPanel(
        ns = ns,
        condition = "input.categoricalVariable !== ''",
        "Controls-to-case ratio",
        Slider.shinyInput(
          ns("ratio"),
          min = 1, max = 5, step = 1, value = 2
        ),
        style = "display: none;"
      ) # conditionalPanel
    })

    output$thirdVariable <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      conditionalPanel(
        ns = ns,
        condition = "input.categoricalVariable !== ''",
        selectInput(
          ns("thirdVariable"), "Second categorical variable",
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
        ), # selectInput
        style = "display: none;"
      ) # conditionalPanel
    })

    output$matchButton <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      conditionalPanel(
        ns = ns,
        condition = "input.categoricalVariable !== ''",
        actionButton(ns("matchButton"), "Match!"),
        style = "display: none;"
      )
    })

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
        matchButton         = input$matchButton
      )
    })

  })
}
