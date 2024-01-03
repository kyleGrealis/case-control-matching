#' @concept Displaying sidebar user inputs to strategize the case-control matching. This will be a collection of conditionalPanels, selectInputs, and a numericInput. The available options will be updated as the user navigates through available variables.

box::use(
  bsicons[bs_icon],
  bslib[tooltip],
  shiny[actionButton, conditionalPanel, fileInput, moduleServer, NS,
        numericInput, reactive, renderText, renderUI, req, selectInput,
        span, tagList, uiOutput, observeEvent, textOutput, reactiveVal,
        validate, need],
  shinyjs[useShinyjs],
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("idVariable")),
    conditionalPanel(
      condition = "input.idVariable === ''",
      ns = ns,
      "This is a test and should show once the data is loaded." # not showing
    ),
    conditionalPanel(
      condition = "input.idVariable !== ''",
      ns = ns,
      uiOutput(ns("caseControl")) # currently displays after data is loaded
    )
  )
}

#' @export
server <- function(id, newFile) {
  # moduleServer(id, function(input, output, server) {
  # change from server -> session per SO answer
  moduleServer(id, function(input, output, session) {
    message("Module ", id, " has been activated.")
    # ns <- NS(id)
    ns <- session$ns  # suggestion from SO

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
      # }
    })

  })
}
