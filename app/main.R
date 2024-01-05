box::use(
  bslib[bs_theme, nav_item, nav_menu, nav_panel, nav_spacer, navset_card_tab,
        page_fillable],
  dplyr[across, mutate, where],
  shiny[icon, mainPanel, moduleServer, NS, observeEvent, observe, reactiveVal,
        sidebarLayout, sidebarPanel, tags, a, div, uiOutput, renderUI, req,
        reactive, renderTable, tableOutput],
)

box::use(
  app/logic/functions[format_numbers],
  app/view/algo,
  app/view/data,
  app/view/data_info,
  app/view/inputs,
  app/view/footer,
)

link_shiny <- tags$a(
  shiny::icon("github"), "Shiny",
  href = "https://github.com/rstudio/shiny", target = "_blank"
)
link_posit <- tags$a(
  shiny::icon("r-project"), "Posit",
  href = "https://posit.co", target = "_blank"
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    # set default bootstrap version
    theme = bs_theme(version = 5),
    style = "margin: auto; max-width: 1500px;",
    div(
      sidebarLayout(
        sidebarPanel(
          data$ui(ns("data_file")),
          inputs$ui(ns("inputs"))
        ),
        mainPanel(
          navset_card_tab(
            nav_panel("Data", data_info$ui(ns("info"))),
            nav_panel(
              "Results",
              navset_card_tab(
                nav_panel(
                  "Matched Data",
                  tableOutput(ns("algo"))
                  # "matched results will go here"
                ),
                nav_panel(
                  "Cases",
                  "this will be the unmatched Cases"
                ),
                nav_panel(
                  "Controls",
                  "this will be the unmatched Controls"
                )
              )
            ), # nav_panel
            nav_spacer(),
            nav_menu(
              title = "Links",
              nav_item(link_shiny),
              nav_item(link_posit)
            )
          ) # navset_card_tab
        ) # mainPanel
      ), # sidebarLayout
      div(footer$footer)
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    newFile <- data$server("data_file")
    data_info$server("info", newFile)
    inputs <- inputs$server("inputs", newFile)

    # Get results from algo$server
    results <- algo$server("algo", newFile, inputs)

    output$algo <- renderTable({
      # Use req to wait for results to be available
      req(results())

      if (is.null(results())) {
        return(NULL)
      }
      results() |>
        mutate(across(where(is.numeric), format_numbers))
    })
  })
}



# # Create a reactive value to store the button click status
# buttonClicked <- reactiveVal(FALSE)
#
# observeEvent(inputs()$matchButton, {
#   # Update the reactive value when the button is clicked
#   buttonClicked(TRUE)
# })
#
# observe({
#   if (buttonClicked()) {
#     matching_algo$do_matching(
#       newFile,
#       inputs()$idVariable, inputs()$caseControl,
#       inputs()$numericVariable, inputs()$numRange,
#       inputs()$categoricalVariable, inputs()$ratio,
#       inputs()$thirdVariable
#     )
#   }
# })
