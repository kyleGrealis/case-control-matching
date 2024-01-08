box::use(
  bslib[bs_theme, nav_item, nav_menu, nav_panel, nav_spacer, navset_card_tab,
        page_fillable],
  shiny[a, div, icon, mainPanel, moduleServer, NS, sidebarLayout, sidebarPanel,
        tags,],
)

box::use(
  app/view/algo,
  app/view/data,
  app/view/data_info,
  app/view/inputs,
  app/view/footer,
  app/view/matched_results,
  app/view/unmatched_results
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
            nav_panel(
              id = "data",
              "Data",
              data_info$ui(ns("info"))
            ),
            nav_panel(
              id = "results",
              "Results",
              navset_card_tab(
                nav_panel(
                  id = "matched_data",
                  "Matched Data",
                  matched_results$ui(ns("matched"))
                ),
                nav_panel(
                  id = "cases",
                  "Cases",
                  unmatched_results$ui(ns("cases"))
                ),
                nav_panel(
                  id = "controls",
                  "Controls",
                  unmatched_results$ui(ns("controls"))
                )
              ), # navset_card_tab
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
    results <- algo$server("algo", newFile, inputs)

    matched_results$server("matched", results)
    unmatched_results$server("cases", newFile, inputs, results, "cases")
    unmatched_results$server("controls", newFile, inputs, results, "controls")

  })
}
