box::use(
  bslib[bs_theme, card, card_body, nav_item, nav_menu, nav_panel, nav_spacer,
        navset_card_tab, nav_select, page_fillable],
  shiny[a, addResourcePath, div, icon, includeHTML, mainPanel, moduleServer, NS,
        sidebarLayout, sidebarPanel, tags, observe, reactiveVal, titlePanel],
  shinyjs[useShinyjs],
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
  shiny::icon("github"), "EpiMatch Repo",
  href = "https://github.com/kyleGrealis/case-control-matching/", target = "_blank"
)
link_website <- tags$a(
  shiny::icon("laptop"), "My webpage",
  href = "https://kylegrealis.github.io/", target = "_blank"
)

addResourcePath("how-to", "app/static/how-to")

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    useShinyjs(),
    theme = bs_theme(version = 5),
    titlePanel("EpiMatch: case-control matching made easy"),
    div(
      sidebarLayout(
        sidebarPanel(
          id = "my-sidebar",
          data$ui(ns("data_file")),
          inputs$ui(ns("inputs"))
        ),
        mainPanel(
          navset_card_tab(
            # must be namespace'd or the observe() in the server will not
            # change the tabs automatically
            id = ns("main-tabset"),
            nav_panel(
              "How to",
              tags$iframe(
                src = "how-to/case-control-how-to.html",
                width = "100%", height = "700px"
              )
            ),
            nav_panel(
              "Data",
              card(
                data_info$ui(ns("info"))
              )
            ),
            nav_panel(
              "Results",
              navset_card_tab(
                nav_panel(
                  id = "matched_data",
                  "Matched Data",
                  card(
                    matched_results$ui(ns("matched"))
                  )
                ),
                nav_panel(
                  id = "cases",
                  "Cases",
                  card(
                    unmatched_results$ui(ns("cases"))
                  )
                ),
                nav_panel(
                  id = "controls",
                  "Controls",
                  card(
                    unmatched_results$ui(ns("controls"))
                  )
                ),
                nav_panel(
                  id = "details",
                  "Details",
                  card(
                    algo$ui(ns("algo"))
                  )
                )
              ) # navset_card_tab
            ), # nav_panel
            nav_spacer(),
            nav_menu(
              title = "Links",
              nav_item(link_website),
              nav_item(link_shiny)
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

    # create an empty reactive value. this will be used to conditionally
    # change the focus to the "data" tab
    fileData <- reactiveVal()
    newFile <- data$server("data_file")

    # if the reactiveVal for the fileData has something in it, change to the
    # "data" tab automatically
    observe({
      fileData(newFile())
      if (!is.null(fileData())) {
        nav_select(id = "main-tabset", selected = "Data")
      }
    })

    # present the data information (number of rows & colums)
    data_info$server("info", newFile)

    # render the inputs and collect their values as a list of reactives
    inputs <- inputs$server("inputs", newFile)

    # create an empty reactive value. this will be used to conditionally
    # change the focus to the "results" tab
    finishedMatching <- reactiveVal()
    results <- algo$server("algo", newFile, inputs)

    # if the reactiveVal for the finishedMatching has something in it, change
    # to the "results" tab automatically
    observe({
      finishedMatching(results())
      if (!is.null(finishedMatching())) {
        nav_select(id = "main-tabset", selected = "Results")
      }
    })

    # present the matched and unsuccessful matches
    matched_results$server("matched", newFile, inputs, results)
    unmatched_results$server("cases", newFile, inputs, results, "cases")
    unmatched_results$server("controls", newFile, inputs, results, "controls")

  })
}
