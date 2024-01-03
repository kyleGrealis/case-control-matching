box::use(
  bslib[page_fixed],
  shiny[mainPanel, moduleServer, NS, reactiveVal,
        sidebarLayout, sidebarPanel],
)

box::use(
  app/logic/matching_algo,
  app/view/data,
  app/view/data_info,
  app/view/inputs,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fixed(
    sidebarLayout(
      sidebarPanel(
        data$ui(ns("data_file")),
        inputs$ui(ns("inputs"))
      ),
      mainPanel(
        data_info$ui(ns("info")),
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    newFile <- reactiveVal()
    newFile <- data$server("data_file")
    data_info$server("info", newFile)
    inputs <- inputs$server("inputs", newFile)
  })
}
