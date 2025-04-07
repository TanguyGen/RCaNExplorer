#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      tags$head(
        includeCSS(app_sys("www/styles.css")),
        tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700&display=swap"),
      ),
      withTags({
        div(
          class = "header",
          checked = NA,
          h1(),
          h2("RCaN Visual Tool")
        )
      }),
      
      sidebarLayout(
        
        # Sidebar Panel (Collapsible)
        sidebarPanel(
          id = "sidebar",
          width = 3,  # Default width when visible
          wellPanel(
            checkboxInput("show_node_labels", "Show Node Labels", TRUE),
            checkboxInput("show_edge_labels", "Show Flux Labels", FALSE),
            fileInput("rcanfile", "Choose CaNSample File", accept = ".RData"),
            downloadButton("savedata", "Download RData"),
            width = 4
          ),
          br(),
          wellPanel(
            selectInput("Typegraph", "Choose a visualization", 
                        c("Select an option...")),
            checkboxInput("groupspecies", "Sum the Biomasses/Flux?", FALSE),
            textInput("groupname", "Name your group", value = "", width = NULL, placeholder = NULL),
            width = 4
          ),
          
        ),
        
        # Main Content with Tabs
        mainPanel(
          width = 9,  # Takes the rest of the space
          tabsetPanel(
            id = "main_tabs",
            tabPanel("Network", visNetwork::visNetworkOutput("Foodweb", height = "80vh")), 
            tabPanel("Plots", 
                     br(),
                     plotOutput("Plots", height = "auto")%>% shinycssloaders::withSpinner(type = 6)
            ) 
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "RCaNvisualtool"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
