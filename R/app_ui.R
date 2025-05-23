#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyWidgets
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    fluidPage(
      tags$head(
        includeCSS(app_sys("www/styles.css")),
        tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700&display=swap"),
      ),
      withTags({
        div(class = "header",
            checked = NA,
            h1(),
            h2("RCaN Explorer"))
      }),
      
      sidebarLayout(
        # Sidebar Panel (Collapsible)
        sidebarPanel(
          id = "sidebar",
          width = 3,
          # Default width when visible
          wellPanel(
            checkboxInput("show_node_labels", "Show Node Labels", TRUE),
            checkboxInput("show_edge_labels", "Show Flux Labels", FALSE),
            fileInput("rcanfile", "Choose CaNSample File", accept = ".RData"),
            downloadButton("savedata", "Download RData"),
            width = 4
          ),
          br(),
          wellPanel(
            selectInput(
              "Typegraph",
              "Choose a visualization",
              c("Select an option...")
            ),
            checkboxInput("groupspecies", "Sum the Biomasses/Flux?", FALSE),
            textInput(
              "groupname",
              "Name your group",
              value = "",
              width = NULL,
              placeholder = NULL
            ),
            actionButton("continue", "Continue", class = "btn-continue"),
            width = 4
          )
        ),
        
        # Main Content with Tabs
        mainPanel(
          width = 9,
          # Takes the rest of the space
          tabsetPanel(
            id = "menu",
            tabPanel(
              "Network",
              div(
                style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
                dropdownButton(
                  inputId = "helpbutton",
                  label = NULL,
                  icon = icon("circle-question"),
                  circle = TRUE,
                  right=FALSE,
                  tooltip = tooltipOptions(title = "Help"),
                  status = "default",
                  
                  tags$div(
                    tags$h4("How to Use"),
                    tags$p("• Upload your CaN RData file using the left panel or use the already implemented one."),
                    tags$p("• Click on one or more species/fluxes from the network to select which ones to visualise (hold Ctrl/cmd to select multiple elements at once)."),
                    tags$p("• Use the Metadata tab to change the colors or names of the species appearing in the plots."),
                    tags$p("• Choose a type of visualization in the left pannel «Choose a visualisation»."),
                    tags$p("• Move to the plot tab to visualise the outputs.")
                  )
                )
              ),
              visNetwork::visNetworkOutput("Foodweb", height = "80vh")%>% 
              shinycssloaders::withSpinner(type = 6)
            ),
            tabPanel(
              "Plots",
              br(),
              plotOutput("Plots", height = "auto") %>% shinycssloaders::withSpinner(type = 6)
            ),
            tabPanel(
              "Metadata",
              br(),
              fileInput("metadatafile", "Input a metadata file", accept = ".csv"),
              br(),
              DT::DTOutput("table_info"),
              br(),
              downloadButton("saveinfo", "Download new graphical metadata")
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
  add_resource_path("www", app_sys("app/www"))
  favicon()
  tags$head(bundle_resources(path = app_sys("app/www"), app_title = "RCaNvisualtool")
  )
}
