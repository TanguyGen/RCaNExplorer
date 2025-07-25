#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyWidgets
#' @import rintrojs
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    fluidPage(
      introjsUI(),
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
            actionButton("help", "Press for instructions"),
            checkboxInput("show_node_labels", "Show Node Labels", TRUE),
            checkboxInput("show_edge_labels", "Show Flux Labels", FALSE),
            introBox(
              fileInput("rcanfile", "Choose CaNSample File", accept = ".RData"),
              data.step = 1,
              data.intro="Upload here your CaN RData file or you can also use the already implemented one from Planque et al. 2024."
            ),
            downloadButton("savedata", "Download RData"),
            width = 4
          ),
          br(),
          wellPanel(
            introBox(
            selectInput(
              "Typegraph",
              "Choose a variable",
              c("Select an option...",
                "Biomass Series",
                "Consumption Series",
                "Predation and Catch Series",
                "Ratio Consumption/Biomass",
                "Ratio Production/Biomass",
                "Mortality Series",
                "Flux Series")
            ),
            data.step = 2,
            data.intro = "Select here the variable you want to visualise."),
            checkboxInput("groupspecies", "Sum the Biomasses/Flux?", FALSE),
            textInput(
              "groupname",
              "Name your group",
              value = "",
              width = NULL,
              placeholder = NULL
            ),introBox(
            actionButton("continue", "Continue", class = "btn-continue"),
            data.step = 4,
            data.intro = "Click on this button once you have choosen a variable and ecosystem components."
            ),
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
              introBox(
              visNetwork::visNetworkOutput("Foodweb", height = "80vh")%>% 
              shinycssloaders::withSpinner(type = 6),
              data.step = 3,
              data.intro = "This is the foodweb newtwork. Click on one or more species/fluxes from the network to select which ones to visualise (Hold Ctrl/cmd to select multiple elements at once).
              The species highlighted in the foodweb are the ones resolved for the selected variable."
              )
            ),
            tabPanel(
              "Plots",
              br(),
              introBox(
              plotOutput("Plots", height = "auto") %>% shinycssloaders::withSpinner(type = 6),
              data.step = 5,
              data.intro = "The plots will appear on this pannel. You will be automatically redirected here when clicking on the  'Continue' button."
              )
            ),
            tabPanel(
              "Metadata",
              introBox(
              br(),
              introBox(
              fileInput("metadatafile", "Input a metadata file", accept = ".csv"),
              data.step = 8,
              data.intro = "...that you will be able to reupload next time you use the app."
              ),
              br(),
              introBox(
              DT::DTOutput("table_info"),
              data.step = 7,
              data.intro = "Here you can edit the graphical features of the app and plots. 
              The variable FullName gives the name of the ecosystem component whcih will be used for the plotting. To edit it double-click on it and make your changes. 
              The Color is the colour used in the plotting of the ecosystem components and of the nodes of the ecosystem network when no image is given. To edit it, click on the palette and change the colour. 
              Lastly, to change the image used to represent the ecosystem component in the foodweb, click on the corresponding Upload button and add an image in format png, jpg, jpeg or svg. The new image should appear in the Image column."
              ),
              br(),
              introBox(
              downloadButton("saveinfo", "Download new graphical metadata"),
              data.step=8,
              data.intro="Here you can download the edited metadata file..."
              )
            ),
            data.step = 6,
            data.intro = "On this metadata tab, you will be able to edit the graphical features of the plots and of the food web network."
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
