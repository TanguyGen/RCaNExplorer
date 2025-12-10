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
      theme=bslib::bs_theme(
        bootswatch = "minty",
        bg = "#FDFFFF",
        fg = "#00171F",
        primary = "#005086",
        secondary = "#08a4a4",
        success = "#629460",
        base_font = bslib::font_google("Inter")
      ),
      introjsUI(),
      tags$head(
        includeCSS(app_sys("www/styles.css"))
      ),
      withTags({
        div(class = "header-bar",
            div(class = "header-left",
                img(src = "www/rcan_logo.png", class = "logo_rcan")
            ),
            div(class = "header-center",
                span("RCaN Explorer", class = "app-title")
            ),
            div(class = "header-right",
                img(src = "www/inrae_logo.png", class = "logo_inrae")),
            img(src = "www/IMR_logo.jpg", class = "logo_imr") 
        )
      }),
      sidebarLayout(
        # Sidebar Panel (Collapsible)
        sidebarPanel(
          id = "sidebar",
          width = 3,
          # Default width when visible
          wellPanel(
            actionButton("help", "Tutorial", class = "btn-blue"),
            br(),
            br(),
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
                "Biomass",
                "Consumption",
                "Predation and Fisheries",
                "Ratio Consumption/Biomass",
                "Ratio Production/Biomass",
                "Mortality and Growth",
                "Fluxes")
            ),
            data.step = 2,
            data.intro = "Select here the variable you want to visualise."),
            checkboxInput("groupspecies", "Group the ecosystem components", FALSE),
            textInput(
              "groupname",
              "Name your group",
              value = "",
              width = NULL,
              placeholder = NULL
            ),introBox(
            actionButton("continue", "Continue", class = "btn-blue"),
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
            type="tabs",
            tabPanel(
              "Network",
              class="network",
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
              downloadButton("savePlot", "Download Plot"), 
              plotOutput("Plots", height = "auto") %>% shinycssloaders::withSpinner(type = 6),
              data.step = 5,
              data.intro = "The plots will appear on this pannel. You will be automatically redirected here when clicking on the  'Continue' button."
              )
            ),
            tabPanel(
              "Download series",
              br(),
              introBox(
                DT::DTOutput("table_series"),
                br(),
                downloadButton("saveseries", "Download RData"),
                data.step = 5,
                data.intro = "The plots will appear on this pannel. You will be automatically redirected here when clicking on the  'Continue' button."
              )
            ),
            tabPanel(
              "Metadata",
              introBox(
              data.step = 6,
              data.intro = "On this metadata tab, you will be able to edit the graphical features of the plots and of the food web network.",
              br(),
              introBox(
              fileInput("metadatafile", "Input a metadata file", accept = ".csv"),
              data.step = 12,
              data.intro = "...that you will be able to reupload next time you use the app."
              ),
              introBox(
                data.step = 7,
                data.intro = "Here you can edit the graphical features of the app and plots.",
                introBox(
                  data.step = 8,
                  data.intro = "The 'FullName' column gives the name of the ecosystem component used in plots. Double-click to edit.",
                  
                  introBox(
                    data.step = 9,
                    data.intro = "The 'Colour' column defines the plotting colour of components. Click to change it.",
                    
                    introBox(
                      data.step = 10,
                      data.intro = "Use the 'Upload' column to add an image for each ecosystem component (PNG, JPG, SVG).",
                      
                      DT::DTOutput("table_info")
                    )
                  )
                )
              ),
              br(),
              br(),
              introBox(
              downloadButton("saveinfo", "Download new graphical metadata"),
              data.step=11,
              data.intro="Here you can download the edited metadata file..."
              )
            )
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
#' @importFrom golem add_resource_path activate_js bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))
  tags$head(bundle_resources(
    path = app_sys("app/www"), 
    app_title = "RCaNvisualtool")
  )
}
