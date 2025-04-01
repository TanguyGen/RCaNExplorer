library(shiny)
library(visNetwork)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinyjs)
library(plotly)
library(stringr)
library(tibble)
library(coda)
library(purrr)
library(patchwork)
library(shinycssloaders)

Info_table<-read.csv("Data/Info_table.csv")
ui <- fluidPage(
  tags$head(
    includeCSS("www/styles.css"),
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
        tabPanel("Network", visNetworkOutput("Foodweb", height = "80vh")), 
        tabPanel("Plots", 
                 br(),
                 plotOutput("Plots", height = "auto")%>% withSpinner(type = 6)
                 ) 
      )
    )
  )
)


server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=1000*1024^2)
  
  # Store in reactiveValues to ensure persistence
  data <- reactiveValues(CaNSample = NULL,CaNSample_long=NULL,numgroups=NULL)
  
  observe({
    if (!is.null(input$rcanfile) && length(input$rcanfile$datapath) > 0) {
      # Clean current data
      data$CaNSample <- NULL
      data$CaNSample_long <- NULL
      gc()
      
      # Load into temp environment
      e <- new.env()
      load(input$rcanfile$datapath, envir = e)
      
      loaded_objs <- ls(envir = e)
      
      if (length(loaded_objs) != 1) {
        showNotification("Please upload an RData file with only one object.", type = "error")
        return()
      }
      
      # Extract the one object (no matter the name)
      obj <- e[[loaded_objs[[1]]]]
      
    } else if (is.null(data$CaNSample)) {
      # Load default if nothing uploaded
      e <- new.env()
      if(file.exists("Data/CaNSample20240306.Rdata")==FALSE){
        load(file=url("https://ftp.nmdc.no/nmdc/IMR/modeldata/CaNSample20240306.Rdata"),envir=e) # data published as in Planque et al. 2024
      } else {
        load("Data/CaNSample20240306.Rdata", envir=e)
      }
      loaded_objs <- ls(envir = e)
      
      if (length(loaded_objs) != 1) {
        showNotification("Default RData must contain only one object.", type = "error")
        return()
      }
      
      obj <- e[[loaded_objs[[1]]]]
      
    } else {
      return()  # Nothing to do
    }
    
    # Assign the object to reactiveValues
    data$CaNSample <- obj
    data$CaNSample_long <- transform_CaNSample(data$CaNSample)
  })
  
  # Function to transform CaNSample (avoids repeating code)
  transform_CaNSample <- function(CaNSample) {
    as_tibble(as.matrix(CaNSample$mcmc)) %>%
      mutate(Sample_id = 1:nrow(as.matrix(CaNSample$mcmc))) %>%
      pivot_longer(
        cols = -Sample_id,
        names_to = c("Var", "Year"),
        names_pattern = "(.*)\\[(.*)\\]",
        values_to = 'value'
      )
  }
  
  
  Positions<-reactiveValues(x=NULL,y=NULL)
  
  observe({
    Positions$x = data$CaNSample$CaNmod$components_param$X
    Positions$y = data$CaNSample$CaNmod$components_param$Y
    
  })
  
  observeEvent(input$node_positions, {
    Positions$x = sapply(input$node_positions, `[[`, "x")/1000
    Positions$y = sapply(input$node_positions, `[[`, "y")/1000
  })

  
  output$Foodweb <- renderVisNetwork({
    req(data$CaNSample)  # Ensure data is loaded before rendering
    
    list_element <- data$CaNSample$CaNmod$components_param$Component
    nodes <- tibble(ID = list_element) %>%
      left_join(Info_table)%>%
      mutate(
        id=ID,
        label = case_when(
        input$show_node_labels ~ id,
        TRUE ~ ""
      ), 
      shape = "image", 
      image=sprintf("img/%s.png", id),
      x = data$CaNSample$CaNmod$components_param$X*1000,
      y = data$CaNSample$CaNmod$components_param$Y*1000,
      font.bold=case_when(
        Biomass ~ 22,
        !Biomass ~ 14
      )
      )
    edges <- tibble(id = paste0(data$CaNSample$CaNmod$fluxes_def$From,"_",data$CaNSample$CaNmod$fluxes_def$To)) %>%
      tibble(label=case_when(
        input$show_edge_labels ~ id,
        TRUE ~ ""
      ),
             from = data$CaNSample$CaNmod$fluxes_def$From, 
             to = data$CaNSample$CaNmod$fluxes_def$To)
    
    visNetwork(nodes, edges) %>%
      visEdges(arrows = list(to = TRUE)) %>%
      visInteraction(multiselect=TRUE) %>%
      visEvents(
        selectNode = "function(nodes) {
          var selectedNodes = this.getSelectedNodes();
          if (selectedNodes.length > 0) {
            Shiny.onInputChange('selected_type', 'node');
            Shiny.onInputChange('selected_components', selectedNodes);
            console.log('Node selected:', selectedNodes);
          }
        }",
        selectEdge = "function(edges) {
          var selectedEdges = this.getSelectedEdges();
          if (selectedEdges.length > 0 && this.getSelectedNodes().length === 0) {
            Shiny.onInputChange('selected_type', 'edge');
            Shiny.onInputChange('selected_components', selectedEdges);
            console.log('Edge selected:', selectedEdges);
          }
        }",
        dragEnd = "function(params) {
      if (params.nodes.length > 0) {
        var positions = this.getPositions();
        Shiny.onInputChange('node_positions', positions);
      }
    }"
      ) %>% 
      visPhysics(
        stabilization = TRUE,
        enable=FALSE
      ) %>%
      visNodes( font = list(size = 20),shapeProperties = list(useImageSize = TRUE)) %>%

      visEvents(
        stabilizationIterationsDone = "function() { this.physics.physicsEnabled = false; }"
      )
  })
  
  
  observeEvent(input$selected_type, {
    if (input$selected_type == "node") {
      updateSelectInput(session, "Typegraph",
                        choices = c("Select an option...", "Biomass Series", "Consumption Series","Predation and Catch Series","Ratio Consumption/Biomass"),
                        selected = "Select an option...")
    } else if (input$selected_type == "edge") {
      updateSelectInput(session, "Typegraph",
                        choices = c("Select an option...", "Flux Series"),
                        selected = "Select an option...")
    } else{
      updateSelectInput(session, "Typegraph",
                        choices = c("Select an option..."),
                        selected = "Select an option...")
    }
  })
  
  output$savedata <- downloadHandler(
    filename = "CaNSample.RData",
    content = function(file) {
      CaNSample<-data$CaNSample
      CaNSample$CaNmod$components_param$X<-Positions$x
      CaNSample$CaNmod$components_param$Y<-Positions$y
      save(CaNSample,file=file)
    }
  )
  
  plot_obj <- reactive({  #to make sure plots are used only once
    req(input$selected_components)
    
    req(input$selected_components)
    ecosystem_components <- input$selected_components
    
    if (input$Typegraph=="Biomass Series"){
      Biomasscheck <- Info_table %>%
        filter(ID %in% input$selected_components)
      
      if (!all(Biomasscheck$Biomass)){
        showNotification(paste0("The biomass of ",paste(Biomasscheck$FullName[Biomasscheck$Biomass==FALSE], collapse = ", ")," is/are not resolved so will not be presented."))
      }
      BiomassSeries(data$CaNSample_long,ecosystem_components,info=Info_table,group=input$groupspecies,grouplabel=input$groupname,session=session)
      
    } else if (input$Typegraph=="Consumption Series"){
      
      ConsumptionSeries(data$CaNSample_long,ecosystem_components,info=Info_table,group=input$groupspecies,grouplabel=input$groupname,session=session)
      
    } else if (input$Typegraph=="Predation and Catch Series"){
      
      PredationSeries(data$CaNSample_long,ecosystem_components,info=Info_table,group=input$groupspecies,grouplabel=input$groupname,session=session)
      
    }else if (input$Typegraph=="Flux Series"){
      
      FluxSerie(data$CaNSample_long,ecosystem_components,info=Info_table,session=session)
    }else if (input$Typegraph=="Flux Series"){
      
      RatioConsumptionBiomass(data$CaNSample_long,ecosystem_components,info=Info_table,info=Info_table,session=session)
    }
    
  })
  
  output$Plots <- renderPlot({
    plot_obj()
  }, height = reactive({
    width <- session$clientData$output_Plots_width
    num_plots <- if (input$groupspecies) 1 else length(input$selected_components)
    
    if (is.null(width)) return(400)
    if (num_plots == 0) return(400)
    
    if (num_plots == 1) width / 2 else width * ceiling(num_plots / 2)
  }))

  
  outputOptions(output, "Plots", suspendWhenHidden = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)
