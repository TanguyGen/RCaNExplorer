#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import tidyr
#' @import coda
#' @noRd
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize=1000*1024^2)
  
  Info_table<-read.csv(system.file("app/www","Info_table.csv", package = 'RCaNExplorer')) #Import the metadata associated to each species (Color, FullName, Representation of the biomass?)
  
  data <- reactiveValues(CaNSample = NULL, CaNSample_long = NULL) #Create a reactive value that will contain the data
  
  observe({
    if (!is.null(input$rcanfile) && length(input$rcanfile$datapath) > 0) { #If a file was downloaded
      
      #Empty the reactive value
      data$CaNSample <- NULL
      data$CaNSample_long <- NULL
      
      e <- new.env()  #Create a new environment
      load(input$rcanfile$datapath, envir = e)       #Load the file in the new environment
      loaded_objs <- ls(envir = e) # Get all the variables from the environment
      
      if (length(loaded_objs) != 1) {
        showNotification("Please upload an RData file with only one object.", type = "error")
        return()
      }
      
      obj <- e[[loaded_objs[[1]]]] #Obj=CaNSample
      
    } else if (is.null(data$CaNSample)) { #If no RCaN file in input and no foodweb already downloaded, input a standard food web
      e <- new.env()#Create a new environment
      
      if (!file.exists("Data/CaNSample20240306.Rdata")) { #If the file does not exist, load it from its repository
          load(file = url("https://ftp.nmdc.no/nmdc/IMR/modeldata/CaNSample20240306.Rdata"), envir = e)
        } else { #else, load the existing file
          load("Data/CaNSample20240306.Rdata", envir = e)
        }
      loaded_objs <- ls(envir = e) # Get all the variables from the environment
      
      #Check that there is only 1 object in the environment
      if (length(loaded_objs) != 1) {
        showNotification("Default RData must contain only one object.", type = "error")
        return()
      }
      
      obj <- e[[loaded_objs[[1]]]] #Obj=CaNSample
      
    } else {
      return()
    }
    
    data$CaNSample <- obj #Put the loaded object into the reactive value
    data$CaNSample_long <- transform_CaNSample(data$CaNSample) #Create CaNSample_long, a 3 columns version of CaNSample to simplify further handling
  })
  
  transform_CaNSample <- function(CaNSample) { #Function to create CaNSample_long
    tibble::as_tibble(as.matrix(CaNSample$mcmc)) %>%
      mutate(Sample_id = 1:nrow(as.matrix(CaNSample$mcmc))) %>%
      pivot_longer(
        cols = -Sample_id,
        names_to = c("Var", "Year"),
        names_pattern = "(.*)\\[(.*)\\]",
        values_to = 'value'
      )
  }
  
  #Render the interactive foodweb
  output$Foodweb <- visNetwork::renderVisNetwork({
    req(data$CaNSample)  #Make sure we have data
    
    list_element <- data$CaNSample$CaNmod$components_param$Component #Get the ecosystem components
    
    existing_images<-list.files("inst/app/www/img")

    nodes <- tibble::tibble(ID = list_element) %>% #Create the nodes of the foodweb network
      left_join(Info_table) %>%
      mutate(
        id = ID, #Give them an ID
        label = case_when( #Show their ID as label only when the user chose to show the labels
          input$show_node_labels ~ id,
          TRUE ~ ""
        ),
        shape = case_when(
          paste0(id,".png") %in% existing_images ~ "image",
          TRUE ~ "dot"
        ),
        image = sprintf("www/img/%s.png", id),
        x = data$CaNSample$CaNmod$components_param$X * 1000,
        y = data$CaNSample$CaNmod$components_param$Y * 1000,
        font.bold = case_when(
          Biomass ~ 22,
          !Biomass ~ 14
        )
      )
    edges <- tibble::tibble(id = paste0(data$CaNSample$CaNmod$fluxes_def$From, "_", data$CaNSample$CaNmod$fluxes_def$To)) %>%
      mutate(
        label = case_when(
          input$show_edge_labels ~ id,
          TRUE ~ ""
        ),
        from = data$CaNSample$CaNmod$fluxes_def$From,
        to = data$CaNSample$CaNmod$fluxes_def$To
      )
    
    visNetwork::visNetwork(nodes, edges) %>%
      visNetwork::visEdges(arrows = list(to = TRUE)) %>%
      visNetwork::visInteraction(multiselect = TRUE) %>%
      visNetwork::visEvents(
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
      visNetwork::visPhysics(stabilization = TRUE, enable = FALSE) %>%
      visNetwork::visNodes(font = list(size = 20), shapeProperties = list(useImageSize = TRUE)) %>%
      visNetwork::visEvents(
        stabilizationIterationsDone = "function() { this.physics.physicsEnabled = false; }"
      )
  })
  
  #Put the positions of the nodes into a reactive values to save them later
  Positions <- reactiveValues(x = NULL, y = NULL)
  
  #Assign the initial positions to the ones from CaNSample
  observe({
    Positions$x <- data$CaNSample$CaNmod$components_param$X
    Positions$y <- data$CaNSample$CaNmod$components_param$Y
  })
  
  #If we move a node, assign the new node position to the reactive value
  observeEvent(input$node_positions, {
    Positions$x <- sapply(input$node_positions, `[[`, "x") / 1000
    Positions$y <- sapply(input$node_positions, `[[`, "y") / 1000
  })
  

  
  observeEvent(input$selected_type, {
    if (input$selected_type == "node") {
      updateSelectInput(session, "Typegraph",
                               choices = c("Select an option...", "Biomass Series", "Consumption Series", "Predation and Catch Series", "Ratio Consumption/Biomass","Ratio Production/Biomass"),
                               selected = "Select an option..."
      )
    } else if (input$selected_type == "edge") {
      updateSelectInput(session, "Typegraph",
                               choices = c("Select an option...", "Flux Series"),
                               selected = "Select an option..."
      )
    } else {
      updateSelectInput(session, "Typegraph",
                               choices = c("Select an option..."),
                               selected = "Select an option..."
      )
    }
  })
  
  output$savedata <- downloadHandler(
    filename = "CaNSample.RData",
    content = function(file) {
      CaNSample <- data$CaNSample
      CaNSample$CaNmod$components_param$X <- Positions$x
      CaNSample$CaNmod$components_param$Y <- Positions$y
      save(CaNSample, file = file)
    }
  )
  
  plot_obj <- reactive({
    req(input$selected_components)
    
    ecosystem_components <- input$selected_components
    
    if (input$Typegraph == "Biomass Series") {
      Biomasscheck <- filter(Info_table, ID %in% input$selected_components)
      
      if (!all(Biomasscheck$Biomass)) {
        showNotification(
          paste0(
            "The biomass of ",
            paste(Biomasscheck$FullName[Biomasscheck$Biomass == FALSE], collapse = ", "),
            " is/are not resolved so will not be presented."
          )
        )
      }
      
      BiomassSeries(data$CaNSample_long, ecosystem_components, info = Info_table, group = input$groupspecies, grouplabel = input$groupname, session = session)
      
    } else if (input$Typegraph == "Consumption Series") {
      
      ConsumptionSeries(data$CaNSample_long, ecosystem_components, info = Info_table, group = input$groupspecies, grouplabel = input$groupname, session = session)
      
    } else if (input$Typegraph == "Predation and Catch Series") {
      
      PredationSeries(data$CaNSample_long, ecosystem_components, info = Info_table, group = input$groupspecies, grouplabel = input$groupname, session = session)
      
    } else if (input$Typegraph == "Flux Series") {
      
      FluxSerie(data$CaNSample_long, ecosystem_components, info = Info_table, session = session)
      
    } else if (input$Typegraph == "Ratio Consumption/Biomass") {
      
      RatioConsumptionBiomass(data$CaNSample_long, ecosystem_components, info = Info_table, group= input$groupspecies,grouplabel=input$groupname,session = session)
      
    } else if (input$Typegraph == "Ratio Production/Biomass") {
      
      RatioProductionBiomass(data$CaNSample_long, ecosystem_components, info = Info_table, group= input$groupspecies,grouplabel=input$groupname,session = session)
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
