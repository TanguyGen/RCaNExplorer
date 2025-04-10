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
  options(shiny.maxRequestSize = 1000 * 1024 ^ 2) #Increase download size limit
  
  Info_table <- read.csv(system.file("app/www", "Info_table.csv", package = 'RCaNExplorer')) #Import the metadata associated to each species (Color, FullName, Representation of the biomass?)
  
  data <- reactiveValues(CaNSample = NULL, CaNSample_long = NULL) #Create a reactive value that will contain the data
  
  observe({
    if (!is.null(input$rcanfile) &&
        length(input$rcanfile$datapath) > 0) {
      #If a file was downloaded
      
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
      
    } else if (is.null(data$CaNSample)) {
      #If no RCaN file in input and no foodweb already downloaded, input a standard food web
      e <- new.env()#Create a new environment
      
      if (!file.exists("CaNSample20240306.Rdata")) {
        #If the file does not exist, load it from its repository
        load(
          file = url(
            "https://ftp.nmdc.no/nmdc/IMR/modeldata/CaNSample20240306.Rdata"
          ),
          envir = e
        )
      } else {
        #else, load the existing file
        load("CaNSample20240306.Rdata", envir = e)
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
  
  transform_CaNSample <- function(CaNSample) {
    #Function to create CaNSample_long
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
    
    img_dir <- system.file("app/www/img", package = "RCaNExplorer")
    existing_images <- list.files(img_dir)
    
    #Create the nodes of the foodweb network
    nodes <- tibble::tibble(ID = list_element) %>%
      left_join(Info_table) %>%
      mutate(
        id = ID,
        #Give them an ID
        label = case_when(#Show their ID as label only when the user chose to show the labels
          input$show_node_labels ~ id, TRUE ~ ""),
        shape = case_when(
          #If there is an image existig for the id, put an image else, a sphere
          paste0(id, ".png") %in% existing_images ~ "image",
          TRUE ~ "dot"
        ),
        image = sprintf("www/img/%s.png", id),
        #Images are in inst/app/www in the format <ID>.png
        x = data$CaNSample$CaNmod$components_param$X * 1000,
        #Get the x and y in a bigger scale to have more space between nodes
        y = data$CaNSample$CaNmod$components_param$Y * 1000,
        font.bold = case_when(#Put a bigger font for species inside the model domain, resolved in terms of biomass ad fluxes
          Biomass ~ 22, !Biomass ~ 14)
      )
    #Create the links between the nodes
    edges <- tibble::tibble(
      id = paste0(
        data$CaNSample$CaNmod$fluxes_def$From,
        "_",
        data$CaNSample$CaNmod$fluxes_def$To
      )
    ) %>% #Get the list of consumers and predator and bind them into one variable
      mutate(
        label = case_when(input$show_edge_labels ~ id, #Show the id as label only when the user chose to show the labels
                          TRUE ~ ""),
        from = data$CaNSample$CaNmod$fluxes_def$From,
        to = data$CaNSample$CaNmod$fluxes_def$To
      )
    
    #Create the network from the nodes and edges
    visNetwork::visNetwork(nodes, edges) %>%
      visNetwork::visEdges(arrows = list(to = TRUE)) %>% #Put arrows to show the direction of the fluxes
      visNetwork::visInteraction(multiselect = TRUE) %>% #Allow the selection of multiple elements at once (clicking ctl or cmd)
      visNetwork::visEvents(
        #Save the selected nodes id into input$selectedNodes
        selectNode = "function(nodes) {
          var selectedNodes = this.getSelectedNodes();
          if (selectedNodes.length > 0) {
            Shiny.onInputChange('selected_type', 'node');
            Shiny.onInputChange('selected_components', selectedNodes);
            console.log('Node selected:', selectedNodes);
          }
        }",
        #Save the selected edges id into input$selectedEdges
        selectEdge = "function(edges) {
          var selectedEdges = this.getSelectedEdges();
          if (selectedEdges.length > 0 && this.getSelectedNodes().length === 0) {
            Shiny.onInputChange('selected_type', 'edge');
            Shiny.onInputChange('selected_components', selectedEdges);
            console.log('Edge selected:', selectedEdges);
          }
        }",
        #Save the positions of the nodes when moved into input$positions
        dragEnd = "function(params) {
          if (params.nodes.length > 0) {
            var positions = this.getPositions();
            Shiny.onInputChange('node_positions', positions);
          }
        }"
      ) %>%
      visNetwork::visPhysics(enable = FALSE) %>%  #Don't allow the entire network to move when moving a node
      visNetwork::visNodes(font = list(size = 20),
                           shapeProperties = list(useImageSize = TRUE)) #Use the image sizes and increase the font of the labels
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
  
  
  #Depending if we select a node, an edge or nothing give graph options
  observeEvent(input$selected_type, {
    if (input$selected_type == "node") {
      updateSelectInput(
        session,
        "Typegraph",
        choices = c(
          "Select an option...",
          "Biomass Series",
          "Consumption Series",
          "Predation and Catch Series",
          "Ratio Consumption/Biomass",
          "Ratio Production/Biomass"
        ),
        selected = "Select an option..."
      )
    } else if (input$selected_type == "edge") {
      updateSelectInput(
        session,
        "Typegraph",
        choices = c("Select an option...", "Flux Series"),
        selected = "Select an option..."
      )
    } else {
      updateSelectInput(
        session,
        "Typegraph",
        choices = c("Select an option..."),
        selected = "Select an option..."
      )
    }
  })
  
  #Download the current CaNSample file with new positions
  output$savedata <- downloadHandler(
    filename = "CaNSample.RData",
    #Name of the saved RData file
    content = function(file) {
      CaNSample <- data$CaNSample
      CaNSample$CaNmod$components_param$X <- Positions$x #Assign the saved positions to the CaNSample
      CaNSample$CaNmod$components_param$Y <- Positions$y
      save(CaNSample, file = file)  #We call CaNSample the variable inside of RData
    }
  )
  
  #Plot the data depending on which choice of visualisation we made and save the plot into a reactive object
  plot_obj <- reactive({
    req(input$Typegraph, input$selected_components)
    
    ecosystem_components <- input$selected_components
    
    #Plot Series of biomass
    if (input$Typegraph == "Biomass Series") {
      Biomasscheck <- filter(Info_table, ID %in% input$selected_components) #Get the metadata to check if the biomass of the nodes is resolved
      
      if (!all(Biomasscheck$Biomass)) {
        #If some nodes biomass are not resolved, make a notification showing which ones were not represented
        showNotification(paste0(
          "The biomass of ",
          paste(Biomasscheck$FullName[Biomasscheck$Biomass == FALSE], collapse = ", "),
          " is/are not resolved so will not be presented."
        ))
      }
      #Call the function to create Biomass plots
      BiomassSeries(
        data$CaNSample_long,
        ecosystem_components,
        info = Info_table,
        group = input$groupspecies,
        grouplabel = input$groupname,
        session = session
      )
      
    } else if (input$Typegraph == "Consumption Series") {
      #Call the function to create Consumptions plots
      ConsumptionSeries(
        data$CaNSample_long,
        ecosystem_components,
        info = Info_table,
        group = input$groupspecies,
        grouplabel = input$groupname,
        session = session
      )
      
    } else if (input$Typegraph == "Predation and Catch Series") {
      #Call the function to create Predation plots
      PredationSeries(
        data$CaNSample_long,
        ecosystem_components,
        info = Info_table,
        group = input$groupspecies,
        grouplabel = input$groupname,
        session = session
      )
      
    } else if (input$Typegraph == "Flux Series") {
      #Call the function to create fluxes plots
      FluxSerie(
        data$CaNSample_long,
        ecosystem_components,
        info = Info_table,
        group = input$groupspecies,
        grouplabel = input$groupname,
        session = session
      )
      
    } else if (input$Typegraph == "Ratio Consumption/Biomass") {
      #Call the function to create Ratio Consumption/Biomass plots
      RatioConsumptionBiomass(
        data$CaNSample_long,
        ecosystem_components,
        info = Info_table,
        group = input$groupspecies,
        grouplabel = input$groupname,
        session = session
      )
      
    } else if (input$Typegraph == "Ratio Production/Biomass") {
      #Call the function to create Ratio Production/Biomass plots
      RatioProductionBiomass(
        data$CaNSample_long,
        ecosystem_components,
        info = Info_table,
        group = input$groupspecies,
        grouplabel = input$groupname,
        session = session
      )
    }
  })
  
  #Render the plot from the reactive value plot_obj
  output$Plots <- renderPlot({
    plot_obj()
  }, height = reactive({
    #Make sure the window size increases with the number of plots shown
    width <- session$clientData$output_Plots_width
    num_plots <- if (input$groupspecies)
      1
    else
      length(input$selected_components)
    if (is.null(width))
      return(400)
    if (num_plots == 0)
      return(400)
    if (num_plots == 1)
      width / 2
    else
      width * ceiling(num_plots / 2)
  }))
  
  #outputOptions(output, "Plots", suspendWhenHidden = FALSE)#Keep running the plot code even when we are not on the plot window
}
