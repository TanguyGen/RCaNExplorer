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
  
  data <- reactiveValues(CaNSample = NULL,
                         CaNSample_long = NULL,
                         Info = NULL) #Create a reactive value that will contain the data
  
  observe({
    if (!is.null(input$rcanfile) &&
        length(input$rcanfile$datapath) > 0) {
      #If a file was downloaded
      
      #Empty the reactive value
      data$CaNSample <- NULL
      data$CaNSample_long <- NULL
      data$Info <- NULL
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
  
  observe({
    if (!is.null(input$metadatafile) &&
        length(input$metadatafile$datapath) == 1) {
      
      table <- read.csv(input$metadatafile$datapath)
      
      if (!identical(colnames(table), c("ID", "FullName", "Color"))) {
        stop("The columns of the metadata must be ID,FullName,Color")
      } else if (!setequal(table$ID,
                           data$CaNSample$CaNmod$components_param$Component)) {
        #if the IDs from the table and from the CaNSample are not matching -> error
        stop(
          "The species from the metadata must be the same as the ones from the CaNSample RData"
        )
      } else Info_table <- table
    } else if (is.null(data$Info)) {
        table <- read.csv(system.file("app/www", "Info_table.csv", package = 'RCaNExplorer'))
        if (!setequal(table$ID,
                      data$CaNSample$CaNmod$components_param$Component)){
          table<-table%>%
            filter(ID %in% data$CaNSample$CaNmod$components_param$Component)
          
          missing_ids <- setdiff(data$CaNSample$CaNmod$components_param$Component, table$ID)
          
          #Colors from paletteer::paletteer_d("ggsci::default_igv")
          colors_vector <- c(
            "#5050ff", "#ce3d32", "#749b58", "#f0e685", "#466983", "#ba6338", "#5db1dd",
            "#802268", "#6bd76b", "#d595a7", "#924822", "#837b8d", "#c75127", "#d58f5c",
            "#7a65a5", "#e4af69", "#3b1b53", "#cddeb7", "#612a79", "#ae1f63", "#e7c76f",
            "#5a655e", "#cc9900", "#99cc00", "#a9a9a9", "#cc9900", "#99cc00", "#33cc00",
            "#00cc33", "#00cc99", "#0099cc", "#0a47ff", "#4775ff", "#ffc20a", "#ffd147",
            "#990033", "#991a00", "#996600", "#809900", "#339900", "#00991a", "#009966",
            "#008099", "#003399", "#1a0099", "#660099", "#990080", "#d60047", "#ff1463",
            "#00d68f", "#14ffb1"
          )
          palette <- rep(colors_vector, length.out = length(missing_ids))
          
          missing_table <- data.frame(ID = missing_ids, FullName=missing_ids,Color = palette, stringsAsFactors = FALSE)
          
          Info_table <- table %>%
            rbind(missing_table)
        }else{
          Info_table<-table
        }
    }
      else {
        return()
      }
    
    Info_table$Image <- vapply(Info_table$ID, function(id) {
      img_path <- sprintf("img/%s.png", id)
      if (file.exists(file.path("inst/app", img_path))) {
        img_tag <- sprintf('<img src="%s" width="60px" />', img_path)
      } else {
        img_tag <- '<span style="color:gray;">No image</span>'
      }
      
      img_tag
    }, character(1))
    
    Info_table$Upload <- vapply(Info_table$ID, function(id) {
      sprintf(
        '<label class="custom-upload">Upload<input type="file" class="image-upload" data-id="%s" accept=".png,.jpg,.jpeg,.svg" style="display:none;" /></label>',
        id
      )
    }, character(1))
    
    data$Info <- Info_table
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
    req(data$Info)
    Info_table <- data$Info
    req(data$CaNSample)  #Make sure we have data
    
    list_element <- data$CaNSample$CaNmod$components_param$Component #Get the ecosystem components
    img_dir <- system.file("app/www/img", package = "RCaNExplorer")
    existing_images <- list.files(img_dir)
    #Create the nodes of the foodweb network
    nodes <- Info_table%>% 
      mutate(
        id = ID,
        #Give them an ID
        label = case_when(#Show their ID as label only when the user chose to show the labels
          input$show_node_labels ~ id, TRUE ~ ""),
        shape = case_when(
          grepl("^<img src=", Info_table$Image) ~ "image",
          paste0(id, ".png") %in% existing_images ~ "image",
          TRUE ~ "dot"
        ),
        color = Color,
        image = ifelse(
          grepl("^<img src=", Info_table$Image),
          sub("^<img src=\"([^\"]+)\".*$", "\\1", Info_table$Image),
          sprintf("img/%s.png", id)
        ),
        #Images are in inst/app/www in the format <ID>.png
        x = data$CaNSample$CaNmod$components_param$X * 1000,
        #Get the x and y in a bigger scale to have more space between nodes
        y = data$CaNSample$CaNmod$components_param$Y * 1000,
        font.bold =22
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
        to = data$CaNSample$CaNmod$fluxes_def$To,
        color = "grey"
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
  
  observeEvent(input$continue, {
    req(input$Typegraph, input$selected_components)
    updateTabsetPanel(session, "menu", selected = "Plots")
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
    
    Info_table <- data$Info
    
    ecosystem_components <- input$selected_components
    
    #Plot Series of biomass
    if (input$Typegraph == "Biomass Series") {
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
    else if (num_plots == 1) {
      return(800)
    } else{
      width * ceiling(num_plots / 3)
    }
    
  }))
  
  output$table_info <- DT::renderDT({
   dt<-data$Info
    DT::datatable(
      dt,
      editable = TRUE,
      rownames = FALSE,
      selection = "none",
      escape = FALSE,
      options = list(
        pageLength=15,
        columnDefs = list(
        list(
          targets = 2,
          render = htmlwidgets::JS(
            "function(data, type, row, meta) {",
            "  return '<input type=\"color\" value=\"' + data + '\" class=\"color-picker\">';",
            "}"
          )
        )
      ))
    ) %>%
      htmlwidgets::onRender(
        "
        // Handle color picker changes
        $(document).on('change', '.color-picker', function() {
          var color = $(this).val();  // Get the new color value
          var row = $(this).closest('tr').index();  // Get the row index
          var col = 2;  // Column index of the color picker
      
          Shiny.setInputValue('color_change', {row: row, color: color}, {priority: 'event'});
        });
      
        // Handle image uploads
        $(document).on('change', '.image-upload', function(e) {
          var file = this.files[0];
          var id = $(this).data('id');
          var row = $(this).closest('tr').index(); 
      
          if (file && ['image/png', 'image/jpeg', 'image/svg+xml'].includes(file.type)) {
            var reader = new FileReader();
            reader.onload = function(evt) {
             Shiny.setInputValue('image_upload', {
              id: id,
              name: file.name,
              type: file.type,
              content: evt.target.result,
              row:row
        }, {priority: 'event'});
      };
      reader.readAsDataURL(file);
    }
  });
        "
      )
  })
  
  # Handle the color change on the server side
  observeEvent(input$color_change, {
    row <- input$color_change$row + 1
    color <- input$color_change$color
    
    # Update the color in data$Info
    data$Info[row, 3] <- color
  })
  
  observeEvent(input$image_upload, {
    upload <- input$image_upload
    req(upload$content)
    row <- upload$row + 1  # Adjust for 1-based indexing
    data$Info[row, "Image"] <- sprintf('<img src="%s" width="40px" />', upload$content)
  })
  
  observeEvent(input$table_info_cell_edit, {
    row  <- input$table_info_cell_edit$row
    clmn <- input$table_info_cell_edit$col + 1
    data$Info[row, clmn] <- input$table_info_cell_edit$value
  })
  
  output$saveinfo <- downloadHandler(
    filename =  paste0("Info_table", format(Sys.time(), "%d-%m-%Y"), ".csv"),
    #Name of the saved RData file
    content = function(file) {
      Info <- data$Info%>% 
        select(-Image, -Upload)
      write.csv(Info, file, row.names = FALSE)
    }
  )
}
