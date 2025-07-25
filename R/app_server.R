#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import tidyr
#' @import coda
#' @import rintrojs
#' @noRd
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize = 1000 * 1024 ^ 2)
  
  #Store the data
  data <- reactiveValues(
    CaNSample = NULL,
    CaNSample_long = NULL,
    Info = NULL,
    Resolution = NULL,
    Resolved_components = NULL
  )
  
  #Function to simplify CaNSample object
  transform_CaNSample <- function(CaNSample) {
    m <- as.matrix(CaNSample$mcmc)
    fluxes_def <- CaNSample$CaNmod$fluxes_def
    tibble::as_tibble(m) |>
      mutate(Sample_id = seq_len(nrow(m))) |>
      pivot_longer(
        cols = -Sample_id,
        names_to = c("Var", "Year"),
        names_pattern = "(.*)\\[(.*)\\]",
        values_to = 'value'
      ) |>
      left_join(fluxes_def, by = c("Var" = "Flux")) |>
      mutate(
        FluxTrophic = Trophic == 1
      ) |>
      select(Sample_id, Var, Year, value, FluxTrophic)
  }
  
  #Function to load CaNSample object
  load_CaNSample <- function(path_or_url) {
    e <- new.env()
    load(path_or_url, envir = e)
    objs <- ls(envir = e)
    if (length(objs) != 1) {
      showNotification("RData must contain exactly one object.", type = "error")
      return(NULL)
    }
    e[[objs[[1]]]]
  }
  
  
  observe({
    obj <- NULL
    
    if (!is.null(input$rcanfile) && length(input$rcanfile$datapath) > 0) {
      obj <- load_CaNSample(input$rcanfile$datapath)
    } else if (is.null(data$CaNSample)) {
      local_file <- "CaNSample20240306.Rdata"
      if (!file.exists(local_file)) { #If the local file does not exist load from url
        obj <- load_CaNSample(url("https://ftp.nmdc.no/nmdc/IMR/modeldata/CaNSample20240306.Rdata"))
      } else { #if it exists load from local
        obj <- load_CaNSample(local_file)
      }
    }
    
    #Assign to the reactive data "data" to store it
    if (!is.null(obj)) { 
      data$CaNSample <- obj
      data$CaNSample_long <- transform_CaNSample(obj)
    }
  })
  
  #Check wich ecosystem components are resolved in terms of biomass, production and consumption
  observe({
    req(data$CaNSample, data$CaNSample_long)
    comp <- data$CaNSample$CaNmod$components_param$Component
    flux <- data$CaNSample$CaNmod$fluxes_def
    
    data$Resolution <- tibble(Component = comp) |>
      mutate(
        IsBiomass = Component %in% data$CaNSample_long$Var,
        IsConsummer = Component %in% flux$To,
        IsPredated = Component %in% flux$From
      )
  })
  
  #Filter the components which are resolved for the chosen representation
  observe({
    req(data$Resolution, input$Typegraph)
    type <- input$Typegraph
    
    data$Resolved_components <- data$Resolution |>
      filter(
        (type == "Biomass Series" & IsBiomass) |
          (type == "Consumption Series" & IsConsummer) |
          (type == "Predation and Catch Series" & IsPredated) |
          (type == "Ratio Consumption/Biomass" & IsConsummer & IsBiomass) |
          (type == "Ratio Production/Biomass" & IsPredated & IsBiomass) |
          (type == "Mortality Series" & IsBiomass & IsPredated & IsConsummer) |
          (type == "Flux Series")
      ) |>
      pull(Component)
  })
  
  #Add graphics if the components are not described in the package
  observe({
    #load the Info data frame containing the IDs, FullNames, colours and images of the ecosystem components
    if (!is.null(input$metadatafile) && length(input$metadatafile$datapath) == 1) { 
      table <- read.csv(input$metadatafile$datapath, stringsAsFactors = FALSE) #Read uploaded table
    } else if (is.null(data$Info)) { #load the default one
      table <- read.csv(system.file("app/www", "Info_table.csv", package = 'RCaNExplorer'), stringsAsFactors = FALSE)
    } else { #load it only once
      return() 
    }
    
    #Check wich IDs from CaNSample are described in the metadata table
    comp <- data$CaNSample$CaNmod$components_param$Component
    if (!setequal(table$ID, comp)) {
      table <- table |> filter(ID %in% comp)
      missing_ids <- setdiff(comp, table$ID)
      
      palette <- rep(c(
        "#5050ff", "#ce3d32", "#749b58", "#f0e685", "#466983", "#ba6338", "#5db1dd",
        "#802268", "#6bd76b", "#d595a7", "#924822", "#837b8d", "#c75127", "#d58f5c"
      ), length.out = length(missing_ids))
      #If not recognised give them the ID from CaNSample, pass the ID as the FullName also and give a colour from the palette
      table <- bind_rows(table, tibble(ID = missing_ids, FullName = missing_ids, Color = palette))
    }
    #Load the images from the package
    img_dir <- system.file("app/www/img", package = "RCaNExplorer")
    existing_images <- list.files(img_dir)
    
    #Check which imaes correspond to the Ids
    table$Image <- sprintf("www/img/%s.png", table$ID)
    table$Image <- ifelse(paste0(table$ID, ".png") %in% existing_images,
                          sprintf('<img src="%s" width="60px" />', table$Image),
                          '<span style="color:gray;">No image</span>') #If not existing assign a basic style
    
    #Add an upload button for the metadata
    table$Upload <- sprintf(
      '<label class="custom-upload">Upload<input type="file" class="image-upload" data-id="%s" accept=".png,.jpg,.jpeg,.svg" style="display:none;" /></label>',
      table$ID
    )
    #Save the new table
    data$Info <- table
  })
  
  #Create the ecosystem network representation
  output$Foodweb <- visNetwork::renderVisNetwork({
    req(data$Info, data$CaNSample)
    
    info <- data$Info
    resolved <- data$Resolved_components #Resolved ecosystem components in the chosen variable
    comp_param <- data$CaNSample$CaNmod$components_param #Components from CaNSample to use for the network
    
    #Create the nodes of the foodweb network
    nodes <- info %>%
      mutate(
        id = ID,
        is_resolved = ID %in% resolved, #Boolean if ecosystem component resolved
        label = if (isTRUE(input$show_node_labels)) ID else "", #if "Show Node Labels" ticked, show labels
        shape = ifelse(grepl("^<img", Image), "image", "dot"), #If image exists show image, else show a dot
        image = ifelse(grepl("^<img", Image), sub('^<img src="([^"]+)".*', "\\1", Image), paste0("www/img/", ID, ".png")), #load image
        opacity = ifelse(is_resolved, 1, 0.5), #If not resolved render the component more transparent
        labelHighlightBold = is_resolved, #Highlight text when component is selected only when resolved
        x = comp_param$X[match(ID, comp_param$Component)] * 1000, #position of the nodes
        y = comp_param$Y[match(ID, comp_param$Component)] * 1000, #position of the nodes
        font.bold = 22
      )
    
    flux_def <- data$CaNSample$CaNmod$fluxes_def #fluxes represented in the model
    
    edges <- tibble(
      id = paste0(flux_def$From, "_", flux_def$To),
      label = if (isTRUE(input$show_edge_labels)) paste0(flux_def$From, "_", flux_def$To) else "", #if "Show Flux Labels" ticked, show labels
      from = flux_def$From,
      to = flux_def$To,
      color = list(highlight=if_else(input$Typegraph == "Flux Series","black","grey")), #if "Flux series" picked highlight edges when selected
      selectionWidth = if (input$Typegraph == "Flux Series") 1 else 0, #if "Flux series" picked highlight edges when selected
      hoverWidth = if (input$Typegraph == "Flux Series") 1 else 0, #if "Flux series" picked highlight edges when selected
      arrowStrikethrough = FALSE #Stop the arrorws at the point of the arrow
    )
    
    #Create the network from the nodes and edges
    visNetwork::visNetwork(nodes, edges) |>
      visNetwork::visEdges(arrows = list(to = TRUE)) |> #Put arrows to show the direction of the fluxes
      visNetwork::visInteraction(multiselect = TRUE, keyboard = TRUE) |>  #Allow the selection of multiple elements at once (clicking ctl or cmd) and allow to move the plot with the arrows of the keyboard
      visNetwork::visEvents(
        #Save the selected nodes id into input$selectedNodes
        selectNode = htmlwidgets::JS("function(nodes) {
          var selectedNodes = this.getSelectedNodes();
          if (selectedNodes.length > 0) {
            Shiny.setInputValue('selected_type', 'node');
            Shiny.setInputValue('selected_components', selectedNodes);
          }
        }"),
        #Save the selected edges id into input$selectedEdges
        selectEdge = htmlwidgets::JS("function(edges) {
          var selectedEdges = this.getSelectedEdges();
          if (selectedEdges.length > 0 && this.getSelectedNodes().length === 0) {
            Shiny.setInputValue('selected_type', 'edge');
            Shiny.setInputValue('selected_components', selectedEdges);
          }
        }"),
        #Save the positions of the nodes when moved into input$positions
        dragEnd = htmlwidgets::JS("function(params) {
          if (params.nodes.length > 0) {
            var positions = this.getPositions();
            Shiny.setInputValue('node_positions', positions);
          }
        }")
      ) |>
      visNetwork::visPhysics(enable = FALSE) |>   #Don't allow the entire network to move when moving a node
      visNetwork::visNodes(font = list(size = 20), shapeProperties = list(useImageSize = TRUE)) #Use the image true size and increase the font of the labels
  })
  #Put the positions of the nodes into a reactive values to save them later
  Positions <- reactiveValues(x = NULL, y = NULL)
  
  #Assign the initial positions to the ones from CaNSample
  observe({
    req(data$CaNSample)
    comp_param <- data$CaNSample$CaNmod$components_param
    Positions$x <- comp_param$X
    Positions$y <- comp_param$Y
  })
  #If we move a node, assign the new node position to the reactive value
  observeEvent(input$node_positions, {
    Positions$x <- sapply(input$node_positions, `[[`, "x") / 1000
    Positions$y <- sapply(input$node_positions, `[[`, "y") / 1000
  })
  
  
  # ---- Tab Navigation ----
  observeEvent(input$continue, {
    req(input$Typegraph, input$selected_components)
    
    valid_selected <- intersect(input$selected_components, data$Resolved_components)
    if (length(valid_selected) > 0) {
      updateTabsetPanel(session, "menu", selected = "Plots")
    } else {
      showNotification("Please select at least one valid ecosystem component.", type = "error")
    }
  })
  
  # Move to plot table when clicking on "Continue" with valid ecosystem components
  output$savedata <- downloadHandler(
    filename = "CaNSample.RData",
    content = function(file) {
      CaNSample <- data$CaNSample
      CaNSample$CaNmod$components_param$X <- Positions$x
      CaNSample$CaNmod$components_param$Y <- Positions$y
      save(CaNSample, file = file)
    }
  )
  
  # Choice of variable to plot function 
  plot_dispatch <- list(
    "Biomass Series"             = BiomassSeries,
    "Consumption Series"         = ConsumptionSeries,
    "Predation and Catch Series" = PredationSeries,
    "Flux Series"                = FluxSerie,
    "Ratio Consumption/Biomass"  = RatioConsumptionBiomass,
    "Ratio Production/Biomass"   = RatioProductionBiomass,
    "Mortality Series"           = MortalitySeries
  )
  
  # ---- Reactive Plot Generator ----
  plot_obj <- reactive({
    req(input$Typegraph, input$selected_components, data$Resolution)
    
    Info_table <- data$Info %>% select(-Image, -Upload)
    
    # Define resolved components
    resolved_components <- data$Resolved_components
    
    ecosystem_components <- intersect(input$selected_components, resolved_components)
    
    # Handle empty selections
    validate(
      need(length(ecosystem_components) > 0, "No valid components selected.")
    )
    
    # Call appropriate plot function
    plot_func <- plot_dispatch[[input$Typegraph]]
    if (is.null(plot_func)) {
      return(NULL)
    }
    
    plot_func(
      data$CaNSample_long,
      ecosystem_components,
      info       = Info_table,
      group      = input$groupspecies,
      grouplabel = input$groupname,
      session    = session
    )
  })
  
  # Render Plot
  output$Plots <- renderPlot({
    plot_obj()
  }, height = reactive({
    width <- session$clientData$output_Plots_width
    num_plots <- if (input$groupspecies) 1 else length(input$selected_components)
    if (is.null(width) || num_plots == 0) return(400)
    if (num_plots == 1) return(800)
    width * ceiling(num_plots / 3)
  }))
  
  # Render an editable Info Table 
  output$table_info <- DT::renderDT({
    DT::datatable(
      data$Info,
      editable = list(target = "cell", disable = list(columns = c(0))),
      rownames = FALSE,
      selection = "none",
      escape = FALSE,
      options = list(
        pageLength = 15,
        columnDefs = list(list(
          targets = 2,
          render = htmlwidgets::JS(
            "function(data, type, row, meta) {",
            "  return '<input type=\"color\" value=\"' + data + '\" class=\"color-picker\">';",
            "}"
          )
        ))
      )
    ) %>% htmlwidgets::onRender("
      $(document).on('change', '.color-picker', function() {
        var color = $(this).val();
        var row = $(this).closest('tr').index();
        Shiny.setInputValue('color_change', {row: row, color: color}, {priority: 'event'});
      });

      $(document).on('change', '.image-upload', function(e) {
        var file = this.files[0];
        var row = $(this).closest('tr').index(); 
        if (file && ['image/png', 'image/jpeg', 'image/svg+xml'].includes(file.type)) {
          var reader = new FileReader();
          reader.onload = function(evt) {
            Shiny.setInputValue('image_upload', {
              name: file.name,
              type: file.type,
              content: evt.target.result,
              row: row
            }, {priority: 'event'});
          };
          reader.readAsDataURL(file);
        }
      });
    ")
  })
  
  # React to Color Picker Input 
  observeEvent(input$color_change, {
    data$Info[input$color_change$row + 1, 3] <- input$color_change$color
  })
  
  # Handle Image Upload
  observeEvent(input$image_upload, {
    upload <- input$image_upload
    req(upload$content)
    row <- upload$row + 1
    data$Info[row, "Image"] <- sprintf('<img src="%s" width="40px" />', upload$content)
  })
  
  #Editable Cell Change
  observeEvent(input$table_info_cell_edit, {
    info_edit <- input$table_info_cell_edit
    data$Info[info_edit$row, info_edit$col + 1] <- info_edit$value
  })
  
  # Download Cleaned Info Table
  output$saveinfo <- downloadHandler(
    filename = paste0("Info_table_", format(Sys.time(), "%d-%m-%Y"), ".csv"),
    content = function(file) {
      data$Info %>%
        select(-Image, -Upload) %>%
        write.csv(file, row.names = FALSE)
    }
  )
  
  #----TUTORIAL----
  
  observeEvent(input$help,
               introjs(session, options = list("nextLabel"="Continue",
                                               "prevLabel"="Back",
                                               "skipLabel"="Skip the tutorial"),
                       events = list("oncomplete"=I('alert("Enjoy your RCaN experience!")'),
                                     "onbeforechange" = readCallback("switchTabs")))
  )
}