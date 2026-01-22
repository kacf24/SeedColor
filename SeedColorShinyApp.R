library(shiny)
library(magick)

# CONFIGURATION
WORK_WIDTH <- "800x" 

ui <- fluidPage(
  titlePanel("Seed Analysis: Batch Workflow + Geometric Mode"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("upload", "Select All Images (Batch)", multiple = TRUE, accept = c("image/png", "image/jpeg")),
      
      div(style="display:flex; justify-content:space-between; align-items:center;",
          actionButton("prev_img", "< Prev"),
          textOutput("img_counter"),
          actionButton("next_img", "Next >")
      ),
      hr(),
      
      radioButtons("tool_mode", "Current Tool:",
                   choices = c(
                     "1. White Balance (Global)" = "wb",
                     "2. Draw Region (Global)" = "roi",
                     "3. Color Seed Mask" = "seed",
                     "4. Geometric Oval Mask" = "oval" 
                   )),
      hr(),
      
      uiOutput("dynamic_controls"),
      hr(),
      
      h4("Batch Actions"),
      actionButton("save_next", "Save Stats & Next Image", class = "btn-success", icon = icon("arrow-right")),
      hr(),
      
      h4("Export Batch Data"),
      downloadButton("download_master", "Download Master CSV"),
      br(),
      br(),
      textOutput("batch_status")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Image View", 
                 div(style="position:relative;",
                     plotOutput("img_plot", 
                                click = "plot_click", 
                                brush = brushOpts(id = "plot_brush", resetOnNew = FALSE),
                                height = "600px")
                 )
        ),
        tabPanel("Current Distributions",
                 h4("Current Seed Distributions"),
                 plotOutput("hist_plot", height = "300px")
        ),
        tabPanel("Batch Results Table",
                 h4("Collected Data So Far"),
                 tableOutput("master_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  values <- reactiveValues(
    file_list = NULL,        
    current_idx = 1,         
    
    working_magick = NULL,   
    display_raster = NULL,   
    img_info = NULL,
    current_filename = "image",
    
    wb_factors = c(1, 1, 1), 
    roi_coords = NULL,       
    
    seed_points = data.frame(id=1:5, x=NA, y=NA, completed=FALSE),
    seed_click_count = 0,
    mask_raster = NULL,      
    final_mask_logical = NULL, 
    current_stats = NULL,
    
    master_data = data.frame() 
  )
  
  # --- 1. UPLOAD & NAVIGATION ---
  load_image <- function(idx) {
    req(values$file_list)
    if (idx < 1) idx <- 1
    if (idx > nrow(values$file_list)) idx <- nrow(values$file_list)
    values$current_idx <- idx
    
    path <- values$file_list$datapath[idx]
    values$current_filename <- values$file_list$name[idx]
    
    img <- image_read(path)
    img <- image_convert(img, colorspace = "rgb")
    img <- image_resize(img, WORK_WIDTH)
    
    raw_data <- as.integer(image_data(img))
    if (!all(values$wb_factors == 1)) {
      raw_data[,,1] <- raw_data[,,1] * values$wb_factors[1]
      raw_data[,,2] <- raw_data[,,2] * values$wb_factors[2]
      raw_data[,,3] <- raw_data[,,3] * values$wb_factors[3]
      raw_data[raw_data < 0] <- 0; raw_data[raw_data > 255] <- 255
      img <- image_read(raw_data / 255)
    }
    
    values$working_magick <- img
    values$img_info <- image_info(img)
    values$display_raster <- as.raster(img)
    
    values$seed_points[, 2:4] <- NA
    values$seed_points$completed <- FALSE
    values$seed_click_count <- 0
    values$mask_raster <- NULL
    values$final_mask_logical <- NULL
    values$current_stats <- NULL
  }
  
  observeEvent(input$upload, {
    values$file_list <- input$upload
    values$current_idx <- 1
    values$wb_factors <- c(1, 1, 1) 
    values$roi_coords <- NULL       
    values$master_data <- data.frame() 
    load_image(1)
  })
  
  observeEvent(input$prev_img, { load_image(values$current_idx - 1) })
  observeEvent(input$next_img, { load_image(values$current_idx + 1) })
  output$img_counter <- renderText({ req(values$file_list); paste(values$current_idx, "/", nrow(values$file_list)) })
  
  # --- 2. CONTROLS ---
  output$dynamic_controls <- renderUI({
    mode <- input$tool_mode
    if (mode == "wb") {
      tagList(
        helpText("Click a neutral gray/white area."),
        actionButton("reset_wb", "Reset Global WB")
      )
    } else if (mode == "roi") {
      tagList(
        helpText("Draw a box. This box will persist for ALL images."),
        actionButton("clear_roi", "Clear Global Region")
      )
    } else if (mode == "seed") {
      tagList(
        helpText("Click 5 points on the seed."),
        sliderInput("tolerance", "Color Matching", min = 1, max = 150, value = 40),
        sliderInput("min_saturation", "Shadow Filter", min = 0, max = 100, value = 15),
        actionButton("reset_seed", "Reset Points")
      )
    } else if (mode == "oval") {
      tagList(
        h4("Geometric Oval"),
        helpText("1. Draw a box around the seed using the mouse."),
        helpText("2. The app fits a perfect oval inside."),
        helpText("3. Use rotation if the seed is tilted."),
        sliderInput("oval_rotation", "Rotation (Degrees)", min = -90, max = 90, value = 0),
        actionButton("clear_roi", "Clear Box")
      )
    }
  })
  
  # --- 3. BATCH SAVE ---
  observeEvent(input$save_next, {
    req(values$current_stats, values$file_list)
    df <- values$current_stats
    new_row <- data.frame(
      Image_ID = values$current_idx,
      Image_Name = values$current_filename,
      Method = ifelse(input$tool_mode == "oval", "Geometric", "Color"), # Track which method used
      R_Mean = df$Mean[1], R_SD = df$SD[1],
      G_Mean = df$Mean[2], G_SD = df$SD[2],
      B_Mean = df$Mean[3], B_SD = df$SD[3],
      Total_Pixels = sum(values$final_mask_logical)
    )
    values$master_data <- rbind(values$master_data, new_row)
    if (values$current_idx < nrow(values$file_list)) {
      load_image(values$current_idx + 1)
      showNotification("Saved! Loading next...", type = "message", duration = 1)
    } else {
      showNotification("Batch Complete!", type = "warning")
    }
  })
  
  # --- 4. INPUT HANDLERS ---
  observeEvent(input$plot_click, {
    if (input$tool_mode == "wb") {
      # WB LOGIC (same as before)
      x <- round(input$plot_click$x); y <- round(input$plot_click$y)
      h <- values$img_info$height; w <- values$img_info$width
      y_flipped <- h - y
      if (y_flipped < 1 || y_flipped > h || x < 1 || x > w) return()
      raw_data <- as.integer(image_data(values$working_magick))
      r <- raw_data[y_flipped, x, 1]; g <- raw_data[y_flipped, x, 2]; b <- raw_data[y_flipped, x, 3]
      target <- mean(c(r, g, b))
      r <- max(1, r); g <- max(1, g); b <- max(1, b)
      values$wb_factors[1] <- values$wb_factors[1] * (target / r)
      values$wb_factors[2] <- values$wb_factors[2] * (target / g)
      values$wb_factors[3] <- values$wb_factors[3] * (target / b)
      load_image(values$current_idx)
    } else if (input$tool_mode == "seed" && values$seed_click_count < 5) {
      values$seed_click_count <- values$seed_click_count + 1
      idx <- values$seed_click_count
      values$seed_points$x[idx] <- input$plot_click$x
      values$seed_points$y[idx] <- input$plot_click$y
      values$seed_points$completed[idx] <- TRUE
    }
  })
  
  observeEvent(input$reset_wb, { values$wb_factors <- c(1, 1, 1); load_image(values$current_idx) })
  observeEvent(input$plot_brush, { 
    # ROI updates for both "roi" tool and "oval" tool
    if (input$tool_mode %in% c("roi", "oval")) values$roi_coords <- input$plot_brush 
  })
  observeEvent(input$clear_roi, { values$roi_coords <- NULL; session$resetBrush("plot_brush") })
  observeEvent(input$reset_seed, { values$seed_click_count <- 0; values$seed_points[, 2:4] <- NA; values$seed_points$completed <- FALSE; values$mask_raster <- NULL; values$final_mask_logical <- NULL; values$current_stats <- NULL })
  
  # --- 5. CALCULATION ENGINE (DUAL MODE) ---
  observe({
    req(values$working_magick)
    
    w <- values$img_info$width
    h <- values$img_info$height
    mask <- NULL
    
    # --- METHOD A: COLOR SEED ---
    if (input$tool_mode == "seed" && values$seed_click_count == 5) {
      raw_data <- as.integer(image_data(values$working_magick))
      
      # 1. Get Target Colors
      target_colors <- matrix(NA, nrow=5, ncol=3)
      for(i in 1:5){
        cx <- round(values$seed_points$x[i]); cy <- round(values$seed_points$y[i])
        cy_flip <- h - cy
        
        # Boundary checks to prevent crashes if points are near edges
        cx <- max(1, min(w, cx)); cy_flip <- max(1, min(h, cy_flip))
        
        target_colors[i,] <- raw_data[cy_flip, cx, ]
      }
      mean_color <- colMeans(target_colors, na.rm = TRUE)
      
      # 2. Distance Mask
      diff_r <- raw_data[,,1] - mean_color[1]
      diff_g <- raw_data[,,2] - mean_color[2]
      diff_b <- raw_data[,,3] - mean_color[3]
      dist_matrix <- sqrt(diff_r^2 + diff_g^2 + diff_b^2)
      
      # Safety check for tolerance slider
      tol <- if(is.null(input$tolerance)) 40 else input$tolerance
      color_mask <- dist_matrix < tol
      
      # 3. Saturation Mask
      pmax_val <- pmax(raw_data[,,1], raw_data[,,2], raw_data[,,3])
      pmin_val <- pmin(raw_data[,,1], raw_data[,,2], raw_data[,,3])
      chroma <- pmax_val - pmin_val
      
      sat_thresh <- if(is.null(input$min_saturation)) 15 else input$min_saturation
      saturation_mask <- chroma > sat_thresh
      
      mask <- color_mask & saturation_mask
      
      # 4. ROI Intersection (Optional for Seed Mode)
      if (!is.null(values$roi_coords)) {
        roi <- values$roi_coords
        y_min_m <- h - roi$ymax; y_max_m <- h - roi$ymin
        r_x1 <- max(1, floor(roi$xmin)); r_x2 <- min(w, ceiling(roi$xmax))
        r_y1 <- max(1, floor(y_min_m)); r_y2 <- min(h, ceiling(y_max_m))
        
        roi_mask <- matrix(FALSE, nrow = h, ncol = w)
        # Ensure valid ranges
        if(r_y1 < r_y2 && r_x1 < r_x2) { 
          roi_mask[r_y1:r_y2, r_x1:r_x2] <- TRUE
          mask <- mask & roi_mask
        }
      }
    }
    
    # --- METHOD B: GEOMETRIC OVAL ---
    # We added 'req' here to stop the crash if inputs aren't ready
    if (input$tool_mode == "oval" && !is.null(values$roi_coords)) {
      req(input$oval_rotation) # <--- THIS FIXES THE CRASH
      
      roi <- values$roi_coords
      
      # Box Dimensions
      bx1 <- roi$xmin; bx2 <- roi$xmax
      by1 <- roi$ymin; by2 <- roi$ymax
      
      # Center and Radius
      cx <- (bx1 + bx2) / 2
      cy <- (by1 + by2) / 2
      rx <- (bx2 - bx1) / 2
      ry <- (by2 - by1) / 2
      
      # Proceed only if the box has actual size
      if (rx > 0 && ry > 0) {
        y_grid <- matrix(rep(h:1, each=w), nrow=h, ncol=w, byrow=TRUE) 
        x_grid <- matrix(rep(1:w, times=h), nrow=h, ncol=w, byrow=TRUE)
        
        theta_rad <- input$oval_rotation * (pi / 180)
        
        dx <- x_grid - cx
        dy <- y_grid - cy 
        
        dx_rot <- dx * cos(-theta_rad) - dy * sin(-theta_rad)
        dy_rot <- dx * sin(-theta_rad) + dy * cos(-theta_rad)
        
        mask <- ((dx_rot^2) / (rx^2)) + ((dy_rot^2) / (ry^2)) <= 1
      }
    }
    
    values$final_mask_logical <- mask
    
    # E. Overlay & Stats
    # Added 'length(mask) > 0' check to ensure we don't assign empty data
    if (!is.null(mask) && length(mask) > 0) {
      overlay <- array(0, dim = c(h, w, 4))
      overlay[,,2] <- mask * 1.0 
      overlay[,,4] <- mask * 0.5 
      values$mask_raster <- as.raster(overlay)
      
      if (sum(mask) > 0) {
        raw_data <- as.integer(image_data(values$working_magick))
        r_vals <- raw_data[,,1][mask]; g_vals <- raw_data[,,2][mask]; b_vals <- raw_data[,,3][mask]
        
        stats_df <- data.frame(
          Channel = c("Red", "Green", "Blue"),
          Mean = c(mean(r_vals), mean(g_vals), mean(b_vals)),
          Median = c(median(r_vals), median(g_vals), median(b_vals)),
          SD = c(sd(r_vals), sd(g_vals), sd(b_vals)),
          Min = c(min(r_vals), min(g_vals), min(b_vals)),
          Max = c(max(r_vals), max(g_vals), max(b_vals))
        )
        values$current_stats <- stats_df
      } else {
        values$current_stats <- NULL
      }
    } else {
      # Clear if no mask
      values$mask_raster <- NULL
      values$current_stats <- NULL
    }
  })
  
  # --- 6. RENDER ---
  output$img_plot <- renderPlot({
    req(values$display_raster)
    w <- values$img_info$width; h <- values$img_info$height
    par(mar = c(0, 0, 0, 0))
    plot(NULL, xlim = c(0, w), ylim = c(0, h), type = "n", xaxs = "i", yaxs = "i", axes = FALSE)
    rasterImage(values$display_raster, 0, 0, w, h)
    if (!is.null(values$mask_raster)) rasterImage(values$mask_raster, 0, 0, w, h)
    
    if (input$tool_mode == "seed" && values$seed_click_count > 0) {
      pts <- values$seed_points[1:values$seed_click_count, ]
      points(pts$x, pts$y, pch=21, bg="yellow", col="black", cex=2, lwd=2)
    }
  })
  
  output$batch_status <- renderText({ paste("Processed:", nrow(values$master_data), "images.") })
  output$master_table <- renderTable({ values$master_data })
  
  output$hist_plot <- renderPlot({
    req(values$final_mask_logical, values$working_magick)
    if(sum(values$final_mask_logical) == 0) return()
    raw_data <- as.integer(image_data(values$working_magick))
    r_vals <- raw_data[,,1][values$final_mask_logical]
    g_vals <- raw_data[,,2][values$final_mask_logical]
    b_vals <- raw_data[,,3][values$final_mask_logical]
    par(mfrow = c(1, 3))
    hist(r_vals, col = "red", main = "Red", xlab = "", breaks=20)
    hist(g_vals, col = "green", main = "Green", xlab = "", breaks=20)
    hist(b_vals, col = "blue", main = "Blue", xlab = "", breaks=20)
  })
  
  output$download_master <- downloadHandler(
    filename = function() { paste0("BATCH_RESULTS_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(values$master_data, file, row.names = FALSE) }
  )
}

shinyApp(ui, server)