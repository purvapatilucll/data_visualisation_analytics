source("credentials.R")
library(RColorBrewer)

numeric_cols <- c("energy", "protein", "fat", "carbohydrates", "sugar", "starch", "dietary_fiber")

server <- function(input, output, session) {
  # Handle authentication conditionally
  if (Sys.getenv("DEV_MODE") != "TRUE") {
    res_auth <- secure_server(
      check_credentials = check_credentials(credentials)
    )
  }

  # Define CSV path
  csv_path <- "data/processed/view_food.csv"

  #  Reactive: Processed data from Python 
  processed_data <- reactiveFileReader(
    intervalMillis = 1000,
    session = session,
    filePath = csv_path,
    readFunc = function(file) {
      tryCatch(
        {
          df <- read.csv(file, stringsAsFactors = FALSE)
          # Coerce id and active
          if ("id" %in% names(df)) df$id <- as.integer(df$id)
          if ("active" %in% names(df)) df$active <- as.integer(df$active)
          # Coerce numeric columns, non-numeric become NA
          for (col in numeric_cols) {
            if (col %in% names(df)) {
              df[[col]] <- as.numeric(df[[col]])
            }
          }
          df
        },
        error = function(e) {
          cat("Error reading CSV:", e$message, "\n")
          data.frame()
        }
      )
    }
  )

  #  Reactive: New products (active = 0) 
  new_products <- reactive({
    tryCatch(
      {
        df <- processed_data()
        if (nrow(df) == 0) {
          return(data.frame())
        }

        # Get all linked ids
        if ("linked_items" %in% names(df)) {
          linked_str <- df$linked_items[!is.na(df$linked_items) & df$linked_items != ""]
          all_linked <- unique(unlist(strsplit(linked_str, ";")))
          all_linked <- as.integer(all_linked[!is.na(all_linked)])
        } else {
          all_linked <- integer(0)
        }

        # Filter active == 0 and not in all_linked
        df <- df[df$active == 0 & !(df$id %in% all_linked), , drop = FALSE]

        if (nrow(df) > 0) {
          df$created <- as.POSIXct(df$created, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
          df <- df[order(df$created, decreasing = TRUE), , drop = FALSE]
        }
        df
      },
      error = function(e) {
        cat("Error in new_products:", e$message, "\n")
        data.frame()
      }
    )
  })

  #  Reactive: Clustered data for 3D plot (only active products) 
  clustered_active_data <- reactive({
    tryCatch(
      {
        df <- processed_data()
        if (nrow(df) == 0) {
          return(NULL)
        }

        df <- df[df$active == 1, , drop = FALSE]

        # Fill NA with 0
        df[, numeric_cols][is.na(df[, numeric_cols])] <- 0

        numeric_df <- df[, numeric_cols, drop = FALSE]

        if (nrow(df) == 0) {
          return(NULL)
        }

        # Remove zero variance columns
        sds <- apply(numeric_df, 2, sd, na.rm = TRUE)
        non_zero_cols <- names(sds)[sds > 0 & !is.na(sds)]
        if (length(non_zero_cols) == 0) {
          cat("All nutritional columns have zero variance in active data.\n")
          return(NULL)
        }
        numeric_df <- numeric_df[, non_zero_cols, drop = FALSE]

        n_clusters <- min(5, nrow(df))
        if (n_clusters < 2) {
          # For 1 row, assign cluster 1
          df$pca1 <- 0
          df$pca2 <- 0
          df$pca3 <- 0
          df$cluster <- as.factor(1)
          return(list(df = df, pca = NULL, km = NULL))
        }

        pca <- prcomp(numeric_df, scale. = TRUE)
        n_comp <- min(3, ncol(pca$x))
        pca_x <- pca$x[, 1:n_comp]
        # Pad with zeros if less than 3
        if (n_comp < 3) {
          pca_x <- cbind(pca_x, matrix(0, nrow = nrow(pca_x), ncol = 3 - n_comp))
        }
        km <- kmeans(pca_x, centers = n_clusters, nstart = 20)

        df$pca1 <- pca_x[, 1]
        df$pca2 <- pca_x[, 2]
        df$pca3 <- pca_x[, 3]
        df$cluster <- as.factor(km$cluster)

        list(df = df, pca = pca, km = km)
      },
      error = function(e) {
        cat("Error in clustered_active_data:", e$message, "\n")
        NULL
      }
    )
  })

  #  Reactive: Clustered data for non-active products 
  clustered_nonactive_data <- reactive({
    tryCatch(
      {
        df <- processed_data()
        if (nrow(df) == 0) {
          return(NULL)
        }

        df <- df[df$active == 0, , drop = FALSE]

        # Fill NA with 0
        df[, numeric_cols][is.na(df[, numeric_cols])] <- 0

        numeric_df <- df[, numeric_cols, drop = FALSE]

        if (nrow(df) == 0) {
          return(NULL)
        }

        # Remove zero variance columns
        sds <- apply(numeric_df, 2, sd, na.rm = TRUE)
        non_zero_cols <- names(sds)[sds > 0 & !is.na(sds)]
        if (length(non_zero_cols) == 0) {
          cat("All nutritional columns have zero variance in non-active data.\n")
          return(NULL)
        }
        numeric_df <- numeric_df[, non_zero_cols, drop = FALSE]

        n_clusters <- min(5, nrow(df))
        if (n_clusters < 2) {
          # For 1 row, assign cluster 1
          df$pca1 <- 0
          df$pca2 <- 0
          df$pca3 <- 0
          df$cluster <- as.factor(1)
          return(list(df = df, pca = NULL, km = NULL))
        }

        pca <- prcomp(numeric_df, scale. = TRUE)
        n_comp <- min(3, ncol(pca$x))
        pca_x <- pca$x[, 1:n_comp]
        # Pad with zeros if less than 3
        if (n_comp < 3) {
          pca_x <- cbind(pca_x, matrix(0, nrow = nrow(pca_x), ncol = 3 - n_comp))
        }
        km <- kmeans(pca_x, centers = n_clusters, nstart = 20)

        df$pca1 <- pca_x[, 1]
        df$pca2 <- pca_x[, 2]
        df$pca3 <- pca_x[, 3]
        df$cluster <- as.factor(km$cluster)

        list(df = df, pca = pca, km = km)
      },
      error = function(e) {
        cat("Error in clustered_nonactive_data:", e$message, "\n")
        NULL
      }
    )
  })

  observe({
    cl_data <- clustered_nonactive_data()
    if (is.null(cl_data)) {
      cat("clustered_nonactive_data returned NULL\n")
    } else if (is.null(cl_data$df)) {
      cat("clustered_nonactive_data$df is NULL\n")
    } else {
      cat("clustered_nonactive_data has", nrow(cl_data$df), "rows\n")
    }
  })

  #  Home Tab 
  output$total_items <- renderInfoBox({
    infoBox(
      "Total Food Items",
      nrow(processed_data()),
      icon = icon("utensils"),
      color = "maroon",
    )
  })

  #  Data Tab 
  output$food_table <- renderDT({
    datatable(processed_data(), options = list(pageLength = 10), selection = "single")
  })

  #  New Products Tab 
  nutrient_cols <- c("energy", "carbohydrates", "fat", "protein", "sugar")

  output$new_products_table <- renderDT({
    df <- new_products()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No new products")))
    }

    nutrient_idx <- which(names(df) %in% nutrient_cols) - 1

    datatable(
      df,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        createdRow = JS(
          sprintf(
            "function(row, data, dataIndex) {
               var nutrientCols = [%s];
               var missing = false;
               for (var i = 0; i < nutrientCols.length; i++) {
                 var v = data[nutrientCols[i]];
                 if (v === null || v === '' || isNaN(parseFloat(v))) { missing = true; }
               }
               if (missing) { $(row).css('background-color', '#fff4cc'); }
             }",
            paste(nutrient_idx, collapse = ",")
          )
        )
      ),
      selection = "single",
      escape = FALSE
    )
  })

  # Track selected product
  selected_product_name <- reactiveVal(NULL)
  selected_id <- reactiveVal(NULL)
  selected_barcode <- reactiveVal(NULL)

  # Observe row selection and update other tabs
  observe({
    selected_row <- input$new_products_table_rows_selected
    if (length(selected_row) > 0) {
      idx <- as.integer(selected_row[1])
      new_prods <- new_products()

      if (nrow(new_prods) >= idx) {
        selected_df <- new_prods[idx, , drop = FALSE]
        selected_id_val <- as.integer(selected_df$id)
        selected_name <- selected_df$name
        selected_barcode_val <- selected_df$barcode

        # Store selected values
        selected_id(selected_id_val)
        selected_product_name(selected_name)
        selected_barcode(selected_barcode_val)

        # Update search_query
        updateTextInput(session, "search_query", value = selected_name)

        # Display selected info
        output$selected_product_info <- renderPrint({
          active_sel <- selected_active_id()
          non_actives <- selected_non_actives()
          
          cat("Selected Active Product:\n")
          if (!is.null(active_sel) && active_sel != "") {
            cat("  ID: ", active_sel, "\n")
            active_df <- processed_data()
            active_row <- active_df[active_df$id == as.integer(active_sel), ]
            if (nrow(active_row) > 0) {
              cat("  Name: ", active_row$name[1], "\n")
              cat("  Barcode: ", active_row$barcode[1], "\n")
            }
          } else {
            cat("  (No active product selected yet)\n")
          }
          
          cat("\nSelected Non-Active Products to Link:\n")
          if (!is.null(non_actives) && length(non_actives) > 0) {
            non_active_df <- processed_data()
            for (id in non_actives) {
              row <- non_active_df[non_active_df$id == as.integer(id), ]
              if (nrow(row) > 0) {
                cat("  - ID: ", id, " | Name: ", row$name[1], "\n")
              }
            }
          } else {
            cat("  (No non-active products selected)\n")
          }
        })

        # Compute recommendations - pass as data.frame
        recommendations <- reactive({
          req(selected_id())
          tryCatch(
            {
              result <- recommend_similar_products(
                existing_csv = "data/processed/view_food.csv",
                new_product_df = selected_df,
                top_n = 5,
                threshold = 0.1
              )
              # Convert directly from pandas DataFrame without py_to_r
              if (!is.null(result) && length(result) > 0) {
                as.data.frame(result)
              } else {
                data.frame()
              }
            },
            error = function(e) {
              cat("Error in recommendations:", e$message, "\n")
              data.frame()
            }
          )
        })

        # Update link_to_id choices (legacy, but keep for now)
        observe({
          rec <- recommendations()
          if (nrow(rec) > 0) {
            updateSelectInput(session, "link_to_id", choices = as.character(rec$id))
          }
        })
      }
    }
  })

  #  AUTO-TRIGGER SEARCH when switching to search tab 
  observe({
    # Trigger when search tab is selected
    if (!is.null(input$body)) {
      # Check if we're on the search tab and have a selected product
      if (input$body == "search" && !is.null(selected_product_name())) {
        # Trigger the search automatically
        shinyjs::runjs("document.getElementById('search_btn').click();")
      }
    }
  })

  # Reactive to control visibility of non-active section
  show_nonactive <- reactiveVal(FALSE)

  # Observe continue button to show non-active and hide active
  observeEvent(input$continue_btn, {
    if (is.null(selected_id())) {
      showModal(modalDialog("Please select a product from the New Products tab before continuing.", easyClose = TRUE))
    } else {
      shinyjs::hide("active_section")
      shinyjs::show("nonactive_section")
    }
  })

  # Observe back button to show active and hide non-active
  observeEvent(input$back_btn, {
    shinyjs::show("active_section")
    shinyjs::hide("nonactive_section")
  })


  #  Search & Compare: Similarity for active 
  similar_active_nutrition <- eventReactive(c(input$search_btn, selected_product_name()), {
    query <- input$search_query
    if (is.null(query) || query == "") {
      return(data.frame())
    }
    tryCatch(
      {
        df <- processed_data()
        df <- df[df$active == 1, , drop = FALSE]
        result <- compute_similarity(df, query)
        as.data.frame(py_to_r(result))
      },
      error = function(e) {
        cat("Error in similar_active_nutrition:", e$message, "\n")
        data.frame()
      }
    )
  })

  #  Search & Compare: Similarity for non-active 
  similar_nonactive_nutrition <- eventReactive(c(input$search_btn, selected_product_name()), {
    query <- input$search_query
    if (is.null(query) || query == "") {
      return(data.frame())
    }
    tryCatch(
      {
        df <- processed_data()
        df <- df[df$active == 0, , drop = FALSE]
        result <- compute_similarity(df, query)
        as.data.frame(py_to_r(result))
      },
      error = function(e) {
        cat("Error in similar_nonactive_nutrition:", e$message, "\n")
        data.frame()
      }
    )
  })

  #  New: Similar active by name (NLP) 
  similar_active <- eventReactive(c(input$search_btn, selected_product_name()), {
    query <- input$search_query
    if (is.null(query) || query == "") {
      return(data.frame())
    }
    tryCatch(
      {
        df <- processed_data()
        df <- df[df$active == 1, , drop = FALSE]
        result <- compute_name_similarity(df, query) # Assume new Python function
        as.data.frame(py_to_r(result))
      },
      error = function(e) {
        cat("Error in similar_active:", e$message, "\n")
        data.frame()
      }
    )
  })

  #  New: Similar non-active by name (NLP) 
  similar_nonactive <- eventReactive(c(input$search_btn, selected_product_name()), {
    query <- input$search_query
    if (is.null(query) || query == "") {
      return(data.frame())
    }
    tryCatch(
      {
        df <- processed_data()
        df <- df[df$active == 0, , drop = FALSE]
        result <- compute_name_similarity(df, query) # Assume new Python function
        as.data.frame(py_to_r(result))
      },
      error = function(e) {
        cat("Error in similar_nonactive:", e$message, "\n")
        data.frame()
      }
    )
  })

  output$similar_active_nutrition_table <- renderDT({
    req(similar_active_nutrition())
    df <- similar_active_nutrition()

    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No similar active items found")))
    }

    # Add radio button column with ID displayed
    Select <- sprintf('%s <input type="radio" name="active_select_shared" value="%s"/>', df$id, df$id)
    df <- cbind(Select, df[, c("name", "barcode", "similarity_score")])

    datatable(
      df,
      escape = FALSE,
      selection = "none",
      options = list(
        pageLength = 10,
        columnDefs = list(list(orderable = FALSE, targets = 0)),
        preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
        drawCallback = JS(
          "function() {",
          "  Shiny.bindAll(this.api().table().node());",
          "  var table = this.api();",
          '  table.$(\'input[type="radio"]\').on("change", function() {',
          '    if ($(this).prop("checked")) {',
          '      Shiny.setInputValue($(this).attr("name"), $(this).val(), {priority: "event"});',
          "    } else {",
          '      Shiny.setInputValue($(this).attr("name"), null, {priority: "event"});',
          "    }",
          "  });",
          "}"
        ),
        createdRow = JS(
          "function(row, data, index) {",
          "  $(row).on('click', function() {",
          "    var radio = $(this).find('input[type=\"radio\"]');",
          "    var wasChecked = radio.prop('checked');",
          "    if (wasChecked) {",
          "      radio.prop('checked', false);",
          "    } else {",
          "      radio.prop('checked', true);",
          "    }",
          "    radio.trigger('change');",
          "  });",
          "}"
        )
      )
    )
  })

  output$similar_nonactive_nutrition_table <- renderDT({
    req(similar_nonactive_nutrition())
    df <- similar_nonactive_nutrition()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No similar non-active items found")))
    }

    if (is.null(input$active_select_shared)) {
      sel_id <- selected_id()
      if (!is.null(sel_id)) {
        df <- df[df$id != sel_id, ]
      }
    }

    # Add checkbox column with ID displayed
    Select <- sprintf('%s <input type="checkbox" name="non_select_nut" value="%s"/>', df$id, df$id)
    df <- cbind(Select, df[, c("name", "barcode", "similarity_score")])

    datatable(
      df,
      escape = FALSE,
      selection = "none",
      options = list(
        pageLength = 10,
        columnDefs = list(list(orderable = FALSE, targets = 0)),
        preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
        drawCallback = JS(
          "function() {",
          "  Shiny.bindAll(this.api().table().node());",
          "  var table = this.api();",
          '  table.$(\'input[type="checkbox"]\').on("change", function() {',
          '    var name = $(this).attr("name");',
          '    var checkboxes = table.$(\'input[name="\' + name + \'"]\');',
          '    var values = checkboxes.filter(":checked").map(function() { return $(this).val(); }).get();',
          '    Shiny.setInputValue(name, values, {priority: "event"});',
          "  });",
          "}"
        ),
        createdRow = JS(
          "function(row, data, index) {",
          "  $(row).on('click', function() {",
          "    var checkbox = $(this).find('input[type=\"checkbox\"]');",
          "    checkbox.prop('checked', !checkbox.prop('checked'));",
          "    checkbox.trigger('change');",
          "  });",
          "}"
        )
      )
    )
  })

  output$similar_active_table <- renderDT({
    req(similar_active())
    df <- similar_active()

    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No similar active products found")))
    }

    # Add radio button column with ID displayed
    Select <- sprintf('%s <input type="radio" name="active_select_shared" value="%s"/>', df$id, df$id)
    df <- cbind(Select, df[, c("name", "barcode", "similarity_score")])

    datatable(
      df,
      escape = FALSE,
      selection = "none",
      options = list(
        pageLength = 10,
        columnDefs = list(list(orderable = FALSE, targets = 0)),
        preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
        drawCallback = JS(
          "function() {",
          "  Shiny.bindAll(this.api().table().node());",
          "  var table = this.api();",
          '  table.$(\'input[type="radio"]\').on("change", function() {',
          '    if ($(this).prop("checked")) {',
          '      Shiny.setInputValue($(this).attr("name"), $(this).val(), {priority: "event"});',
          "    } else {",
          '      Shiny.setInputValue($(this).attr("name"), null, {priority: "event"});',
          "    }",
          "  });",
          "}"
        ),
        createdRow = JS(
          "function(row, data, index) {",
          "  $(row).on('click', function() {",
          "    var radio = $(this).find('input[type=\"radio\"]');",
          "    var wasChecked = radio.prop('checked');",
          "    if (wasChecked) {",
          "      radio.prop('checked', false);",
          "    } else {",
          "      radio.prop('checked', true);",
          "    }",
          "    radio.trigger('change');",
          "  });",
          "}"
        )
      )
    )
  })

  output$similar_nonactive_table <- renderDT({
    req(similar_nonactive())
    df <- similar_nonactive()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No similar non-active products found")))
    }

    if (is.null(input$active_select_shared)) {
      sel_id <- selected_id()
      if (!is.null(sel_id)) {
        df <- df[df$id != sel_id, ]
      }
    }

    # Add checkbox column with ID displayed
    Select <- sprintf('%s <input type="checkbox" name="non_select_name" value="%s"/>', df$id, df$id)
    df <- cbind(Select, df[, c("name", "barcode", "similarity_score")])

    datatable(
      df,
      escape = FALSE,
      selection = "none",
      options = list(
        pageLength = 10,
        columnDefs = list(list(orderable = FALSE, targets = 0)),
        preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
        drawCallback = JS(
          "function() {",
          "  Shiny.bindAll(this.api().table().node());",
          "  var table = this.api();",
          '  table.$(\'input[type="checkbox"]\').on("change", function() {',
          '    var name = $(this).attr("name");',
          '    var checkboxes = table.$(\'input[name="\' + name + \'"]\');',
          '    var values = checkboxes.filter(":checked").map(function() { return $(this).val(); }).get();',
          '    Shiny.setInputValue(name, values, {priority: "event"});',
          "  });",
          "}"
        ),
        createdRow = JS(
          "function(row, data, index) {",
          "  $(row).on('click', function() {",
          "    var checkbox = $(this).find('input[type=\"checkbox\"]');",
          "    checkbox.prop('checked', !checkbox.prop('checked'));",
          "    checkbox.trigger('change');",
          "  });",
          "}"
        )
      )
    )
  })

  output$similarity_heatmap_active <- renderPlotly({
    df <- similar_active_nutrition()
    if (nrow(df) == 0) {
      return(NULL)
    }

    df$similarity_score <- round(as.numeric(df$similarity_score), 2)
    plot_ly(
      x = df$name,
      y = "Similarity",
      z = matrix(df$similarity_score, nrow = 1),
      type = "heatmap",
      colorscale = "Blues"
    ) %>% layout(
      yaxis = list(title = "", showticklabels = FALSE),
      xaxis = list(title = "Top Similar Active Products")
    )
  })

  output$similarity_heatmap_nonactive <- renderPlotly({
    df <- similar_nonactive_nutrition()
    if (nrow(df) == 0) {
      return(NULL)
    }

    df$similarity_score <- round(as.numeric(df$similarity_score), 2)
    plot_ly(
      x = df$name,
      y = "Similarity",
      z = matrix(df$similarity_score, nrow = 1),
      type = "heatmap",
      colorscale = "Blues"
    ) %>% layout(
      yaxis = list(title = "", showticklabels = FALSE),
      xaxis = list(title = "Top Similar Non-Active Products")
    )
  })

#  Search & Compare: 3D cluster plot active 
output$cluster_plot_active <- renderPlotly({
  cl_data <- clustered_active_data()
  req(cl_data)
  req(input$search_query)
  target_id <- selected_active_id()
  if (is.null(target_id)) {
    return(NULL)
  }
  target_id <- as.integer(target_id)

  df_orig <- cl_data$df
  if (nrow(df_orig) == 0) {
    return(NULL)
  }
  if (!"cluster" %in% colnames(df_orig)) {
    return(NULL)
  }

  df <- df_orig
  df <- df[df$pca1 <= 20 & df$pca2 <= 20 & df$pca3 <= 20, ]

  df <- df[df$id != target_id, ]

  # Get nutritional columns used in PCA (non-zero variance)
  vars <- names(cl_data$pca$center)  # These are the non_zero_cols

  # Get target nutrition vector
  if (target_id %in% df_orig$id) {
    target_nut <- df_orig[df_orig$id == target_id, vars, drop = FALSE]
  } else {
    sel_df <- processed_data()[processed_data()$id == target_id, ]
    if (nrow(sel_df) == 0) {
      return(NULL)
    }
    target_nut <- sel_df[, vars, drop = FALSE]
    target_nut[is.na(target_nut)] <- 0
  }

  # Scale the target using PCA's center/scale
  scaled_target <- scale(target_nut, center = cl_data$pca$center, scale = cl_data$pca$scale)

  # Get the nutritional matrix for df (already NA=0 from clustering)
  nut_matrix <- df[, vars, drop = FALSE]

  # Scale the matrix (for consistency)
  scaled_matrix <- scale(nut_matrix, center = cl_data$pca$center, scale = cl_data$pca$scale)

  # Filter to closest N if specified (using Euclidean in full scaled space)
  num_closest <- input$num_closest_active
  if (!is.null(num_closest) && num_closest > 0) {
    # Compute Euclidean distances in full scaled nutritional space
    distances <- apply(scaled_matrix, 1, function(row) {
      sqrt(sum((row - as.vector(scaled_target))^2))
    })
    if (length(distances) > 0) {
      ordered_indices <- order(distances)
      df <- df[ordered_indices[1:min(num_closest, nrow(df))], ]
    }
  }

  p <- plot_ly(df,
    x = ~pca1, y = ~pca2, z = ~pca3,
    color = ~cluster,
    colors = "Set1",
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 5, opacity = 0.5),
    text = ~ paste(
      "Name:", name,
      "<br>Cluster:", cluster,
      "<br>PCA1:", round(pca1, 2),
      "<br>PCA2:", round(pca2, 2),
      "<br>PCA3:", round(pca3, 2)
    ),
    name = ~cluster
  )

# Highlight target
if (!is.null(target_id)) {
  if (target_id %in% df_orig$id) {
    target_df <- df_orig[df_orig$id == target_id, ]
    target_cluster <- target_df$cluster
    target_name <- target_df$name
    target_pca <- as.numeric(target_df[, c("pca1", "pca2", "pca3")])
  } else {
    # Project target (fix: safely handle <3 components)
    vars <- names(cl_data$pca$center)
    sel_df <- processed_data()[processed_data()$id == target_id, ]
    if (nrow(sel_df) == 0) {
      return(p)
    }
    num_sel <- sel_df[, vars, drop = FALSE]
    num_sel[is.na(num_sel)] <- 0
    
    # Predict, then safely extract up to 3 components
    projected_full <- predict(cl_data$pca, newdata = num_sel)
    n_comp <- min(3, ncol(projected_full))
    projected <- projected_full[, seq_len(n_comp), drop = FALSE]
    
    # Pad with zeros if <3
    if (n_comp < 3) {
      projected <- cbind(projected, matrix(0, nrow = 1, ncol = 3 - n_comp))
    }
    
    # Cluster prediction
    dists <- apply(cl_data$km$centers, 1, function(c) sum((projected - c)^2))
    pred_cluster <- which.min(dists)
    
    target_cluster <- as.factor(pred_cluster)
    target_name <- sel_df$name
    target_df <- data.frame(
      pca1 = projected[1, 1],
      pca2 = projected[1, 2],
      pca3 = projected[1, 3],
      cluster = target_cluster,
      name = target_name,
      id = target_id
    )
    target_pca <- as.numeric(target_df[, c("pca1", "pca2", "pca3")])
  }

    col_map <- setNames(brewer.pal(nlevels(df_orig$cluster), "Set1"), levels(df_orig$cluster))

    p <- add_trace(
      p,
      data = target_df,
      x = ~pca1, y = ~pca2, z = ~pca3,
      type = "scatter3d", mode = "markers",
      marker = list(
        size = 10, opacity = 1,
        color = col_map[as.character(target_cluster)],
        line = list(color = "black", width = 2)
      ),
      text = ~ paste(
        "Target Name:", name, "<br>Cluster:", cluster,
        "<br>PCA1:", round(pca1, 2), "<br>PCA2:", round(pca2, 2),
        "<br>PCA3:", round(pca3, 2)
      ),
      name = "Target", showlegend = TRUE
    )
  }

  # Layout
  p <- layout(p, scene = list(
    xaxis = list(title = "PCA1"),
    yaxis = list(title = "PCA2"),
    zaxis = list(title = "PCA3")
  ))

  p
})

#  Search & Compare: 3D cluster plot nonactive 
output$cluster_plot_nonactive <- renderPlotly({
  cl_data <- clustered_nonactive_data()
  req(cl_data)
  req(input$search_query)
  target_id <- selected_active_id()
  if (is.null(target_id)) {
    return(NULL)
  }
  target_id <- as.integer(target_id)

  df_orig <- cl_data$df
  if (nrow(df_orig) == 0) {
    return(NULL)
  }
  if (!"cluster" %in% colnames(df_orig)) {
    return(NULL)
  }

  df <- df_orig
  df <- df[df$pca1 <= 20 & df$pca2 <= 20 & df$pca3 <= 20, ]

  df <- df[df$id != target_id, ]

  # Get nutritional columns used in PCA (non-zero variance)
  vars <- names(cl_data$pca$center)  # These are the non_zero_cols

  # Get target nutrition vector
  if (target_id %in% df_orig$id) {
    target_nut <- df_orig[df_orig$id == target_id, vars, drop = FALSE]
  } else {
    sel_df <- processed_data()[processed_data()$id == target_id, ]
    if (nrow(sel_df) == 0) {
      return(NULL)
    }
    target_nut <- sel_df[, vars, drop = FALSE]
    target_nut[is.na(target_nut)] <- 0
  }

  # Scale the target using PCA's center/scale
  scaled_target <- scale(target_nut, center = cl_data$pca$center, scale = cl_data$pca$scale)

  # Get the nutritional matrix for df (already NA=0 from clustering)
  nut_matrix <- df[, vars, drop = FALSE]

  # Scale the matrix (for consistency)
  scaled_matrix <- scale(nut_matrix, center = cl_data$pca$center, scale = cl_data$pca$scale)

  # Filter to closest N if specified (using Euclidean in full scaled space)
  num_closest <- input$num_closest_nonactive
  if (!is.null(num_closest) && num_closest > 0) {
    # Compute Euclidean distances in full scaled nutritional space
    distances <- apply(scaled_matrix, 1, function(row) {
      sqrt(sum((row - as.vector(scaled_target))^2))
    })
    if (length(distances) > 0) {
      ordered_indices <- order(distances)
      df <- df[ordered_indices[1:min(num_closest, nrow(df))], ]
    }
  }

  p <- plot_ly(df,
    x = ~pca1, y = ~pca2, z = ~pca3,
    color = ~cluster,
    colors = "Set1",
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 5, opacity = 0.5),
    text = ~ paste(
      "Name:", name,
      "<br>Cluster:", cluster,
      "<br>PCA1:", round(pca1, 2),
      "<br>PCA2:", round(pca2, 2),
      "<br>PCA3:", round(pca3, 2)
    ),
    name = ~cluster
  )

  # Highlight target
  if (!is.null(target_id)) {
    if (target_id %in% df_orig$id) {
      target_df <- df_orig[df_orig$id == target_id, ]
      target_cluster <- target_df$cluster
      target_name <- target_df$name
      target_pca <- as.numeric(target_df[, c("pca1", "pca2", "pca3")])
    } else {
      # Project target (fix: safely handle <3 components)
      vars <- names(cl_data$pca$center)
      sel_df <- processed_data()[processed_data()$id == target_id, ]
      if (nrow(sel_df) == 0) {
        return(p)
      }
      num_sel <- sel_df[, vars, drop = FALSE]
      num_sel[is.na(num_sel)] <- 0
      
      # Predict, then safely extract up to 3 components
      projected_full <- predict(cl_data$pca, newdata = num_sel)
      n_comp <- min(3, ncol(projected_full))
      projected <- projected_full[, seq_len(n_comp), drop = FALSE]
      
      # Pad with zeros if <3
      if (n_comp < 3) {
        projected <- cbind(projected, matrix(0, nrow = 1, ncol = 3 - n_comp))
      }
      
      # Cluster prediction
      dists <- apply(cl_data$km$centers, 1, function(c) sum((projected - c)^2))
      pred_cluster <- which.min(dists)
      
      target_cluster <- as.factor(pred_cluster)
      target_name <- sel_df$name
      target_df <- data.frame(
        pca1 = projected[1, 1],
        pca2 = projected[1, 2],
        pca3 = projected[1, 3],
        cluster = target_cluster,
        name = target_name,
        id = target_id
      )
      target_pca <- as.numeric(target_df[, c("pca1", "pca2", "pca3")])
    }

    col_map <- setNames(brewer.pal(nlevels(df_orig$cluster), "Set1"), levels(df_orig$cluster))

    p <- add_trace(
      p,
      data = target_df,
      x = ~pca1, y = ~pca2, z = ~pca3,
      type = "scatter3d", mode = "markers",
      marker = list(
        size = 10, opacity = 1,
        color = col_map[as.character(target_cluster)],
        line = list(color = "black", width = 2)
      ),
      text = ~ paste(
        "Target Name:", name, "<br>Cluster:", cluster,
        "<br>PCA1:", round(pca1, 2), "<br>PCA2:", round(pca2, 2),
        "<br>PCA3:", round(pca3, 2)
      ),
      name = "Target", showlegend = TRUE
    )
  }

  # Layout
  p <- layout(p, scene = list(
    xaxis = list(title = "PCA1"),
    yaxis = list(title = "PCA2"),
    zaxis = list(title = "PCA3")
  ))

  p
})

  # Closest products table for active
  output$closest_products_active <- renderDT({
    cl_data <- clustered_active_data()
    req(cl_data)
    req(input$search_query)
    target_id <- selected_active_id()
    if (is.null(target_id)) {
      return(datatable(data.frame(Message = "No target selected")))
    }
    target_id <- as.integer(target_id)

    df_orig <- cl_data$df
    if (nrow(df_orig) == 0) {
      return(datatable(data.frame(Message = "No products available")))
    }

    vars <- names(cl_data$pca$center)
    sel_df <- processed_data()[processed_data()$id == target_id, ]
    if (nrow(sel_df) == 0) {
      return(datatable(data.frame(Message = "Target not found")))
    }

    target_nut <- sel_df[, vars, drop = FALSE]
    target_nut[is.na(target_nut)] <- 0
    scaled_target <- scale(target_nut, center = cl_data$pca$center, scale = cl_data$pca$scale)

    nut_matrix <- df_orig[, vars, drop = FALSE]
    scaled_matrix <- scale(nut_matrix, center = cl_data$pca$center, scale = cl_data$pca$scale)

    distances <- apply(scaled_matrix, 1, function(row) {
      sqrt(sum((row - as.vector(scaled_target))^2))
    })

    df_with_dist <- df_orig
    df_with_dist$distance <- distances
    df_with_dist <- df_with_dist[df_with_dist$id != target_id, ]
    df_with_dist <- df_with_dist[order(df_with_dist$distance), ]
    df_with_dist <- head(df_with_dist, 20)

    df_display <- df_with_dist[, c("id", "name", "distance")]
    df_display$distance <- round(df_display$distance, 3)

    Select <- sprintf('%s <input type="radio" name="active_select_shared" value="%s"/>', df_display$id, df_display$id)
    df_display <- cbind(Select, df_display[, c("name", "distance")])

    datatable(
      df_display,
      escape = FALSE,
      selection = "none",
      options = list(
        pageLength = 10,
        columnDefs = list(list(orderable = FALSE, targets = 0)),
        preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
        drawCallback = JS(
          "function() {",
          "  Shiny.bindAll(this.api().table().node());",
          "  var table = this.api();",
          '  table.$(\'input[type="radio"]\').on("change", function() {',
          '    if ($(this).prop("checked")) {',
          '      Shiny.setInputValue($(this).attr("name"), $(this).val(), {priority: "event"});',
          "    } else {",
          '      Shiny.setInputValue($(this).attr("name"), null, {priority: "event"});',
          "    }",
          "  });",
          "}"
        ),
        createdRow = JS(
          "function(row, data, index) {",
          "  $(row).on('click', function() {",
          "    var radio = $(this).find('input[type=\"radio\"]');",
          "    var wasChecked = radio.prop('checked');",
          "    if (wasChecked) {",
          "      radio.prop('checked', false);",
          "    } else {",
          "      radio.prop('checked', true);",
          "    }",
          "    radio.trigger('change');",
          "  });",
          "}"
        )
      )
    )
  })

  # Closest products table for non-active
  output$closest_products_nonactive <- renderDT({
    cl_data <- clustered_nonactive_data()
    req(cl_data)
    req(input$search_query)
    target_id <- selected_active_id()
    if (is.null(target_id)) {
      return(datatable(data.frame(Message = "No target selected")))
    }
    target_id <- as.integer(target_id)

    df_orig <- cl_data$df
    if (nrow(df_orig) == 0) {
      return(datatable(data.frame(Message = "No products available")))
    }

    vars <- names(cl_data$pca$center)
    sel_df <- processed_data()[processed_data()$id == target_id, ]
    if (nrow(sel_df) == 0) {
      return(datatable(data.frame(Message = "Target not found")))
    }

    target_nut <- sel_df[, vars, drop = FALSE]
    target_nut[is.na(target_nut)] <- 0
    scaled_target <- scale(target_nut, center = cl_data$pca$center, scale = cl_data$pca$scale)

    nut_matrix <- df_orig[, vars, drop = FALSE]
    scaled_matrix <- scale(nut_matrix, center = cl_data$pca$center, scale = cl_data$pca$scale)

    distances <- apply(scaled_matrix, 1, function(row) {
      sqrt(sum((row - as.vector(scaled_target))^2))
    })

    df_with_dist <- df_orig
    df_with_dist$distance <- distances
    df_with_dist <- df_with_dist[df_with_dist$id != target_id, ]
    df_with_dist <- df_with_dist[order(df_with_dist$distance), ]
    df_with_dist <- head(df_with_dist, 20)

    df_display <- df_with_dist[, c("id", "name", "distance")]
    df_display$distance <- round(df_display$distance, 3)

    Select <- sprintf('%s <input type="checkbox" name="non_select_nut" value="%s"/>', df_display$id, df_display$id)
    df_display <- cbind(Select, df_display[, c("name", "distance")])

    datatable(
      df_display,
      escape = FALSE,
      selection = "none",
      options = list(
        pageLength = 10,
        columnDefs = list(list(orderable = FALSE, targets = 0)),
        preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
        drawCallback = JS(
          "function() {",
          "  Shiny.bindAll(this.api().table().node());",
          "  var table = this.api();",
          '  table.$(\'input[type="checkbox"]\').on("change", function() {',
          '    var name = $(this).attr("name");',
          '    var checkboxes = table.$(\'input[name="\' + name + \'"]\');',
          '    var values = checkboxes.filter(":checked").map(function() { return $(this).val(); }).get();',
          '    Shiny.setInputValue(name, values, {priority: "event"});',
          "  });",
          "}"
        ),
        createdRow = JS(
          "function(row, data, index) {",
          "  $(row).on('click', function() {",
          "    var checkbox = $(this).find('input[type=\"checkbox\"]');",
          "    checkbox.prop('checked', !checkbox.prop('checked'));",
          "    checkbox.trigger('change');",
          "  });",
          "}"
        )
      )
    )
  })


  #  Missing nutritional fields 
  output$missing_table <- renderDT({
    df <- highlight_missing(processed_data())
    df <- as.data.frame(py_to_r(df))
    if (nrow(df) == 0) {
      return(datatable(data.frame()))
    }
    datatable(df[, c("name", "energy", "protein", "fat", "carbohydrates", "missing_fields")])
  })

  selected_active_id <- reactive({
    selected <- input$active_select_shared
    if (is.null(selected) || selected == "") {
      selected_id()
    } else {
      selected
    }
  })

  # Debug: Observe changes to input$active_select_shared
  observe({
    cat("active_select_shared input changed to:", input$active_select_shared, "\n")
  })

  selected_non_actives <- reactive({
    non_ids <- c(input$non_select_name, input$non_select_nut)
    if (length(non_ids) > 0) non_ids else NULL
  })

  # Debug: Observe changes to selected_non_actives
  observe({
    cat("selected_non_actives changed to:", paste(selected_non_actives(), collapse = ", "), "\n")
  })
  # Removed target_cluster updates for selected_active_id

  #  Handle decision buttons 
  # Updated for multiple linking
  observeEvent(input$decision_link, {
    # Get selected active (single)
    active_id <- selected_active_id()
    if (!is.null(active_id)) active_id <- as.integer(active_id)

    # Get selected non-actives (multiple)
    non_ids <- selected_non_actives()

    # Get original selected id (the main product from new products tab)
    orig_id <- selected_id()
    req(orig_id)

    # Always include orig_id in non_ids if not already present
    if (!(orig_id %in% non_ids)) {
      non_ids <- c(non_ids, orig_id)
    }
    non_ids <- unique(non_ids)
    non_ids <- as.integer(non_ids)

    # Read CSV
    df <- read.csv(csv_path, stringsAsFactors = FALSE)

    # Add linked_items column if it doesn't exist
    if (!"linked_items" %in% names(df)) {
      df$linked_items <- ""
    }

    is_new_active <- FALSE

    # Check if the active_id is actually the orig_id (new product, active=0)
    active_row_idx <- which(df$id == active_id)
    if (length(active_row_idx) == 0) {
      return()
    }
    if (df$active[active_row_idx] == 0) {
      # It's the new product, activate it
      df$active[active_row_idx] <- 1
      is_new_active <- TRUE
    }

    # Filter out the active_id from non_ids (no self-linking)
    non_ids <- non_ids[non_ids != active_id]

    if (length(non_ids) == 0) {
      if (is_new_active) {
        # Just activated orig_id, save changes
        write.csv(df, csv_path, row.names = FALSE)
    
        # Show toast
        shinyjs::runjs(sprintf("
          const toast = document.createElement('div');
          toast.innerHTML = '✓ %s successfully activated!';
          toast.style.cssText = 'position: fixed; top: 20px; right: 20px; background-color: #28a745; color: white; padding: 15px 20px; border-radius: 5px; font-size: 14px; z-index: 9999; box-shadow: 0 2px 8px rgba(0,0,0,0.2); animation: slideIn 0.3s ease-in-out;';
          document.body.appendChild(toast);
          setTimeout(() => {
            toast.style.animation = 'slideOut 0.3s ease-in-out';
            setTimeout(() => toast.remove(), 300);
          }, 3000);
        ", df$name[active_row_idx])) # or whichever column has the product name
    
      } else {
        showModal(modalDialog("Please select at least one non-active product to link.", easyClose = TRUE))
        return()
      }
    } else {
      # Get existing linked_items
      existing_linked <- df$linked_items[active_row_idx]
      existing_list <- if (existing_linked == "") c() else strsplit(existing_linked, ";")[[1]]

      # Add new non_ids, avoiding duplicates
      new_to_add <- setdiff(as.character(non_ids), existing_list)

      if (length(new_to_add) > 0) {
        updated_linked <- if (length(existing_list) > 0) {
          paste(c(existing_list, new_to_add), collapse = ";")
        } else {
          paste(new_to_add, collapse = ";")
        }
        df$linked_items[active_row_idx] <- updated_linked

        # Remove non-active rows
        rows_to_remove <- which(df$id %in% non_ids)
        if (length(rows_to_remove) > 0) {
          df <- df[-rows_to_remove, ]
        }

        # Write back
        write.csv(df, csv_path, row.names = FALSE)

        # Show success toast
        shinyjs::runjs(sprintf("
          const toast = document.createElement('div');
          toast.innerHTML = '✓ %s successfully linked with selected products!';
          toast.style.cssText = 'position: fixed; top: 20px; right: 20px; background-color: #28a745; color: white; padding: 15px 20px; border-radius: 5px; font-size: 14px; z-index: 9999; box-shadow: 0 2px 8px rgba(0,0,0,0.2); animation: slideIn 0.3s ease-in-out;';
          document.body.appendChild(toast);
          setTimeout(() => {
            toast.style.animation = 'slideOut 0.3s ease-in-out';
            setTimeout(() => toast.remove(), 300);
          }, 3000);
        ", df$name[active_row_idx]))

        # Add toast CSS animations (only once)
        shinyjs::runjs("
          if (!document.getElementById('toast-styles')) {
            const style = document.createElement('style');
            style.id = 'toast-styles';
            style.innerHTML = `
              @keyframes slideIn {
                from { transform: translateX(400px); opacity: 0; }
                to { transform: translateX(0); opacity: 1; }
              }
              @keyframes slideOut {
                from { transform: translateX(0); opacity: 1; }
                to { transform: translateX(400px); opacity: 0; }
              }
            `;
            document.head.appendChild(style);
          }
        ")
      }
    }
  })
  
  rv_data <- reactiveVal(read.csv(csv_path, stringsAsFactors = FALSE))

  # Reactive : newly added (active = 1) - created today or within last 7 days
  added_products <- reactive({
    df <- rv_data()
    if (!"active" %in% names(df)) {
      return(data.frame())
    } # safety check
    if (!"created" %in% names(df)) {
      return(data.frame())
    } # must have created column

    # Convert created to Date
    df$created <- as.Date(df$created, format = "%Y-%m-%d")

    # Filter: active = 1 AND created within last 7 days (or today)
    today <- Sys.Date()
    week_ago <- today - 7

    added <- df[df$active == 1 & df$created >= week_ago & df$created <= today, , drop = FALSE]

    # Handle any NA dates
    added$created[is.na(added$created)] <- Sys.Date()

    added
  })

  # Render Added Products Table
  output$added_products_table <- renderDT({
    df <- added_products()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No added products yet")))
    }
    datatable(df, options = list(pageLength = 10))
  })

  # Render Products Added Per Day (Bar Chart)
  output$added_products_chart <- renderPlotly({
    df <- added_products()
    if (nrow(df) == 0) {
      return(NULL)
    }
    df$created <- as.Date(df$created)
    daily_count <- as.data.frame(table(df$created))
    names(daily_count) <- c("Date", "Count")

    p <- plot_ly(daily_count, x = ~Date, y = ~Count, type = "bar", marker = list(color = "green")) %>%
      layout(
        title = "Products Added Per Day",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Number of Products Added")
      )
    p
  })

  # Nutrition Completeness Gauge - shows data quality of new products
  output$nutrition_completeness_chart <- renderPlotly({
    df <- added_products()
    if (nrow(df) == 0) {
      return(NULL)
    }

    nutrition_cols <- c("energy", "protein", "fat", "carbohydrates", "sugar", "sodium", "dietary_fiber")

    # Calculate completeness for each product
    completeness_per_product <- data.frame()

    for (i in 1:nrow(df)) {
      product_name <- df$name[i]
      filled_count <- sum(!is.na(df[i, nutrition_cols]) & df[i, nutrition_cols] != "")
      total_count <- length(nutrition_cols)
      completeness_pct <- (filled_count / total_count) * 100

      completeness_per_product <- rbind(
        completeness_per_product,
        data.frame(
          Product = product_name,
          Completeness = completeness_pct
        )
      )
    }

    # Sort by completeness
    completeness_per_product <- completeness_per_product[order(completeness_per_product$Completeness, decreasing = TRUE), ]

    # Create bar chart for nutrition completeness
    p <- plot_ly(completeness_per_product,
      x = ~ reorder(Product, Completeness),
      y = ~Completeness,
      type = "bar",
      marker = list(
        color = ~Completeness,
        colorscale = "RdYlGn",
        showscale = TRUE,
        colorbar = list(title = "Completeness %")
      )
    ) %>%
      layout(
        title = "Nutrition Data Completeness by Product",
        xaxis = list(title = "Product", tickangle = -45),
        yaxis = list(title = "Completeness (%)", range = c(0, 100)),
        margin = list(b = 100),
        showlegend = FALSE
      )
    p
  })

  # create a new food product
  observeEvent(input$decision_create, {
    req(selected_id())

    # Read CSV
    df <- read.csv(csv_path, stringsAsFactors = FALSE)

    # Find row
    new_row_idx <- which(df$id == selected_id())

    if (length(new_row_idx) == 0) {
      return()
    }

    # Set active to 1
    df$active[new_row_idx] <- 1

    # Ensure created column exists and set current timestamp
    if (!"created" %in% names(df)) {
      df$created <- NA_character_ # Initialize with NA
    }
    df$created[new_row_idx] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

    # Write back
    write.csv(df, csv_path, row.names = FALSE)

    # Update reactive dataset so UI updated automatically
    rv_data(df)

    # Show auto-dismissing green toast notification
    shinyjs::runjs("
      const toast = document.createElement('div');
      toast.innerHTML = '✓ Successfully added product!';
      toast.style.cssText = 'position: fixed; top: 20px; right: 20px; background-color: #28a745; color: white; padding: 15px 20px; border-radius: 5px; font-size: 14px; z-index: 9999; box-shadow: 0 2px 8px rgba(0,0,0,0.2); animation: slideIn 0.3s ease-in-out;';
      document.body.appendChild(toast);
      setTimeout(() => {
        toast.style.animation = 'slideOut 0.3s ease-in-out';
        setTimeout(() => toast.remove(), 300);
      }, 3000);
    ")

    # Add CSS animations
    shinyjs::runjs("
      if (!document.getElementById('toast-styles')) {
        const style = document.createElement('style');
        style.id = 'toast-styles';
        style.innerHTML = `
          @keyframes slideIn {
            from { transform: translateX(400px); opacity: 0; }
            to { transform: translateX(0); opacity: 1; }
          }
          @keyframes slideOut {
            from { transform: translateX(0); opacity: 1; }
            to { transform: translateX(400px); opacity: 0; }
          }
        `;
        document.head.appendChild(style);
      }
    ")
  })

  # delete a food product
  observeEvent(input$decision_delete, {
    req(selected_id())

    # Read CSV
    df <- read.csv(csv_path, stringsAsFactors = FALSE)

    # Find row to delete
    delete_idx <- which(df$id == selected_id())

    if (length(delete_idx) == 0) {
      return()
    }

    # Remove the row
    df <- df[-delete_idx, ]

    # Write back to CSV
    write.csv(df, csv_path, row.names = FALSE)

    # Update reactive dataset so UI refreshes
    rv_data(df)

    # Show auto-dismissing red toast notification
    shinyjs::runjs("
      const toast = document.createElement('div');
      toast.innerHTML = '✗ Successfully deleted product!';
      toast.style.cssText = 'position: fixed; top: 20px; right: 20px; background-color: #dc3545; color: white; padding: 15px 20px; border-radius: 5px; font-size: 14px; z-index: 9999; box-shadow: 0 2px 8px rgba(0,0,0,0.2); animation: slideIn 0.3s ease-in-out;';
      document.body.appendChild(toast);
      setTimeout(() => {
        toast.style.animation = \"slideOut 0.3s ease-in-out\";
        setTimeout(() => toast.remove(), 300);
      }, 3000);
    ")

    # Add CSS animations (same as create)
    shinyjs::runjs("
      if (!document.getElementById('toast-styles')) {
        const style = document.createElement('style');
        style.id = 'toast-styles';
        style.innerHTML = `
          @keyframes slideIn {
            from { transform: translateX(400px); opacity: 0; }
            to { transform: translateX(0); opacity: 1; }
          }
          @keyframes slideOut {
            from { transform: translateX(0); opacity: 1; }
            to { transform: translateX(400px); opacity: 0; }
          }
        `;
        document.head.appendChild(style);
      }
    ")
  })


    #  Reactive: Combined products for editing 
    edit_products_data <- reactiveVal(NULL)

    observe({
      df <- processed_data()
      if (nrow(df) == 0) return()

      # Inactive first
      df <- df[order(df$active), ]

      edit_products_data(df)
    })

    # Columns of interest for highlighting
    numeric_cols <- c("energy", "protein", "fat", "carbohydrates", "sugar", "starch", "dietary_fiber")

    #  Render DT with single selection 
    output$edit_products_table <- renderDT({
      df <- edit_products_data()
      if (nrow(df) == 0) return(datatable(data.frame(Message="No products")))


      datatable(
        df,
        selection = "single",
        escape = FALSE,
        rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE)
      )
    })

    #  Observe row selection for editing 
    observeEvent(input$edit_products_table_rows_selected, {
      sel <- input$edit_products_table_rows_selected
      if (length(sel) == 0) return()

      df <- edit_products_data()
      if (nrow(df) < sel) return()
      selected_row <- df[sel, , drop = FALSE]
      prod_id <- as.integer(selected_row$id)

      # Create modal inputs (side-by-side)
      cols <- names(selected_row)
      modal_inputs <- lapply(cols, function(col) {
        val <- selected_row[[col]][1]
        input_id <- paste0("edit_", col)

        if (is.numeric(val)) {
          numericInput(input_id, label = col, value = val, width = "150px")
        } else if (inherits(val, "POSIXct")) {
          textInput(input_id, label = col, value = as.character(val), width = "150px")
        } else {
          textInput(input_id, label = col, value = as.character(val), width = "150px")
        }
      })

      # Arrange in grid of 3 columns per row
      n_cols <- 3
      rows <- split(modal_inputs, ceiling(seq_along(modal_inputs)/n_cols))
      layout_tags <- lapply(rows, function(r) {
        fluidRow(lapply(r, function(input_tag) column(4, input_tag)))
      })

      # Show modal with styled Save/Cancel buttons
      showModal(
        modalDialog(
          title = paste("Edit Product:", selected_row$name),
          footer = NULL,  # buttons on top
          size = "l",
          easyClose = TRUE,
          tagList(
            fluidRow(
              style = "display: flex; gap: 10px; margin-bottom: 15px;",
                actionButton(
                  "save_edit_product", "Save",
                  style = "background-color: #06D6A0; color: white; font-weight: bold; border-radius: 5px; padding: 10px 20px;"
                ),
                actionButton(
                  "cancel_edit_product", "Cancel",
                  style = "background-color: #EF476F; color: white; font-weight: bold; border-radius: 5px; padding: 10px 20px;"
                )


            ),
            tags$hr(),
            do.call(tagList, layout_tags)
          )
        )
      )
      # Apply style via JS after modal is rendered
      shinyjs::runjs("
        $('.modal-content').css({
        'background-color': '#FFE1EA',
        'color': 'maroon',
        'border-radius': '10px',
        'border': '2px solid maroon',
        'padding': '15px'
      });
      $('.modal-header, .modal-body, .modal-footer').css('color', 'maroon');
    ")

      selected_id(prod_id)  # store selected product ID
    })

    #  Cancel button closes modal 
    observeEvent(input$cancel_edit_product, {
      removeModal()
  })


    #  Save changes 
    observeEvent(input$save_edit_product, {
      req(selected_id())
      df <- edit_products_data()
      prod_id <- selected_id()
      idx <- which(df$id == prod_id)

      if (length(idx) == 0) return()

      # Update all columns from modal inputs
      for (col in names(df)) {
        input_id <- paste0("edit_", col)
        if (!is.null(input[[input_id]])) {
          val <- input[[input_id]]
          if (is.numeric(df[[col]])) {
            df[idx, col] <- as.numeric(val)
          } else if (inherits(df[[col]], "POSIXct")) {
            df[idx, col] <- as.POSIXct(val, tz = "UTC")
          } else {
            df[idx, col] <- val
          }
        }
      }

      # Update reactive
      edit_products_data(df)

      # Write back to CSV
      write.csv(df, csv_path, row.names = FALSE)

      removeModal()

      # Show auto-dismissing green toast notification
      shinyjs::runjs("
        const toast = document.createElement('div');
        toast.innerHTML = '✓ Changes successfully saved!';
        toast.style.cssText = 'position: fixed; top: 20px; right: 20px; background-color: #28a745; color: white; padding: 15px 20px; border-radius: 5px; font-size: 14px; z-index: 9999; box-shadow: 0 2px 8px rgba(0,0,0,0.2); animation: slideIn 0.3s ease-in-out;';
        document.body.appendChild(toast);
        setTimeout(() => {
          toast.style.animation = 'slideOut 0.3s ease-in-out';
          setTimeout(() => toast.remove(), 300);
        }, 3000);
      ")
    })

    # --- Brand & Category: variance box plots ---
    # Builds an interactive Plotly boxplot showing nutrient variability
    # within brands or categories (data prepared in Python)
    build_variance_boxplot <- function(variance_obj, title_text, color) {
      
      # Guard clause: nothing to plot if Python returned no data
      if (is.null(variance_obj) || length(variance_obj$data) == 0) return(NULL)

      # Nutrients available for the dropdown
      nutrients <- names(variance_obj$data)
      # Groups = top brands or categories
      groups <- variance_obj$groups
      m <- length(groups)

      # Plotting & computing margins
      plot_height <- min(2000, 40 * m + 200)
      max_label_len <- max(nchar(as.character(groups)))
      left_margin <- min(500, max(120, max_label_len * 7))

      p <- plot_ly(height = plot_height)
      buttons <- list()

      # For each nutrient, create one boxplot per group
      for (j in seq_along(nutrients)) {
        nutrient <- nutrients[j]

        for (g in groups) {
          vals <- variance_obj$data[[nutrient]][[g]]
          if (length(vals) == 0) vals <- NA

          p <- add_trace(
            p,
            type = "box",
            x = vals,
            name = g,
            orientation = "h",
            boxpoints = "outliers",
            marker = list(color = color),
            visible = (j == 1),
            showlegend = FALSE,
            hovertemplate = paste0(
              "%{y}<br>", nutrient, ": %{x:.3f}<extra></extra>"
            )
          )
        }

        # Control visibility so only one nutrient's traces show at a time
        vis <- rep(FALSE, length(nutrients) * m)
        start_idx <- (j - 1) * m + 1
        vis[start_idx:(start_idx + m - 1)] <- TRUE

        # Dropdown button for this nutrient
        buttons[[j]] <- list(
          method = "restyle",
          args = list(list(visible = vis)),
          label = nutrient
        )
      }

      layout(
        p,
        title = title_text,
        xaxis = list(title = "Value"),
        yaxis = list(
          title = variance_obj$group_type,
          automargin = TRUE,
          autorange = "reversed",
          tickfont = list(size = 10)
        ),
        updatemenus = list(list(
          active = 0,
          buttons = buttons,
          x = 1.02 + (left_margin / 1000),
          y = 1,
          showactive = TRUE
        )),
        margin = list(l = left_margin, r = 220, t = 60, b = 80)
      )
    }

    # Variance by brand
    output$plot_variance_brand <- renderPlotly({
      req(input$tabs == "variance")

      df <- processed_data()
      if (nrow(df) == 0) return(NULL)

      # Suppress warnings from coercion to numeric
      variance <- suppressWarnings(
        py$compute_variance_by_brand(
          df,
          numeric_cols = numeric_cols,
          top_n = 15
        )
      )

      build_variance_boxplot(
        variance,
        title_text = "Nutrient Distribution by Brand",
        color = "steelblue"
      )
    })

    # Variance by category
    output$plot_variance_category <- renderPlotly({
      req(input$tabs == "variance")
      
      df <- processed_data()
      if (nrow(df) == 0) return(NULL)

      # Suppress warnings from coercion to numeric
      variance <- suppressWarnings(
        py$compute_variance_by_category(
          df,
          numeric_cols = numeric_cols,
          top_n = 15
        )
      )

      build_variance_boxplot(
        variance,
        title_text = "Nutrient Distribution by Category",
        color = "darkorange"
      )
    }) 
}

