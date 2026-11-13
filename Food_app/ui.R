source("credentials.R")

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Food App Dashboard",
    titleWidth = 250
  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data View", tabName = "data", icon = icon("table")),
      menuItem("New Products", tabName = "new_products", icon = icon("exclamation-circle")),
      menuItem("Search & Compare", tabName = "search", icon = icon("search")),
      menuItem("Nutrient Variance Brand & Category", tabName = "variance", icon = icon("chart-line")),
      menuItem("Added Products", tabName = "added_products", icon = icon("plus-circle")),
      menuItem("Edit Products", tabName = "edit_products", icon = icon("edit"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "home",
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: linear-gradient(135deg, #FFE1EA 0%, #FFE1EA 100%);
                       padding: 40px; border-radius: 15px; text-align: center;
                       box-shadow: 0 4px 12px rgba(0,0,0,0.08); margin-bottom: 25px; border: 2px solid #EDA4BD;",
              h1("Welcome to the Food App", style = "color: maroon; font-weight: 800; font-size: 38px;"),
              p("Analyze and explore food data effortlessly.", style = "color: maroon; font-size: 18px; margin-top: 10px;")
            ),
            div(
              style = "background: #FFE1EA; padding: 25px; border-radius: 15px;
                       box-shadow: 0 3px 10px rgba(0,0,0,0.05); margin-top: 20px; border: 2px solid #EDA4BD;",
              h3("Get Started!", style = "color: maroon; font-weight: 700;"),
              tags$ul(
                style = "list-style: none; padding-left: 0; line-height: 2.2;",
                tags$li(tags$span(style="background-color:#FF6B6B; color:white; padding:5px 10px; border-radius:10px; margin-right:8px;", icon("table")),
                        strong("Data View:"), " Browse the full food dataset with filtering and sorting."),
                tags$li(tags$span(style="background-color:#FF9F43; color:white; padding:5px 10px; border-radius:10px; margin-right:8px;", icon("star")),
                        strong("New Products:"), " Check and verify active == 0 products."),
                tags$li(tags$span(style="background-color:#1DD1A1; color:white; padding:5px 10px; border-radius:10px; margin-right:8px;", icon("search")),
                        strong("Search & Compare:"), " Find products, compare, delete, and link or create new entries."),
                tags$li(tags$span(style="background-color:#54A0FF; color:white; padding:5px 10px; border-radius:10px; margin-right:8px;", icon("plus-circle")),
                        strong("Added Products:"), " Track recently added products and check nutrition completeness. Stay updated!"),
                tags$li(tags$span(style="background-color:#9B59B6; color:white; padding:5px 10px; border-radius:10px; margin-right:8px;",icon("edit")),
                        strong("Edit Products:"), " Modify existing food products, update nutritional values, and correct product details."),
              tags$li(tags$span(style="background-color:#1ABC9C; color:white; padding:5px 10px; border-radius:10px; margin-right:8px;", icon("line-chart")),
                        strong("Nutrient Variance Brand & Category:"), " Visualize nutrient variability across brands and categories with interactive plots to spot differences, outliers, and trends.")
              )
            )
          ),
          column(
            style = "margin-top: 30px;",
            width = 12,
            infoBoxOutput("total_items", width = 12)
          )
        )
      ),
      tabItem(
        tabName = "pending_reviews",
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: linear-gradient(135deg, #FFE1EA 0%, #FFE1EA 100%);
                       padding: 40px; border-radius: 15px; text-align: center;
                       box-shadow: 0 4px 12px rgba(0,0,0,0.08); margin-bottom: 25px; border: 2px solid #EDA4BD;",
              h1("Welcome to the Food App", style = "color: maroon; font-weight: 800; font-size: 38px;"),
              p("Analyze and explore food data effortlessly.", style = "color: maroon; font-size: 18px; margin-top: 10px;")
            ),
            div(
              style = "background: #FFE1EA; padding: 25px; border-radius: 15px;
                       box-shadow: 0 3px 10px rgba(0,0,0,0.05); margin-top: 20px; border: 2px solid #EDA4BD;",
              h3("Get Started!", style = "color: maroon; font-weight: 700;"),
              tags$ul(
                style = "list-style: none; padding-left: 0; line-height: 2.2;",
                tags$li(tags$span(style="background-color:#FF6B6B; color:white; padding:5px 10px; border-radius:10px; margin-right:8px;", icon("table")),
                        strong("Data View:"), " Browse the full food dataset with filtering and sorting."),
                tags$li(tags$span(style="background-color:#FF9F43; color:white; padding:5px 10px; border-radius:10px; margin-right:8px;", icon("star")),
                        strong("New Products:"), " Check and verify active == 0 products."),
                tags$li(tags$span(style="background-color:#1DD1A1; color:white; padding:5px 10px; border-radius:10px; margin-right:8px;", icon("search")),
                        strong("Search & Compare:"), " Find products, compare, delete, and link or create new entries."),
                tags$li(tags$span(style="background-color:#54A0FF; color:white; padding:5px 10px; border-radius:10px; margin-right:8px;", icon("plus-circle")),
                        strong("Added Products:"), " Track recently added products and check nutrition completeness. Stay updated!"),
                tags$li(tags$span(style="background-color:#9B59B6; color:white; padding:5px 10px; border-radius:10px; margin-right:8px;",icon("edit")),
                        strong("Edit Products:"), " Modify existing food products, update nutritional values, and correct product details."),
                tags$li(tags$span(style="background-color:#1ABC9C; color:white; padding:5px 10px; border-radius:10px; margin-right:8px;", icon("line-chart")),
                        strong("Nutrient Variance Brand & Category:"), " Visualize nutrient variability across brands and categories with interactive plots to spot differences, outliers, and trends.")
              )
            )
          ),
          column(
            style = "margin-top: 30px;",
            width = 12,
            infoBoxOutput("total_items", width = 12)
          )
        )
      ),
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            width = 12,
            title = "Pending Reviews Table",
            status = "info",
            solidHeader = TRUE,
            style = "overflow-x: auto;",
            DTOutput("food_table")
          )
        )
      ),
      tabItem(
        tabName = "new_products",
        fluidRow(
          box(
            width = 12,
            title = "New Food Products Needing Verification",
            status = "warning",
            solidHeader = TRUE,
            DTOutput("new_products_table"),
            p("This section highlights new food products added to the dataset. Click a row to select and auto-populate in other tabs.")
          )
        )
      ),
      tabItem(
        tabName = "search",
        fluidRow(
          box(
            width = 12,
            title = "Search Food Products",
            status = "warning",
            solidHeader = TRUE,
            textInput("search_query", "Enter Food Name or Barcode:", value = ""),
            tags$div(
              style = "display: flex; gap: 10px; align-items: center; flex-wrap: wrap;",
              actionButton("search_btn", "Search", class = "btn-primary btn-lg",
                         style = "background-color: #0565ffff; color: white; padding: 10px 20px; font-weight: bold; border-radius: 5px;"),
              actionButton("decision_create", "Create New", class = "btn-success btn-lg",
                           style = "background-color: #06D6A0; color: white; padding: 10px 15px; font-weight: bold; border-radius: 5px;"),
              actionButton("decision_delete", "Delete", class = "btn-danger btn-lg",
                           style = "background-color: #EF476F; color: white; padding: 10px 15px; font-weight: bold; border-radius: 5px;")
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Selected Product & Actions",
            status = "success",
            solidHeader = TRUE,
            verbatimTextOutput("selected_product_info"),
            br(),
            tags$div(
              style = "display: flex; gap: 10px; flex-wrap: wrap;",
              actionButton("decision_link", "Link Products", class = "btn-info btn-lg", 
                           style = "background-color: #E36397; color: white; padding: 10px 15px; font-weight: bold; border-radius: 5px;")
            )
          )
        ),
        fluidRow(
          div(
            id = "active_section",
            box(
              width = 12,
              title = "Review Similar Active Products",
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              p("Select an active product to link with, or continue to create a new one",
                style = "font-weight: bold; color: #0c5460;"),
              tags$div(
                style = "text-align: right; margin-bottom: 15px;",
                actionButton("continue_btn", "Continue to Non-Active Products", icon = icon("arrow-right"), 
                             class = "btn-warning btn-lg",
                             style = "background-color: #FFE1EA; color: maroon; padding: 10px 15px; font-weight: bold; border-radius: 5px; font-size: 14px;")
              ),
              hr(),
              h4("Similar Active Products by Name (NLP)", style = "color: maroon; font-weight: bold;"),
              DTOutput("similar_active_table"),
              br(),
              h4("Similar Active Items by Nutrition", style = "color: maroon; font-weight: bold;"),
              DTOutput("similar_active_nutrition_table"),
              br(),
              h4("Similarity Heatmap (Active)", style = "color: maroon; font-weight: bold;"),
              plotlyOutput("similarity_heatmap_active", height = "400px"),
              br(),
              h4("Cluster Visualization (Active)", style = "color: maroon; font-weight: bold;"),
              plotlyOutput("cluster_plot_active", height = "600px"),
              br(),
              numericInput("num_closest_active", "Number of closest products to show (0 for all):", value = 0, min = 0),
              br(),
              h4("Closest Products by Distance (Top 20)", style = "color: maroon; font-weight: bold;"),
              DTOutput("closest_products_active")
            )
          ),
          shinyjs::hidden(
            div(
              id = "nonactive_section",
              box(
                width = 12,
                title = "Step 2: Review Similar Non-Active Products to Merge",
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                p("Select non-active products to merge with the active product you chose",
                  style = "font-weight: bold; color: #664d03;"),
                tags$div(
                  style = "text-align: left; margin-bottom: 15px;",
                  actionButton("back_btn", "Back to Active Products", icon = icon("arrow-left"),
                               class = "btn-info btn-lg",
                               style = "background-color: #FFE1EA; color: maroon; padding: 10px 15px; font-weight: bold; border-radius: 5px; font-size: 14px;")
                ),
                hr(),
                h4("Similar Non-Active Products by Name (NLP)", style = "color: maroon; font-weight: bold;"),
                DTOutput("similar_nonactive_table"),
                br(),
                h4("Similar Non-Active Items by Nutrition", style = "color: maroon; font-weight: bold;"),
                DTOutput("similar_nonactive_nutrition_table"),
                br(),
                h4("Similarity Heatmap (Non-Active)", style = "color: maroon; font-weight: bold;"),
                plotlyOutput("similarity_heatmap_nonactive", height = "400px"),
                br(),
                h4("Cluster Visualization (Non-Active)", style = "color: maroon; font-weight: bold;"),
                plotlyOutput("cluster_plot_nonactive", height = "600px"),
                h4("Closest Products by Distance (Top 20)", style = "color: maroon; font-weight: bold;"),
                DTOutput("closest_products_nonactive"),
                br(),
                numericInput("num_closest_nonactive", "Number of closest products to show (0 for all):", value = 0, min = 0)
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Missing Nutritional Fields",
            status = "danger",
            solidHeader = TRUE,
            collapsible = TRUE,
            DTOutput("missing_table")
          )
        )
      ),
      tabItem(
        tabName = "added_products",
        fluidRow(
          box(
            width = 12,
            title = "Recently Added Food Product",
            status = "success",
            solidHeader = TRUE,
            DTOutput("added_products_table")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Products Added Per Day",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("added_products_chart", height = "400px")
          ),
          box(
            width = 6,
            title = "Nutrition Data Completeness",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("nutrition_completeness_chart", height = "400px")
          )
        )
      ),
      tabItem(
        tabName = "edit_products",
        fluidRow(
          box(
            width = 12,
            title = "Edit Food Products",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("edit_products_table"),
            p("Select a product from the table to review and update its details. Changes are saved to the CSV automatically."),
            uiOutput("edit_success_msg")
          )
        )
      ),
    tabItem(
      tabName = "variance",
        fluidRow(
          box(
            width = 12,
            title = "Brand Nutrition Variance Boxplot",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("plot_variance_brand", height = "800px")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Category Nutrition Variance Boxplot",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("plot_variance_category", height = "800px")
          )
        ),
      )
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage

# Conditional wrap for login (toggle based on env var)
if (Sys.getenv("DEV_MODE") == "TRUE") {
  ui <- ui
} else {
  ui <- secure_app(ui, credentials = credentials)
}
