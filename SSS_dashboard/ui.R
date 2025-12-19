library(shiny)
library(plotly)
library(DT)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = "#fafbfc",
    fg = "#2c3e50",
    primary = "#2d6a4f",
    secondary = "#52796f",
    success = "#40916c",
    info = "#588157",
    warning = "#bc6c25",
    danger = "#c1121f",
    base_font = font_google("Inter"),
    heading_font = font_google("Playfair Display"),
    code_font = font_google("Source Code Pro"),
    font_scale = 0.95
  ) %>%
    bs_add_rules(
      "
      /* Global Styling */
      body {
        background-color: #f4f6f5;
        color: #344e41;
      }

      /* Title Panel */
      .container-fluid {
        padding: 0;
      }

      /* Navigation Tabs */
      .nav-tabs {
        border-bottom: 2px solid #d4e4dd;
        background-color: white;
        padding: 10px 20px 0;
        border-radius: 8px 8px 0 0;
        margin-top: 20px;
      }

      .nav-tabs > li > a {
        color: #52796f;
        font-weight: 500;
        padding: 12px 24px;
        border: none;
        border-radius: 6px 6px 0 0;
        transition: all 0.3s ease;
      }

      .nav-tabs > li > a:hover {
        background-color: #e8f2ed;
        color: #2d6a4f;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        background-color: #2d6a4f;
        color: white;
        border: none;
        font-weight: 600;
      }

      /* Headers */
      h2, h3, h4 {
        font-family: 'Playfair Display', serif;
        color: #2d6a4f;
        font-weight: 600;
        letter-spacing: -0.5px;
      }

      h3 {
        margin-top: 0;
        padding-top: 10px;
        border-bottom: 2px solid #d4e4dd;
        padding-bottom: 12px;
      }

      /* Sidebar Styling */
      .well {
        background: linear-gradient(to bottom, #ffffff, #f8faf9);
        border: 1px solid #d4e4dd;
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(45, 106, 79, 0.08);
        padding: 20px;
      }

      .sidebar {
        background-color: white;
        border-right: 1px solid #e8eeed;
        padding: 20px;
        border-radius: 8px;
      }

      /* Main Panel */
      .col-sm-8, .col-sm-9 {
        background-color: white;
        padding: 30px;
        border-radius: 8px;
        box-shadow: 0 2px 12px rgba(0, 0, 0, 0.06);
      }

      /* Buttons */
      .btn-primary {
        background: linear-gradient(135deg, #2d6a4f 0%, #40916c 100%);
        border: none;
        font-weight: 500;
        padding: 8px 16px;
        transition: all 0.3s ease;
        box-shadow: 0 2px 6px rgba(45, 106, 79, 0.2);
      }

      .btn-primary:hover {
        background: linear-gradient(135deg, #1b4332 0%, #2d6a4f 100%);
        transform: translateY(-1px);
        box-shadow: 0 4px 12px rgba(45, 106, 79, 0.3);
      }

      .btn-secondary {
        background-color: #84a98c;
        border: none;
        font-weight: 500;
        transition: all 0.3s ease;
      }

      .btn-secondary:hover {
        background-color: #6b8e7f;
        transform: translateY(-1px);
      }

      .btn-success {
        background: linear-gradient(135deg, #40916c 0%, #52b788 100%);
        border: none;
        font-weight: 600;
        box-shadow: 0 4px 12px rgba(64, 145, 108, 0.3);
        transition: all 0.3s ease;
      }

      .btn-success:hover {
        background: linear-gradient(135deg, #2d6a4f 0%, #40916c 100%);
        transform: translateY(-2px);
        box-shadow: 0 6px 16px rgba(64, 145, 108, 0.4);
      }

      /* Form Controls */
      .form-control, .selectize-input {
        border: 1.5px solid #d4e4dd;
        border-radius: 6px;
        padding: 10px 14px;
        transition: all 0.3s ease;
      }

      .form-control:focus, .selectize-input.focus {
        border-color: #40916c;
        box-shadow: 0 0 0 3px rgba(64, 145, 108, 0.1);
      }

      /* Sliders */
      .irs-bar {
        background: linear-gradient(to right, #40916c, #52b788);
        border: none;
      }

      .irs-from, .irs-to, .irs-single {
        background: #2d6a4f;
      }

      .irs-handle {
        background: white;
        border: 3px solid #2d6a4f;
        box-shadow: 0 2px 6px rgba(0, 0, 0, 0.15);
      }

      /* Box Components */
      .box {
        background-color: white;
        border-radius: 8px;
        border: 1px solid #e8eeed;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.04);
        margin-bottom: 20px;
        transition: all 0.3s ease;
      }

      .box:hover {
        box-shadow: 0 4px 16px rgba(0, 0, 0, 0.08);
      }

      .box-header {
        background: linear-gradient(135deg, #f1f8f4 0%, #e8f2ed 100%);
        color: #2d6a4f;
        border-bottom: 2px solid #d4e4dd;
        padding: 15px 20px;
        border-radius: 8px 8px 0 0;
        font-weight: 600;
      }

      .box.box-solid.box-primary > .box-header {
        background: linear-gradient(135deg, #2d6a4f 0%, #40916c 100%);
        color: white;
      }

      .box.box-solid.box-success > .box-header {
        background: linear-gradient(135deg, #40916c 0%, #52b788 100%);
        color: white;
      }

      .box.box-solid.box-info > .box-header {
        background: linear-gradient(135deg, #588157 0%, #6b9080 100%);
        color: white;
      }

      /* Tables */
      .dataTables_wrapper .dataTables_length select,
      .dataTables_wrapper .dataTables_filter input {
        border: 1.5px solid #d4e4dd;
        border-radius: 6px;
        padding: 6px 12px;
      }

      table.dataTable thead th {
        background: linear-gradient(to bottom, #f1f8f4, #e8f2ed);
        color: #2d6a4f;
        font-weight: 600;
        border-bottom: 2px solid #b7d9c9;
      }

      table.dataTable tbody tr:hover {
        background-color: #f8faf9;
      }

      /* Hr Separators */
      hr {
        border-top: 1px solid #d4e4dd;
        margin: 20px 0;
      }

      /* Help Text */
      .help-block {
        color: #6b8e7f;
        font-size: 0.9em;
        font-style: italic;
      }

      /* Strong Text */
      strong {
        color: #2d6a4f;
        font-weight: 600;
      }

      /* Wellpanel specific */
      .well.well-sm {
        background-color: #fafbfc;
        border: 1px solid #e8eeed;
      }

      /* Tab Content */
      .tab-content {
        background-color: white;
        padding: 30px;
        border-radius: 0 0 8px 8px;
        box-shadow: 0 2px 12px rgba(0, 0, 0, 0.06);
      }

      /* Custom Checkbox */
      input[type='checkbox'] {
        accent-color: #40916c;
      }

      /* Plotly graphs */
      .js-plotly-plot {
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.04);
      }
      /* Box title text color fix */
      .box.box-solid.box-primary > .box-header,
      .box.box-solid.box-primary > .box-header > .box-title {
        color: white !important;
      }

      .box.box-solid.box-success > .box-header,
      .box.box-solid.box-success > .box-header > .box-title {
        color: white !important;
      }

      .box.box-solid.box-info > .box-header,
      .box.box-solid.box-info > .box-header > .box-title {
        color: white !important;
      }
      "
    ),

  # Title
  div(
    style = "background: linear-gradient(135deg, #1b4332 0%, #2d6a4f 50%, #40916c 100%);
             padding: 40px 30px;
             margin: -15px -15px 0 -15px;
             box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
             border-bottom: 3px solid #081c15;",
    div(
      style = "max-width: 1200px; margin: 0 auto;",
      h1("National Parks Exploration Dashboard",
        style = "margin: 0;
                  font-weight: 700;
                  color: white;
                  text-align: center;
                  font-family: 'Playfair Display', serif;
                  letter-spacing: 1px;
                  text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.3);"
      ),
      p("Discover, Analyze, and Plan Your Perfect National Park Adventure",
        style = "text-align: center;
                 color: #b7e4c7;
                 margin: 15px 0 0 0;
                 font-size: 1.1em;
                 font-weight: 300;
                 letter-spacing: 0.5px;"
      )
    )
  ),

  # Main content with padding
  div(
    style = "max-width: 1400px; margin: 0 auto; padding: 0 20px;",

    # Main tab panels
    tabsetPanel(
      id = "main_tabs",

      # Tab 1: Interactive Park Map
      tabPanel(
        "Park Map",
        value = "map_tab",
        br(),
        sidebarLayout(
          sidebarPanel(
            style = "background-color: white; border-radius: 8px; padding: 25px; box-shadow: 0 2px 8px rgba(0,0,0,0.06);",
            h4("Interactive Map", style = "margin-top: 0; color: #2d6a4f; font-size: 1.4em;"),
            p("Explore the geographic distribution of America's National Parks. Each marker represents a unique park, while state colors indicate the number of parks within each state.",
              style = "line-height: 1.6; color: #52796f;"
            ),
            hr(style = "border-color: #d4e4dd; margin: 25px 0;"),
            div(
              style = "background-color: #f8faf9; padding: 15px; border-radius: 6px; border-left: 3px solid #40916c;",
              h5("How to Use:", style = "margin-top: 0; color: #2d6a4f; font-weight: 600;"),
              tags$ul(
                style = "margin-bottom: 0; padding-left: 20px;",
                tags$li("Hover over markers to view park names"),
                tags$li("Hover over states to see park counts and popular activities"),
                tags$li("Zoom and pan to explore specific regions")
              )
            )
          ),
          mainPanel(
            style = "background-color: white; padding: 30px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.06);",
            h3("United States National Parks"),
            plotlyOutput("park_location_map",
              width = "100%",
              height = "600px"
            ),
            hr(style = "border-color: #e8eeed; margin: 25px 0;"),
            p(strong("Note:"), " This interactive visualization displays all 59 U.S. National Parks with their geographic locations and state-level statistics.",
              style = "color: #6b8e7f; font-style: italic;"
            )
          )
        )
      ),

      # Tab 2: Clustering Analysis
      tabPanel(
        "Clustering Analysis",
        value = "cluster_tab",
        br(),
        sidebarLayout(
          sidebarPanel(
            style = "background-color: white; border-radius: 8px; padding: 25px; box-shadow: 0 2px 8px rgba(0,0,0,0.06);",
            h4("Clustering Settings", style = "margin-top: 0; color: #2d6a4f; font-size: 1.4em;"),
            div(
              style = "background-color: #f8faf9; padding: 15px; border-radius: 6px; margin-bottom: 20px;",
              sliderInput("k_clusters",
                "Number of Clusters:",
                min = 2,
                max = 9,
                value = 4
              )
            ),
            p("Group parks with similar characteristics using k-means clustering algorithm. Select features below to customize the analysis.",
              style = "line-height: 1.6; color: #52796f;"
            ),
            hr(style = "border-color: #d4e4dd; margin: 25px 0;"),
            wellPanel(
              style = "max-height: 400px; overflow-y: auto; background-color: white; border-radius: 6px; border: 1px solid #d4e4dd;",
              h4("Interest Selection", style = "margin-top: 0; color: #2d6a4f;"),
              div(
                style = "margin-bottom: 15px;",
                actionButton("select_all", "Select All", class = "btn-sm btn-primary"),
                actionButton("deselect_all", "Deselect All", class = "btn-sm btn-secondary")
              ),
              hr(style = "margin: 15px 0;"),
              uiOutput("feature_checkboxes"),
              hr(style = "margin: 15px 0;"),
              htmlOutput("selected_features_count")
            )
          ),
          mainPanel(
            style = "background-color: white; padding: 30px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.06);",
            tabsetPanel(
              tabPanel(
                "Cluster Visualization",
                br(),
                plotlyOutput("cluster_plot",
                  width = "100%",
                  height = "600px"
                ),
                htmlOutput("cluster_note")
              ),
              tabPanel(
                "Elbow Plot",
                br(),
                plotlyOutput("elbow_plot",
                  height = "600px"
                ),
                div(
                  style = "background-color: #f8faf9; padding: 15px; border-radius: 6px; margin-top: 20px; border-left: 3px solid #588157;",
                  p(strong("About the Elbow Method:"), " This plot helps identify the optimal number of clusters by showing the within-cluster sum of squares (WCSS) for different values of k. The 'elbow' point suggests a good balance between model complexity and cluster cohesion.",
                    style = "margin: 0; line-height: 1.6;"
                  )
                )
              ),
              tabPanel(
                "Cluster Results",
                br(),
                fluidRow(
                  box(
                    title = "Cluster Assignments",
                    DTOutput("cluster_table"),
                    status = "success",
                    width = 12,
                    solidHeader = TRUE
                  ),
                  box(
                    title = "Interpretation",
                    htmlOutput("cluster_summary"),
                    status = "info",
                    width = 12,
                    solidHeader = TRUE
                  ),
                  box(
                    title = "Cluster Feature Profiles",
                    width = 12,
                    status = "info",
                    solidHeader = TRUE,
                    p("This heatmap visualizes the average value of each selected interest across clusters.
                      Darker colors indicate higher relative values. Parks within the same cluster share similar
                      patterns across these features.",
                      style = "margin-bottom: 15px; font-size: 0.9em; color: #6b8e7f;"
                    ),
                    plotlyOutput("cluster_profile_heatmap", height = "450px"),
                    p(strong("How to read:"), " Each row represents a cluster, and each column represents a selected interest.
                     The color intensity shows how strongly that interest characterizes that cluster (normalized across all clusters).
                      Hover over cells to see exact values.",
                      style = "margin-top: 15px; font-size: 0.9em; color: #6b8e7f;"
                    )
                  )
                )
              )
            )
          )
        )
      ),

      # Tab 3: Park Recommendations
      tabPanel(
        "Park Recommendations",
        value = "rec_tab",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            style = "background-color: white; border-radius: 8px; padding: 25px; box-shadow: 0 2px 8px rgba(0,0,0,0.06);",
            h4("Recommendation Engine", style = "margin-top: 0; color: #2d6a4f; font-size: 1.3em;"),
            p(
              style = "font-size: 0.95em; color: #52796f; line-height: 1.6;",
              "Discover parks tailored to your interests and travel preferences using our intelligent recommendation system."
            ),
            hr(style = "border-color: #d4e4dd; margin: 20px 0;"),

            # Month selection
            div(
              style = "margin-bottom: 20px;",
              selectInput("pred_month",
                "Travel Month:",
                choices = c("Select a month" = "Any Month", month.name),
                selected = "Any Month"
              ),
              helpText("Recommendations are optimized for your selected month")
            ),
            hr(style = "border-color: #d4e4dd; margin: 20px 0;"),

            # Alpha slider
            div(
              style = "margin-bottom: 20px;",
              sliderInput("alpha_weight",
                "Balance Factor (α):",
                min = 0,
                max = 1,
                value = 0.5,
                step = 0.25
              ),
              htmlOutput("alpha_explanation")
            ),
            hr(style = "border-color: #d4e4dd; margin: 20px 0;"),

            # Number of recommendations
            sliderInput("n_recommendations",
              "Results to Display:",
              min = 5,
              max = 20,
              value = 10,
              step = 1
            ),
            hr(style = "border-color: #d4e4dd; margin: 20px 0;"),

            # Topic selection
            wellPanel(
              style = "max-height: 350px; overflow-y: auto; background-color: #fafbfc; border-radius: 6px; border: 1px solid #d4e4dd;",
              h4("Your Interests", style = "margin-top: 0; color: #2d6a4f;"),
              div(
                style = "margin-bottom: 15px;",
                actionButton("select_all_rec", "Select All", class = "btn-sm btn-primary"),
                actionButton("deselect_all_rec", "Clear All", class = "btn-sm btn-secondary")
              ),
              hr(style = "margin: 15px 0;"),
              uiOutput("recommendation_topic_checkboxes"),
              hr(style = "margin: 15px 0;"),
              htmlOutput("selected_topics_count_rec")
            ),
            hr(style = "border-color: #d4e4dd; margin: 20px 0;"),

            # Get recommendations button
            actionButton(
              "get_recommendations",
              "Generate Recommendations",
              class = "btn-success btn-lg",
              style = "width: 100%; font-weight: 600; font-size: 16px; padding: 14px;"
            )
          ),
          mainPanel(
            width = 9,
            style = "background-color: white; padding: 30px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.06);",
            tabsetPanel(
              # Main Recommendations
              tabPanel(
                "Your Results",
                br(),
                htmlOutput("recommendation_summary"),
                br(),
                plotlyOutput("recommendation_plot", height = "500px"),
                br(),
                h4("Detailed Rankings", style = "color: #2d6a4f; border-bottom: 2px solid #d4e4dd; padding-bottom: 10px;"),
                p("Final Score = α × Popularity + (1-α) × Suitability",
                  style = "color: #6b8e7f; font-style: italic; margin-bottom: 20px;"
                ),
                DTOutput("recommendation_table"),
                br(),
                htmlOutput("selected_interests_display")
              ),

              # Park Locations Map
              tabPanel(
                "Geographic View",
                br(),
                wellPanel(
                  style = "background: linear-gradient(to right, #f1f8f4, #e8f2ed); border-radius: 8px; border: 1px solid #d4e4dd; padding: 20px;",
                  h3("Recommended Parks Map", style = "margin-top: 0; color: #2d6a4f;"),
                  p("Explore the locations of your personalized park recommendations across the United States.",
                    style = "margin-bottom: 0; line-height: 1.6;"
                  )
                ),
                br(),
                plotlyOutput("recommendation_map", height = "600px"),
                br(),
                p(
                  style = "color: #6b8e7f; font-size: 0.95em; font-style: italic;",
                  "Hover over markers to view detailed scores. Color intensity represents recommendation strength."
                )
              ),

              # About This Model
              tabPanel(
                "Methodology",
                br(),
                wellPanel(
                  style = "background: linear-gradient(to bottom, #f8faf9, #ffffff); border-radius: 8px; border: 1px solid #d4e4dd; padding: 25px;",
                  h3("Understanding the Recommendation System", style = "margin-top: 0; color: #2d6a4f;"),
                  p("Our recommendation engine combines two key metrics to provide personalized park suggestions:",
                    style = "line-height: 1.6; margin-bottom: 20px;"
                  ),
                  tags$ul(
                    style = "line-height: 1.8; margin-bottom: 20px;",
                    tags$li(tags$strong("Suitability Score:"), " Measures how well a park's features align with your selected interests using correlation analysis"),
                    tags$li(tags$strong("Popularity Score:"), " Reflects visitor traffic patterns based on 2024 annual visitation data")
                  ),
                  p("The balance between these components is controlled by the α (alpha) parameter in the sidebar.",
                    style = "line-height: 1.6; margin-bottom: 0;"
                  )
                ),
                br(),
                wellPanel(
                  style = "background: linear-gradient(to bottom, #fef9f5, #ffffff); border-radius: 8px; border: 1px solid #e8ddd0; padding: 25px;",
                  h4("The Balance Factor (α)", style = "margin-top: 0; color: #bc6c25;"),
                  p("Adjust α to control the weighting between personalization and popularity:",
                    style = "line-height: 1.6; margin-bottom: 20px;"
                  ),
                  tags$ul(
                    style = "line-height: 1.8;",
                    tags$li(tags$strong("α = 0.0:"), " Pure personalization – recommendations based solely on your interests"),
                    tags$li(tags$strong("α = 0.25:"), " Interest-focused with slight popularity influence"),
                    tags$li(tags$strong("α = 0.5:"), " Balanced approach – equal weight to both factors"),
                    tags$li(tags$strong("α = 0.75:"), " Popularity-focused with personalization adjustments"),
                    tags$li(tags$strong("α = 1.0:"), " Pure popularity – shows most visited parks regardless of interests")
                  )
                )
              )
            )
          )
        )
      ),

      # Tab 4: Data Explorer
      tabPanel(
        "Data Explorer",
        value = "data_tab",
        br(),
        div(
          style = "background-color: white; padding: 35px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.06);",
          h3("Park Characteristics Dataset", style = "margin-top: 0; color: #2d6a4f;"),
          p("Browse the complete dataset of park features, activities, and characteristics used throughout this dashboard.",
            style = "color: #6b8e7f; line-height: 1.6; margin-bottom: 30px;"
          ),
          hr(style = "border-color: #d4e4dd; margin-bottom: 30px;"),
          DTOutput("data_table")
        )
      )
    )
  )
)
