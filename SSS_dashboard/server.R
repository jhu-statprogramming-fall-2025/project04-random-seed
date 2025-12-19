library(shiny)
library(plotly)
library(DT)
library(dplyr)
library(tidyverse)
library(factoextra)
library(cluster)
library(ggplot2)
library(usmap)
library(maps)
library(sf)

# Read the data
parks_data <- read.csv("Data_Folder/national_parks_topic_matrix.csv")
load("Data_Folder/national_parks_df.Rdata")

clean_col <- function(x) {
    x <- str_replace_all(x, "\\.+", " ")
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
}

new_names <- names(parks_data)
special <- c("parkCode", "fullName")

idx_other <- which(!new_names %in% special)
new_names[idx_other] <- clean_col(new_names[idx_other])

names(parks_data) <- new_names

# remove "Great Smoky Mountains National Park" from national_parks_df, 27th row
national_parks_df_clean <- national_parks_df[-c(27), ]
nrow(national_parks_df_clean)

national_parks_df_clean$fullName

park_coords <- national_parks_df %>% select(parkCode, latitude, longitude)
park_coords_state <- national_parks_df %>% select(parkCode, fullName, latitude, longitude, states)

source("PredictionModel.R")

month_mapping <- data.frame(
    abbr = c(
        "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
        "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
    ),
    full = month.name,
    number = 1:12
)

function(input, output, session) {
    # ==================== SET UP ====================

    # Reactive data
    parks_reactive <- reactive({
        parks_data
    })

    parks_coord_reactive <- reactive({
        park_coords
    })

    # All available features
    available_features <- reactive({
        df <- parks_reactive()
        names(df)[!names(df) %in% c("parkCode", "fullName")]
    })


    make_valid_id <- function(name) {
        gsub("[^A-Za-z0-9_]", "_", name)
    }

    # UI: Dynamic feature checkboxes
    output$feature_checkboxes <- renderUI({
        features <- available_features()

        checkbox_list <- lapply(features, function(feat) {
            # Create label with spaces
            label <- gsub("\\.", " ", feat)
            label <- gsub("_", " ", label)

            # Create valid ID without spaces
            valid_id <- make_valid_id(feat)

            checkboxInput(
                inputId = paste0("feat_", valid_id),
                label = label,
                value = TRUE
            )
        })
        do.call(tagList, checkbox_list)
    })


    # UI: Display count of selected features
    output$selected_features_count <- renderUI({
        features <- available_features()
        selected <- length(selected_features())

        color <- if (selected < 3) "red" else if (selected < 5) "orange" else "green"

        HTML(paste0(
            "<b>Selected Interests: </b>",
            "<span style='color: ", color, "; font-weight: bold;'>", selected, " / ", length(features), "</span>",
            if (selected < 3) "<br><em style='color: red;'>Warning: Select at least 3 features for meaningful clustering</em>" else ""
        ))
    })

    selected_features <- reactive({
        features <- available_features()
        selected <- sapply(features, function(f) {
            valid_id <- make_valid_id(f)
            val <- input[[paste0("feat_", valid_id)]]
            if (is.null(val)) {
                return(TRUE)
            }
            return(val)
        })
        features[selected]
    })

    # Action: Select all features
    observeEvent(input$select_all, {
        features <- names(parks_data)[!names(parks_data) %in% c("parkCode", "fullName")]
        lapply(features, function(f) {
            valid_id <- make_valid_id(f)
            updateCheckboxInput(session, paste0("feat_", valid_id), value = TRUE)
        })
    })

    # Action: Deselect all features
    observeEvent(input$deselect_all, {
        features <- names(parks_data)[!names(parks_data) %in% c("parkCode", "fullName")]
        lapply(features, function(f) {
            valid_id <- make_valid_id(f)
            updateCheckboxInput(session, paste0("feat_", valid_id), value = FALSE)
        })
    })

    # ==================== TAB 1: PARK MAP AND INFORMATION ====================

    output$park_location_map <- renderPlotly({
        # Create park coordinates
        park_coords <- national_parks_df_clean %>%
            select(parkCode, fullName, states, latitude, longitude)

        # Join activities with national parks coordinates with topics matrix
        parks_with_topics <- inner_join(park_coords, parks_data, by = c("parkCode", "fullName"))

        # Get activity columns (everything except the first 5 columns which are park info)
        activity_cols <- names(parks_with_topics)[6:ncol(parks_with_topics)]

        # Calculate parks per state and most popular activities
        state_summary <- parks_with_topics %>%
            group_by(states) %>%
            summarize(
                num_parks = n(),
                park_names = paste(fullName, collapse = "<br>"),
                # Calculate sums for all activity columns
                across(all_of(activity_cols), ~ sum(.x, na.rm = TRUE)),
                .groups = "drop"
            )

        # Calculate top 3 activities for each state
        state_summary <- state_summary %>%
            mutate(
                top_activities = {
                    # For each row, get the activity sums and find top 3
                    activity_data <- select(., all_of(activity_cols))
                    apply(activity_data, 1, function(row_vals) {
                        # Create named vector of activities and their sums
                        named_vals <- setNames(as.numeric(row_vals), activity_cols)
                        # Sort and get top 3
                        top_3_names <- names(sort(named_vals, decreasing = TRUE)[1:3])
                        # Clean up names: replace dots and underscores with spaces
                        top_3_clean <- gsub("\\.", " ", top_3_names)
                        # Collapse into single string
                        paste(top_3_clean, collapse = ", ")
                    })
                }
            ) %>%
            # Keep only the columns we need
            select(states, num_parks, park_names, top_activities) %>%
            rename(location = states)

        # Create base state data for all US states
        all_states <- data.frame(
            location = state.abb,
            state_name = state.name,
            stringsAsFactors = FALSE
        ) %>%
            left_join(state_summary, by = "location") %>%
            mutate(
                num_parks = replace_na(num_parks, 0),
                park_names = replace_na(park_names, ""),
                top_activities = replace_na(top_activities, ""),
                hover_text = ifelse(
                    num_parks > 0,
                    paste0(
                        "<b>", state_name, "</b><br>",
                        "National Parks: ", num_parks, "<br>",
                        "Popular Topics: ", top_activities, "<br><br>",
                        park_names
                    ),
                    paste0("<b>", state_name, "</b><br>No National Parks")
                )
            )

        # Create the interactive map
        fig <- plot_ly() %>%
            # Add state layer with hover info
            add_trace(
                type = "choropleth",
                locationmode = "USA-states",
                locations = ~location,
                z = ~num_parks,
                text = ~hover_text,
                data = all_states,
                hovertemplate = "%{text}<extra></extra>",
                colorscale = list(
                    c(0, "grey80"),
                    c(0.01, "tan1"),
                    c(1, "tan4")
                ),
                showscale = TRUE,
                colorbar = list(title = "# of Parks"),
                name = "States"
            ) %>%
            # Add park markers using scattergeo
            add_trace(
                type = "scattergeo",
                locationmode = "USA-states",
                data = park_coords,
                lat = ~latitude,
                lon = ~longitude,
                text = ~fullName,
                hovertemplate = "%{text}<extra></extra>",
                mode = "markers",
                marker = list(
                    size = 10,
                    color = "black",
                    line = list(color = "white", width = 1.5),
                    opacity = 0.8
                ),
                name = "National Parks",
                showlegend = FALSE
            ) %>%
            # Map layout
            layout(
                geo = list(
                    scope = "usa",
                    projection = list(type = "albers usa"),
                    showland = TRUE,
                    landcolor = toRGB("white")
                )
            )

        fig
    })



    # ==================== TAB 2: CLUSTERING ANALYSIS ====================

    ## ============ Cluster Visualization Tab ============

    # Processed data for clustering with selected features
    processedData <- reactive({
        req(parks_reactive())
        req(length(selected_features()) > 0)

        df <- parks_reactive()
        sel_features <- selected_features()

        # Need at least 2 features for clustering
        if (length(sel_features) < 2) {
            showNotification("Please select at least 2 features for clustering",
                type = "error",
                duration = 5
            )
            return(NULL)
        }

        # Select only the chosen features
        data_subset <- df[, sel_features, drop = FALSE]
        complete_rows <- complete.cases(data_subset) # number of parks available for clustering
        data_clean <- data_subset[complete_rows, , drop = FALSE]

        # Calculate number of unique combinations
        n_unique <- nrow(unique(data_clean))
        max_k_feasible <- n_unique

        # Number of clusters cannot exceed number of unique observations
        if (input$k_clusters > max_k_feasible) {
            showNotification(
                paste("Maximum", max_k_feasible, "clusters possible with current features (only", n_unique, "unique combinations). Adjust the slider."),
                type = "warning",
                duration = 5
            )
        }

        list(
            data = data_clean,
            complete_rows = complete_rows,
            n_parks = sum(complete_rows),
            features_used = sel_features,
            max_k = max_k_feasible
        )
    })

    # K-means clustering
    kmeans_result <- reactive({
        req(processedData())
        processed <- processedData()

        if (is.null(processed)) {
            return(NULL)
        }

        data_cleaned <- processed$data
        k <- input$k_clusters

        set.seed(305)
        kmeans(data_cleaned, centers = k)
    })

    # Cluster visualization on US map
    output$cluster_plot <- renderPlotly({
        req(parks_reactive(), parks_coord_reactive(), kmeans_result(), processedData())

        processed <- processedData()
        cluster_result <- kmeans_result()

        if (is.null(processed) || is.null(cluster_result)) {
            p <- ggplot() +
                annotate("text",
                    x = 0.5, y = 0.5,
                    label = "Please select features for clustering",
                    size = 6
                ) +
                theme_void()
            return(ggplotly(p))
        }

        # Subset to parks used in clustering
        parks_df <- parks_reactive()
        coords_df <- parks_coord_reactive()
        parks_clustered <- parks_df[processed$complete_rows, ]

        # Merge cluster assignments with park coordinates
        cluster_data <- data.frame(
            parkCode = parks_clustered$parkCode,
            fullName = parks_clustered$fullName,
            cluster = as.factor(cluster_result$cluster)
        ) %>%
            left_join(coords_df, by = "parkCode") %>%
            filter(!is.na(latitude) & !is.na(longitude))

        # Transform coordinates for usmap
        transformed_coords <- usmap_transform(
            data = cluster_data,
            input_names = c("longitude", "latitude")
        ) %>%
            cbind(st_coordinates(.)) %>%
            # need to drop "geometry" otherwise ggplotly will throw an error somehow :(
            st_drop_geometry()

        # Create US map with cluster colors
        p <- plot_usmap(labels = FALSE) +
            geom_point(
                data = transformed_coords,
                aes(
                    x = X, y = Y, color = cluster,
                    text = paste("Park:", fullName, "<br>Cluster:", cluster)
                ),
                size = 4,
                alpha = 0.8
            ) +
            labs(title = "National Parks Clustering Map", color = "Cluster")

        ggplotly(p, tooltip = "text") %>%
            layout(autosize = TRUE)
    })

    # UI: Notes on clustering
    output$cluster_note <- renderUI({
        processed <- processedData()
        if (is.null(processed)) {
            return(HTML("<b>Note:</b> Please select (enough) interests to perform clustering."))
        }

        HTML(paste0(
            "<b>Note:</b> This map shows the geographic distribution of national parks colored by their cluster assignments
            based on <b>", length(processed$features_used), "</b> selected interests. ",
            "Parks with the same color share similar characteristics for the selected interests."
        ))
    })

    ## ============ Elbow Tab ============

    # Elbow plot
    output$elbow_plot <- renderPlotly({
        req(processedData())
        processed <- processedData()

        if (is.null(processed)) {
            p <- ggplot() +
                annotate("text",
                    x = 0.5, y = 0.5,
                    label = "Please select features for clustering",
                    size = 6
                ) +
                theme_void()
            return(ggplotly(p))
        }

        data_cleaned <- processed$data

        # Determine maximum feasible k based on unique data points
        n_unique <- nrow(unique(data_cleaned))
        max_k_possible <- min(10, n_unique)

        # Check if there are enough unique data points
        if (max_k_possible < 2) {
            p <- ggplot() +
                annotate("text",
                    x = 0.5, y = 0.5,
                    label = paste0(
                        "Not enough distinct data points for elbow analysis.\n",
                        "Only ", n_unique, " unique combination(s) with selected features.\n",
                        "Try selecting more or different features."
                    ),
                    size = 5,
                    color = "#c1121f"
                ) +
                theme_void()
            return(ggplotly(p))
        }

        # Calculate WCSS up to feasible max_k
        tryCatch(
            {
                wcss <- sapply(1:max_k_possible, function(k) {
                    set.seed(305)
                    kmeans(data_cleaned, centers = k)$tot.withinss
                })
                elbow_data <- data.frame(k = 1:max_k_possible, wcss = wcss)

                p <- ggplot(elbow_data, aes(x = k, y = wcss)) +
                    geom_point(size = 3, color = "#2E86AB") +
                    geom_line(color = "#2E86AB", linewidth = 1) +
                    theme_minimal() +
                    labs(
                        title = "Elbow Plot",
                        x = "Number of Clusters (k)",
                        y = "Within-Cluster Sum of Squares (WCSS)"
                    ) +
                    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
                    scale_x_continuous(breaks = 1:max_k_possible)

                ggplotly(p)
            },
            error = function(e) {
                p <- ggplot() +
                    annotate("text",
                        x = 0.5, y = 0.5,
                        label = paste0(
                            "Unable to generate elbow plot.\n",
                            "Error: ", e$message, "\n\n",
                            "Try selecting different features."
                        ),
                        size = 5,
                        color = "#c1121f"
                    ) +
                    theme_void()
                ggplotly(p)
            }
        )
    })

    ## ============ Cluster Results Tab ============

    # UI: Cluster assignments table
    output$cluster_table <- renderDT({
        req(kmeans_result(), parks_reactive(), processedData())

        processed <- processedData()
        cluster_result <- kmeans_result()

        if (is.null(processed) || is.null(cluster_result)) {
            return(datatable(data.frame(Message = "Please select (enough) features for clustering")))
        }

        parks_df <- parks_reactive()
        parks_clustered <- parks_df[processed$complete_rows, ]

        cluster_assignments <- data.frame(
            Cluster = as.factor(cluster_result$cluster),
            Park = parks_clustered$fullName,
            Code = parks_clustered$parkCode
        ) %>%
            arrange(Cluster, Park) %>%
            group_by(Cluster) %>%
            summarise(
                Count = n(),
                Parks = paste(Park, collapse = ", ")
            )

        datatable(cluster_assignments, rownames = FALSE, options = list(pageLength = 10, dom = "t"))
    })

    # UI: Cluster characteristics summary
    output$cluster_summary <- renderUI({
        req(kmeans_result(), parks_reactive(), processedData())

        processed <- processedData()
        cluster_result <- kmeans_result()

        if (is.null(processed) || is.null(cluster_result)) {
            return(HTML("<p>Please select (enough) features for clustering.</p>"))
        }

        parks_df <- parks_reactive()
        parks_clustered <- parks_df[processed$complete_rows, ]
        cluster_sizes <- table(cluster_result$cluster)

        features_text <- paste(head(processed$features_used, 10), collapse = ", ")
        if (length(processed$features_used) > 10) {
            features_text <- paste0(features_text, ", ...")
        }

        HTML(sprintf(
            "<p>
                The k-means algorithm identified <b>%d</b> clusters using <b>%d</b> features. These features are: <em>%s</em>.
            </p>
            <p>
                Parks in the same cluster likely exhibit similar profiles across these selected features.
                For example, when landscape-related features are included, parks in a given cluster may share comparable
                terrain or scenic characteristics. When interests-based features such as photography are used, the clustering
                may reflect that past visitors likely visit the park to pursue similar activities. When features
                such as unique species are used, the clustering may group parks that serve as habitats for similar or distinctive wildlifes.
            </p>
            <p>
                We acknowledge that the current list of features is not exhaustive, and including more features can make the dashboard
                more helpful to the public. To help with interpretation, checkout the cluster-profile heatmap below
                and the <b>Data Explorer</b> tab to get more customized information on each park.
            </p>",
            input$k_clusters,
            length(processed$features_used),
            paste(processed$features_used, collapse = ", ")
        ))
    })

    # Calculate cluster profiles
    cluster_profiles <- reactive({
        req(kmeans_result(), parks_reactive(), processedData())

        processed <- processedData()
        cluster_result <- kmeans_result()

        if (is.null(processed) || is.null(cluster_result)) {
            return(NULL)
        }

        # Get the data used for clustering
        data_clustered <- processed$data

        # Add cluster assignments
        data_with_clusters <- data_clustered %>%
            mutate(Cluster = as.factor(cluster_result$cluster))

        # Calculate mean values for each feature by cluster
        cluster_means <- data_with_clusters %>%
            group_by(Cluster) %>%
            summarise(across(everything(), mean, .names = "{.col}")) %>%
            ungroup()

        # Convert to long format for plotting
        cluster_long <- cluster_means %>%
            pivot_longer(cols = -Cluster, names_to = "Feature", values_to = "Value")

        list(
            means = cluster_means,
            long = cluster_long
        )
    })

    # Output: Cluster profile heatmap
    output$cluster_profile_heatmap <- renderPlotly({
        req(cluster_profiles())

        profile_data <- cluster_profiles()

        if (is.null(profile_data)) {
            p <- ggplot() +
                annotate("text",
                    x = 0.5, y = 0.5,
                    label = "Select features for clustering",
                    size = 6
                ) +
                theme_void()
            return(ggplotly(p))
        }

        # Normalize values to 0-1 scale for better comparison
        cluster_long_normalized <- profile_data$long %>%
            group_by(Feature) %>%
            mutate(
                Normalized = (Value - min(Value)) / (max(Value) - min(Value) + 0.001),
                HoverText = sprintf(
                    "Cluster: %s<br>Feature: %s<br>Avg Value: %.3f<br>Normalized: %.2f",
                    Cluster, Feature, Value, Normalized
                )
            ) %>%
            ungroup()

        p <- ggplot(
            cluster_long_normalized,
            aes(x = Feature, y = Cluster, fill = Normalized, text = HoverText)
        ) +
            geom_tile(color = "white", linewidth = 1.5) +
            scale_fill_gradient2(
                low = "#f4f1de",
                mid = "#e07a5f",
                high = "#3d405b",
                midpoint = 0.5,
                name = "Relative\nIntensity"
            ) +
            labs(
                title = "Cluster Feature Profiles",
                x = "Selected Interests",
                y = "Cluster"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
                axis.text.y = element_text(size = 12, face = "bold"),
                panel.grid = element_blank()
            )

        ggplotly(p, tooltip = "text") %>%
            layout(
                hoverlabel = list(bgcolor = "white"),
                margin = list(b = 120, l = 60)
            )
    })

    # ==================== TAB 3: ML RECOMMENDATION SYSTEM ====================
    # Load pre-computed park feature vectors
    park_data <- load_park_vectors()

    # Available topics for recommendation
    available_topics_rec <- reactive({
        topic_names <- gsub("\\.", " ", park_data$topic_cols)
        topic_names <- gsub("_", " ", topic_names)
        setNames(park_data$topic_cols, topic_names)
    })

    # UI: Dynamic topic checkboxes
    output$recommendation_topic_checkboxes <- renderUI({
        topics <- available_topics_rec()

        checkbox_list <- lapply(names(topics), function(display_name) {
            checkboxInput(
                inputId = paste0("rec_topic_", topics[display_name]),
                label = display_name,
                value = FALSE
            )
        })

        do.call(tagList, checkbox_list)
    })

    # Selected topics
    selected_topics_rec <- reactive({
        topics <- available_topics_rec()

        selected <- sapply(topics, function(topic_col) {
            val <- input[[paste0("rec_topic_", topic_col)]]
            if (is.null(val)) {
                return(FALSE)
            }
            return(val)
        })

        topics[selected]
    })

    # UI: Display count
    output$selected_topics_count_rec <- renderUI({
        selected <- length(selected_topics_rec())
        color <- if (selected == 0) "red" else if (selected < 3) "orange" else "green"

        HTML(paste0(
            "<b>Selected Topics: </b>",
            "<span style='color: ", color, "; font-weight: bold;'>", selected, "</span>",
            if (selected == 0) "<br><em style='color: red;'>Please select at least one topic</em>" else ""
        ))
    })

    # Actions
    observeEvent(input$select_all_rec, {
        topics <- available_topics_rec()
        lapply(topics, function(topic_col) {
            updateCheckboxInput(session, paste0("rec_topic_", topic_col), value = TRUE)
        })
    })

    observeEvent(input$deselect_all_rec, {
        topics <- available_topics_rec()
        lapply(topics, function(topic_col) {
            updateCheckboxInput(session, paste0("rec_topic_", topic_col), value = FALSE)
        })
    })

    # Output: Alpha explanation
    output$alpha_explanation <- renderUI({
        alpha <- input$alpha_weight

        explanation <- if (alpha == 0) {
            list(text = "Pure suitability - best match to your interests", color = "lightgreen")
        } else if (alpha == 0.25) {
            list(text = "Mostly suitability, some popularity", color = "palegreen")
        } else if (alpha == 0.5) {
            list(text = "Balanced - equal weight", color = "#FFC107")
        } else if (alpha == 0.75) {
            list(text = "Mostly popularity, some suitability", color = "#FF9800")
        } else {
            list(text = "Pure popularity - most visited parks", color = "#FF5715")
        }

        HTML(sprintf(
            "<div style='padding: 8px; background-color: %s; color: white; border-radius: 4px; text-align: center; font-size: 0.85em;'>
              <strong>%s</strong>
            </div>",
            explanation$color, explanation$text
        ))
    })

    # Generate recommendations
    park_recommendations <- eventReactive(input$get_recommendations, {
        selected_topics <- selected_topics_rec()

        if (length(selected_topics) == 0) {
            showNotification("Please select at least one topic", type = "error", duration = 5)
            return(NULL)
        }

        preferred_month_full <- input$pred_month
        if (preferred_month_full == "Any Month") {
            showNotification("Please select a specific month (not 'Any Month')",
                type = "error", duration = 5
            )
            return(NULL)
        }

        preferred_month <- month_mapping$abbr[month_mapping$full == preferred_month_full]

        showNotification("Calculating recommendations...", type = "message", duration = 2, id = "rec_loading")

        tryCatch(
            {
                recommendations <- recommend_parks(
                    park_data = park_data,
                    selected_topics = selected_topics,
                    preferred_month = preferred_month,
                    alpha = input$alpha_weight,
                    top_n = input$n_recommendations
                )

                removeNotification(id = "rec_loading")
                showNotification("Recommendations ready!", type = "message", duration = 2)

                return(recommendations)
            },
            error = function(e) {
                removeNotification(id = "rec_loading")
                showNotification(paste("Error:", e$message), type = "error", duration = 10)
                return(NULL)
            }
        )
    })

    # Output: Table
    output$recommendation_table <- renderDT({
        recs <- park_recommendations()

        if (is.null(recs)) {
            return(datatable(
                data.frame(Message = "Select topics, month, and click 'Get Recommendations'"),
                rownames = FALSE, options = list(dom = "t")
            ))
        }

        display_table <- recs %>%
            select(Rank, Park, FinalScorePct, SuitabilityPct, PopularityPct) %>%
            rename(
                `Final Score (%)` = FinalScorePct,
                `Suitability (%)` = SuitabilityPct,
                `Popularity (%)` = PopularityPct
            )

        datatable(
            display_table,
            rownames = FALSE,
            options = list(
                pageLength = 15, dom = "t",
                columnDefs = list(list(className = "dt-center", targets = c(0, 2, 3, 4)))
            ),
            class = "cell-border stripe hover"
        ) %>%
            formatStyle("Final Score (%)",
                background = styleColorBar(c(0, 100), "#87CEEB"),
                backgroundSize = "100% 90%", backgroundRepeat = "no-repeat",
                backgroundPosition = "center"
            )
    })

    # Output: Plot
    output$recommendation_plot <- renderPlotly({
        recs <- park_recommendations()

        if (is.null(recs)) {
            p <- ggplot() +
                annotate("text",
                    x = 0.5, y = 0.5,
                    label = "Select topics, month, and click\n'Get Recommendations'",
                    size = 6, color = "gray50"
                ) +
                theme_void()
            return(ggplotly(p))
        }

        plot_data <- recs %>%
            mutate(Park = factor(Park, levels = rev(Park)))

        p <- ggplot(plot_data, aes(x = FinalScorePct, y = Park, fill = FinalScorePct)) +
            geom_col() +
            scale_fill_gradient(low = "#FFF5BA", high = "#FF6B6B", guide = "none") +
            labs(
                title = "Top Recommended Parks",
                x = "Recommendation Score (%)", y = NULL
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                axis.text.y = element_text(size = 10),
                panel.grid.major.y = element_blank()
            ) +
            scale_x_continuous(expand = c(0, 0), limits = c(0, 100))

        ggplotly(p, tooltip = c("x", "y")) %>%
            layout(hoverlabel = list(bgcolor = "white"), margin = list(l = 180))
    })

    # Output: Summary
    output$recommendation_summary <- renderUI({
        recs <- park_recommendations()

        if (is.null(recs)) {
            return(HTML(
                "<div style='padding: 15px; background-color: #f0f8ff; border-radius: 5px; border-left: 4px solid #4169E1;'>
                  <h4 style='margin-top: 0; color: #4169E1;'>Park Recommendations</h4>
                  <p>This system recommends parks based on:</p>
                  <ul>
                    <li><strong>Suitability:</strong> How well the park matches your interests</li>
                    <li><strong>Popularity:</strong> How many visitors the park attracts</li>
                  </ul>
                  <p>You control the balance with the <strong>α slider</strong>!</p>
                </div>"
            ))
        }

        n_topics <- length(attr(recs, "topics_selected"))
        month <- attr(recs, "month")
        alpha <- attr(recs, "alpha")

        month_full <- month_mapping$full[month_mapping$abbr == month]

        top_park <- recs$Park[1]
        top_score <- recs$FinalScorePct[1]
        top_suit <- recs$SuitabilityPct[1]
        top_pop <- recs$PopularityPct[1]

        weight_text <- if (alpha == 0) {
            "only suitability (best match)"
        } else if (alpha == 0.25) {
            "mostly suitability"
        } else if (alpha == 0.5) {
            "balanced"
        } else if (alpha == 0.75) {
            "mostly popularity"
        } else {
            "only popularity (most visited)"
        }

        HTML(sprintf(
            "<div style='padding: 15px; background-color: #e8f5e9; border-radius: 5px; border-left: 4px solid #4CAF50;'>
              <h4 style='margin-top: 0; color: #2E7D32;'>Your Top Recommendation</h4>
              <p>Based on <strong>%d interests</strong> in <strong>%s</strong> with <strong>%s</strong> weighting:</p>
              <h3 style='color: #2E7D32; margin: 10px 0;'>%s</h3>
              <p><strong>Overall Score:</strong> %.1f%%</p>
              <p><strong>Breakdown:</strong></p>
              <ul>
                <li>Suitability (match to your interests): %.1f%%</li>
                <li>Popularity (2024 visitors): %.1f%%</li>
              </ul>
            </div>",
            n_topics, month_full, weight_text, top_park, top_score, top_suit, top_pop
        ))
    })

    # Output: Selected interests
    output$selected_interests_display <- renderUI({
        selected <- selected_topics_rec()

        if (length(selected) == 0) {
            return(HTML("<p style='color: gray;'><em>No topics selected</em></p>"))
        }

        topic_names <- gsub("\\.", " ", names(selected))
        topic_names <- gsub("_", " ", topic_names)

        tags_html <- paste(sapply(topic_names, function(name) {
            sprintf("<span style='background-color: #5C6BC0; color: white;
                    padding: 6px 14px; margin: 4px; border-radius: 20px;
                    display: inline-block; font-size: 13px;'>%s</span>", name)
        }), collapse = " ")

        HTML(paste0(
            "<div style='padding: 12px; background-color: #f5f5f5; border-radius: 5px;'>",
            "<strong>Your Interests:</strong><br><br>", tags_html, "</div>"
        ))
    })

    # Output: Score breakdown visualization
    output$score_breakdown <- renderPlotly({
        recs <- park_recommendations()
        if (is.null(recs)) {
            return(NULL)
        }

        # Prepare data for stacked bar
        plot_data <- recs %>%
            head(10) %>%
            mutate(
                Park = factor(Park, levels = rev(Park)),
                Suitability_contrib = (1 - attr(recs, "alpha")) * Suitability * 100,
                Popularity_contrib = attr(recs, "alpha") * Popularity * 100
            )

        p <- ggplot(plot_data) +
            geom_col(aes(x = Park, y = Suitability_contrib, fill = "Suitability"),
                position = "stack"
            ) +
            geom_col(aes(x = Park, y = Popularity_contrib, fill = "Popularity"),
                position = "stack"
            ) +
            coord_flip() +
            scale_fill_manual(values = c("Suitability" = "#4CAF50", "Popularity" = "#FF9800")) +
            labs(title = "Score Composition", x = NULL, y = "Score Contribution (%)", fill = "Component") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"))

        ggplotly(p) %>% layout(margin = list(l = 150))
    })

    # Output: Recommendation map
    output$recommendation_map <- renderPlotly({
        recs <- park_recommendations()

        if (is.null(recs)) {
            p <- ggplot() +
                annotate("text",
                    x = 0.5, y = 0.5,
                    label = "Select topics, month, and click\n'Get Recommendations'\nto see park locations",
                    size = 6, color = "gray50"
                ) +
                theme_void()
            return(ggplotly(p))
        }

        # Get coordinates for recommended parks
        coords_df <- parks_coord_reactive()

        # Merge recommendations with coordinates
        map_data <- recs %>%
            left_join(coords_df, by = "parkCode") %>%
            filter(!is.na(latitude) & !is.na(longitude))

        # Transform coordinates for usmap
        transformed_coords <- usmap_transform(
            data = map_data,
            input_names = c("longitude", "latitude")
        ) %>%
            cbind(st_coordinates(.)) %>%
            st_drop_geometry()

        # Create US map with recommended parks
        p <- plot_usmap(labels = FALSE) +
            geom_point(
                data = transformed_coords,
                aes(
                    x = X, y = Y,
                    color = FinalScorePct,
                    size = FinalScorePct,
                    text = paste0(
                        "Rank: ", Rank, "<br>",
                        "Park: ", Park, "<br>",
                        "Final Score: ", FinalScorePct, "%<br>",
                        "Suitability: ", SuitabilityPct, "%<br>",
                        "Popularity: ", PopularityPct, "%"
                    )
                ),
                alpha = 0.8
            ) +
            scale_color_gradient(
                low = "#FFE5B4", high = "#FF4500",
                name = "Score (%)"
            ) +
            scale_size_continuous(range = c(3, 8), guide = "none") +
            labs(title = "Recommended Parks on US Map")

        ggplotly(p, tooltip = "text") %>%
            layout(autosize = TRUE)
    })


    # ==================== TAB 4: DATA EXPLORER ====================
    output$data_table <- renderDT({
        req(parks_reactive())

        datatable(parks_reactive(),
            options = list(
                pageLength = 15,
                scrollX = TRUE,
                autoWidth = TRUE,
                searching = TRUE,
                lengthChange = TRUE,
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css(
                         {'background-color': '#27ae60', 'color': '#fff'});",
                    "}"
                )
            ),
            class = "cell-border stripe hover",
            rownames = FALSE,
            filter = "top"
        )
    })
}
