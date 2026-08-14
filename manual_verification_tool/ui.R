#UI Script
ui <- page_navbar(
  title = "Data Processing Pipeline",
  id = "tabs",
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light") #Toggle light vs dark mode
  ),
  theme = bs_theme(preset = "bootstrap"),



  #### Tab 1: Data Selection ####
  nav_panel(
    title = "Data Selection",
    card(
      card_header("Select Your Data"),
      card_body(
        uiOutput("conditional_data_ui")
      )
    )
  ),


  #### Tab 2: Data Verification ####
  nav_panel(
    title = "Data Verification",
    layout_columns(
      col_widths = c(8,4),

      #### Left column ####
      layout_columns(
        col_widths = c(12, 6, 6),
        # Main plot (top left)
         card(
          style = "height: 80vh;",
        #   card_header(
        #     div(
        #       actionButton("prev_tab", "← Back to Selection", class = "btn-info"),
        #       keys::useKeys(),
        #       keys::keysInput("q_key", "q"),
        #       actionButton("quit_app", "Quit", class = "btn-danger")
        #     )
        #   ),
          card_body(
            plotOutput("main_plot",
                       brush = brushOpts(
                         id = "plot_brush",
                         resetOnNew = FALSE  # This allows multiple brush selections
                       ))
          ),
          card_footer(
            layout_columns(
              col_widths = c(3, 6, 3),

              # Left: Additional sites selection
              div(
                selectizeInput("add_sites", "Additional Sites:",
                               choices = available_sites,
                               multiple = TRUE,
                               options = list(plugins = "remove_button"),
                               width = "100%")
              ),

              # Middle: Checkbox group
              div(
                class = "d-flex justify-content-center align-items-center h-100",
                checkboxGroupInput("plot_options", label = NULL,
                                   choices = c("Remove Omit" = "remove_omit",
                                               "Remove Flag" = "remove_flag",
                                               "Plot Line" = "add_line",
                                               "Thresholds" = "incl_thresholds",
                                               "Log 10" = "plot_log10",
                                               "Extra Data" = "incl_ex_days",
                                               "Show Legend" = "show_legend"),
                                   selected = c("incl_ex_days", "show_legend"),
                                   inline = TRUE)
              ),

              # Right: Buttons (Reset/Quit on top, Prev/Next below)
              div(
                class = "d-flex flex-column align-items-end justify-content-center gap-2",
                div(
                  class = "d-flex gap-2",
                  actionButton("reset_week", "Reset", class = "btn-danger"),
                  keys::useKeys(),
                  keys::keysInput("q_key", "q"),
                  actionButton("quit_app", "Quit", class = "btn-danger")
                ),
                div(
                  class = "d-flex gap-2",
                  actionButton("prev_week", "← Prev", class = "btn-secondary"),
                  actionButton("next_week", "Next →", class = "btn-secondary")
                )
              )
            )
          )
        ),

        #### Weekly decision card (bottom left, shorter height) ####
        card(
          style = "height: 25vh; overflow-y: auto;",
          # card_header(
          #   h6("Make weekly decision")
          # ),
          card_body(
            div(
              div(
                class = "d-flex gap-5", # Add flexbox with gap between elements
                div(
                  uiOutput("weekly_decision_radio")
                )
              ),
              uiOutput("submit_decision_ui")
            )
          )
        ),

        # Data selection card (moved from bottom right to middle left)
        card(
          style = "height: 25vh; overflow-y: auto;",
          card_header(
            h6("Data Brush")
          ),
          card_body(
            layout_columns(
              col_widths = c(8, 4),
              # Left side: Select action then Select flag
              div(
                class = "d-flex flex-column gap-2",
                radioButtons("brush_action",
                             "Select Action:",
                             choices = c("Accept" = "A",
                                         "Flag" = "F",
                                         "Omit" = "O"),
                             selected = character(0),
                             inline = TRUE),
                selectizeInput("user_brush_flags", label = NULL,
                               choices = available_flags,
                               multiple = TRUE,
                               options = list(plugins = "remove_button"))
              ),
              # Right side: Clear button then Submit button
              div(
                class = "d-flex flex-column align-items-end gap-2",
                actionButton("clear_brushes", "Clear"),
                uiOutput("brush_submit_ui")
              )
            )
          )
        )

        #### end Weekly decision card ####
      ),

      #### Right column ####
      layout_columns(
        col_widths = 12,
        # Sub plots card (top right)
        navset_card_tab(
          id = "additional_tabs",
          title = h6("Additional Parameters", class = "m-0"),
          nav_panel("Plots",
            # Sub parameter selection
            layout_columns(
              col_widths = c(6, 6),
              selectizeInput("sub_parameters", "Select Parameters:",
                             choices = available_parameters,
                             multiple = TRUE,
                             options = list(plugins = "remove_button")),
              selectizeInput("sub_sites", "Select Sites:",
                             choices = available_sites,
                             multiple = TRUE,
                             options = list(plugins = "remove_button"))
            ),
            div(
              class = "flex-fill d-flex flex-column",
              style = "overflow-y: auto; min-height: 600px;",  # Make this div scrollable and fill remaining height
              plotlyOutput("sub_plots", width = "100%", height = "100%")
            )
          ),
          nav_panel("Field Notes",
            div(
              class = "flex-fill d-flex flex-column",
              style = "overflow-y: auto; min-height: 600px;",
              DT::dataTableOutput("field_notes_table")
            )
          )
        )
      )
    )
  ),

  #### Tab 3: Final Data View ####

  nav_panel(
    title = "Finalize Data",
    layout_columns(
      col_widths = c(10, 2),

      # Main Plot Card
      #To Do: Convert to plotly object for better data vis
      card(
        card_body(
          plotlyOutput("final_plot", height = "100%", width = "100%")
        )
      ),

      # Week Selection and Actions Card
      card(
        card_header("Modify Verification"),
        card_body(
          materialSwitch(
            inputId = "remove_omit_finalplot",
            label = "Remove omitted data from plot",
            value = FALSE,
            width = "200px",
            status = "success"
          ),
          materialSwitch(
            inputId = "log10_finalplot",
            label = "Log Transform",
            value = FALSE,
            width = "200px",
            status = "success"
          ),
          selectInput("final_week_selection", "Select Week:", choices = NULL),
          actionButton("goto_final_week", "Return to Selected Week",
                       class = "btn-primary w-100 mb-3"),
          hr(),
          uiOutput("submit_final_button") # Replaced the direct button with a dynamic UI output
        )
      )
    )
  )

)

