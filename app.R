ui <- page_navbar(
  title = "The Mighty Dependency Graph!",
  
  # This adds the icon to the right side of the navbar
  nav_spacer(),
  nav_item(
    tooltip(
      tags$a(
        href = "https://github.com/iamYannC/package-dependency/blob/main/app.R",
        target = "_blank",
        icon("github"),
        class = "btn btn-outline-light btn-sm"
      ),
      "Download original code and run locally to viz your installed packages!",
      placement = "bottom"
    )
  ),
  
  # Sidebar and Main Content move into a nav_panel
  sidebar = sidebar(
    title = "Live Configuration",
    selectizeInput("pkg_source", "Source Package:", 
                   choices = NULL, options = list(placeholder = 'Type to search...')),
    selectizeInput("pkg_target", "Target Dependency:", 
                   choices = NULL, options = list(placeholder = 'e.g., rlang')),
    checkboxInput("flip", "Flip Relationship Direction", FALSE),
    hr(),
    selectInput("layout", "Graph Layout:", 
                choices = c("sugiyama", "fr", "kk", "lgl", "mds", "circle"),
                selected = "sugiyama"),
    colorPickr("edge_col", "Line Color:", "#3498DB"),
    colorPickr("node_col", "Node Color:", "#2C3E50")
  ),
  
  nav_panel(
    title = NULL, # Hide the tab name since we only have one
    layout_columns(
      card(
        full_screen = TRUE,
        card_header("Dependency Visualization"),
        shinycssloaders::withSpinner(plotOutput("dep_plot", height = "550px"))
      ),
      card(
        card_header("Searchable Edge List"),
        DTOutput("dep_table")
      ),
      col_widths = c(8, 4)
    )
  ),
  
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2C3E50")
)
