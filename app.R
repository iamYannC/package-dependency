library(shiny)
library(bslib)
library(shinyWidgets)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(DT)

# --- Logic Layer remains the same ---
get_dep_edges <- function(pkg, deps, flip = FALSE) {
  if (flip) { tmp <- pkg; pkg <- deps; deps <- tmp }
  if (is.null(pkg) || is.null(deps) || pkg == "" || deps == "" || pkg == deps) return(NULL)
  info <- pak::pkg_deps_explain(pkg, deps) |> suppressMessages()
  paths <- info$paths[[deps]]
  if (is.null(paths)) return(NULL)
  paths %>%
    map_df(~ tibble(from = head(.x, -1), to = tail(.x, -1))) %>%
    count(from, to, name = "strength")
}

ui <- bslib::page_navbar(
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
# --- Server Layer remains the same ---
server <- function(input, output, session) {
  all_pkgs <- row.names(installed.packages())
  updateSelectizeInput(session, "pkg_source", choices = all_pkgs, selected = "ggplot2", server = TRUE)
  updateSelectizeInput(session, "pkg_target", choices = all_pkgs, selected = "rlang", server = TRUE)
  
  debounced_pkg_source <- reactive({ input$pkg_source }) |> debounce(1000)
  debounced_pkg_target <- reactive({ input$pkg_target }) |> debounce(1000)
  debounced_flip       <- reactive({ input$flip })       |> debounce(500)
  
  edge_data <- reactive({
    req(debounced_pkg_source(), debounced_pkg_target())
    get_dep_edges(debounced_pkg_source(), debounced_pkg_target(), debounced_flip())
  })
  
  output$dep_plot <- renderPlot({
    df <- edge_data()
    if (is.null(df)) {
      return(ggplot() + annotate("text", x=1, y=1, label="No connection found.") + theme_void())
    }
    graph <- as_tbl_graph(df) %>% mutate(importance = centrality_degree(mode = "in"))
    ggraph(graph, layout = input$layout) +
      geom_edge_diagonal(aes(edge_width = strength), arrow = arrow(length = unit(3, "mm")), 
                         end_cap = circle(5, "mm"), alpha = 0.4, color = input$edge_col) +
      geom_node_point(aes(size = importance), color = input$node_col) +
      geom_node_text(aes(label = name), vjust = -1, repel = TRUE, fontface = "bold") +
      scale_edge_width(range = c(0.8, 4)) + theme_graph() + theme(legend.position = "none")
  })
  
  output$dep_table <- renderDT({
    req(edge_data())
    datatable(edge_data(), options = list(pageLength = 10, dom = 'ftp'), rownames = FALSE, style = "bootstrap5")
  })
}

shinyApp(ui, server)
