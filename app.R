# ==============================================================
#  Netflix Information Visualization Dashboard
#  Stack : R | Shiny | bs4Dash | Plotly | ggplot2 | DT |
#          dplyr | tidyr | shinyWidgets
#  Data  : netflix_titles.csv (Kaggle - "Netflix Movies and TV Shows")
#          Place the CSV in the same folder as app.R.
#          If the file is missing, a small demo dataset is generated
#          so the app always runs.
# ==============================================================

library(shiny)
library(bs4Dash)
library(plotly)
library(ggplot2)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)
library(shinyWidgets)

# --------------------------------------------------------------
# MODULE 0 : DATA  (load + clean)
# --------------------------------------------------------------

generate_demo_data <- function(n = 600) {
  set.seed(42)
  countries <- c("United States", "India", "United Kingdom", "Japan",
                 "South Korea", "France", "Canada", "Spain", "Mexico", "Germany")
  genres <- c("Dramas", "Comedies", "Documentaries", "Action & Adventure",
              "International TV Shows", "Thrillers", "Romantic Movies",
              "Kids' TV", "Horror Movies", "Crime TV Shows")
  ratings <- c("TV-MA", "TV-14", "TV-PG", "R", "PG-13", "PG", "TV-Y", "TV-G")
  directors <- paste("Director", LETTERS[1:20])
  actors <- paste("Actor", 1:60)
  type <- sample(c("Movie", "TV Show"), n, replace = TRUE, prob = c(0.7, 0.3))
  data.frame(
    show_id = paste0("s", 1:n),
    type = type,
    title = paste("Demo Title", 1:n),
    director = ifelse(runif(n) < 0.25, NA,
                      sapply(1:n, function(i) paste(sample(directors, sample(1:2, 1)), collapse = ", "))),
    cast = sapply(1:n, function(i) paste(sample(actors, sample(3:6, 1)), collapse = ", ")),
    country = sapply(1:n, function(i) paste(sample(countries, sample(1:2, 1)), collapse = ", ")),
    date_added = format(sample(seq(as.Date("2015-01-01"), as.Date("2021-09-01"), by = "day"),
                               n, replace = TRUE), "%B %d, %Y"),
    release_year = sample(1980:2021, n, replace = TRUE,
                          prob = seq(0.2, 3, length.out = length(1980:2021))),
    rating = sample(ratings, n, replace = TRUE),
    duration = ifelse(type == "Movie",
                      paste(sample(60:180, n, replace = TRUE), "min"),
                      paste(sample(1:8, n, replace = TRUE), "Seasons")),
    listed_in = sapply(1:n, function(i) paste(sample(genres, sample(1:3, 1)), collapse = ", ")),
    description = "Demo description.",
    stringsAsFactors = FALSE
  )
}

load_netflix <- function() {
  if (file.exists("netflix_titles.csv")) {
    message("Loading netflix_titles.csv ...")
    read.csv("netflix_titles.csv", stringsAsFactors = FALSE,
             na.strings = c("", "NA"))
  } else {
    message("netflix_titles.csv not found - using demo data.")
    generate_demo_data()
  }
}

raw <- load_netflix()

netflix <- raw %>%
  mutate(across(where(is.character), trimws)) %>%
  mutate(
    # A few rows in the Kaggle file have the duration stored in `rating`
    duration = ifelse(str_detect(coalesce(rating, ""), "min$"), rating, duration),
    rating   = ifelse(str_detect(coalesce(rating, ""), "min$") | is.na(rating),
                      "Unrated", rating),
    date_added_p = as.Date(date_added, format = "%B %d, %Y"),
    year_added   = as.integer(format(date_added_p, "%Y")),
    month_added  = factor(format(date_added_p, "%b"), levels = month.abb),
    duration_num = suppressWarnings(as.integer(str_extract(duration, "\\d+"))),
    primary_country = ifelse(is.na(country), NA, sub(",.*$", "", country))
  )

# ---- helpers --------------------------------------------------

# Count values of a comma-separated column (genre, country, cast, director)
count_split <- function(df, col, n = 10) {
  df %>%
    filter(!is.na(.data[[col]]), .data[[col]] != "") %>%
    separate_rows(all_of(col), sep = ",\\s*") %>%
    filter(.data[[col]] != "") %>%
    count(.data[[col]], sort = TRUE, name = "count") %>%
    rename(value = 1) %>%
    head(n)
}

# TRUE/FALSE vector: does a comma-separated cell contain any selected value?
match_multi <- function(column, selected) {
  if (is.null(selected) || length(selected) == 0) return(rep(TRUE, length(column)))
  vapply(strsplit(ifelse(is.na(column), "", column), ",\\s*"),
         function(x) any(x %in% selected), logical(1))
}

# Choice lists for the sidebar filters
country_choices  <- count_split(netflix, "country",   500)$value
genre_choices    <- sort(count_split(netflix, "listed_in", 500)$value)
rating_choices   <- netflix %>% count(rating, sort = TRUE) %>% pull(rating)
director_choices <- sort(count_split(netflix, "director", 10000)$value)
actor_choices    <- sort(count_split(netflix, "cast", 50000)$value)
year_min <- min(netflix$release_year, na.rm = TRUE)
year_max <- max(netflix$release_year, na.rm = TRUE)

# Consistent colour palette
pal_type <- c("Movie" = "#E50914", "TV Show" = "#1F6FEB")
nf_red   <- "#E50914"
nf_blue  <- "#1F6FEB"
nf_dark  <- "#221F1F"

# Shared plotly layout
nf_layout <- function(p, ...) {
  layout(p,
         font = list(family = "Inter, sans-serif", color = nf_dark),
         paper_bgcolor = "rgba(0,0,0,0)",
         plot_bgcolor  = "rgba(0,0,0,0)",
         margin = list(t = 30, r = 10),
         ...)
}

# --------------------------------------------------------------
# MODULE 1 + 2 : UI  (layout + sidebar filters)
# --------------------------------------------------------------

sidebar_filters <- div(
  class = "sidebar-filters",
  hr(class = "sidebar-divider"),
  div(class = "filter-title", icon("filter"), " Filters"),
  br(),
  prettyRadioButtons("f_type", "Content Type",
                     choices = c("All", "Movie", "TV Show"),
                     selected = "All", status = "danger", inline = FALSE),
  pickerInput("f_country", "Country", choices = country_choices, multiple = TRUE,
              options = pickerOptions(liveSearch = TRUE, actionsBox = TRUE,
                                      noneSelectedText = "All countries",
                                      selectedTextFormat = "count > 2")),
  pickerInput("f_genre", "Genre", choices = genre_choices, multiple = TRUE,
              options = pickerOptions(liveSearch = TRUE, actionsBox = TRUE,
                                      noneSelectedText = "All genres",
                                      selectedTextFormat = "count > 2")),
  pickerInput("f_rating", "Rating", choices = rating_choices, multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE,
                                      noneSelectedText = "All ratings",
                                      selectedTextFormat = "count > 2")),
  sliderInput("f_year", "Release Year", min = year_min, max = year_max,
              value = c(year_min, year_max), sep = "", step = 1),
  selectizeInput("f_director", "Director", choices = NULL, multiple = TRUE,
                 options = list(placeholder = "All directors")),
  selectizeInput("f_actor", "Actor", choices = NULL, multiple = TRUE,
                 options = list(placeholder = "All actors")),
  actionButton("f_reset", label = tagList(icon("rotate-left"), " Reset Filters"),
               class = "btn-reset")
)

ui <- bs4DashPage(
  title = "Netflix Dashboard",
  dark = NULL, help = NULL, fullscreen = TRUE,

  header = bs4DashNavbar(
    title = bs4DashBrand(title = "NETFLIX", color = NULL,
                         image = NULL, opacity = 1),
    status = "white",
    span(style = "font-weight:700; font-size:1.05rem; color:#221F1F;",
         "Information Visualization Dashboard")
  ),

  sidebar = bs4DashSidebar(
    skin = "dark", status = "danger", elevation = 3,
    collapsed = FALSE,   # always open on first load
    minified  = FALSE,   # collapse hides it fully (no half-visible mini strip)
    sidebarMenu(
      id = "tabs",
      menuItem("Overview",         tabName = "overview", icon = icon("chart-pie")),
      menuItem("Content Analysis", tabName = "content",  icon = icon("film")),
      menuItem("Geography",        tabName = "geo",      icon = icon("earth-americas")),
      menuItem("People",           tabName = "people",   icon = icon("users")),
      menuItem("Data Explorer",    tabName = "explorer", icon = icon("table")),
      menuItem("About",            tabName = "about",    icon = icon("circle-info"))
    ),
    sidebar_filters
  ),

  body = bs4DashBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),

    tabItems(

      # ---------------- 1. OVERVIEW ----------------
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("kpi_total",     width = 3),
          valueBoxOutput("kpi_movies",    width = 3),
          valueBoxOutput("kpi_tv",        width = 3),
          valueBoxOutput("kpi_countries", width = 3)
        ),
        fluidRow(
          valueBoxOutput("kpi_ratings",  width = 3),
          valueBoxOutput("kpi_years",    width = 3),
          valueBoxOutput("kpi_directors", width = 3),
          valueBoxOutput("kpi_genres",   width = 3)
        ),
        fluidRow(
          box(title = "Movies vs TV Shows", width = 5, status = "danger",
              solidHeader = FALSE, plotlyOutput("plt_type_donut", height = 340)),
          box(title = "Release Trend Over Time", width = 7, status = "danger",
              plotlyOutput("plt_release_trend", height = 340))
        ),
        fluidRow(
          box(title = "Content Added to Netflix by Year", width = 6, status = "danger",
              plotlyOutput("plt_added_year", height = 330)),
          box(title = "Content Added Heatmap (Year x Month)", width = 6, status = "danger",
              plotlyOutput("plt_heatmap", height = 330))
        )
      ),

      # ---------------- 2. CONTENT ANALYSIS ----------------
      tabItem(
        tabName = "content",
        fluidRow(
          box(title = "Top 10 Genres", width = 6, status = "danger",
              plotlyOutput("plt_genres", height = 360)),
          box(title = "Ratings Distribution", width = 6, status = "danger",
              plotlyOutput("plt_ratings", height = 360))
        ),
        fluidRow(
          box(title = "Movie Duration Distribution (minutes)", width = 6, status = "danger",
              plotlyOutput("plt_duration", height = 360)),
          box(title = "Movies vs TV Shows Added by Year", width = 6, status = "danger",
              plotlyOutput("plt_type_year", height = 360))
        )
      ),

      # ---------------- 3. GEOGRAPHY ----------------
      tabItem(
        tabName = "geo",
        fluidRow(
          box(title = "Top 10 Producing Countries", width = 5, status = "danger",
              plotlyOutput("plt_countries", height = 380)),
          box(title = "Global Content Growth Over Time", width = 7, status = "danger",
              plotlyOutput("plt_worldmap", height = 380))
        ),
        fluidRow(
          box(title = "Country Comparison (Movies vs TV Shows)", width = 12, status = "danger",
              pickerInput("cmp_countries", NULL, choices = country_choices,
                          selected = head(country_choices, 5), multiple = TRUE,
                          options = pickerOptions(liveSearch = TRUE, maxOptions = 8,
                                                  actionsBox = TRUE), width = "420px"),
              plotlyOutput("plt_country_cmp", height = 340))
        )
      ),

      # ---------------- 4. PEOPLE ----------------
      tabItem(
        tabName = "people",
        fluidRow(
          column(width = 12,
                 sliderInput("topn_people", "Show Top N", min = 5, max = 20,
                             value = 10, step = 1, width = "300px"))
        ),
        fluidRow(
          box(title = "Top Directors", width = 6, status = "danger",
              plotlyOutput("plt_directors", height = 420)),
          box(title = "Top Actors", width = 6, status = "danger",
              plotlyOutput("plt_actors", height = 420))
        )
      ),

      # ---------------- 5. DATA EXPLORER ----------------
      tabItem(
        tabName = "explorer",
        fluidRow(
          box(title = "Interactive Data Explorer", width = 12, status = "danger",
              p(class = "text-muted",
                "Search, sort, filter and export the currently filtered dataset."),
              DTOutput("tbl_explorer"))
        )
      ),

      # ---------------- 6. ABOUT ----------------
      tabItem(
        tabName = "about",
        fluidRow(
          box(title = "About the Dataset", width = 6, status = "danger",
              p(strong("Dataset:"), " Netflix Movies and TV Shows (Kaggle)"),
              p(strong("Source:"),
                tags$a(href = "https://www.kaggle.com/datasets/shivamb/netflix-shows",
                       target = "_blank", "kaggle.com/datasets/shivamb/netflix-shows")),
              p(strong("Records:"), textOutput("about_n", inline = TRUE),
                " titles listed on Netflix up to 2021."),
              p(strong("Attributes:")),
              div(
                lapply(c("type", "title", "director", "cast", "country", "date_added",
                         "release_year", "rating", "duration", "listed_in"),
                       function(x) span(class = "about-badge", x))
              )
          ),
          box(title = "Author", width = 6, status = "danger",
              p(strong("Name:"), " Sami Al Jaber Tasin"),
              p(strong("Student ID:"), " 89232048"),
              p(strong("Course:"), " Information Visualization"),
              p(strong("Technology:"), " R, Shiny, bs4Dash, Plotly, ggplot2, DT, dplyr, tidyr, shinyWidgets"))
        ),
        fluidRow(
          box(title = "Project Objectives", width = 12, status = "danger",
              tags$ol(
                tags$li("Explore the composition of the Netflix catalogue (movies vs TV shows, genres, ratings)."),
                tags$li("Analyse how the catalogue has grown over time and identify content-addition trends."),
                tags$li("Compare content production across countries and visualise its global distribution."),
                tags$li("Identify the most frequent directors and actors on the platform."),
                tags$li("Provide an interactive, filterable interface that supports free exploration of the data.")
              ))
        )
      )
    )
  ),

  footer = bs4DashFooter(
    left = span("Netflix Information Visualization Dashboard - built with R Shiny"),
    right = span(format(Sys.Date(), "%Y"))
  )
)

# --------------------------------------------------------------
# MODULE 3 + 4 + 5 : SERVER  (KPI cards, charts, data explorer)
# --------------------------------------------------------------

server <- function(input, output, session) {

  # Server-side selectize for the very long director / actor lists
  updateSelectizeInput(session, "f_director", choices = director_choices, server = TRUE)
  updateSelectizeInput(session, "f_actor",    choices = actor_choices,    server = TRUE)

  # ---- Module 2 : central filtered dataset ----
  filtered <- reactive({
    df <- netflix
    if (input$f_type != "All") df <- df %>% filter(type == input$f_type)
    df <- df %>% filter(release_year >= input$f_year[1],
                        release_year <= input$f_year[2])
    if (length(input$f_rating) > 0) df <- df %>% filter(rating %in% input$f_rating)
    df <- df[match_multi(df$country,   input$f_country), ]
    df <- df[match_multi(df$listed_in, input$f_genre), ]
    df <- df[match_multi(df$director,  input$f_director), ]
    df <- df[match_multi(df$cast,      input$f_actor), ]
    df
  })

  observeEvent(input$f_reset, {
    updatePrettyRadioButtons(session, "f_type", selected = "All")
    updatePickerInput(session, "f_country", selected = character(0))
    updatePickerInput(session, "f_genre",   selected = character(0))
    updatePickerInput(session, "f_rating",  selected = character(0))
    updateSliderInput(session, "f_year", value = c(year_min, year_max))
    updateSelectizeInput(session, "f_director", choices = director_choices,
                         selected = character(0), server = TRUE)
    updateSelectizeInput(session, "f_actor", choices = actor_choices,
                         selected = character(0), server = TRUE)
  })

  # ---- Module 3 : KPI cards ----
  output$kpi_total <- renderValueBox({
    valueBox(format(nrow(filtered()), big.mark = ","), "Total Titles",
             icon = icon("layer-group"), color = "primary")
  })
  output$kpi_movies <- renderValueBox({
    valueBox(format(sum(filtered()$type == "Movie"), big.mark = ","), "Movies",
             icon = icon("film"), color = "danger")
  })
  output$kpi_tv <- renderValueBox({
    valueBox(format(sum(filtered()$type == "TV Show"), big.mark = ","), "TV Shows",
             icon = icon("tv"), color = "primary")
  })
  output$kpi_countries <- renderValueBox({
    valueBox(nrow(count_split(filtered(), "country", Inf)), "Countries",
             icon = icon("earth-americas"), color = "danger")
  })
  output$kpi_ratings <- renderValueBox({
    valueBox(n_distinct(filtered()$rating), "Ratings",
             icon = icon("star"), color = "danger")
  })
  output$kpi_years <- renderValueBox({
    df <- filtered()
    rng <- if (nrow(df) > 0) paste0(min(df$release_year), "-", max(df$release_year)) else "-"
    valueBox(rng, "Release Year Span", icon = icon("calendar"), color = "primary")
  })
  output$kpi_directors <- renderValueBox({
    valueBox(format(nrow(count_split(filtered(), "director", Inf)), big.mark = ","),
             "Directors", icon = icon("clapperboard"), color = "primary")
  })
  output$kpi_genres <- renderValueBox({
    valueBox(nrow(count_split(filtered(), "listed_in", Inf)), "Genres",
             icon = icon("tags"), color = "danger")
  })

  # ---- Module 4 : charts ----

  # 1. Movies vs TV Shows (donut)
  output$plt_type_donut <- renderPlotly({
    d <- filtered() %>% count(type)
    plot_ly(d, labels = ~type, values = ~n, type = "pie", hole = 0.55,
            marker = list(colors = unname(pal_type[d$type]),
                          line = list(color = "#fff", width = 2)),
            textinfo = "label+percent") %>%
      nf_layout(showlegend = TRUE, legend = list(orientation = "h", x = 0.25))
  })

  # 2. Release trend
  output$plt_release_trend <- renderPlotly({
    d <- filtered() %>% filter(release_year >= 1950) %>%
      count(release_year, type)
    plot_ly(d, x = ~release_year, y = ~n, color = ~type, colors = pal_type,
            type = "scatter", mode = "lines", fill = "tozeroy",
            hovertemplate = "%{y} titles (%{x})<extra></extra>") %>%
      nf_layout(xaxis = list(title = "Release Year"),
                yaxis = list(title = "Number of Titles"),
                legend = list(orientation = "h", x = 0.3))
  })

  # 3. Content added by year (total)
  output$plt_added_year <- renderPlotly({
    d <- filtered() %>% filter(!is.na(year_added)) %>% count(year_added)
    plot_ly(d, x = ~year_added, y = ~n, type = "bar",
            marker = list(color = nf_red, line = list(color = "#fff", width = 1)),
            hovertemplate = "%{y} titles added in %{x}<extra></extra>") %>%
      nf_layout(xaxis = list(title = "Year Added"),
                yaxis = list(title = "Titles Added"))
  })

  # 10. Heatmap : year x month added
  output$plt_heatmap <- renderPlotly({
    d <- filtered() %>%
      filter(!is.na(year_added), !is.na(month_added)) %>%
      count(year_added, month_added) %>%
      complete(year_added, month_added, fill = list(n = 0))
    plot_ly(d, x = ~month_added, y = ~ordered(year_added), z = ~n,
            type = "heatmap",
            colorscale = list(c(0, "#FFF5F5"), c(1, nf_red)),
            hovertemplate = "%{x} %{y}: %{z} titles<extra></extra>") %>%
      nf_layout(xaxis = list(title = ""), yaxis = list(title = "", autorange = "reversed"))
  })

  # 5. Top genres
  output$plt_genres <- renderPlotly({
    d <- count_split(filtered(), "listed_in", 10)
    plot_ly(d, x = ~count, y = ~reorder(value, count), type = "bar",
            orientation = "h", marker = list(color = nf_red),
            hovertemplate = "%{y}: %{x} titles<extra></extra>") %>%
      nf_layout(xaxis = list(title = "Titles"), yaxis = list(title = ""))
  })

  # 6. Ratings distribution
  output$plt_ratings <- renderPlotly({
    d <- filtered() %>% count(rating, type)
    plot_ly(d, x = ~reorder(rating, -n, sum), y = ~n, color = ~type,
            colors = pal_type, type = "bar",
            hovertemplate = "%{x}: %{y}<extra></extra>") %>%
      nf_layout(barmode = "stack",
                xaxis = list(title = "Rating"),
                yaxis = list(title = "Titles"),
                legend = list(orientation = "h", x = 0.3))
  })

  # 7. Duration histogram (ggplot2 -> plotly)
  output$plt_duration <- renderPlotly({
    d <- filtered() %>% filter(type == "Movie", !is.na(duration_num))
    validate(need(nrow(d) > 0, "No movies in the current selection."))
    g <- ggplot(d, aes(x = duration_num)) +
      geom_histogram(binwidth = 10, fill = nf_red, colour = "white") +
      labs(x = "Duration (minutes)", y = "Number of Movies") +
      theme_minimal(base_family = "Inter")
    ggplotly(g) %>% nf_layout()
  })

  # 12. Movies vs TV Shows added by year (stacked)
  output$plt_type_year <- renderPlotly({
    d <- filtered() %>% filter(!is.na(year_added)) %>% count(year_added, type)
    plot_ly(d, x = ~year_added, y = ~n, color = ~type, colors = pal_type,
            type = "bar") %>%
      nf_layout(barmode = "stack",
                xaxis = list(title = "Year Added"),
                yaxis = list(title = "Titles"),
                legend = list(orientation = "h", x = 0.3))
  })

  # 4. Top countries
  output$plt_countries <- renderPlotly({
    d <- count_split(filtered(), "country", 10)
    plot_ly(d, x = ~count, y = ~reorder(value, count), type = "bar",
            orientation = "h", marker = list(color = nf_blue),
            hovertemplate = "%{y}: %{x} titles<extra></extra>") %>%
      nf_layout(xaxis = list(title = "Titles"), yaxis = list(title = ""))
  })

  # 13. Rotatable globe (animated bubbles: cumulative growth by year)
  output$plt_worldmap <- renderPlotly({
    d <- filtered() %>%
      filter(!is.na(country), !is.na(year_added)) %>%
      separate_rows(country, sep = ",\\s*") %>%
      count(year_added, country) %>%
      complete(year_added = full_seq(year_added, 1), country,
               fill = list(n = 0)) %>%
      group_by(country) %>% arrange(year_added) %>%
      mutate(total = cumsum(n)) %>% ungroup() %>%
      filter(total > 0)
    validate(need(nrow(d) > 0, "No country data in the current selection."))
    plot_ly(d, locations = ~country, locationmode = "country names",
            frame = ~year_added, type = "scattergeo", mode = "markers",
            text = ~paste0(country, ": ", total, " titles"),
            hoverinfo = "text",
            marker = list(
              size = ~total, sizemode = "area",
              sizeref = 2 * max(d$total) / (48^2), sizemin = 4,
              color = "#E50914", opacity = 0.75,
              line = list(color = "#FFFFFF", width = 1))) %>%
      layout(
        font = list(family = "Inter, sans-serif", color = "#221F1F"),
        paper_bgcolor = "rgba(0,0,0,0)",
        margin = list(t = 0, b = 0, l = 0, r = 0),
        geo = list(
          projection = list(type = "orthographic",
                            rotation = list(lon = -30, lat = 15),
                            scale = 1),
          domain = list(x = c(0, 1), y = c(0.10, 1)),
          bgcolor = "rgba(0,0,0,0)",
          showland = TRUE, landcolor = "#E8EAED",
          showcountries = TRUE, countrycolor = "#FFFFFF",
          showocean = TRUE, oceancolor = "#F4F8FB",
          showframe = FALSE,
          showcoastlines = TRUE, coastlinecolor = "#D0D5DA")) %>%
      animation_opts(frame = 700, transition = 0, redraw = TRUE) %>%
      animation_slider(currentvalue = list(prefix = "Year: ",
                                           font = list(color = "#E50914", size = 15))) %>%
      animation_button(label = "Play")
  })

  # 14. Country comparison
  output$plt_country_cmp <- renderPlotly({
    sel <- input$cmp_countries
    validate(need(length(sel) >= 2, "Select at least two countries to compare."))
    d <- filtered() %>%
      filter(!is.na(country)) %>%
      separate_rows(country, sep = ",\\s*") %>%
      filter(country %in% sel) %>%
      count(country, type)
    plot_ly(d, x = ~country, y = ~n, color = ~type, colors = pal_type,
            type = "bar") %>%
      nf_layout(barmode = "group",
                xaxis = list(title = ""), yaxis = list(title = "Titles"),
                legend = list(orientation = "h", x = 0.35))
  })

  # 8. Top directors
  output$plt_directors <- renderPlotly({
    d <- count_split(filtered(), "director", input$topn_people)
    validate(need(nrow(d) > 0, "No director data in the current selection."))
    plot_ly(d, x = ~count, y = ~reorder(value, count), type = "bar",
            orientation = "h", marker = list(color = nf_red),
            hovertemplate = "%{y}: %{x} titles<extra></extra>") %>%
      nf_layout(xaxis = list(title = "Titles"), yaxis = list(title = ""))
  })

  # 9. Top actors
  output$plt_actors <- renderPlotly({
    d <- count_split(filtered(), "cast", input$topn_people)
    validate(need(nrow(d) > 0, "No cast data in the current selection."))
    plot_ly(d, x = ~count, y = ~reorder(value, count), type = "bar",
            orientation = "h", marker = list(color = nf_blue),
            hovertemplate = "%{y}: %{x} titles<extra></extra>") %>%
      nf_layout(xaxis = list(title = "Titles"), yaxis = list(title = ""))
  })

  # ---- Module 5 : data explorer ----
  output$tbl_explorer <- renderDT({
    filtered() %>%
      select(Type = type, Title = title, Director = director,
             Country = country, `Date Added` = date_added,
             `Release Year` = release_year, Rating = rating,
             Duration = duration, Genres = listed_in)
  },
  extensions = "Buttons",
  filter = "top",
  rownames = FALSE,
  options = list(
    pageLength = 15,
    scrollX = TRUE,
    dom = "Bfrtip",
    buttons = list(
      list(extend = "csv",   text = "Export CSV",   filename = "netflix_filtered"),
      list(extend = "excel", text = "Export Excel", filename = "netflix_filtered"),
      "copy"
    )
  ),
  server = FALSE)

  # ---- About ----
  output$about_n <- renderText(format(nrow(netflix), big.mark = ","))
}

shinyApp(ui, server)
