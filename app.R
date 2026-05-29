library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(stringr)

flyers_path <- "data/flyers.csv"

required_columns <- c(
  "id",
  "title",
  "date_issued",
  "year",
  "subjects",
  "genre",
  "item_url",
  "doi_url",
  "thumbnail_url",
  "image_url",
  "pdf_url",
  "rights"
)

if (!file.exists(flyers_path)) {
  stop("data/flyers.csv was not found. Run fetch_data.R to fetch RUcore metadata.")
}

flyers <- read_csv(
  flyers_path,
  col_types = cols(
    id = col_character(),
    title = col_character(),
    date_issued = col_character(),
    year = col_integer(),
    subjects = col_character(),
    genre = col_character(),
    item_url = col_character(),
    doi_url = col_character(),
    thumbnail_url = col_character(),
    image_url = col_character(),
    pdf_url = col_character(),
    rights = col_character()
  )
)

missing_columns <- setdiff(required_columns, names(flyers))
if (length(missing_columns) > 0) {
  stop("data/flyers.csv is missing required columns: ", paste(missing_columns, collapse = ", "))
}

flyers <- flyers |>
  mutate(
    year = suppressWarnings(as.integer(year)),
    display_date = if_else(is.na(date_issued) | date_issued == "", as.character(year), date_issued),
    decade = if_else(!is.na(year), paste0(floor(year / 10) * 10, "s"), "Unknown"),
    subjects = if_else(is.na(subjects) | subjects == "", "No subjects listed", subjects),
    genre = if_else(is.na(genre) | genre == "", "Ephemera", genre),
    display_image_url = thumbnail_url,
    full_image_url = if_else(is.na(image_url) | image_url == "", thumbnail_url, image_url),
    short_title = str_trunc(title, 82)
  ) |>
  filter(!is.na(year)) |>
  arrange(year, title) |>
  mutate(
    timeline_order = row_number(),
    lane = (timeline_order - 1) %% 7,
    choice_label = paste0(year, " - ", str_trunc(title, 70))
  )

first_flyer_id <- flyers$id[[1]]

ui <- fluidPage(
  tags$head(
    tags$title("New Brunswick Music Scene in Flyers"),
    tags$style(HTML("
      body {
        background: #f5f1e9;
        color: #222426;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      }

      .app-header {
        background: #CC0033;
        color: #fffaf0;
        margin: 0 -15px 22px -15px;
        padding: 28px 34px;
        border-bottom: 5px solid #1f2d2e;
      }

      .app-header h1 {
        margin: 0 0 8px 0;
        font-size: 32px;
        font-weight: 750;
      }

      .app-header p {
        max-width: 980px;
        margin: 0;
        font-size: 16px;
        line-height: 1.5;
      }

      .exhibit-note,
      .feature-panel,
      .timeline-panel,
      .gallery-card,
      .metric {
        background: #fffdf8;
        border: 1px solid #d8d0c3;
        border-radius: 8px;
        box-shadow: 0 1px 2px rgba(25, 27, 29, 0.06);
      }

      .exhibit-note {
        padding: 16px 18px;
        margin-bottom: 18px;
      }

      .metric-row {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 12px;
        margin-bottom: 18px;
      }

      .metric {
        padding: 13px 15px;
      }

      .metric-label {
        color: #6b645b;
        font-size: 12px;
        letter-spacing: 0.04em;
        text-transform: uppercase;
      }

      .metric-value {
        font-size: 24px;
        font-weight: 750;
      }

      .timeline-panel,
      .feature-panel {
        padding: 18px;
        margin-bottom: 18px;
      }

      .feature-grid {
        display: grid;
        grid-template-columns: minmax(220px, 330px) minmax(0, 1fr);
        gap: 20px;
        align-items: start;
      }

      .feature-image,
      .gallery-image {
        width: 100%;
        background: #e9e1d4;
        object-fit: contain;
        display: block;
      }

      .feature-image {
        max-height: 520px;
        border-radius: 6px;
      }

      .feature-title {
        margin: 0 0 8px 0;
        font-size: 24px;
        line-height: 1.2;
      }

      .metadata-line {
        margin: 0 0 8px 0;
        line-height: 1.45;
      }

      .link-row {
        display: flex;
        flex-wrap: wrap;
        gap: 10px;
        margin-top: 14px;
      }

      .link-row a {
        background: #1f2d2e;
        color: #ffffff;
        border-radius: 6px;
        padding: 8px 11px;
        text-decoration: none;
        font-weight: 650;
      }

      .gallery-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(190px, 1fr));
        gap: 14px;
        margin-bottom: 28px;
      }

      .gallery-card {
        overflow: hidden;
      }

      .gallery-image {
        height: 240px;
        border-bottom: 1px solid #d8d0c3;
      }

      .gallery-body {
        padding: 12px;
      }

      .gallery-body h3 {
        margin: 0 0 8px 0;
        font-size: 15px;
        line-height: 1.25;
      }

      .gallery-body p {
        color: #5d554c;
        margin: 0 0 8px 0;
        font-size: 13px;
      }

      .gallery-body a {
        color: #8f1d21;
        font-weight: 700;
      }

      details {
        margin-top: 14px;
        color: #554d46;
      }

      @media (max-width: 860px) {
        .metric-row,
        .feature-grid {
          grid-template-columns: 1fr;
        }

        .app-header {
          padding: 24px 22px;
        }
      }
    "))
  ),

  div(
    class = "app-header",
    h1("New Brunswick Music Scene in Flyers"),
    p(
      "A visual timeline of selected flyers and ephemera from Rutgers Libraries' ",
      "New Brunswick Music Scene Archive Digital Collection."
    )
  ),

  div(
    class = "exhibit-note",
    p(
      "This exhibit uses RUcore metadata and Rutgers-hosted images to highlight local venues, ",
      "bands, benefit shows, student spaces, and DIY music culture documented through flyers. ",
      "Though not a complete history the New Brunswick music scene, this exhibit is meant", 
      "as an ode to this beloved local music scene & the life that has shaped it, and, as always, as", 
      "an appreciation for special collections metadata and library infrastructure that allow us to preserve history", 
    )
  ),

  div(
    class = "metric-row",
    div(
      class = "metric",
      div(class = "metric-label", "Rutgers items"),
      div(class = "metric-value", comma(nrow(flyers)))
    ),
    div(
      class = "metric",
      div(class = "metric-label", "Years covered"),
      div(class = "metric-value", paste0(min(flyers$year), "-", max(flyers$year)))
    ),
    div(
      class = "metric",
      div(class = "metric-label", "Genres"),
      div(class = "metric-value", comma(n_distinct(flyers$genre)))
    )
  ),

  div(
    class = "timeline-panel",
    h2("Timeline"),
    selectInput(
      "featured_id",
      "Featured flyer",
      choices = setNames(flyers$id, flyers$choice_label),
      selected = first_flyer_id
    ),
    plotOutput("timeline_plot", height = "310px", click = "timeline_click")
  ),

  uiOutput("featured_flyer"),

  h2("Flyer Gallery"),
  uiOutput("gallery")
)

server <- function(input, output, session) {
  timeline_data <- reactive({
    flyers |>
      mutate(is_selected = id == input$featured_id)
  })

  observeEvent(input$timeline_click, {
    nearest <- nearPoints(
      timeline_data(),
      input$timeline_click,
      xvar = "year",
      yvar = "lane",
      maxpoints = 1,
      threshold = 20
    )

    if (nrow(nearest) == 1) {
      updateSelectInput(session, "featured_id", selected = nearest$id[[1]])
    }
  })

  selected_flyer <- reactive({
    flyers |>
      filter(id == input$featured_id) |>
      slice_head(n = 1)
  })

  output$timeline_plot <- renderPlot({
    data <- timeline_data()
    selected <- data |> filter(is_selected)

    ggplot(data, aes(x = year, y = lane)) +
      geom_hline(yintercept = 3, color = "#c7bdad", linewidth = 0.7) +
      geom_point(aes(size = is_selected), color = "#8f1d21", alpha = 0.82) +
      geom_point(data = selected, color = "#1f2d2e", size = 5) +
      geom_text(
        data = selected,
        aes(label = str_wrap(str_trunc(title, 48), width = 24)),
        nudge_y = 0.75,
        size = 3.7,
        lineheight = 0.95,
        color = "#1f2d2e"
      ) +
      scale_x_continuous(breaks = pretty_breaks()) +
      scale_y_continuous(NULL, breaks = NULL, limits = c(-0.6, 7.4)) +
      scale_size_manual(values = c(`FALSE` = 2.6, `TRUE` = 5), guide = "none") +
      labs(x = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
  })

  output$featured_flyer <- renderUI({
    flyer <- selected_flyer()

    div(
      class = "feature-panel",
      div(
        class = "feature-grid",
        tags$img(
          class = "feature-image",
          src = flyer$display_image_url,
          alt = paste("Flyer image for", flyer$title)
        ),
        div(
          h2(class = "feature-title", flyer$title),
          p(class = "metadata-line", strong("Date issued: "), flyer$display_date),
          p(class = "metadata-line", strong("Genre: "), flyer$genre),
          p(class = "metadata-line", strong("Subjects: "), flyer$subjects),
          div(
            class = "link-row",
            tags$a("View at Rutgers", href = flyer$item_url, target = "_blank", rel = "noopener noreferrer"),
            tags$a("Persistent DOI", href = flyer$doi_url, target = "_blank", rel = "noopener noreferrer"),
            tags$a("Image file", href = flyer$full_image_url, target = "_blank", rel = "noopener noreferrer")
          ),
          tags$details(
            tags$summary("Rights note"),
            p(flyer$rights)
          )
        )
      )
    )
  })

  output$gallery <- renderUI({
    cards <- lapply(seq_len(nrow(flyers)), function(index) {
      flyer <- flyers[index, ]

      div(
        class = "gallery-card",
        tags$img(
          class = "gallery-image",
          src = flyer$thumbnail_url,
          alt = paste("Thumbnail for", flyer$title)
        ),
        div(
          class = "gallery-body",
          h3(flyer$short_title),
          p(paste(flyer$year, flyer$genre, sep = " | ")),
          tags$a("Rutgers record", href = flyer$item_url, target = "_blank", rel = "noopener noreferrer")
        )
      )
    })

    div(class = "gallery-grid", cards)
  })
}

shinyApp(ui = ui, server = server)
