library(httr2)
library(dplyr)
library(readr)
library(stringr)
library(tibble)

source_ids_path <- "data/source_ids.csv"
output_path <- "data/flyers.csv"
api_url <- "https://rucore.libraries.rutgers.edu/api/search/record/"

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

clean_text <- function(value, fallback = "") {
  if (is.null(value) || length(value) == 0 || all(is.na(value))) {
    return(fallback)
  }

  value <- value[!is.na(value)]
  value <- str_squish(as.character(value))
  value <- value[value != ""]

  if (length(value) == 0) {
    fallback
  } else {
    paste(unique(value), collapse = "; ")
  }
}

pluck_value <- function(record, path, fallback = "") {
  value <- record

  for (name in path) {
    if (is.null(value[[name]])) {
      return(fallback)
    }
    value <- value[[name]]
  }

  clean_text(value, fallback = fallback)
}

as_list <- function(value) {
  if (is.null(value)) {
    list()
  } else if (is.list(value) && !is.null(names(value)) && !is.null(value$display)) {
    list(value)
  } else if (is.list(value)) {
    value
  } else {
    list(value)
  }
}

extract_display_values <- function(value) {
  values <- lapply(as_list(value), function(item) {
    if (is.list(item) && !is.null(item$display)) {
      item$display
    } else {
      item
    }
  })

  clean_text(unlist(values), fallback = "")
}

extract_year <- function(date_value, title_value) {
  candidates <- c(date_value, title_value)
  candidates <- candidates[!is.na(candidates)]

  if (length(candidates) == 0) {
    return(NA_integer_)
  }

  match <- str_extract(paste(candidates, collapse = " "), "\\b(19|20)\\d{2}\\b")

  if (is.na(match)) {
    NA_integer_
  } else {
    as.integer(match)
  }
}

fetch_record <- function(id) {
  message("Fetching ", id, "...")

  request(api_url) |>
    req_url_query(
      key = "root",
      `id[]` = id,
      output = "json"
    ) |>
    req_user_agent("new-brunswick-flyer-timeline/1.0") |>
    req_perform() |>
    resp_body_json(simplifyVector = FALSE)
}

record_to_row <- function(id, response) {
  result <- response$results$result
  metadata <- result$metadata
  files <- result$files
  title <- pluck_value(metadata, c("titleInfo", "display"), fallback = "Untitled")
  date_issued <- pluck_value(metadata, c("datesIssued", "dateIssued", "display"))
  subjects <- extract_display_values(metadata$subjects$subject)
  genre <- extract_display_values(metadata$genres$genre)
  rights <- pluck_value(metadata, c("rights", "display"))
  year <- extract_year(date_issued, title)

  tibble(
    id = id,
    title = title,
    date_issued = date_issued,
    year = year,
    subjects = subjects,
    genre = genre,
    item_url = paste0("https://rucore.libraries.rutgers.edu/", str_replace(id, ":", "/"), "/"),
    doi_url = pluck_value(result, c("system", "handle")),
    thumbnail_url = pluck_value(files, c("thumbnail", "ref")),
    image_url = pluck_value(files, c("jpeg", "ref")),
    pdf_url = pluck_value(files, c("pdf", "ref")),
    rights = rights
  )
}

if (!file.exists(source_ids_path)) {
  stop("Missing ", source_ids_path, ". Add a CSV with an id column of RUcore IDs.")
}

source_ids <- read_csv(
  source_ids_path,
  col_types = cols(id = col_character())
) |>
  distinct(id) |>
  filter(!is.na(id), id != "")

flyers <- bind_rows(lapply(source_ids$id, function(id) {
  response <- fetch_record(id)
  Sys.sleep(0.15)
  record_to_row(id, response)
})) |>
  arrange(year, title) |>
  select(all_of(required_columns))

write_csv(flyers, output_path, na = "")

message("Wrote ", nrow(flyers), " Rutgers flyer records to ", output_path, ".")
