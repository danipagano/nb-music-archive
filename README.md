# nb-music-archive
digital exhibit of New Brunswick's DIY music scene using RUcore metadata + Rutgers-hosted images

## data sources
- source collection is (for now) just the New Brunswick Music Scene Archive Digital Collection, part of Rutgers Libraries' RUcore repo
- starts from a selected list of RUcore item IDs stored in data/source_ids.csv. fetch script uses the RUcore Search API to retrieve metadata and image links for each item.
- each record includes the rights statement returned by RUcore & Rutgers-hosted images are displayed remotely + linked back to the original item records and persistent DOI pages

## files
  - app.R: Shiny app for timeline, featured flyer, and gallery
  - fetch_data.R: fetches metadata from the RUcore Search API
  - data/source_ids.csv: local seed list of selected RUcore item IDs
  - data/flyers.csv: generated dataset used by the app

## data fields
data/flyers.csv contains:
    
    id, title, date_issued, year, subjects, genre, item_url, doi_url, thumbnail_url, image_url, pdf_url, rights
  
  *** images are NOT copied into this repo! the app displays Rutgers-hosted image and thumbnail URLs returned by RUcore

## install packages
run this once in R:

    install.packages(c(
    "shiny",
    "dplyr",
    "ggplot2",
    "readr",
    "scales",
    "stringr",
    "httr2",
    "tibble"
    ))

## fetch data
(make sure you're in project folder) & run:

    Rscript fetch_data.R
  ... this reads data/source_ids.csv, calls the RUcore Search API, and writes data/flyers.csv.

## run app
(make sure you're in project folder again) & run:

    Rscript -e "shiny::runApp('.', launch.browser = TRUE)"
  ... or just open app.R in RStudio and run from there :P
