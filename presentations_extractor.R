pacman::p_load(rio, tidyverse)

# import presentations excel file
source("files_directories.R")

presentations_list <- import(file.path(pubs_dir, "presentations_list.xlsx"))
glimpse(presentations_list)

# ==========================================================================
# Step 1: function to generate per year presentations qmd files from excel ----
# ==========================================================================

generate_yearly_presentations <- function() {
  # Read the presentations data
  presentations <- import(file.path(pubs_dir, "presentations_list.xlsx"))

  # years to iterate over
  years <- sort(unique(presentations$year), decreasing = TRUE)

  for (yr in years) {
    # filter data for year
    data <- presentations |>
      filter(year == yr)

    # skip if no presentations for that year
    if (nrow(data) == 0) {
      next
    }

    # initialize markdown content
    markdown <- ""

    # loop through presentations
    for (i in seq_len(nrow(data))) {
      authors <- data$author[i]
      title <- data$title[i]
      conference <- data$conference[i]
      url <- data$url[i]
      date <- data$date[i]
      location <- data$location[i]
      type <- data$type[i]

      # bold my name
      authors <- gsub(
        "Azadnajafabad S",
        "**Azadnajafabad S**",
        authors
      )

      # italicize conference name
      conference <- paste0("*", conference, "*")

      # append formatted markdown
      markdown <- paste0(
        markdown,
        "* ",
        authors,
        ". ",
        title,
        ". [",
        conference,
        "](",
        url,
        "). ",
        date,
        ". ",
        location,
        ". ",
        type,
        "\n\n"
      )
    }

    # write output file
    outfile <- paste0("_pubs_files/presentations_", yr, ".qmd")
    writeLines(markdown, outfile)
  }

  cat(paste0(
    "Generated presentations files for years: ",
    paste(years, collapse = ", ")
  ), "\n")
}

# ================================================================================
# Final run of workflow ----
# ================================================================================
cat("Generating yearly presentations files...\n")
generate_yearly_presentations()

cat("Completed! Yearly presentations files generated.\n")
