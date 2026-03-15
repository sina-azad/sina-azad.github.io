pacman::p_load(rio, tidyverse, officer, xml2)

# import bibliography word file path
source("files_directories.R")

word_file_path <- list.files(my_publications_folder, full.names = TRUE) |>
  str_subset("My_bibliography_.*\\.docx$")

# word_file_path

# output pubs_list.csv path, all qmd output files are alson in _pubs_files/
pubs_list_dir <- "_pubs_files/pubs_list.csv"

# ============================================================
# Step 1: function to extract bibliography from Word file ----
# ============================================================

extract_bibliography_from_word <- function(word_file_path) {
  temp_dir <- tempdir()
  unzip(word_file_path, exdir = temp_dir, overwrite = TRUE)
  doc_xml <- read_xml(file.path(temp_dir, "word/document.xml"))

  # Get all text nodes
  texts <- xml_find_all(doc_xml, ".//w:t", ns = xml_ns(doc_xml))
  full_text <- xml_text(texts)

  # Find the "Bibliography" header
  bib_index <- which(stringr::str_detect(full_text, "(?i)^bibliography$"))

  if (length(bib_index) == 0) {
    cat(
      "Could not find 'Bibliography' header. Available text nodes around bibliography:\n"
    )
    bib_like <- which(stringr::str_detect(full_text, "(?i)bibliography"))
    if (length(bib_like) > 0) {
      print(full_text[
        max(1, bib_like[1] - 2):min(length(full_text), bib_like[1] + 10)
      ])
    }
    stop("Could not find 'Bibliography' header in the Word document")
  }

  # Extract text nodes after Bibliography header
  bib_start <- bib_index[1] + 1
  text_nodes <- full_text[bib_start:length(full_text)]

  # Remove empty nodes
  text_nodes <- text_nodes[
    text_nodes != "" & stringr::str_trim(text_nodes) != ""
  ]

  # Group text nodes by numbered entries
  # Entry numbers are 1-3 digits (like 1., 2., ..., 999.) NOT 4-digit years (2023)
  entry_groups <- numeric(length(text_nodes))
  current_entry <- 0

  for (i in seq_along(text_nodes)) {
    # Check if this node starts with 1-3 digit number followed by period (entry number, not year)
    # This avoids confusing years (2023, 2024) with entry numbers
    if (stringr::str_detect(text_nodes[i], "^\\d{1,3}\\.$")) {
      current_entry <- current_entry + 1
    }
    entry_groups[i] <- current_entry
  }

  # Combine text nodes within each entry group
  entries <- character()
  for (entry_num in unique(entry_groups)) {
    if (entry_num == 0) {
      next
    } # Skip any text before first numbered entry

    # Get all nodes for this entry
    entry_text_nodes <- text_nodes[entry_groups == entry_num]
    # Combine them into one string
    combined_text <- paste(entry_text_nodes, collapse = " ")
    # Clean up extra spaces
    combined_text <- stringr::str_squish(combined_text)
    entries <- c(entries, combined_text)
  }

  # Remove the leading number and dot from each entry
  entries <- stringr::str_remove(entries, "^\\d{1,3}\\.\\s*")

  entries
}

# ========================================================================================
# Step 2: function to parse bibliography entries into structured data and save to CSV ----
# ========================================================================================

parse_bibliography_entries <- function(entries, output_csv = pubs_list_dir) {
  result <- tibble(text = entries) |>
    rowwise() |>
    mutate(
      # Extract components using pattern matching
      # Fields are separated by ". " followed by field marker with ":"
      # Content can include colons, so match until ". " + next field marker
      author = str_trim(str_extract(text, "(?<=author:\\s).*?(?=\\.\\s(?:title:|journal:|year:|conf:|doi:|pmid:|ref\\s|url:))")),
      title = str_trim(str_extract(text, "(?<=title:\\s).*?(?=\\.\\s(?:journal:|year:|conf:|doi:|pmid:|ref\\s|url:|author:))")),
      journal = str_trim(str_extract(text, "(?<=journal:\\s).*?(?=\\.\\s(?:year:|doi:|pmid:|ref\\s|url:|author:|title:|conf:))")),
      year = str_extract(text, "(?<=year:\\s)\\d{4}"),
      conference = str_trim(str_extract(text, "(?<=conf:\\s).*?(?=\\.\\s(?:journal:|doi:|pmid:|ref\\s|url:|author:|title:|year:))")),
      doi = str_remove(str_extract(text, "(?<=doi:\\s)\\S+"), "\\.$"),
      pmid = str_remove(str_extract(text, "(?<=pmid:\\s)\\S+"), "\\.$"),
      ref_type = str_trim(str_extract(text, "(?<=ref\\stype:\\s).*?(?=\\.\\s(?:url:|author:|title:|journal:|year:|conf:|doi:|pmid:))|(?<=ref\\stype:\\s)[^.]*$")),
      url = str_extract(text, "(?<=url:\\s)\\S+")
    ) |>
    ungroup() |>

    # handle NAs in doi and pmid
    mutate(
      doi = ifelse(is.na(doi), "N/A", doi),
      pmid = ifelse(is.na(pmid), "N/A", pmid)
    ) |>
    select(author, title, journal, year, doi, pmid, ref_type, url, conference)

  # Save to CSV
  export(x = result, file = pubs_list_dir)

  result
}

# =============================================================================
# Step 3: function to generate selected publications bibliography qmd file ----
# =============================================================================

generate_selected_bibliography <- function(
  csv_file = pubs_list_dir,
  selected_pmids = c(
    "41781047",
    "40996033",
    "40171038",
    "39971675",
    "37598214",
    "37124828",
    "35770711",
    "35397236",
    "33665966",
    "33554610"
  )
) {
  # Read the publication data
  pubs <- import(csv_file)

  # Filter for selected PMIDs, and Journal article, and select relevant columns
  data <- pubs |>
    filter(ref_type == "Journal Article") |>
    select(author, title, journal, year, doi, pmid, url) |>
    filter(pmid %in% selected_pmids) |>
    arrange(match(pmid, selected_pmids)) # Maintain order of provided PMIDs

  # initialize markdown content
  markdown <- ""

  # loop through publications
  for (i in seq_len(nrow(data))) {
    authors <- data$author[i]
    title <- data$title[i]
    url <- data$url[i]
    journal <- data$journal[i]
    year <- data$year[i]
    doi <- data$doi[i]
    pmid <- data$pmid[i]

    # bold my name
    authors <- gsub(
      "Azadnajafabad S",
      "**Azadnajafabad S**",
      authors
    )

    # italicize journal name
    journal <- paste0("*", journal, "*")

    # append formatted markdown
    markdown <- paste0(
      markdown,
      "::: {.altmetric-publication}\n",
      "* ",
      authors,
      ". [",
      title,
      "](",
      url,
      "). ",
      journal,
      ". ",
      year,
      ". DOI: ",
      doi,
      ". PMID: ",
      pmid,
      "\n\n",
      # Dimensions badge - shows citation count and other metrics
      "<div class=\"badges-container\">\n",
      "  <span class=\"__dimensions_badge_embed__\" \n",
      "        data-doi=\"",
      doi,
      "\"\n",
      "        data-legend=\"hover-bottom\"\n",
      "        data-style=\"small_circle\"\n",
      "        data-hide-zero-citations=\"true\">\n",
      "  </span>\n\n",
      # Altmetric badge - shows online mentions and social media impact
      "  <div data-badge-popover=\"bottom\" \n",
      "       data-badge-type=\"donut\" \n",
      "       data-condensed=\"false\" \n",
      "       data-doi=\"",
      doi,
      "\" \n",
      "       data-hide-no-mentions=\"true\" \n",
      "       data-hide-less-than=\"0\" \n",
      "       class=\"altmetric-embed\">\n",
      "  </div>\n",
      "</div>\n",
      ":::\n\n"
    )
  }

  # write output file
  outfile <- "_pubs_files/publist_selected.qmd"
  writeLines(markdown, outfile)

  cat(paste0(
    "Generated selected bibliography file: ",
    outfile,
    " (",
    nrow(data),
    " publications)\n"
  ))
}

# ==========================================================================
# Step 4: function to generate per year bibliography qmd files from CSV ----
# ==========================================================================

generate_yearly_bibliography <- function(csv_file = pubs_list_dir) {
  # Read the publication data
  pubs <- import(csv_file)

  # filter for reference type "Journal Article" and select relevant columns
  pubs <- pubs |>
    filter(ref_type == "Journal Article") |>
    select(author, title, journal, year, doi, pmid, url)

  # years to iterate over
  years <- sort(unique(pubs$year), decreasing = TRUE)

  for (yr in years) {
    # filter data for year
    data <- pubs |>
      filter(year == yr)

    # skip if no publications for that year
    if (nrow(data) == 0) {
      next
    }

    # initialize markdown content
    markdown <- ""

    # loop through publications
    for (i in seq_len(nrow(data))) {
      authors <- data$author[i]
      title <- data$title[i]
      url <- data$url[i]
      journal <- data$journal[i]
      year <- data$year[i]
      doi <- data$doi[i]
      pmid <- data$pmid[i]

      # bold my name
      authors <- gsub(
        "Azadnajafabad S",
        "**Azadnajafabad S**",
        authors
      )

      # italicize journal name
      journal <- paste0("*", journal, "*")

      # append formatted markdown
      markdown <- paste0(
        markdown,
        "::: {.altmetric-publication}\n",
        "* ",
        authors,
        ". [",
        title,
        "](",
        url,
        "). ",
        journal,
        ". ",
        year,
        ". DOI: ",
        doi,
        ". PMID: ",
        pmid,
        "\n\n",
        # Dimensions badge - shows citation count and other metrics
        "<div class=\"badges-container\">\n",
        "  <span class=\"__dimensions_badge_embed__\" \n",
        "        data-doi=\"",
        doi,
        "\"\n",
        "        data-legend=\"hover-bottom\"\n",
        "        data-style=\"small_circle\"\n",
        "        data-hide-zero-citations=\"true\">\n",
        "  </span>\n\n",
        # Altmetric badge - shows online mentions and social media impact
        "  <div data-badge-popover=\"bottom\" \n",
        "       data-badge-type=\"donut\" \n",
        "       data-condensed=\"false\" \n",
        "       data-doi=\"",
        doi,
        "\" \n",
        "       data-hide-no-mentions=\"true\" \n",
        "       data-hide-less-than=\"0\" \n",
        "       class=\"altmetric-embed\">\n",
        "  </div>\n",
        "</div>\n",
        ":::\n\n"
      )
    }

    # write output file
    outfile <- paste0("_pubs_files/publist_", yr, ".qmd")
    writeLines(markdown, outfile)
  }

  cat(paste0(
    "Generated bibliography files for years: ",
    paste(years, collapse = ", ")
  ), "\n")
}

# ==========================================================================
# Step 5: function to generate preprints qmd file from CSV ----
# ==========================================================================

generate_preprints_bibliography <- function(csv_file = pubs_list_dir) {
  # Read the publication data
  pubs <- import(csv_file)

  # filter for reference type "Unpublished Work" and select relevant columns
  data <- pubs |>
    filter(ref_type == "Unpublished Work") |>
    select(author, title, journal, year, doi, url)

  # initialize markdown content
  markdown <- ""

  # loop through publications
  for (i in seq_len(nrow(data))) {
    authors <- data$author[i]
    title <- data$title[i]
    url <- data$url[i]
    journal <- data$journal[i]
    year <- data$year[i]
    doi <- data$doi[i]

    # bold my name
    authors <- gsub(
      "Azadnajafabad S",
      "**Azadnajafabad S**",
      authors
    )

    # italicize journal name
    journal <- paste0("*", journal, "*")

    # append formatted markdown
    markdown <- paste0(
      markdown,
      "::: {.altmetric-publication}\n",
      "* ",
      authors,
      ". [",
      title,
      "](",
      url,
      "). ",
      journal,
      ". ",
      year,
      ". DOI: ",
      doi,
      "\n\n",
      # Dimensions badge - shows citation count and other metrics
      "<div class=\"badges-container\">\n",
      "  <span class=\"__dimensions_badge_embed__\" \n",
      "        data-doi=\"",
      doi,
      "\"\n",
      "        data-legend=\"hover-bottom\"\n",
      "        data-style=\"small_circle\"\n",
      "        data-hide-zero-citations=\"true\">\n",
      "  </span>\n\n",
      # Altmetric badge - shows online mentions and social media impact
      "  <div data-badge-popover=\"bottom\" \n",
      "       data-badge-type=\"donut\" \n",
      "       data-condensed=\"false\" \n",
      "       data-doi=\"",
      doi,
      "\" \n",
      "       data-hide-no-mentions=\"true\" \n",
      "       data-hide-less-than=\"0\" \n",
      "       class=\"altmetric-embed\">\n",
      "  </div>\n",
      "</div>\n",
      ":::\n\n"
    )
  }

  # write output file
  outfile <- "_pubs_files/publist_preprints.qmd"
  writeLines(markdown, outfile)

  cat(paste0(
    "Generated preprints bibliography file: ",
    outfile,
    " (",
    nrow(data),
    " preprints)\n"
  ))
}

# ==========================================================================
# Step 6: function to generate conference abstracts qmd file from CSV ----
# ==========================================================================

generate_conference_bibliography <- function(csv_file = pubs_list_dir) {
  # Read the publication data
  pubs <- import(csv_file)

  # filter for reference type "Conference Paper" and select relevant columns
  data <- pubs |>
    filter(ref_type == "Conference Paper") |>
    select(author, title, journal, year, doi, url, conference)

  # initialize markdown content
  markdown <- ""

  # loop through publications
  for (i in seq_len(nrow(data))) {
    authors <- data$author[i]
    title <- data$title[i]
    url <- data$url[i]
    conference <- data$conference[i]
    journal <- data$journal[i]
    year <- data$year[i]
    doi <- data$doi[i]

    # bold my name
    authors <- gsub(
      "Azadnajafabad S",
      "**Azadnajafabad S**",
      authors
    )

    # italicize journal name
    journal <- paste0("*", journal, "*")

    # append formatted markdown
    markdown <- paste0(
      markdown,
      "::: {.altmetric-publication}\n",
      "* ",
      authors,
      ". [",
      title,
      "](",
      url,
      "). ",
      conference,
      ". ",
      journal,
      ". ",
      year,
      ". DOI: ",
      doi,
      "\n\n",
      # Dimensions badge - shows citation count and other metrics
      "<div class=\"badges-container\">\n",
      "  <span class=\"__dimensions_badge_embed__\" \n",
      "        data-doi=\"",
      doi,
      "\"\n",
      "        data-legend=\"hover-bottom\"\n",
      "        data-style=\"small_circle\"\n",
      "        data-hide-zero-citations=\"true\">\n",
      "  </span>\n\n",
      # Altmetric badge - shows online mentions and social media impact
      "  <div data-badge-popover=\"bottom\" \n",
      "       data-badge-type=\"donut\" \n",
      "       data-condensed=\"false\" \n",
      "       data-doi=\"",
      doi,
      "\" \n",
      "       data-hide-no-mentions=\"true\" \n",
      "       data-hide-less-than=\"0\" \n",
      "       class=\"altmetric-embed\">\n",
      "  </div>\n",
      "</div>\n",
      ":::\n\n"
    )
  }

  # write output file
  outfile <- "_pubs_files/publist_conferences.qmd"
  writeLines(markdown, outfile)

  cat(paste0(
    "Generated conferences bibliography file: ",
    outfile,
    " (",
    nrow(data),
    " conferences)\n"
  ))
}

# ================================================================================
# Complete workflow function: word file -> CSV -> selected & yearly qmd files ----
# ================================================================================

complete_bibliography_workflow <- function(
  word_file_path,
  output_csv = pubs_list_dir
) {
  # Step 1: Extract bibliography from word file
  cat("Extracting bibliography from word file...\n")
  entries <- extract_bibliography_from_word(word_file_path)

  # Step 2: Parse entries into structured format and save to CSV
  cat("Parsing bibliography entries...\n")
  pubs_list <- parse_bibliography_entries(entries, output_csv)

  # Step 3: Generate selected bibliography file
  cat("Generating selected bibliography file...\n")
  generate_selected_bibliography(output_csv)

  # Step 4: Generate yearly bibliography files
  cat("Generating yearly bibliography files...\n")
  generate_yearly_bibliography(output_csv)

  # Step 5: Generate preprints bibliography file
  cat("Generating preprints bibliography file...\n")
  generate_preprints_bibliography(output_csv)

  # Step 6: Generate conference abstracts bibliography file
  cat("Generating conference abstracts bibliography file...\n")
  generate_conference_bibliography(output_csv)

  cat("Complete! Bibliography saved to CSV and qmd files generated.\n")

  pubs_list
}


# Final run of complete workflow
complete_bibliography_workflow(
  word_file_path,
  output_csv = pubs_list_dir
)
