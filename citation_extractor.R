pacman::p_load(rvest, tidyverse, gt)

# ============================================================================
# HOW TO UPDATE THIS DATA:
# 1. Open Google Scholar profile in browser
# 2. Save full page (expand for all publications) in Webpage, Single File (*.mhtml) 
# format to _pubs_files/ folder, replacing the old .mhtml file
# 3. Run this script to extract updated data
# ============================================================================

# Google Scholar profile URL
url <- "https://scholar.google.com/citations?hl=en&user=OqOPJOEAAAAJ"
# browseURL(url)

# ============================================================================
# Read and parse the saved Google Scholar MHTML file
# ============================================================================

# Read the MHTML file
mhtml_file <- "_pubs_files/_Sina Azadnajafabad_ - _Google Scholar_.mhtml"
mhtml_lines <- readLines(mhtml_file, encoding = "UTF-8")

# Locate the HTML content block within the MHTML file
# MHTML files have multiple sections separated by boundaries
# The HTML content starts after the "Content-Transfer-Encoding: quoted-printable" header
html_start <- which(str_detect(mhtml_lines, "Content-Transfer-Encoding: quoted-printable"))[1] + 2

# Extract all lines from the HTML content section
html_lines <- mhtml_lines[html_start:length(mhtml_lines)]

# Find the end of the HTML section (before the next MIME boundary marker)
html_end <- which(str_detect(html_lines, "^------MultipartBoundary"))[1] - 1
if (is.na(html_end)) html_end <- length(html_lines)  # Use all lines if no boundary found

# Decode the HTML content from quoted-printable format
# =3D represents '=' and =\n represents line continuation in this encoding
html_content <- html_lines[1:html_end] |>
  paste(collapse = "\n") |>
  str_replace_all("=3D", "=") |>        # Decode equals signs
  str_replace_all("=\n", "")             # Remove soft line breaks

# ============================================================================
# Parse HTML and extract publications table
# ============================================================================
# Google Scholar uses CSS classes: gsc_a_tr (table row), gsc_a_at (title link),
# gsc_a_ac (citation count link), gsc_a_y (year)

parsed_html <- read_html(html_content)

# Extract each publication as a separate row
# CSS selector "tr.gsc_a_tr" targets each publication row in the table
publications <- parsed_html |>
  html_elements("tr.gsc_a_tr") |>
  # Process each row and extract: title, citation count, year
  map_df(function(row) {
    title <- row |> html_element("a.gsc_a_at") |> html_text() |> str_trim()
    cited_by <- row |> html_element("a.gsc_a_ac") |> html_text() |> str_trim()
    year <- row |> html_element(".gsc_a_y") |> html_text() |> str_trim()
    
    list(
      title = title,
      cited_by = as.numeric(cited_by),
      year = year
    )
  }) |> 
  
  # Clean up the extracted publications data
  # Remove encoded special characters that remain from quoted-printable decoding
  # Also normalize whitespace and trim text
  mutate(
    title = str_replace_all(title, "=E2=80=93", "-"),  # en dash → dash
    title = str_replace_all(title, "=E2=80=92", "-"),  # en dash variant
    title = str_replace_all(title, "=E2=80=94", "-"),  # em dash → dash
    title = str_replace_all(title, "=E2=80=A6", "..."), # ellipsis → ...
    title = str_replace_all(title, "\\s+", " "),  # collapse multiple spaces to single space
    year = str_trim(year)  # remove leading/trailing whitespace
  ) |> 
  
  # assign year to one missing publication (PMID: 37124828)
  mutate(
    year = replace_values(year, "" ~ "2023")
    )

publications

# ============================================================================
# Extract citation metrics table
# ============================================================================

# Extract the metrics table (id="gsc_rsb_st") from the parsed HTML
citation_metrics <- parsed_html |>
  html_element("table#gsc_rsb_st") |>
  html_table(fill = TRUE) |>
  # .name_repair = "universal" handles empty column names gracefully
  as_tibble(.name_repair = "universal") |>
  rename(metric = 1) |>  # First column contains metric names
  mutate(
    All = as.numeric(All),
    Since.2021 = as.numeric(Since.2021)
  ) |>
  rename(all = All, since_2021 = Since.2021)

citation_metrics

# ============================================================================
# Create summary statistics 
# ============================================================================

summary_statistics <- tibble(
  Statistic = c(
    "Total publications",
    "Total citations",
    "Average citations (per paper)",
    "Median citations (per paper)",
    "h-index",
    "i10-index"
    # "h-index (since 2021)",
    # "i10-index (since 2021)"
  ),
  Value = c(
    nrow(publications),                                                    
    sum(publications$cited_by, na.rm = TRUE),
    round(mean(publications$cited_by, na.rm = TRUE), 0),
    round(median(publications$cited_by, na.rm = TRUE), 0),
    citation_metrics$all[which(citation_metrics$metric == "h-index")],
    citation_metrics$all[which(citation_metrics$metric == "i10-index")]
    # citation_metrics$since_2021[which(citation_metrics$metric == "h-index")],
    # citation_metrics$since_2021[which(citation_metrics$metric == "i10-index")]
  )
)

summary_statistics

# write_csv(summary_statistics, "_pubs_files/summary_statistics.csv")


# citation_table
citation_table <- summary_statistics |>
  # convert the "Statistic" column to row names for gt
  tibble::column_to_rownames(var = "Statistic") |>
  gt(rownames_to_stub = TRUE) |>
  fmt_number(
    columns = where(is.numeric),
    decimals = 0,
    sep_mark = ","
  ) |>
  # assign a title to the row names column
  tab_stubhead("Statistic") |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_stubhead()
  ) |>
  tab_footnote(
    footnote = "h-index is the largest number h such that h publications have at least h citations.",
    locations = cells_stub(rows = "h-index")
  ) |>
  tab_footnote(
    footnote = "i10-index is the number of publications with at least 10 citations.",
    locations = cells_stub(rows = "i10-index")
  ) |>
  tab_options(
    table.width = pct(70),
    table_body.vlines.style = "hidden"
  )

citation_table
