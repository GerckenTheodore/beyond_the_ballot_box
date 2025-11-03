library(minorparties, tidyverse)

# Build tibble of platforms
tibble <- purrr::map_dfr(c("dem", "rep", "minor"), function(party_type) {
  platforms <- list.files(file.path("platforms", party_type))

  purrr::map_dfr(platforms, function(platform) {
    platform_name <- tools::file_path_sans_ext(platform)
    tibble::tibble(
      party = dplyr::case_when(
        party_type == "dem" ~ paste0("Democratic Party ", platform_name),
        party_type == "rep" ~ paste0("Republican Party ", platform_name),
        party_type == "minor" ~ platform_name
      ),
      text = readLines(file.path("Platforms", party_type, platform), encoding = "UTF-8", warn = FALSE),
      minor_party = ifelse(party_type == "minor", TRUE, FALSE)
    )
  })
})

additional_data <- read.csv("platforms/additional_data.csv") |>
  dplyr::mutate(major_party_platforms = purrr::map2(movement_start, movement_end, function(movement_start, movement_end) {
    list(
      list(
        before = paste("Republican Party", movement_start, sep = " "),
        after = paste("Republican Party", movement_end + 4, sep = " "),
        weight = 1
      ),
      list(
        before = paste("Democratic Party", movement_start, sep = " "),
        after = paste("Democratic Party", movement_end + 4, sep = " "),
        weight = 1
      )
    )
  })) |>
  dplyr::select(-movement_start, -movement_end)

tibble <- dplyr::left_join(tibble, additional_data, by = "party")

# Run IScore Analysis
minorparties::install_python(env_name = "iscores")
results <- minorparties::process_platform_emphasis(tibble) |>
  minorparties::process_platform_position() |>
  minorparties::calculate_iscores(confidence_intervals = TRUE) |>
  dplyr::left_join(additional_data, by = "party") |>
  dplyr::select(-major_party_platforms) |>
  saveRDS("calculation_results.rds")
