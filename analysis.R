set.seed(1992)

# Compile Data

## Build tibble of platforms
data <- purrr::map_dfr(c("dem", "rep", "minor"), function(party_type) {
  platforms <- list.files(file.path("platforms", party_type))

  purrr::map_dfr(platforms, function(platform) {
    platform_name <- tools::file_path_sans_ext(platform)
    tibble::tibble(
      party = dplyr::case_when(
        party_type == "dem" ~ paste0("Democratic Party ", platform_name),
        party_type == "rep" ~ paste0("Republican Party ", platform_name),
        party_type == "minor" ~ platform_name
      ),
      text = readLines(file.path("platforms", party_type, platform), encoding = "UTF-8", warn = FALSE),
      minor_party = ifelse(party_type == "minor", TRUE, FALSE)
    )
  })
})

## Add additional data on minor parties and correctly format it for IScore analysis
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

data <- dplyr::left_join(data, additional_data, by = "party")
saveRDS(data, file.path("data", "data.rds"))

# Calculate IScores
minorparties::install_python(env_name = "iscores")
classification_results <- minorparties::process_platform_emphasis(data)
position_results <- minorparties::process_platform_position(classification_results)
calculation_results <- position_results |>
  minorparties::calculate_iscores(confidence_intervals = TRUE, calculation_tables = TRUE) |>
  dplyr::left_join(additional_data, by = "party") |>
  dplyr::select(-major_party_platforms)
saveRDS(classification_results, file.path("data", "classification_results.rds"))
saveRDS(position_results, file.path("data", "position_results.rds"))
saveRDS(calculation_results, file.path("data", "calculation_results.rds"))

# Build Graphs

## Figure 1: IScores
fig_1 <- calculation_results |>
  tidyr::unnest_wider(scores) |>
  ggplot2::ggplot(ggplot2::aes(x = ie_score_interpreted, y = ip_score, label = party)) +
  ggplot2::geom_point() +
  ggplot2::coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
  ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.5, alpha = 0.3) +
  ggplot2::geom_vline(xintercept = 0, color = "black", linewidth = 0.5, alpha = 0.3) +
  ggplot2::labs(
    title = "Figure 1: I-Scores of Post-World War II American Minor Party Candidates",
    x = "Interpreted Ie (Issue Emphasis) Score",
    y = "Ip (Issue Position) Score"
  ) +
  ggrepel::geom_text_repel() +
  ggplot2::theme_bw()
ggplot2::ggsave(plot = fig_1, filename = file.path("plots", "fig1.png"), width = 10, height = 10, dpi = 300)

## Figure 2: Confidence Intervals
fig_2_tibble <- calculation_results |>
  dplyr::mutate(
    ie = purrr::map_dbl(scores, "ie_score_interpreted"),
    ie_low = purrr::map_dbl(confidence_intervals, function(tibble) tibble$ie_score_interpreted[tibble$side == "lower"]),
    ie_high = purrr::map_dbl(confidence_intervals, function(tibble) tibble$ie_score_interpreted[tibble$side == "upper"]),
    ip = purrr::map_dbl(scores, "ip_score"),
    ip_low = purrr::map_dbl(confidence_intervals, function(tibble) tibble$ip_score[tibble$side == "lower"]),
    ip_high = purrr::map_dbl(confidence_intervals, function(tibble) tibble$ip_score[tibble$side == "upper"])
  )
fig_2_tibble <- dplyr::bind_rows(
  dplyr::transmute(fig_2_tibble, party, metric = "Interpreted Ie (Issue Emphasis) Score", est = ie, low = ie_low, high = ie_high),
  dplyr::transmute(fig_2_tibble, party, metric = "Ip (Issue Position) Score", est = ip, low = ip_low, high = ip_high)
)
fig_2 <- ggplot2::ggplot(fig_2_tibble, ggplot2::aes(y = party, x = est)) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = low, xmax = high), height = 0.2) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
  ggplot2::facet_wrap(~metric, ncol = 2, scales = "fixed") +
  ggplot2::labs(
    title = "Figure 2: 95% Confidence Intervals of I-Scores of Post-World War II American Minor Party Candidates",
    x = "Score",
    y = NULL
  ) +
  ggplot2::theme_bw()
ggplot2::ggsave(plot = fig_2, filename = file.path("plots", "fig2.png"), width = 10, height = 10, dpi = 300)

# Calculate Correlations
correlation_table <- calculation_results |>
  tidyr::unnest_wider(scores) |>
  dplyr::select(ie_score_interpreted, ip_score, vscore, sscore) |>
  dplyr::mutate(higher_i_score = pmax(ie_score_interpreted, ip_score))
correlation_vars <- colnames(correlation_table)
correlation_table <- expand.grid(var1 = correlation_vars, var2 = correlation_vars, stringsAsFactors = FALSE) |>
  tibble::as_tibble() |>
  dplyr::filter(var1 > var2) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    test = list(cor.test(
      correlation_table[[var1]],
      correlation_table[[var2]],
      use = "pairwise.complete.obs"
    )),
    correlation = test$estimate,
    p_value = test$p.value
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-test)
saveRDS(correlation_table, file.path("data", "correlation_table.rds"))

# Check Robustness

## Recalculate results for different combinations of thresholds
inclusion_thresholds <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
core_thresholds <- c(0.01, 0.025, 0.05, 0.1)
p_thresholds <- c(0.1, 0.05, 0.01, 0.001)

processed_results <- purrr::map(inclusion_thresholds, function(inclusion_threshold) {
  minorparties::process_platform_position(classification_results, inclusion_threshold)
}) |>
  purrr::set_names(as.character(inclusion_thresholds))
threshold_combo_results <- expand.grid(inclusion_thresholds = inclusion_thresholds, core_thresholds = core_thresholds, p_thresholds = p_thresholds) |>
  tibble::as_tibble() |>
  dplyr::mutate(results = purrr::pmap(list(inclusion_thresholds, core_thresholds, p_thresholds), function(inclusion_threshold, core_threshold, p_threshold) {
    minorparties::calculate_iscores(processed_results[[as.character(inclusion_threshold)]], p_threshold, core_threshold)
  })) |>
  tidyr::unnest(results) |>
  tidyr::unnest_wider(scores)
saveRDS(threshold_combo_results, file.path("data", "threshold_combo_results.rds"))

## Calculate correlations between thresholds and scores
robustness_correlations <- expand.grid(var1 = c("inclusion_thresholds", "core_thresholds", "p_thresholds"), var2 = c("ie_score_interpreted", "ip_score"), stringsAsFactors = FALSE) |>
  tibble::as_tibble() |>
  dplyr::rowwise() |>
  dplyr::mutate(
    test = list(cor.test(
      threshold_combo_results[[var1]],
      threshold_combo_results[[var2]],
      use = "pairwise.complete.obs"
    )),
    correlation = test$estimate,
    p_value = test$p.value
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-test)
saveRDS(robustness_correlations, file.path("data", "robustness_correlations.rds"))

## Plot relationships
purrr::pwalk(robustness_correlations, function(var1, var2, correlation, p_value) {
  plot <- ggplot2::ggplot(threshold_combo_results, ggplot2::aes(x = .data[[var1]], y = .data[[var2]])) +
    ggplot2::geom_point() +
    ggplot2::labs(x = var1, y = var2, title = paste0("Correlation between ", var1, " and ", var2, ".")) +
    ggplot2::theme_bw()
  ggplot2::ggsave(plot = plot, filename = file.path("plots", "robustness", paste0(var1, "_", var2, ".png")), width = 10, height = 10, dpi = 300)
})

## Calculate proportion of variance explained by thresholds
ip_variance <- aov(ip_score ~ inclusion_thresholds + core_thresholds + p_thresholds + party, data = threshold_combo_results) |>
  eta_squared()
ie_variance <- aov(ie_score_interpreted ~ inclusion_thresholds + core_thresholds + p_thresholds + party, data = threshold_combo_results) |>
  eta_squared()
saveRDS(ip_variance, file.path("data", "ip_variance.rds"))
saveRDS(ie_variance, file.path("data", "ie_variance.rds"))

# Causality Example

## Pull position scores on the "Economic Orthodoxy" issue-area over time
perot_issue_score <- position_results |>
  dplyr::filter(party == "Perot 1992") |>
  purrr::pluck("position_scores", 1) |>
  dplyr::filter(issue == "Economic Orthodoxy") |>
  purrr::pluck("score")
major_issue_scores <- position_results |>
  dplyr::filter(stringr::str_detect(party, "Democratic|Republican")) |>
  dplyr::mutate(distance_from_perot = purrr::map_dbl(position_scores, function(scores) {
    score <- scores |>
      dplyr::filter(issue == "Economic Orthodoxy") |>
      purrr::pluck("score")
    abs(score - perot_issue_score)
  })) |>
  dplyr::select(party, distance_from_perot) |>
  tidyr::separate(party, into = c("party", "year"), sep = "\\s(?=\\d{4})") |>
  dplyr::mutate(year = as.integer(year))

## Figure 3: Causality Demonstration
fig_3 <- ggplot2::ggplot(major_issue_scores, ggplot2::aes(x = year, y = distance_from_perot, linetype = party)) +
  ggplot2::geom_line() +
  ggplot2::scale_linetype_manual(values = c("Democratic Party" = "solid", "Republican Party" = "dashed")) +
  ggplot2::annotate("rect", xmin = 1992, xmax = 2000, ymin = -Inf, ymax = Inf, fill = "#000000", alpha = 0.1) +
  ggplot2::annotate("text", x = 1996, y = max(major_issue_scores$distance_from_perot) + 0.2, label = "Extended Ross\nPerot Movement", color = "#000000", hjust = 0.5, vjust = 1.5) +
  ggplot2::labs(title = paste0("Figure 3: Distance Between Major Party Platforms' and Ross Perot's Position Scores on the\n'Economic Orthodoxy' Issue-Area Over Time"), x = "Year", y = "Distance from Perot's Position", linetype = "Party") +
  ggplot2::theme_bw()
ggplot2::ggsave(plot = fig_3, filename = file.path("plots", "fig3.png"), width = 10, height = 10, dpi = 300)

## Conduct an interrupted time series analysis
itsa <- dplyr::mutate(major_issue_scores, year = year - 1992, after_perot = ifelse(year >= 0, 1, 0), years_since_perot = ifelse(year >= 0, year, 0))
itsa <- lm(distance_from_perot ~ party * (year + after_perot + years_since_perot), data = itsa)
saveRDS(itsa, file.path("data", "itsa.rds"))
