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
  }))
additional_data_slim <- additional_data |>
  dplyr::select(-movement_start, -movement_end)
data <- dplyr::left_join(data, additional_data_slim, by = "party")
saveRDS(data, file.path("data", "data.rds"))

# Calculate IScores
minorparties::install_python(env_name = "iscores")
classification_results <- minorparties::process_platform_emphasis(data)
position_results <- minorparties::process_platform_position(classification_results)
calculation_results <- position_results |>
  minorparties::calculate_iscores(confidence_intervals = TRUE, calculation_tables = TRUE) |>
  dplyr::left_join(additional_data_slim, by = "party") |>
  dplyr::select(-major_party_platforms)
saveRDS(classification_results, file.path("data", "classification_results.rds"))
saveRDS(position_results, file.path("data", "position_results.rds"))
saveRDS(calculation_results, file.path("data", "calculation_results.rds"))

# Calculate Scores For Perot, Considering Campaigns Separately
perot_row_1 <- position_results |>
  dplyr::filter(party == "Perot 1992") |>
  dplyr::mutate(major_party_platforms = list(
    list(
      list(before = paste("Republican Party 1992"), after = paste("Republican Party 1996"), weight = 1),
      list(before = paste("Democratic Party 1992"), after = paste("Democratic Party 1996"), weight = 1)
    )
  ))
perot_row_2 <- position_results |>
  dplyr::filter(party == "Perot 1992") |>
  dplyr::mutate(major_party_platforms = list(
    list(
      list(before = paste("Republican Party 1996"), after = paste("Republican Party 2000"), weight = 1),
      list(before = paste("Democratic Party 1996"), after = paste("Democratic Party 2000"), weight = 1)
    )
  ), party = "Perot 1996")
major_rows <- position_results |>
  dplyr::filter(stringr::str_detect(party, "(?=.*Democratic|Republican)(?=.*(1992|1996|2000))"))
perot_scores_seperate <- dplyr::bind_rows(perot_row_1, perot_row_2, major_rows) |>
  minorparties::calculate_iscores(confidence_intervals = TRUE)
saveRDS(perot_scores_seperate, file.path("data", "perot_scores_seperate.rds"))

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

# Build Tables

## Table Styling
style_table <- function(table) {
  table |>
    gt::fmt_missing(columns = gt::everything(), missing_text = "") |>
    gt::cols_align("left") |>
    gt::tab_options(
      table.font.names = "Times New Roman",
      heading.align = "left",
      column_labels.font.weight = "bold",
      row_group.font.weight = "bold",
      footnotes.marks = "standard"
    ) |>
    gt::opt_row_striping(TRUE)
}
style_iscore_calc_table <- function(table) {
  table |>
    gt::fmt_number(columns = c(`1992`, `2000`, change), decimals = 1) |>
    gt::fmt_scientific(columns = significance, decimals = 1) |>
    gt::cols_label(issue = "Issue", `1992` = "1992", `2000` = "2000", change = "Change", significance = "Significance") |>
    gt::cols_hide(columns = significance_codes) |>
    gt::tab_footnote(
      footnote = "Significance codes:  0 ‘***’ 0.01 ‘**’ 0.05 ‘*’ 0.1 ‘ ’ 1",
      locations = gt::cells_column_labels(columns = significance)
    )
}

## Table 1: Ie-Score
table_1_perot <- classification_results |>
  dplyr::filter(party == "Perot 1992") |>
  purrr::pluck("overall_emphasis_scores", 1) |>
  dplyr::filter(score > 0.05) |>
  dplyr::mutate(party = "Ross Perot") |>
  dplyr::rename(`1992` = score)
perot_top_issues <- table_1_perot$issue
table_1 <- classification_results |>
  dplyr::filter(stringr::str_detect(party, "(?=.*Democratic|Republican)(?=.*(1992|2000))")) |>
  tidyr::separate(party, into = c("party", "year"), sep = " (?=\\d{4}$)") |>
  dplyr::select(party, year, overall_emphasis_scores) |>
  tidyr::unnest(overall_emphasis_scores) |>
  dplyr::filter(issue %in% perot_top_issues) |>
  tidyr::pivot_wider(names_from = year, values_from = score) |>
  dplyr::mutate(change = `2000` - `1992`)
ie_calculation_table <- calculation_results |>
  dplyr::filter(party == "Perot 1992") |>
  purrr::pluck("calculation_tables", 1, 1)
ie_party_nums <- ie_calculation_table |>
  dplyr::filter(name == "before") |>
  dplyr::select(party, party_number) |>
  dplyr::mutate(party = stringr::str_extract(party, "Democratic Party|Republican Party"))
ie_significance_col <- ie_calculation_table |>
  dplyr::filter(name == "significance") |>
  dplyr::mutate(party = ie_party_nums$party[match(party_number, ie_party_nums$party_number)]) |>
  tidyr::pivot_longer(cols = all_of(perot_top_issues), names_to = "issue", values_to = "significance") |>
  dplyr::select(party, issue, significance)
table_1 <- dplyr::left_join(table_1, ie_significance_col, by = c("party", "issue")) |>
  dplyr::bind_rows(table_1_perot) |>
  dplyr::arrange(factor(issue, levels = perot_top_issues)) |>
  dplyr::arrange(factor(party, levels = c("Ross Perot", "Republican Party", "Democratic Party"))) |>
  dplyr::mutate(significance_codes = dplyr::case_when(significance < 0.01 ~ "***", significance < 0.05 ~ "**", significance < 0.1 ~ "*", TRUE ~ ""))

table_1_styled <- table_1 |>
  gt::gt(groupname_col = "party", rowname_col = "issue") |>
  gt::tab_header(title = md("**Table 1: Change In The Emphasis Of Perot’s Top Issues In Major Party Platforms Over The Extended Ross Perot Movement**")) |>
  gt::text_transform(
    locations = cells_body(columns = significance),
    fn = function(x) {
      paste0(x, " ", table_2$significance_codes)
    }
  ) |>
  gt::tab_footnote(
    footnote = "The numbers in this table are measured in percentage points.",
    locations = gt::cells_column_labels(columns = `1992`)
  ) |>
  gt::tab_footnote(
    footnote = "All figures in this paper were generated from whole, unrounded data, so derivative statistics, such as change, may appear to be off by +/-0.1.",
    locations = gt::cells_column_labels(columns = change)
  ) |>
  style_table() |>
  style_iscore_calc_table() |>
  gt::fmt_number(columns = c(`1992`, `2000`, change), decimals = 1, scale_by = 100)
gt::gtsave(table_1_styled, file.path("tables", "table1.png"))

## Table 2: Ip-Score
table_2_perot <- position_results |>
  dplyr::filter(party == "Perot 1992") |>
  purrr::pluck("position_scores", 1) |>
  dplyr::filter(issue %in% perot_top_issues) |>
  dplyr::mutate(party = "Ross Perot", significance_codes = "") |>
  dplyr::rename(`1992` = score) |>
  dplyr::arrange(factor(issue, levels = perot_top_issues)) |>
  dplyr::select(issue, `1992`, party)
table_2 <- position_results |>
  dplyr::filter(stringr::str_detect(party, "(?=.*Democratic|Republican)(?=.*(1992|2000))")) |>
  tidyr::separate(party, into = c("party", "year"), sep = " (?=\\d{4}$)") |>
  tidyr::unnest(position_scores) |>
  dplyr::filter(issue %in% perot_top_issues) |>
  dplyr::select(party, year, issue, score) |>
  tidyr::pivot_wider(names_from = year, values_from = score)
ip_calculation_table <- calculation_results |>
  dplyr::filter(party == "Perot 1992") |>
  purrr::pluck("calculation_tables", 1, 2)
ip_party_nums <- ip_calculation_table |>
  dplyr::filter(name == "before") |>
  dplyr::select(party, party_number) |>
  dplyr::mutate(party = stringr::str_extract(party, "Democratic Party|Republican Party"))
ip_significance_col <- ip_calculation_table |>
  dplyr::filter(stringr::str_detect(name, "significance|change")) |>
  dplyr::mutate(party = ip_party_nums$party[match(party_number, ip_party_nums$party_number)]) |>
  tidyr::pivot_longer(cols = all_of(perot_top_issues), names_to = "issue", values_to = "value") |>
  tidyr::pivot_wider(names_from = name, values_from = value) |>
  dplyr::select(party, issue, change, significance)
table_2 <- dplyr::left_join(table_2, ip_significance_col, by = c("party", "issue")) |>
  dplyr::bind_rows(table_2_perot) |>
  dplyr::arrange(factor(issue, levels = perot_top_issues)) |>
  dplyr::arrange(factor(party, levels = c("Ross Perot", "Republican Party", "Democratic Party"))) |>
  dplyr::mutate(significance_codes = dplyr::case_when(significance < 0.01 ~ "***", significance < 0.05 ~ "**", significance < 0.1 ~ "*", TRUE ~ ""))

table_2_styled <- table_2 |>
  gt::gt(groupname_col = "party", rowname_col = "issue") |>
  gt::tab_header(title = md("**Table 2: Change In The Position Taken By Major Party Platforms On Perot’s Top Issues Over The Extended Ross Perot Movement**")) |>
  gt::text_transform(
    locations = cells_body(columns = significance),
    fn = function(x) {
      paste0(x, " ", table_2$significance_codes)
    }
  ) |>
  gt::tab_footnote(
    footnote = "Because 'Change' is the in distance from Perot's position, 'Change' is not necessarily equal to the difference between 2000 and 1992 scores.",
    locations = gt::cells_column_labels(columns = change)
  ) |>
  style_table() |>
  style_iscore_calc_table()
gt::gtsave(table_2_styled, file.path("tables", "table2.png"))

## Table 3: General Results
table_3 <- calculation_results |>
  tidyr::unnest_wider(scores) |>
  dplyr::select(party, ie_score_interpreted, ip_score) |>
  dplyr::left_join(additional_data, by = "party") |>
  dplyr::mutate(election = purrr::map2_chr(movement_start, movement_end, function(movement_start, movement_end) {
    if (movement_start == movement_end) {
      as.character(movement_start)
    } else {
      paste0(movement_start, "-", movement_end)
    }
  }), party = stringr::str_extract(party, "^\\S+")) |>
  dplyr::arrange(desc(movement_start)) |>
  dplyr::select(election, party, vscore, sscore, ie_score_interpreted, ip_score)

table_3_styled <- table_3 |>
  gt::gt() |>
  gt::tab_header(title = md("**Table 3: I-Scores For Post-World War II American Minor Party Candidates**")) |>
  gt::cols_label(election = "Election(s)", party = "Party/Candidate Name", vscore = "Vote Share", sscore = "Seats", ie_score_interpreted = "Ie-Score (Interpreted)", ip_score = "Ip-Score") |>
  gt::fmt_number(columns = c(sscore), decimals = 0) |>
  gt::fmt_number(columns = c(ie_score_interpreted, ip_score), decimals = 2) |>
  gt::tab_footnote(
    footnote = "Each election year’s major party platforms were sourced from the American Presidency Project. 'Party Platforms | The American Presidency Project,' accessed August 8, 2025, https://www.presidency.ucsb.edu/documents/app-categories/elections-and-transitions/party-platforms.",
    locations = gt::cells_title(groups = "title")
  ) |>
  gt::tab_footnote(
    footnote = "Some independent candidates, such as Evan McMullin, did not have a formal platform; therefore, this study used an alternative document that intended to outline the campaign’s positions on a wide range of issues.",
    locations = gt::cells_title(groups = "title")
  ) |>
  gt::tab_footnote(
    footnote = "Vote share is measured in percentage points.",
    locations = gt::cells_column_labels(columns = vscore)
  ) |>
  gt::tab_footnote(
    footnote = "Vote share and seats won were sourced from the official FEC Federal Election Results packets for elections after 1976 and entries in Dave Leip's Atlas of U.S. Presidential Elections for elections before that date. 'Election Results and Voting Information,' FEC.Gov, accessed August 8, 2025, https://www.fec.gov/introduction-campaign-finance/election-results-and-voting-information/. Dave Leip, 'Dave Leip’s Atlas of U.S. Presidential Elections,' accessed August 8, 2025, https://uselectionatlas.org/.",
    locations = gt::cells_column_labels(columns = vscore)
  ) |>
  gt::tab_footnote(
    footnote = "'2016 Green Party Platform.Pdf,' Ballotpedia, April 2020, https://ballotpedia.org/File:2016_Green_Party_Platform.pdf.",
    locations = gt::cells_body(columns = party, rows = `election` == "2016" & party == "Green")
  ) |>
  gt::tab_footnote(
    footnote = "'On The Issues,' Evan McMullin for President, 2016, https://web.archive.org/web/20161107080610/https://www.evanmcmullin.com/issues.",
    locations = gt::cells_body(columns = party, rows = `election` == "2016" & party == "McMullin")
  ) |>
  gt::tab_footnote(
    footnote = "'National Platform 2016,' LPedia, May 2016, https://lpedia.org/wiki/Document:National_Platform_2016.",
    locations = gt::cells_body(columns = party, rows = `election` == "2012-2020" & party == "Libertarian")
  ) |>
  gt::tab_footnote(
    footnote = "'Political Issues That Matter,' Vote Nader, 2008, https://www.votenader.org/issues/.",
    locations = gt::cells_body(columns = party, rows = `election` == "2008" & party == "Nader")
  ) |>
  gt::tab_footnote(
    footnote = "'2000 Platform,' *Green Party of the United States*, n.d., accessed August 8, 2025, https://gpus.org/committees/platform/2000-platform/.",
    locations = gt::cells_body(columns = party, rows = `election` == "1996-2000" & party == "Green")
  ) |>
  gt::tab_footnote(
    footnote = "'National Platform 1996,' July 1996, https://lpedia.org/wiki/Document:National_Platform_1996.",
    locations = gt::cells_body(columns = party, rows = `election` == "1996" & party == "Libertarian")
  ) |>
  gt::tab_footnote(
    footnote = "Perot, *United We Stand*.",
    locations = gt::cells_body(columns = party, rows = `election` == "1992-1996" & party == "Perot")
  ) |>
  gt::tab_footnote(
    footnote = "The Anderson campaign released two platforms in 1980: 'Rebuilding a Society That Works: An Agenda for America' and 'The Program of the Anderson/Lucey National Unity Campaign.' This study uses the former because it is the document designed to be 'relatively short and deliberately selective.' This quality is likely to make the agenda more revealing of the campaign’s priorities than the more comprehensive program because its space constraint almost certainly forced Anderson to prioritize what issues he covered and at what length. Clifford W. Brown Jr and Robert J. Walker, *A Campaign of Ideas: The 1980 Anderson/Lucey Platform* (Praeger, 1984).",
    locations = gt::cells_body(columns = party, rows = `election` == "1980" & party == "Anderson")
  ) |>
  gt::tab_footnote(
    footnote = "'National Platform 1980,' LPedia, 1980, https://lpedia.org/wiki/Document:National_Platform_1980",
    locations = gt::cells_body(columns = party, rows = `election` == "1980" & party == "Libertarian")
  ) |>
  gt::tab_footnote(
    footnote = "'The Spirit of Independence,' 1976.",
    locations = gt::cells_body(columns = party, rows = `election` == "1976" & party == "McCarthy")
  ) |>
  gt::tab_footnote(
    footnote = "'American Independent Party Platform of 1968,' October 13, 1968, https://www.presidency.ucsb.edu/documents/american-independent-party-platform-1968.",
    locations = gt::cells_body(columns = party, rows = `election` == "1968-1972" & party == "Independent")
  ) |>
  gt::tab_footnote(
    footnote = "'Platform of the States Rights Democratic Party,' The American Presidency Project, August 14, 1948, https://www.presidency.ucsb.edu/documents/platform-the-states-rights-democratic-party.",
    locations = gt::cells_body(columns = party, rows = `election` == "1948" & party == "Thurmond")
  ) |>
  gt::tab_footnote(
    footnote = "'Progressive Party Platform of 1948,' The American Presidency Project, July 23, 1948, https://www.presidency.ucsb.edu/documents/progressive-party-platform-1948.",
    locations = gt::cells_body(columns = party, rows = `election` == "1948" & party == "Wallace")
  ) |>
  gt::tab_footnote(
    footnote = "Leo Isacson won a special election in 1948 after receiving Henry Wallace’s endorsement. He was, however, elected as a member of the American Labor Party. He is counted here due to the considerable overlap between labor movements in the 1940s, but removing him from the Progressive party’s total will not change the eventual conclusion that the number of legislative seats a party wins is wholly uncorrelated with more robust measures of minor party success. In fact, removing Isacson will lower the correlation coefficient, by definition, given the lack of any other Congresspeople representing minor parties listed here, to 0.",
    locations = gt::cells_body(columns = sscore, rows = `election` == "1948" & party == "Wallace")
  ) |>
  style_table() |>
  gt::tab_options(footnotes.marks = "letters")
gt::gtsave(table_3_styled, file.path("tables", "table3.png"))

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
