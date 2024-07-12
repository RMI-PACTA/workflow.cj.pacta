suppressPackageStartupMessages({
  library(pacta.portfolio.utils)
  library(pacta.portfolio.allocate)
  library(dplyr)
  library(jsonlite)
})


# ------------------------------------------------------------------------------

cfg_path <- commandArgs(trailingOnly = TRUE)
if (length(cfg_path) == 0 || cfg_path == "") { cfg_path <- "input_dir/default_config.json" }
cfg <- fromJSON(cfg_path)


# quit if there's no relevant PACTA assets -------------------------------------

total_portfolio_path <- file.path(cfg$output_dir, "total_portfolio.rds")
if (file.exists(total_portfolio_path)) {
  total_portfolio <- readRDS(total_portfolio_path)
  quit_if_no_pacta_relevant_data(total_portfolio)
} else {
  warning("This is weird... the `total_portfolio.rds` file does not exist in the `30_Processed_inputs` directory.")
}


# fix parameters ---------------------------------------------------------------

if (cfg$project_code == "GENERAL") {
  language_select <- "EN"
}

portfolio_name <- "fake_portfolio_name"
investor_name <- "fake_investor_name"


# load PACTA results -----------------------------------------------------------

readRDS_or_return_alt_data <- function(filepath, alt_return = NULL) {
  if (file.exists(filepath)) {
    return(readRDS(filepath))
  }
  alt_return
}

add_inv_and_port_names_if_needed <- function(data) {
  if (!inherits(data, "data.frame")) {
    return(data)
  }

  if (!"portfolio_name" %in% names(data)) {
    data <- mutate(data, portfolio_name = .env$portfolio_name, .before = everything())
  }

  if (!"investor_name" %in% names(data)) {
    data <- mutate(data, investor_name = .env$investor_name, .before = everything())
  }

  data
}

audit_file <- readRDS_or_return_alt_data(
  filepath = file.path(cfg$output_dir, "audit_file.rds"),
  alt_return = empty_audit_file()
)
audit_file <- add_inv_and_port_names_if_needed(audit_file)

portfolio_overview <- readRDS_or_return_alt_data(
  filepath = file.path(cfg$output_dir, "overview_portfolio.rds"),
  alt_return = empty_portfolio_overview()
)
portfolio_overview <- add_inv_and_port_names_if_needed(portfolio_overview)

emissions <- readRDS_or_return_alt_data(
  filepath = file.path(cfg$output_dir, "emissions.rds"),
  alt_return = empty_emissions_results()
)
emissions <- add_inv_and_port_names_if_needed(emissions)

total_portfolio <- readRDS_or_return_alt_data(
  filepath = file.path(cfg$output_dir, "total_portfolio.rds"),
  alt_return = empty_portfolio_results()
)
total_portfolio <- add_inv_and_port_names_if_needed(total_portfolio)

equity_results_portfolio <- readRDS_or_return_alt_data(
  filepath = file.path(cfg$output_dir, "Equity_results_portfolio.rds"),
  alt_return = empty_portfolio_results()
)
equity_results_portfolio <- add_inv_and_port_names_if_needed(equity_results_portfolio)

bonds_results_portfolio <- readRDS_or_return_alt_data(
  filepath = file.path(cfg$output_dir, "Bonds_results_portfolio.rds"),
  alt_return = empty_portfolio_results()
)
bonds_results_portfolio <- add_inv_and_port_names_if_needed(bonds_results_portfolio)

equity_results_company <- readRDS_or_return_alt_data(
  filepath = file.path(cfg$output_dir, "Equity_results_company.rds"),
  alt_return = empty_company_results()
)
equity_results_company <- add_inv_and_port_names_if_needed(equity_results_company)

bonds_results_company <- readRDS_or_return_alt_data(
  filepath = file.path(cfg$output_dir, "Bonds_results_company.rds"),
  alt_return = empty_company_results()
)
bonds_results_company <- add_inv_and_port_names_if_needed(bonds_results_company)

equity_results_map <- readRDS_or_return_alt_data(
  filepath = file.path(cfg$output_dir, "Equity_results_map.rds"),
  alt_return = empty_map_results()
)
equity_results_map <- add_inv_and_port_names_if_needed(equity_results_map)

bonds_results_map <- readRDS_or_return_alt_data(
  filepath = file.path(cfg$output_dir, "Bonds_results_map.rds"),
  alt_return = empty_map_results()
)
bonds_results_map <- add_inv_and_port_names_if_needed(bonds_results_map)

peers_equity_results_portfolio <- readRDS_or_return_alt_data(
  filepath = file.path(cfg$output_dir, paste0(cfg$project_code, "_peers_equity_results_portfolio.rds")),
  alt_return = empty_portfolio_results()
)

peers_bonds_results_portfolio <- readRDS_or_return_alt_data(
  filepath = file.path(cfg$output_dir, paste0(cfg$project_code, "_peers_bonds_results_portfolio.rds")),
  alt_return = empty_portfolio_results()
)

peers_equity_results_user <- readRDS_or_return_alt_data(
  filepath = file.path(cfg$output_dir, paste0(cfg$project_code, "_peers_equity_results_portfolio_ind.rds")),
  alt_return = empty_portfolio_results()
)

peers_bonds_results_user <- readRDS_or_return_alt_data(
  filepath = file.path(cfg$output_dir, paste0(cfg$project_code, "_peers_bonds_results_portfolio_ind.rds")),
  alt_return = empty_portfolio_results()
)

indices_equity_results_portfolio <- readRDS(file.path(cfg$data_dir, "Indices_equity_results_portfolio.rds"))

indices_bonds_results_portfolio <- readRDS(file.path(cfg$data_dir, "Indices_bonds_results_portfolio.rds"))


# check for indices ------------------------------------------------------------

if (nrow(indices_equity_results_portfolio) == 1L && all(is.na(indices_equity_results_portfolio))) {
  cli::cli_alert_danger("Since the benchmark equity data is empty, some plots may not work as expected.")
}

if (nrow(indices_bonds_results_portfolio) == 1L && all(is.na(indices_bonds_results_portfolio))) {
  cli::cli_alert_danger("Since the benchmark bonds data is empty, some plots may not work as expected.")
}


# check for peer results -------------------------------------------------------

if (cfg$project_code != "GENERAL") {
  if (nrow(peers_equity_results_user) == 1L && all(is.na(peers_equity_results_user))) {
    cli::cli_alert_danger("Since this is not a \"GENERAL\" report, it's probably necessary to have peer results, but the peer results equity user data is empty.")
  }

  if (nrow(peers_bonds_results_user) == 1L && all(is.na(peers_bonds_results_user))) {
    cli::cli_alert_danger("Since this is not a \"GENERAL\" report, it's probably necessary to have peer results, but the peer results bonds user data is empty.")
  }

  if (nrow(peers_equity_results_portfolio) == 1L && all(is.na(peers_equity_results_portfolio))) {
    cli::cli_alert_danger("Since this is not a \"GENERAL\" report, it's probably necessary to have peer results, but the peer results equity portfolio data is empty.")
  }

  if (nrow(peers_bonds_results_portfolio) == 1L && all(is.na(peers_bonds_results_portfolio))) {
    cli::cli_alert_danger("Since this is not a \"GENERAL\" report, it's probably necessary to have peer results, but the peer results bonds portfolio data is empty.")
  }
}


# check scenario sources -------------------------------------------------------

if (!all(na.omit(unique(equity_results_portfolio$scenario_source)) %in% cfg$scenario_sources_list)) {
  cli::cli_alert_danger("The equity results data contain scenario sources that are not specified in the config. This may lead to unexpected behavior or errors in some plots.")
}

if (!all(na.omit(unique(bonds_results_portfolio$scenario_source)) %in% cfg$scenario_sources_list)) {
  cli::cli_alert_danger("The bonds results data contain scenario sources that are not specified in the config. This may lead to unexpected behavior or errors in some plots.")
}

if (!all(na.omit(unique(indices_equity_results_portfolio$scenario_source)) %in% cfg$scenario_sources_list)) {
  cli::cli_alert_danger("The benchmark equity results data contain scenario sources that are not specified in the config. This may lead to unexpected behavior or errors in some plots.")
}

if (!all(na.omit(unique(indices_bonds_results_portfolio$scenario_source)) %in% cfg$scenario_sources_list)) {
  cli::cli_alert_danger("The benchmark bonds results data contain scenario sources that are not specified in the config. This may lead to unexpected behavior or errors in some plots.")
}

if (cfg$project_code != "GENERAL") {
  if (!all(na.omit(unique(peers_equity_results_user$scenario_source)) %in% cfg$scenario_sources_list)) {
    cli::cli_alert_danger("The peers equity user results data contain scenario sources that are not specified in the config. This may lead to unexpected behavior or errors in some plots.")
  }

  if (!all(na.omit(unique(peers_bonds_results_user$scenario_source)) %in% cfg$scenario_sources_list)) {
    cli::cli_alert_danger("The peers bonds user results data contain scenario sources that are not specified in the config. This may lead to unexpected behavior or errors in some plots.")
  }

  if (!all(na.omit(unique(peers_equity_results_portfolio$scenario_source)) %in% cfg$scenario_sources_list)) {
    cli::cli_alert_danger("The peers equity portfolio results data contain scenario sources that are not specified in the config. This may lead to unexpected behavior or errors in some plots.")
  }

  if (!all(na.omit(unique(peers_bonds_results_portfolio$scenario_source)) %in% cfg$scenario_sources_list)) {
    cli::cli_alert_danger("The peers bonds portfolio results data contain scenario sources that are not specified in the config. This may lead to unexpected behavior or errors in some plots.")
  }
}








# -------------------------------------------------------------------------

currency_exchange_value <- 1
display_currency <- "USD"
pacta_sectors <- c("Power", "Automotive", "Oil&Gas", "Coal", "Steel", "Cement", "Aviation")


# translations -----------------------------------------------------------------

dataframe_translations <- readr::read_csv(
  system.file("extdata/translation/dataframe_labels.csv", package = "pacta.portfolio.report"),
  col_types = readr::cols()
)

header_dictionary <- readr::read_csv(
  system.file("extdata/translation/dataframe_headers.csv", package = "pacta.portfolio.report"),
  col_types = readr::cols()
)

js_translations <- jsonlite::fromJSON(
  txt = system.file("extdata/translation/js_labels.json", package = "pacta.portfolio.report")
)

sector_order <- readr::read_csv(
  system.file("extdata/sector_order/sector_order.csv", package = "pacta.portfolio.report"),
  col_types = readr::cols()
)

dictionary <-
  pacta.portfolio.report:::choose_dictionary_language(
    data = dataframe_translations,
    language = language_select
  )

header_dictionary <- pacta.portfolio.report:::replace_contents(header_dictionary, display_currency)


# -------------------------------------------------------------------------

pacta.portfolio.report:::prep_audit_table(
  audit_file,
  investor_name,
  portfolio_name,
  currency_exchange_value
) %>%
  pacta.portfolio.report:::translate_df_contents("data_included_table", dictionary, inplace = TRUE) %>%
  pacta.portfolio.report:::translate_df_headers("data_included_table", language_select, header_dictionary) %>%
  jsonlite::write_json(path = file.path(cfg$output_dir, "data_audit_table.json"))


# Exhibit 2 - exposure to key sectors by value (exploded pie) --------------

if (nrow(audit_file) > 0) {
  pacta.portfolio.report:::prep_exposure_pie(
    audit_file,
    "Equity",
    investor_name,
    portfolio_name,
    pacta_sectors,
    currency_exchange_value
  ) %>%
    pacta.portfolio.report:::translate_df_contents("data_value_pie_equity", dictionary) %>%
    jsonlite::write_json(path = file.path(cfg$output_dir, "data_value_pie_equity.json"))

  pacta.portfolio.report:::prep_exposure_pie(
    audit_file,
    "Bonds",
    investor_name,
    portfolio_name,
    pacta_sectors,
    currency_exchange_value
  ) %>%
    pacta.portfolio.report:::translate_df_contents("data_value_pie_bonds", dictionary) %>%
    jsonlite::write_json(path = file.path(cfg$output_dir, "data_value_pie_bonds.json"))
}


# Exhibit 3 - exposure to key sectors by emissions (exploded pie) ----------

pacta.portfolio.report:::prep_emissions_pie(
  emissions,
  "Equity",
  investor_name,
  portfolio_name,
  pacta_sectors
) %>%
  pacta.portfolio.report:::translate_df_contents("data_emissions_pie_equity", dictionary) %>%
  jsonlite::write_json(path = file.path(cfg$output_dir, "data_emissions_pie_equity.json"))

pacta.portfolio.report:::prep_emissions_pie(
  emissions,
  "Bonds",
  investor_name,
  portfolio_name,
  pacta_sectors
) %>%
  pacta.portfolio.report:::translate_df_contents("data_emissions_pie_bonds", dictionary) %>%
  jsonlite::write_json(path = file.path(cfg$output_dir, "data_emissions_pie_bonds.json"))


# Exhibit 5 - exposure to sectors and techs (bar charts) -----------------

techexposure_data <-
  pacta.portfolio.report:::prep_techexposure(
    equity_results_portfolio,
    bonds_results_portfolio,
    investor_name,
    portfolio_name,
    indices_equity_results_portfolio,
    indices_bonds_results_portfolio,
    peers_equity_results_portfolio,
    peers_bonds_results_portfolio,
    peer_group,
    select_scenario_other,
    select_scenario,
    start_year,
    green_techs,
    equity_market_levels,
    all_tech_levels
  )

if (nrow(techexposure_data) > 0) {
  techexposure_data %>%
    pacta.portfolio.report:::translate_df_contents("techexposure_data", dictionary) %>%
    jsonlite::write_json(path = file.path(cfg$output_dir, "data_techexposure.json"))
}


# Exhibit 5a - future exposure to sectors and techs (bar charts) ---------

# FIXME: div_id = techexposure_future TODO check

techexposure_future_data <-
  prep_techexposure_future(
    equity_results_portfolio,
    bonds_results_portfolio,
    indices_equity_results_portfolio,
    indices_bonds_results_portfolio,
    peers_equity_results_portfolio,
    peers_bonds_results_portfolio,
    investor_name,
    portfolio_name,
    tech_roadmap_sectors,
    peer_group,
    start_year,
    year_span,
    all_tech_levels
  )

if (nrow(techexposure_future_data) > 0) {
  techexposure_future_data %>%
    translate_df_contents("techexposure_future_data", dictionary) %>%
    export_data_utf8("data_techexposure_future", output_dir = output_dir)
}

# Exhibit 6 - exposure to sectors and techs by location (map) ------------

prep_exposure_map(
  equity_results_map,
  bonds_results_map,
  portfolio_name,
  start_year
) %>%
  translate_df_contents("data_map", dictionary) %>%
  export_data_utf8("data_map", output_dir = output_dir)


# Exhibit 9 - production/scenario alignment (line/area/timelines) --------

data_trajectory_alignment <-
  prep_trajectory_alignment(
    equity_results_portfolio,
    bonds_results_portfolio,
    peers_equity_results_portfolio,
    peers_bonds_results_portfolio,
    indices_equity_results_portfolio,
    indices_bonds_results_portfolio,
    investor_name,
    portfolio_name,
    tech_roadmap_sectors,
    peer_group,
    start_year,
    year_span,
    scen_geo_levels,
    all_tech_levels
  )

if (nrow(data_trajectory_alignment) > 0) {
  data_trajectory_alignment %>%
    translate_df_contents("data_trajectory_alignment", dictionary) %>%
    export_data_utf8("data_trajectory_alignment", output_dir = output_dir)
}


# Exhibit 11 - emissions trajectory (line timeline) ----------------------

data_emissions <-
  prep_emissions_trajectory(
    equity_results_portfolio,
    bonds_results_portfolio,
    investor_name,
    portfolio_name,
    select_scenario_other,
    select_scenario,
    pacta_sectors,
    year_span,
    start_year
  )

if (nrow(data_emissions) > 1) {
  data_emissions %>%
    translate_df_contents("data_emissions", dictionary) %>%
    export_data_utf8("data_emissions", output_dir = output_dir)
}


# Exhibit 13 - company comparisons (bubble scatter) ----------------------

prep_company_bubble(
  equity_results_company,
  bonds_results_company,
  portfolio_name,
  select_scenario_other,
  select_scenario,
  start_year,
  green_techs
) %>%
  translate_df_contents("data_company_bubble", dictionary) %>%
  export_data_utf8("data_company_bubble", output_dir = output_dir)


# Exhibit 13b - company charts -------------------------------------------

prep_key_bars_company(
  equity_results_company,
  bonds_results_company,
  portfolio_name,
  start_year,
  select_scenario,
  select_scenario_other,
  pacta_sectors_not_analysed,
  all_tech_levels
) %>%
  translate_df_contents("data_key_bars_company", dictionary) %>%
  export_data_utf8("data_key_bars_company", output_dir = output_dir)

prep_key_bars_portfolio(
  equity_results_portfolio,
  bonds_results_portfolio,
  portfolio_name,
  select_scenario,
  select_scenario_other,
  start_year,
  pacta_sectors_not_analysed,
  all_tech_levels
) %>%
  translate_df_contents("data_key_bars_portfolio", dictionary) %>%
  export_data_utf8("data_key_bars_portfolio", output_dir = output_dir)


# Exhibit 14 - peer portfolio comparison (stacked bar charts) ------------

prep_peercomparison(
  equity_results_portfolio,
  bonds_results_portfolio,
  peers_equity_results_user,
  peers_bonds_results_user,
  investor_name,
  portfolio_name,
  start_year,
  select_scenario,
  select_scenario_other,
  peer_group
) %>%
  translate_df_contents("data_peercomparison", dictionary) %>%
  export_data_utf8("data_peercomparison", output_dir = output_dir)


# Exhibit 14b - peer current to future production alignment (bubble) -----

prep_peer_bubbles(
  equity_results_portfolio,
  bonds_results_portfolio,
  peers_equity_results_user,
  peers_bonds_results_user,
  investor_name,
  portfolio_name,
  peer_group,
  select_scenario,
  select_scenario_other,
  start_year,
  green_techs,
  portfolio_allocation_method
) %>%
  translate_df_contents("data_peer_bubbles", dictionary) %>%
  export_data_utf8("data_peer_bubbles", output_dir = output_dir)


# Exhibit 14c - peer rank table ------------------------------------------

prep_peer_table(
  equity_results_portfolio,
  bonds_results_portfolio,
  peers_equity_results_user,
  peers_bonds_results_user,
  investor_name,
  portfolio_name,
  portfolio_allocation_method,
  start_year,
  tech_roadmap_sectors,
  peer_group,
  select_scenario,
  select_scenario_other
) %>%
  translate_df_contents("data_peer_table", dictionary, inplace = TRUE) %>%
  translate_df_headers("data_peer_table", language_select, header_dictionary) %>%
  export_data_utf8("data_peer_table", output_dir = output_dir)
