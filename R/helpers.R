
# Create regression formula based on setup_specs
create_formula <- function(x, y, controls, random_effects, ...) {

  if (controls == "no covariates") controls <- 1

  if (random_effects == "no random effects") {
    paste(y, "~", x, "+", controls)
  } else {
    paste(y, "~", x, "+", controls, "+", random_effects)
  }
}

# run individual specification
run_spec <- function(specs, df, random_effects = random_effects, conf.level, keep.results = FALSE) {

  # dependencies
  require(dplyr)
  require(purrr)

  # if (!is.null(random_effects)) {
  #   mod_formulas <- specs %>%
  #     mutate(formula = pmap(., create_formula)) %>%
  #     tidyr::unnest(formula) %>%
  #     select(formula)
  #
  #   params <- list()
  #   for(i in 1:length(mod_formulas$formula)) {
  #     tmp <- list(formula = mod_formulas$formula[i], data = df)
  #     params[[i]] <- tmp
  #   }
  #
  #   df_in <- tibble(model = unique(specs$model), params = params)
  #
  #   results = df_in %>%
  #     mutate(res = invoke_map(.$model, .$params)) %>%
  #     mutate(coefs = map(res, broom.mixed::tidy, conf.int = TRUE, conf.level = conf.level),
  #            obs = map(res, nobs)) %>%
  #     tidyr::unnest(coefs) %>%
  #     tidyr::unnest(obs) %>%
  #     dplyr::filter(term %in% unique(specs$x)) %>%
  #     dplyr::select(-.data$formula, -.data$term)
  #
  # } else {

    results <- specs %>%
      dplyr::mutate(formula = pmap(specs, create_formula)) %>%
      tidyr::unnest(formula) %>%
      dplyr::mutate(res = map2(.data$model,
                               formula,
                               ~ do.call(.x, list(data = df,
                                                  formula = .y)))) %>%
      dplyr::mutate(coefs = map(.data$res,
                                broom::tidy,
                                conf.int = TRUE,
                                conf.level = conf.level),
                    obs = map(.data$res, nobs)) %>%
      tidyr::unnest(.data$coefs) %>%
      tidyr::unnest(.data$obs) %>%
      dplyr::filter(.data$term == .data$x) %>%
      dplyr::select(-.data$formula, -.data$term)

  # }

  if (isFALSE(keep.results)) {
    results <- results %>%
      select(-res)
  }

  return(results)
}

# create subsets
create_subsets <- function(df, subsets) {

  # dependencies
  require(dplyr)

  subsets %>%
    stack %>%
    purrr::pmap(~ filter(df, get(as.character(..2)) == ..1) %>%
                  mutate(filter = paste(..2, "=", ..1)))
}


format_results <- function(df, null = 0, desc = FALSE) {

  # dependencies
  require(dplyr)
  # rank specs
  if (isFALSE(desc)) {
    df <- df %>%
      arrange(estimate)
  } else {
    df <- df %>%
      arrange(desc(estimate))
  }
  df <- df %>%
    mutate(specifications = 1:n(),
           color = case_when(conf.low > null ~ "black",
                             conf.high < null ~ "black",
                             TRUE ~ "darkgrey"))

  return(df)
}

