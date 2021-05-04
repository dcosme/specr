#' Plot how analytical choices affect results
#'
#' This functions plots how analytical choices affect the obtained results (i.e., the rank within the curve). Significant results are highlighted (negative = red, positive = blue, grey = nonsignificant). This functions creates the lower panel in \code{plot_specs()}.
#'
#' @param df a data frame resulting from \code{run_specs()}.
#' @param var which variable should be evaluated? Defaults to estimate (the effect sizes computed by [run_specs()]).
#' @param choices a vector specifying which analytical choices should be plotted. By default, all choices are plotted.
#' @param desc logical value indicating whether the curve should the arranged in a descending order. Defaults to FALSE.
#' @param null Indicate what value represents the 'null' hypothesis (Defaults to zero).
#'
#' @return a \link[ggplot2]{ggplot} object.
#' @export
#'
#' @examples
#' # Run specification curve analysis
#' results <- run_specs(df = example_data,
#'                      y = c("y1", "y2"),
#'                      x = c("x1", "x2"),
#'                      model = c("lm"),
#'                      controls = c("c1", "c2"),
#'                      subsets = list(group1 = unique(example_data$group1),
#'                                     group2 = unique(example_data$group2)))
#'
#' # Plot simple table of choices
#' plot_choices(results)
#'
#' # Plot only specific choices
#' plot_choices(results,
#'              choices = c("x", "y", "controls"))
plot_choices <- function(df,
                         var = .data$estimate,
                         choices = c("x", "y", "model", "controls", "subsets"),
                         desc = FALSE,
                         null = 0,
                         size = 3.35,
                         alpha_values = c(.25, 1),
                         color_vars = NULL,
                         palette = NULL,
                         rename_controls = FALSE,
                         ignore_vars = FALSE) {

  value <- key <- NULL

  var <- enquo(var)

  if (!is.null(color_vars)) {
    if (color_vars == "x") {

      color_num_key = df %>%
        select(x) %>%
        unique() %>%
        arrange(x) %>%
        mutate(color_num = row_number())

      data_df = df %>%
        format_results(var = var, null = null, desc = desc) %>%
        left_join(., color_num_key) %>%
        mutate(controls = ifelse(grepl("[+]", controls), "all covariates", controls),
               alpha = ifelse(color == "black", "yes", "no"),
               color = sprintf("%s", eval(parse(text = "palette[color_num]")))) %>%
        tidyr::gather(key, value, choices) %>%
        mutate(key = ifelse(isFALSE(rename_controls) == FALSE & key == "controls", rename_controls, key),
               value = ifelse(isFALSE(ignore_vars) == FALSE & value %in% ignore_vars, NA, value)) %>%
        filter(!is.na(value)) %>%
        mutate(key = factor(key, levels=unique(key)))
    }

    if (color_vars == "y") {

      color_num_key = df %>%
        select(y) %>%
        unique() %>%
        arrange(y) %>%
        mutate(color_num = row_number())

      data_df = df %>%
        format_results(var = var, null = null, desc = desc) %>%
        left_join(., color_num_key) %>%
        mutate(controls = ifelse(grepl("[+]", controls), "all covariates", controls),
               alpha = ifelse(color == "black", "yes", "no"),
               color = sprintf("%s", eval(parse(text = "palette[color_num]")))) %>%
        tidyr::gather(key, value, choices) %>%
        mutate(key = ifelse(isFALSE(rename_controls) == FALSE & key == "controls", rename_controls, key),
               value = ifelse(isFALSE(ignore_vars) == FALSE & value %in% ignore_vars, NA, value)) %>%
        filter(!is.na(value)) %>%
        mutate(key = factor(key, levels=unique(key)))
    }
  } else {

    data_df = df %>%
      format_results(var = var, null = null, desc = desc) %>%
      mutate(controls = ifelse(grepl("[+]", controls), "all covariates", controls)) %>%
      tidyr::gather(key, value, choices) %>%
      mutate(key = ifelse(isFALSE(rename_controls) == FALSE & key == "controls", rename_controls, key),
             value = ifelse(isFALSE(ignore_vars) == FALSE & value %in% ignore_vars, NA, value),
             alpha = "yes") %>%
      filter(!is.na(value)) %>%
      mutate(key = factor(key, levels=unique(key)))
  }

  data_df %>%
    ggplot(aes(x = .data$specifications,
               y = .data$value,
               color = .data$color)) +
    geom_point(aes(x = .data$specifications,
                   y = .data$value,
                   alpha = alpha),
               shape = 124,
               size = size) +
    scale_color_identity() +
    scale_alpha_manual(values = alpha_values) +
    theme_minimal() +
    facet_grid(.data$key~1, scales = "free_y", space = "free_y") +
    theme(
      axis.line = element_line("black", size = .5),
      legend.position = "none",
      panel.spacing = unit(.75, "lines"),
      axis.text = element_text(colour = "black"),
      strip.text.x = element_blank()) +
    labs(x = "", y = "")

}
