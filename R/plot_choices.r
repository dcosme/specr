#' Plot how analytical choices affect results
#'
#' This functions plots how the analytical choices affect the obtained results (i.e., the rank within the curve). Significant results are highlighted. Further customization via ggplot2 is possible.
#'
#' @param df a data frame containing the choices and results of each specification (resulting from \code{run_specs}).
#' @param choices a vector specifying which analytical choices should be plotted. By default, all choices are plotted.
#' @param desc logical value indicating whether the curve should the arranged in a descending order. Defaults to FALSE.
#' @param null Indicate what value represents the null hypothesis (Defaults to zero).
#'
#' @return
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
#' plot_choices(results, choices = c("x", "y", "controls"))
plot_choices <- function(df,
                         choices = c("x", "y", "model", "controls", "subsets"),
                         desc = FALSE,
                         null = 0,
                         rename_controls = FALSE,
                         ignore_vars = FALSE) {

  require(ggplot2, quietly = TRUE)
  require(dplyr, quietly = TRUE)

  df %>%
    format_results(desc = desc, null = null) %>%
    mutate(controls = ifelse(grepl("[+]", controls), "all covariates", controls)) %>%
    tidyr::gather(key, value, choices) %>%
    mutate(key = ifelse(isFALSE(rename_controls) == FALSE & key == "controls", rename_controls, key),
           value = ifelse(isFALSE(ignore_vars) == FALSE & value %in% ignore_vars, NA, value)) %>%
    filter(!is.na(value)) %>%
    mutate(key = factor(key, levels=unique(key))) %>%
    ggplot(aes(x = specifications,
               y = value,
               color = color)) +
    geom_point(aes(x = specifications,
                   y = value),
               shape = 124,
               size = 3.35) +
    scale_color_identity() +
    theme_minimal() +
    facet_grid(key~1, scales = "free_y", space = "free_y") +
    theme(
          axis.line = element_line("black", size = .5),
          legend.position = "none",
          panel.spacing = unit(.75, "lines"),
          axis.text = element_text(colour = "black"),
          strip.text.x = element_blank()) +
    labs(x = "", y = "")

}
