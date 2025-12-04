# important variables plot

source("R/00_libraries.R")
library(tidytext)

final_models <- "outputs/model_objects/wfs_best_mape_pi.rds"

model_types <- readRDS(final_models) |>
  map(
    ~ pluck(.x, "wf", "pre", "actions", "recipe", "recipe", "template")
  ) |>
  list_rbind(
    names_to = "performance_metric"
  ) |>
  # filter(!grepl("52", performance_metric)) |>
  select(
    !any_of(c("total_cases", "month", "quarter", "year", "nhs_region", "org"))
  ) |>
  summarise(
    across(everything(), ~ sum(.x < 0, na.rm = TRUE)),
    .by = performance_metric
  ) |>
  pivot_longer(
    cols = !("performance_metric"),
    names_to = "metric",
    values_to = "number_negative"
  ) |>
  summarise(
    type = sum(number_negative),
    .by = performance_metric
  ) |>
  mutate(
    type = case_when(
      type > 0 ~ "change",
      .default = "proportion"
    )
  )


plot_perm_imp <- function(model_types, type_filter, final_models) {
  p <- readRDS(final_models) |>
    map(
      ~ pluck(.x, "perm_imp")
    ) |>
    list_rbind(
      names_to = "performance_metric"
    ) |>
    arrange(
      performance_metric,
      desc(Importance)
    ) |>
    slice_head(
      n = 5,
      by = performance_metric
    ) |>
    left_join(
      model_types,
      by = join_by(
        performance_metric
      ),
      relationship = "many-to-one"
    ) |>
    filter(
      type %in% type_filter
    ) |>
    mutate(
      Variable = stringr::str_wrap(Variable, 45),
      # performance_metric = stringr::str_wrap(performance_metric, 30)
    ) |>
    mutate(
      performance_metric = case_when(
        performance_metric ==
          "Proportion of incomplete pathways greater than 52 weeks from referral" ~ "RTT52 metric",
        performance_metric ==
          "Proportion of suspected cancer or referral to first definitive treatment that are longer than 62 days wait" ~ "Cancer metric",
        performance_metric ==
          "Proportion of A&E attendances with greater than 4 hours wait (Type 1 Departments - Major A&E)" ~ "A&E metric",
        performance_metric ==
          "Proportion of attended GP appointments (over 4 weeks wait time)" ~ "GP metric",
        performance_metric ==
          "Proportion of incomplete pathways greater than 18 weeks from referral" ~ "RTT18 metric",
        .default = NA_character_
      )
    ) |>
    ggplot(
      aes(
        x = Importance,
        y = tidytext::reorder_within(
          Variable,
          Importance,
          performance_metric
        )
      )
    ) +
    geom_col(
      fill = "gray75",
      colour = "black"
    ) +
    geom_errorbarh(
      aes(
        xmin = Importance - StDev,
        xmax = Importance + StDev
      ),
      height = 0.2
    ) +
    facet_wrap(
      facets = vars(performance_metric),
      scales = "free",
      ncol = 2
    ) +
    scale_y_reordered() +
    theme_bw() +
    scale_x_continuous(
      expand = expansion(
        mult = c(0, 0.1)
      )
    ) +
    coord_cartesian(
      xlim = c(0, NA)
    ) +
    # xlim(0, NA) +
    labs(
      y = NULL,
      x = NULL
    ) +
    theme(
      axis.ticks.y = element_blank()
    )

  return(p)
}

perm_imp <- plot_perm_imp(
  model_types = model_types,
  type_filter = c("proportion", "change"),
  final_models = final_models
)


ggsave(
  plot = perm_imp,
  "ad_hoc/paper/images/Fig_5.png",
  width = 10,
  height = 8.5,
  units = "in",
  bg = "white"
)

ggsave(
  plot = perm_imp,
  "ad_hoc/paper/images/Fig_5.pdf",
  width = 10,
  height = 8.5,
  units = "in",
  bg = "white"
)
