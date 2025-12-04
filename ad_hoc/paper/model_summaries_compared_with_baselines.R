source("R/00_libraries.R")
model_summary <- readRDS("tests/model_testing/model_summary_information.rds")

long_data <- model_summary |>
  filter(
    `Tuning objective` == "mape" #,
    # !grepl("52", `Target variable`)
  ) |>
  pivot_longer(
    cols = c(`Test set value`, starts_with("Baseline")),
    names_to = "scenario",
    values_to = "mape"
  ) |>
  mutate(
    count_of_models = n(),
    .by = c(`Target variable`, `Model type`, `Target variable type`)
  ) |>
  mutate(
    `Target variable detailed` = str_wrap(`Target variable`, 25),
    `Target variable broad` = str_wrap(`Target variable`, 40),
    `Number lagged target years` = gsub(
      " lagged years",
      "",
      `Number lagged target years`
    ),
    `Years in dataset` = `Number training years` + 1,
    `Model type` = str_to_sentence(gsub("_", " ", `Model type`)),
    `Model type` = case_when(
      `Model type` == "Logistic regression" ~ "GLM",
      `Model type` == "Random forest" ~ "RF",
      .default = "unknown"
    ),
    `Model type` = case_when(
      `Target variable type` == "proportion" ~ `Model type`,
      .default = paste0(`Model type`, "y")
    ),
    # `Target variable type` = case_when(
    #   `Target variable type` == "proportion" ~ "(proportion)",
    #   .default = "(CiP)"
    # ),
    scenario_broad = paste0(
      `Target variable type`,
      " (n=",
      count_of_models,
      ")"
    ),
    scenario = case_when(
      grepl("linear", scenario) ~ "NB2",
      grepl("same as last year", scenario) ~ "NB1",
      .default = scenario
    )
  ) |>
  select(
    "Target variable detailed",
    "Target variable broad",
    "Number lagged target years",
    "Years in dataset",
    "Model type",
    "Target variable type",
    "mape",
    "Number lagged years",
    "scenario",
    "scenario_broad"
  )


detailed_plot_data <- long_data |>
  mutate(
    scenario = case_when(
      `Number lagged target years` == 1 &
        scenario ==
          "Test set value" ~ "Test set value (with lagged target value incl. as predictor)",
      .default = scenario
    ),
    facet = `Model type`,
    facet = factor(
      facet,
      levels = c(
        "GLM",
        "RF",
        "RFy"
      )
    ),
    x_axis = paste0(
      `Years in dataset`,
      "[",
      `Number lagged years`,
      "]"
    )
  )

x_breaks <- detailed_plot_data |>
  pull(
    x_axis
  ) |>
  unique()

x_labels <- x_breaks |>
  sapply(
    function(x) parse(text = x)[[1]]
  )

detailed_plot <- detailed_plot_data |>
  mutate(
    `Target variable detailed` = case_when(
      `Target variable detailed` ==
        "Proportion of incomplete\npathways greater than 52\nweeks from referral" ~ "RTT52 metric",
      `Target variable detailed` ==
        "Proportion of suspected\ncancer or referral\nto first definitive\ntreatment that are longer\nthan 62 days wait" ~ "Cancer metric",
      `Target variable detailed` ==
        "Proportion of A&E\nattendances with greater\nthan 4 hours wait (Type 1\nDepartments - Major A&E)" ~ "A&E metric",
      `Target variable detailed` ==
        "Proportion of attended\nGP appointments (over 4\nweeks wait time)" ~ "GP metric",
      `Target variable detailed` ==
        "Proportion of incomplete\npathways greater than 18\nweeks from referral" ~ "RTT18 metric",
      .default = NA_character_
    )
  ) |>
  ggplot(
    aes(
      y = mape,
      x = x_axis
    )
  ) +
  geom_point(
    aes(
      shape = scenario,
      size = scenario,
      colour = scenario
    ),
    fill = NA,
  ) +
  geom_line(
    aes(
      group = interaction(`Years in dataset`, scenario),
      linetype = scenario,
      colour = scenario
    ),
    linewidth = 1
  ) +
  scale_shape_manual(
    name = "",
    values = c(
      "NB1" = 15,
      "NB2" = 15,
      "Test set value" = 16,
      "Test set value (with lagged target value incl. as predictor)" = 21
    )
  ) +
  scale_linetype_manual(
    name = "",
    values = c(
      "NB1" = "solid",
      "NB2" = "solid",
      "Test set value" = NA,
      "Test set value (with lagged target value incl. as predictor)" = NA
    )
  ) +
  scale_size_manual(
    name = "",
    values = c(
      "NB1" = 0.8,
      "NB2" = 0.8,
      "Test set value" = 1,
      "Test set value (with lagged target value incl. as predictor)" = 1
    )
  ) +
  scale_colour_manual(
    name = "",
    values = c(
      "NB1" = "gray45",
      "NB2" = "gray75",
      "Test set value" = "black",
      "Test set value (with lagged target value incl. as predictor)" = "black"
    )
  ) +
  scale_x_discrete(
    breaks = x_breaks,
    labels = x_labels
  ) +
  facet_grid(
    rows = vars(`Target variable detailed`),
    cols = vars(facet),
    scales = "free",
    axes = "all",
    axis.labels = "all_x"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    y = "Mean Absolute Percentage Error (MAPE)",
    x = expression(`Years in dataset`[`Number of lagged years`])
  ) +
  scale_y_log10()

ggsave(
  plot = detailed_plot,
  "ad_hoc/paper/images/Fig_4.png",
  width = 12,
  height = 10,
  units = "in",
  bg = "white"
)

ggsave(
  plot = detailed_plot,
  "ad_hoc/paper/images/Fig_4.pdf",
  width = 12,
  height = 10,
  units = "in",
  bg = "white"
)


boxplot_mape_results <- function(data) {
  plot <- data |>
    mutate(
      `Target variable broad` = case_when(
        `Target variable detailed` ==
          "Proportion of incomplete\npathways greater than 52\nweeks from referral" ~ "RTT52 metric",
        `Target variable detailed` ==
          "Proportion of suspected\ncancer or referral\nto first definitive\ntreatment that are longer\nthan 62 days wait" ~ "Cancer metric",
        `Target variable detailed` ==
          "Proportion of A&E\nattendances with greater\nthan 4 hours wait (Type 1\nDepartments - Major A&E)" ~ "A&E metric",
        `Target variable detailed` ==
          "Proportion of attended\nGP appointments (over 4\nweeks wait time)" ~ "GP metric",
        `Target variable detailed` ==
          "Proportion of incomplete\npathways greater than 18\nweeks from referral" ~ "RTT18 metric",
        .default = NA_character_
      )
    ) |>
    ggplot(
      aes(x = scenario, y = mape)
    ) +
    geom_boxplot() +
    facet_wrap(
      facets = vars(`Target variable broad`),
      scales = "free_x"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5,
        size = 7
      ),
      legend.position = "bottom"
    ) +
    labs(
      y = "Mean Absolute Percentage Error (MAPE)",
      x = ""
    ) +
    coord_cartesian(
      ylim = c(0, 100)
    )

  return(plot)
}

reorder_vector <- function(vector, first_vars) {
  new_vector <- c(
    first_vars,
    vector[!(vector %in% first_vars)]
  )

  return(new_vector)
}

summary_plot <- long_data |>
  mutate(
    scenario = case_when(
      scenario == "Test set value" ~ paste(
        `Model type`,
        gsub("^.*\\(", "\\(", scenario_broad),
        sep = " - "
      ),
      .default = scenario
    ),
    scenario = gsub("\\) \\(", ", ", scenario),
    scenario = str_wrap(scenario, 20),
    scenario = factor(
      scenario,
      levels = reorder_vector(
        unique(scenario),
        first_vars = c("NB1", "NB2")
      )
    )
  ) |>
  boxplot_mape_results()


ggsave(
  plot = summary_plot,
  "ad_hoc/paper/images/Fig_3.png",
  width = 9,
  height = 6,
  units = "in",
  bg = "white"
)

ggsave(
  plot = summary_plot,
  "ad_hoc/paper/images/Fig_3.pdf",
  width = 9,
  height = 6,
  units = "in",
  bg = "white"
)
