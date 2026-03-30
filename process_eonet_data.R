if (dir.exists(".Rlibs")) {
  .libPaths(c(".Rlibs", .libPaths()))
}

library(tidyverse)
library(plotly)
library(leaflet)
library(readr)
library(mgcv)

month_labels <- month.abb

df <- read_csv("data/eonet_events.csv", show_col_types = FALSE) %>%
  mutate(
    date = as.Date(date),
    year = as.integer(format(date, "%Y")),
    month = as.integer(format(date, "%m")),
    category = as.character(category)
  ) %>%
  filter(!is.na(date), !is.na(year), !is.na(month), !is.na(category))

df_wf <- df %>% filter(category == "Wildfires")

category_levels <- df %>%
  count(category, sort = TRUE) %>%
  pull(category)

category_palette <- c(
  "Wildfires" = "#d73027",
  "Sea and Lake Ice" = "#4575b4",
  "Volcanoes" = "#7b3294",
  "Severe Storms" = "#fdae61"
)

palette_values <- setNames(
  ifelse(category_levels %in% names(category_palette), category_palette[category_levels], "#636EFA"),
  category_levels
)

category_counts <- df %>%
  count(category, sort = TRUE) %>%
  mutate(
    share = n / sum(n),
    share_label = paste0(round(share * 100, 1), "%"),
    is_wildfire = category == "Wildfires"
  )

wildfire_share <- category_counts %>%
  filter(category == "Wildfires") %>%
  pull(share_label)

p_category <- plot_ly(
  category_counts,
  x = ~fct_reorder(category, n, .desc = TRUE),
  y = ~n,
  type = "bar",
  marker = list(
    color = ~ifelse(is_wildfire, "#d73027", "#91a8d0"),
    line = list(color = "#ffffff", width = 1)
  ),
  text = ~share_label,
  textposition = "outside",
  hovertemplate = paste(
    "Category: %{x}<br>",
    "Count: %{y:,}<br>",
    "Share: %{text}<extra></extra>"
  )
) %>%
  layout(
    showlegend = FALSE,
    yaxis = list(title = "Observation Count"),
    xaxis = list(title = "Category", tickangle = -20),
    annotations = list(
      list(
        x = 0.5,
        y = 1.12,
        xref = "paper",
        yref = "paper",
        text = paste0("Wildfires dominate the dataset (", wildfire_share, ")"),
        showarrow = FALSE,
        font = list(size = 12, color = "#b22222")
      )
    )
  )

annual_counts <- df %>%
  count(year) %>%
  arrange(year) %>%
  mutate(
    smooth = stats::predict(
      stats::loess(n ~ year, data = ., span = 0.6, control = stats::loess.control(surface = "direct"))
    )
  )

peak_row <- annual_counts %>% filter(n == max(n)) %>% slice(1)

wf_annual_counts <- df_wf %>%
  count(year) %>%
  arrange(year) %>%
  mutate(
    smooth = stats::predict(
      stats::loess(n ~ year, data = ., span = 0.6, control = stats::loess.control(surface = "direct"))
    )
  )

p_annual <- plot_ly(annual_counts, x = ~year) %>%
  add_trace(
    y = ~n,
    type = "scatter",
    mode = "lines+markers",
    name = "Raw annual counts",
    line = list(color = "#2c7fb8", width = 2.5),
    marker = list(size = 6, color = "#2c7fb8"),
    hovertemplate = "Year: %{x}<br>Count: %{y:,}<extra></extra>"
  ) %>%
  add_lines(
    y = ~smooth,
    name = "Loess trend",
    line = list(color = "#d95f0e", width = 2, dash = "dash"),
    hovertemplate = "Year: %{x}<br>Smoothed: %{y:.1f}<extra></extra>"
  ) %>%
  layout(
    xaxis = list(title = "Year"),
    yaxis = list(title = "Recorded Observations"),
    annotations = list(
      list(
        x = peak_row$year,
        y = peak_row$n,
        text = "2024 peak",
        showarrow = TRUE,
        arrowhead = 2,
        ax = -50,
        ay = -45
      ),
      list(
        x = 0.5,
        y = 1.12,
        xref = "paper",
        yref = "paper",
        text = "Recorded observations, not direct physical hazard frequency",
        showarrow = FALSE,
        font = list(size = 12, color = "#555555")
      )
    )
  )

p_wf_annual <- plot_ly(wf_annual_counts, x = ~year) %>%
  add_trace(
    y = ~n,
    type = "scatter",
    mode = "lines+markers",
    name = "Wildfire annual counts",
    line = list(color = "#d73027", width = 2.5),
    marker = list(size = 6, color = "#d73027"),
    hovertemplate = "Year: %{x}<br>Wildfire count: %{y:,}<extra></extra>"
  ) %>%
  add_lines(
    y = ~smooth,
    name = "Loess trend",
    line = list(color = "#7f0000", width = 2, dash = "dash"),
    hovertemplate = "Year: %{x}<br>Smoothed: %{y:.1f}<extra></extra>"
  ) %>%
  layout(
    xaxis = list(title = "Year"),
    yaxis = list(title = "Wildfire observations"),
    annotations = list(
      list(
        x = 0.5,
        y = 1.12,
        xref = "paper",
        yref = "paper",
        text = "Wildfire-only view of the dominant signal",
        showarrow = FALSE,
        font = list(size = 12, color = "#7f0000")
      )
    )
  )

category_year_counts <- df %>%
  count(year, category) %>%
  group_by(year) %>%
  mutate(year_share = n / sum(n)) %>%
  ungroup() %>%
  mutate(category = factor(category, levels = category_levels))

category_names <- levels(category_year_counts$category)
wildfire_visible <- as.logical(category_names == "Wildfires")
non_wildfire_visible <- !wildfire_visible

p_category_trend <- plot_ly()
for (cat_name in category_names) {
  cat_df <- category_year_counts %>% filter(category == cat_name)
  p_category_trend <- p_category_trend %>%
    add_trace(
      data = cat_df,
      x = ~year,
      y = ~n,
      type = "scatter",
      mode = "lines+markers",
      name = cat_name,
      line = list(width = 3, color = unname(palette_values[cat_name])),
      marker = list(size = 6, color = unname(palette_values[cat_name])),
      hovertemplate = paste0(
        "Category: ", cat_name,
        "<br>Year: %{x}",
        "<br>Count: %{y:,}",
        "<br>Share in year: %{customdata:.1%}<extra></extra>"
      ),
      customdata = ~year_share,
      visible = TRUE
    )
}

p_category_trend <- p_category_trend %>%
  layout(
    xaxis = list(title = "Year"),
    yaxis = list(title = "Observation Count"),
    legend = list(orientation = "h", y = -0.25),
    updatemenus = list(
      list(
        type = "buttons",
        direction = "right",
        active = 0,
        x = 0,
        y = 1.15,
        buttons = list(
          list(
            method = "update",
            args = list(
              list(visible = as.list(rep(TRUE, length(category_names)))),
              list()
            ),
            label = "All categories"
          ),
          list(
            method = "update",
            args = list(
              list(visible = as.list(wildfire_visible)),
              list()
            ),
            label = "Wildfires only"
          ),
          list(
            method = "update",
            args = list(
              list(visible = as.list(non_wildfire_visible)),
              list()
            ),
            label = "Non-wildfire categories"
          )
        )
      )
    ),
    annotations = list(
      list(
        x = 0.99,
        y = 1.08,
        xref = "paper",
        yref = "paper",
        text = "2024 surge is primarily wildfire-driven",
        showarrow = FALSE,
        xanchor = "right",
        font = list(size = 12, color = "#b22222")
      )
    )
  )

category_month_counts <- df %>%
  count(month, category) %>%
  mutate(month_label = factor(month_labels[month], levels = month_labels))

category_month_norm <- category_month_counts %>%
  group_by(category) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

p_heatmap_abs <- plot_ly(
  category_month_counts,
  x = ~month_label,
  y = ~category,
  z = ~n,
  type = "heatmap",
  colors = colorRamp(c("#f7fbff", "#6baed6", "#08306b")),
  hovertemplate = "Category: %{y}<br>Month: %{x}<br>Count: %{z:,}<extra></extra>"
) %>%
  layout(
    xaxis = list(title = "Month"),
    yaxis = list(title = "Category")
  )

p_heatmap_norm <- plot_ly(
  category_month_norm,
  x = ~month_label,
  y = ~category,
  z = ~prop,
  type = "heatmap",
  colors = colorRamp(c("#fff5eb", "#fdae6b", "#a63603")),
  hovertemplate = "Category: %{y}<br>Month: %{x}<br>Within-category share: %{z:.1%}<extra></extra>"
) %>%
  layout(
    xaxis = list(title = "Month"),
    yaxis = list(title = "Category")
  )

# Keep legacy object name for compatibility.
p_heatmap <- p_heatmap_abs

p_monthly_pattern_by_category <- plot_ly(
  category_month_norm,
  x = ~month,
  y = ~prop,
  color = ~category,
  colors = palette_values,
  type = "scatter",
  mode = "lines+markers",
  hovertemplate = "Category: %{fullData.name}<br>Month: %{x}<br>Share: %{y:.1%}<extra></extra>"
) %>%
  layout(
    xaxis = list(title = "Month", tickvals = 1:12, ticktext = month_labels),
    yaxis = list(title = "Within-category monthly share", tickformat = ".0%")
  )

map_data <- df %>%
  filter(!is.na(longitude), !is.na(latitude))

make_popup <- function(data_in) {
  paste0(
    "<b>Title:</b> ", data_in$title, "<br>",
    "<b>Category:</b> ", data_in$category, "<br>",
    "<b>Date:</b> ", data_in$date, "<br>",
    "<b>Coordinates:</b> ", round(data_in$latitude, 2), ", ", round(data_in$longitude, 2)
  )
}

make_map <- function(data_in) {
  map_df <- data_in %>%
    mutate(color_value = unname(palette_values[as.character(category)]))

  legend_df <- map_df %>%
    distinct(category, color_value) %>%
    arrange(category)

  leaflet(map_df) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      radius = 4,
      stroke = FALSE,
      fillOpacity = 0.7,
      color = ~color_value,
      popup = make_popup(map_df),
      clusterOptions = markerClusterOptions()
    ) %>%
    addLegend(
      "bottomright",
      colors = legend_df$color_value,
      labels = as.character(legend_df$category),
      title = "Category",
      opacity = 0.8
    )
}

p_map_all <- make_map(map_data)
p_map_wildfires <- make_map(map_data %>% filter(category == "Wildfires"))
p_map_ice <- make_map(map_data %>% filter(category == "Sea and Lake Ice"))
p_map_volcanoes <- make_map(map_data %>% filter(category == "Volcanoes"))
p_map_storms <- make_map(map_data %>% filter(category == "Severe Storms"))

# Keep legacy object name for compatibility.
p_map <- p_map_all

monthly_series <- df %>%
  count(year, month, category, name = "count") %>%
  mutate(category = factor(category, levels = category_levels))

max_year <- max(monthly_series$year, na.rm = TRUE)
split_year <- max_year - 1L
train <- monthly_series %>% filter(year <= split_year)
test <- monthly_series %>% filter(year > split_year)

metric_fn <- function(actual, pred) {
  mae <- mean(abs(actual - pred), na.rm = TRUE)
  rmse <- sqrt(mean((actual - pred)^2, na.rm = TRUE))
  denom <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  r2 <- ifelse(denom == 0, NA_real_, 1 - sum((actual - pred)^2, na.rm = TRUE) / denom)
  c(MAE = mae, RMSE = rmse, R2 = r2)
}

baseline_lookup <- train %>%
  group_by(category, month) %>%
  summarise(pred_baseline = mean(count), .groups = "drop")

test <- test %>%
  left_join(baseline_lookup, by = c("category", "month")) %>%
  mutate(pred_baseline = replace_na(pred_baseline, mean(train$count)))

train <- train %>%
  mutate(
    month_sin = sin(2 * pi * month / 12),
    month_cos = cos(2 * pi * month / 12),
    category_code = as.integer(category)
  )
test <- test %>%
  mutate(
    month_sin = sin(2 * pi * month / 12),
    month_cos = cos(2 * pi * month / 12),
    category_code = as.integer(factor(category, levels = levels(train$category)))
  )

# Use matrix-based OLS so prediction remains robust when late-period categories appear.
x_train <- model.matrix(~ year + month_sin + month_cos + category_code, data = train)
x_test <- model.matrix(~ year + month_sin + month_cos + category_code, data = test)
ols_coef <- qr.solve(x_train, train$count)
test$pred_ols <- pmax(0, as.numeric(x_test %*% ols_coef))

full_unique_years <- dplyr::n_distinct(train$year)
if (full_unique_years >= 4) {
  full_k <- min(6, full_unique_years - 1)
  gam_fit <- mgcv::gam(count ~ s(year, k = full_k) + month_sin + month_cos + category_code, data = train, method = "REML")
} else {
  gam_fit <- mgcv::gam(count ~ year + month_sin + month_cos + category_code, data = train, method = "REML")
}
test$pred_gam <- pmax(0, predict(gam_fit, newdata = test))

has_xgboost <- requireNamespace("xgboost", quietly = TRUE)
if (has_xgboost) {
  x_train <- model.matrix(~ year + month_sin + month_cos + category, data = train)[, -1, drop = FALSE]
  x_test <- model.matrix(~ year + month_sin + month_cos + category, data = test)[, -1, drop = FALSE]
  dtrain <- xgboost::xgb.DMatrix(data = x_train, label = train$count)
  dtest <- xgboost::xgb.DMatrix(data = x_test)
  xgb_fit <- xgboost::xgb.train(
    params = list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      eta = 0.05,
      max_depth = 4,
      subsample = 0.8,
      colsample_bytree = 0.8
    ),
    data = dtrain,
    nrounds = 150,
    verbose = 0
  )
  test$pred_xgb <- pmax(0, as.numeric(predict(xgb_fit, dtest)))
} else {
  test$pred_xgb <- NA_real_
}

metric_rows <- list(
  tibble(model = "Seasonal baseline", !!!as.list(metric_fn(test$count, test$pred_baseline))),
  tibble(model = "OLS", !!!as.list(metric_fn(test$count, test$pred_ols))),
  tibble(model = "GAM", !!!as.list(metric_fn(test$count, test$pred_gam)))
)
if (has_xgboost) {
  metric_rows <- append(
    metric_rows,
    list(tibble(model = "XGBoost", !!!as.list(metric_fn(test$count, test$pred_xgb))))
  )
}

model_eval <- bind_rows(metric_rows) %>%
  mutate(across(c(MAE, RMSE, R2), as.numeric))

# --- Wildfire-only predictive benchmarking ---
wf_monthly <- df_wf %>%
  count(year, month, name = "count") %>%
  arrange(year, month)

wf_train <- wf_monthly %>% filter(year <= split_year)
wf_test <- wf_monthly %>% filter(year > split_year)

wf_baseline_lookup <- wf_train %>%
  group_by(month) %>%
  summarise(pred_baseline = mean(count), .groups = "drop")

wf_test <- wf_test %>%
  left_join(wf_baseline_lookup, by = "month") %>%
  mutate(pred_baseline = replace_na(pred_baseline, mean(wf_train$count)))

wf_train <- wf_train %>%
  mutate(
    month_sin = sin(2 * pi * month / 12),
    month_cos = cos(2 * pi * month / 12)
  )
wf_test <- wf_test %>%
  mutate(
    month_sin = sin(2 * pi * month / 12),
    month_cos = cos(2 * pi * month / 12)
  )

wf_x_train <- model.matrix(~ year + month_sin + month_cos, data = wf_train)
wf_x_test <- model.matrix(~ year + month_sin + month_cos, data = wf_test)
wf_ols_coef <- qr.solve(wf_x_train, wf_train$count)
wf_test$pred_ols <- pmax(0, as.numeric(wf_x_test %*% wf_ols_coef))

wf_unique_years <- dplyr::n_distinct(wf_train$year)
if (wf_unique_years >= 4) {
  wf_k <- min(6, wf_unique_years - 1)
  wf_gam_fit <- mgcv::gam(count ~ s(year, k = wf_k) + month_sin + month_cos, data = wf_train, method = "REML")
} else {
  wf_gam_fit <- mgcv::gam(count ~ year + month_sin + month_cos, data = wf_train, method = "REML")
}
wf_test$pred_gam <- pmax(0, predict(wf_gam_fit, newdata = wf_test))

if (has_xgboost) {
  wf_train_xgb <- as.matrix(wf_train %>% select(year, month, month_sin, month_cos))
  wf_test_xgb <- as.matrix(wf_test %>% select(year, month, month_sin, month_cos))
  wf_dtrain <- xgboost::xgb.DMatrix(data = wf_train_xgb, label = wf_train$count)
  wf_dtest <- xgboost::xgb.DMatrix(data = wf_test_xgb)
  wf_xgb_fit <- xgboost::xgb.train(
    params = list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      eta = 0.05,
      max_depth = 4,
      subsample = 0.8,
      colsample_bytree = 0.8
    ),
    data = wf_dtrain,
    nrounds = 150,
    verbose = 0
  )
  wf_test$pred_xgb <- pmax(0, as.numeric(predict(wf_xgb_fit, wf_dtest)))
} else {
  wf_test$pred_xgb <- NA_real_
}

wf_metric_rows <- list(
  tibble(model = "Seasonal baseline", !!!as.list(metric_fn(wf_test$count, wf_test$pred_baseline))),
  tibble(model = "OLS", !!!as.list(metric_fn(wf_test$count, wf_test$pred_ols))),
  tibble(model = "GAM", !!!as.list(metric_fn(wf_test$count, wf_test$pred_gam)))
)
if (has_xgboost) {
  wf_metric_rows <- append(
    wf_metric_rows,
    list(tibble(model = "XGBoost", !!!as.list(metric_fn(wf_test$count, wf_test$pred_xgb))))
  )
}

wf_model_eval <- bind_rows(wf_metric_rows) %>%
  mutate(across(c(MAE, RMSE, R2), as.numeric))

p_model_compare <- plot_ly()
metric_list <- c("MAE", "RMSE", "R2")
for (i in seq_along(metric_list)) {
  m <- metric_list[i]
  m_df <- model_eval %>% select(model, value = all_of(m))
  p_model_compare <- p_model_compare %>%
    add_bars(
      data = m_df,
      x = ~model,
      y = ~value,
      name = m,
      visible = i == 1,
      text = ~ifelse(is.na(value), "N/A", round(value, 3)),
      textposition = "auto",
      hovertemplate = "Model: %{x}<br>Value: %{y:.3f}<extra></extra>"
    )
}

p_model_compare <- p_model_compare %>%
  layout(
    yaxis = list(title = "MAE"),
    xaxis = list(title = "Model"),
    barmode = "group",
    updatemenus = list(
      list(
        type = "buttons",
        direction = "right",
        x = 0,
        y = 1.18,
        buttons = lapply(seq_along(metric_list), function(i) {
          list(
            method = "update",
            args = list(
              list(visible = as.list(seq_along(metric_list) == i)),
              list(yaxis = list(title = metric_list[i]))
            ),
            label = metric_list[i]
          )
        })
      )
    )
  )

# Scope comparison: full target vs wildfire-only target
scope_compare <- bind_rows(
  model_eval %>% mutate(scope = "Full multi-category"),
  wf_model_eval %>% mutate(scope = "Wildfire-only")
) %>%
  select(scope, model, MAE, RMSE, R2)

p_scope_compare <- plot_ly()
for (i in seq_along(metric_list)) {
  m <- metric_list[i]
  m_df <- scope_compare %>% select(scope, model, value = all_of(m))
  p_scope_compare <- p_scope_compare %>%
    add_bars(
      data = m_df,
      x = ~model,
      y = ~value,
      color = ~scope,
      colors = c("Full multi-category" = "#2c7fb8", "Wildfire-only" = "#d73027"),
      visible = i == 1,
      text = ~ifelse(is.na(value), "N/A", round(value, 3)),
      textposition = "auto",
      hovertemplate = paste0(
        "Model: %{x}<br>",
        "Scope: %{fullData.name}<br>",
        m, ": %{y:.3f}<extra></extra>"
      )
    )
}

p_scope_compare <- p_scope_compare %>%
  layout(
    barmode = "group",
    xaxis = list(title = "Model"),
    yaxis = list(title = "MAE"),
    legend = list(title = list(text = "Prediction scope")),
    updatemenus = list(
      list(
        type = "buttons",
        direction = "right",
        x = 0,
        y = 1.18,
        buttons = lapply(seq_along(metric_list), function(i) {
          list(
            method = "update",
            args = list(
              list(visible = as.list(seq_along(metric_list) == i)),
              list(yaxis = list(title = metric_list[i]))
            ),
            label = metric_list[i]
          )
        })
      )
    )
  )

category_model_compare <- test %>%
  group_by(category) %>%
  summarise(
    Baseline = sqrt(mean((count - pred_baseline)^2, na.rm = TRUE)),
    OLS = sqrt(mean((count - pred_ols)^2, na.rm = TRUE)),
    GAM = sqrt(mean((count - pred_gam)^2, na.rm = TRUE)),
    .groups = "drop"
  )

if (has_xgboost) {
  xgb_by_category <- test %>%
    group_by(category) %>%
    summarise(XGBoost = sqrt(mean((count - pred_xgb)^2, na.rm = TRUE)), .groups = "drop")
  category_model_compare <- category_model_compare %>%
    left_join(xgb_by_category, by = "category")
}

category_model_compare <- category_model_compare %>%
  pivot_longer(cols = -category, names_to = "model", values_to = "rmse")

p_category_model_compare <- plot_ly(
  category_model_compare,
  x = ~category,
  y = ~rmse,
  color = ~model,
  type = "bar",
  hovertemplate = "Category: %{x}<br>Model: %{fullData.name}<br>RMSE: %{y:.2f}<extra></extra>"
) %>%
  layout(
    xaxis = list(title = "Category"),
    yaxis = list(title = "RMSE"),
    barmode = "group"
  )

xgboost_status_note <- if (has_xgboost) {
  "All four models are available in the current environment."
} else {
  "XGBoost is unavailable in the current environment, so benchmarking charts show Baseline/OLS/GAM only."
}
