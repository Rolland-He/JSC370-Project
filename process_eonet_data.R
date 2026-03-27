library(tidyverse)
library(plotly)
library(leaflet)
library(readr)

df <- read_csv("data/eonet_events.csv")

df <- df %>%
  mutate(
    date = as.Date(date),
    year = as.integer(format(date, "%Y")),
    month = as.integer(format(date, "%m"))
  )

category_counts <- df %>%
  count(category) %>%
  mutate(share = n / sum(n))

annual_counts <- df %>%
  count(year)

category_year_counts <- df %>%
  count(year, category)

category_month_counts <- df %>%
  count(month, category)

p_category <- plot_ly(
  category_counts,
  x = ~category,
  y = ~n,
  type = "bar",
  text = ~paste0("Count: ", n, "<br>Share: ", round(share * 100, 1), "%"),
  hoverinfo = "text"
)

p_annual <- plot_ly(
  annual_counts,
  x = ~year,
  y = ~n,
  type = "scatter",
  mode = "lines+markers",
  text = ~paste0("Year: ", year, "<br>Count: ", n),
  hoverinfo = "text"
)

p_category_trend <- plot_ly(
  category_year_counts,
  x = ~year,
  y = ~n,
  color = ~category,
  type = "scatter",
  mode = "lines+markers"
)

p_heatmap <- plot_ly(
  category_month_counts,
  x = ~month,
  y = ~category,
  z = ~n,
  type = "heatmap"
)

p_map <- leaflet(df) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 3,
    popup = ~paste0(
      "<b>Category:</b> ", category, "<br>",
      "<b>Date:</b> ", date, "<br>",
      "<b>Title:</b> ", title
    )
  )
