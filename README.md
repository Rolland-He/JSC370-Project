# JSC370 Project

This repository contains course work for JSC370, including midterm/final report artifacts and a project website built with R Markdown.

## Project Website

- Website: [https://rolland-he.github.io/JSC370-Project/](https://rolland-he.github.io/JSC370-Project/)

- Data source: NASA EONET API v3 (`https://eonet.gsfc.nasa.gov/api/v3/events`)


## Website Files

- `index.Rmd`, `Interactive_Vis.Rmd`, `Report.Rmd`: website pages
- `_site.yml`: site navigation and output settings
- `process_eonet_data.R`: data processing and interactive plots
- `styles.css`: website style overrides
- `docs/`: rendered website output for GitHub Pages
- Local cached dataset: `data/eonet_events.csv`

- Midterm Report: `midterm/midterm.html`
- Midterm QMD code: `midterm/midterm.qmd`
- Midterm PDF: `midterm/midterm_2026.pdf`
- Midterm Render command: `quarto render midterm/midterm.qmd`
