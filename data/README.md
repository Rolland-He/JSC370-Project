# Data Folder

This folder contains the local cached dataset used by the project:

- `eonet_events.csv`

## Data Source

The data comes from NASA EONET API v3:

- [https://eonet.gsfc.nasa.gov/api/v3/events](https://eonet.gsfc.nasa.gov/api/v3/events)

## How to Reacquire Data

If you need to regenerate the dataset, query the EONET API endpoint above and save the processed event-level output as:

- `data/eonet_events.csv`

Required columns used by the website pipeline include:

- `date`
- `category`
- `latitude`
- `longitude`
- `title`
