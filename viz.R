# (c) 2023 
# MIT License
# Miller, J.

# Front Matter ------------------------------------------------------------
'
The follow in an R language script that pulls publicly available data,
computes statistics, and produces reproducible data visualizations
to accompany the book.

In many cases, I ended up using PowerPoint rather than the plot.ly
or ggplot2 plots. In those cases I have included the .PPTX source files
on GitHub with comments in the R code. You can still replicate the same
general image within the R code via plot.ly, etc.
'
# Noteable resources (non-exhaustive list): 
# https://data.gov
# https://www.bls.gov/cpi/
# https://www.cbp.gov/sites/default/files/assets/documents/2019-Mar/bp-southwest-border-sector-apps-fy1960-fy2018.pdf
# https://www.dir.ca.gov/OPRL/CPI/EntireCCPI.PDF
#
# Other sources are noted within the indivudal figures

# Libraries and Options ---------------------------------------------------
# Prepare kaleido for plotly::save_image, if needed
# install.packages('reticulate')
# reticulate::install_miniconda()
# reticulate::conda_install('r-reticulate', 'python-kaleido')
# reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
reticulate::use_miniconda('r-reticulate')
reticulate::py_run_string("import sys") # for save_image

# Set your Plotly credentials
plotly_user <- Sys.getenv("PLOTLY_USER") # Your username
plotly_key  <- Sys.getenv("PLOTLY_KEY")  # Your key

# Package manager
if (!require(pacman)) { install.packages("pacman"); library(pacman) }

# Bioconductor
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
#BiocManager::install(version = "3.16")

# Load Libs
p_load(ACSWR, BiocManager, data.table, devtools, dplyr, FedData, 
       federalregister, fuzzyjoin, fuzzywuzzyR, ggExtra, ggplot2, 
       ggplotify, ggthemes, gridExtra, 
       httr, jsonlite, lattice, latticeExtra, lubridate, magick,
       pdftools, plotly, readr, readxl, remotes, reticulate, rjson, rvest, 
       scales, tidycensus, tidyverse) 

#remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))
#devtools::install_github("keberwein/blscrapeR")
require(blscrapeR)
require(tabulizer)

# API Keys
api_key.bls    <- Sys.getenv("BLS_KEY")
api_key.census <- Sys.getenv("CENSUS_KEY")
census_api_key(api_key.census, install = T)
readRenviron("~/.Renviron")

# Random seed
set.seed(42)

# Figure 1 ----------------------------------------------------------------
# Data preparation
years <- c(2000, 2021)

foreign_population <- lapply(years, function(year) {
  if (year == 2000) {
    data <- get_decennial(geography   = "state", 
                          year        = year, 
                          state       = "CA", 
                          table       = "P021",
                          cache_table = T, 
                          show_call   = T, 
                          sumfile.    = "sf3")
  } else {
    data <- get_acs(geography = "state", variables = c("B05001_001", "B05001_002", 
                                                       "B05001_003", "B05001_004", 
                                                       "B05001_005", "B05001_006"),
                    year = year, state = "CA", cache_table = T)
  }
  data$year <- year
  data
}) %>% bind_rows()

foreign_population$estimate[is.na(foreign_population$estimate)] <- 
  foreign_population$value[is.na(foreign_population$estimate)]

foreign_population <- foreign_population %>%
  select(-value, -moe)

# Filter relevant variables
foreign_population_ca <- foreign_population[foreign_population$variable %in% c("P021013",
                                                                               "P021002","B05001_001",
                                                                               "B05001_002", "B05001_003", "B05001_004", 
                                                                               "B05001_005", "B05001_006"
),]

# Santa Clara data
sc_2000 <- get_decennial(geography   = "county", 
                         year        = 2000, 
                         state       = "CA", 
                         table       = "P021",
                         cache_table = T, 
                         show_call   = T, 
                         sumfile     = "sf3",
                         county      = "Santa Clara")
sc_2021 <- get_acs(geography = "county", variables = c("B05001_001", "B05001_002", 
                                                       "B05001_003", "B05001_004", 
                                                       "B05001_005", "B05001_006"),
                   year = 2021, state = "CA", cache_table = T,
                   county = "Santa Clara")

sc_data <- rbind(
  data.frame(geo      = "Santa Clara County",
             variable = sc_2000$variable,
             estimate = sc_2000$value,
             year     = 2000),
  data.frame(geo      = "Santa Clara County",
             variable = sc_2021$variable,
             estimate = sc_2021$estimate,
             year     = 2021)
)

# Combine datasets
foreign_population_ca$geo <- "California"
foreign_population_ca     <- bind_rows(foreign_population_ca, sc_data)

# Calculate foreign population
foreign_population_ca$foreign <- ifelse(foreign_population_ca$variable %in% 
                                          c("P021013", "B05001_006", "B05001_005"),
                                        T, F)

foreign_population_ca <- foreign_population_ca[foreign_population_ca$foreign %in% T,]

foreign_population_ca  <- aggregate(estimate ~ year + geo, sum, 
                                    data = foreign_population_ca)
foreign_population_ca
saveRDS(foreign_population_ca, "foreign_population_ca.RDS")

### Create the Plotly bar chart
# Create a custom function to round and format numbers with the appropriate suffix
format_number <- function(x) {
  if (x >= 1e6) {
    paste(round(x / 1e6, 1), "Mil.")
  } else {
    paste(round(x / 1e3, 0), "k")
  }
}

# Vectorize the format_number function
format_number_vec <- Vectorize(format_number)

# Define colors
old_glory_blue <- "#002147"
old_glory_red  <- "#BB133E"
  
# Sort the data by year and location
fig1_plot_data <- foreign_population_ca %>%
  arrange(year, geo)

# Update the plot
fig1 <- plot_ly() %>%
  add_trace(data = fig1_plot_data %>% filter(geo == "California"),
            x = ~paste(year, geo), y = ~(estimate),
            type = "bar", text = ~format_number_vec(estimate),
            hoverinfo = "text", textposition = "outside",
            insidetextanchor = "middle", textfont = list(size = 12),
            marker = list(color = old_glory_blue),
            name = "California",
            yaxis = "y") %>%
  add_trace(data = fig1_plot_data %>% filter(geo == "Santa Clara County"),
            x = ~paste(year, geo), y = ~(estimate), 
            type = "bar", text = ~format_number_vec(estimate),
            hoverinfo = "text", textposition = "outside",
            insidetextanchor = "middle", textfont = list(size = 12),
            marker = list(color = old_glory_red),
            name = "Santa Clara County",
            yaxis = "y2") %>%
  layout(title = list(text = "<b>Figure 1: Foreign-born Population Growth in<br>California State and Santa Clara County (2000 - 2021)</b>",
                      font = list(size = 26, family = "Arial", face = "bold")),
         xaxis = list(title = list(text = "Year", font = list(size = 20),
                                   standoff = 20, x = 0.5, y = -0.1), 
                      tickangle = 0, title_standoff = 20, 
                      tickfont  = list(size = 16),
                      tickvals  = c("2000 California", "2000 Santa Clara County", 
                                    "2021 California", "2021 Santa Clara County"),
                      ticktext = c("2000", "2000", "2021", "2021")),
         yaxis = list(title = "Population (Millions)", titlefont = list(size = 15),
                      title_standoff = 5, tickfont = list(size = 14),
                      range = c(0, 1.4 * max(fig1_plot_data$estimate))),
         yaxis2 = list(title = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Population (Thousands)", overlaying = "y", 
                       side = "right",
                       titlefont = list(size = 15), 
                       title_standoff = 5, 
                       tickfont = list(size = 14),
                       range = c(0, 1.4 * max(
                         fig1_plot_data$estimate[
                           fig1_plot_data$geo == 
                             "Santa Clara County"]))),
         barmode = 'group',
         margin = list(l = 60, r = 50, b = 100, t = 125, pad = 0),
         legend = list(bgcolor = 'rgba(255, 255, 255, 0.5)', bordercolor = 'black', borderwidth = 1,
                       xanchor = "center", x = 0.75, y = 1.01, orientation = "v"), # Adjusted x and y values, and set orientation to vertical
         annotations = list(
           list(
             x = 1,
             y = -0.35,
             xref = "paper",
             yref = "paper",
             text = 'Source: <a href="https://data.census.gov/">U.S. Census Bureau</a>, Decennial Census and ACS',
             showarrow = FALSE,
             font = list(size = 14),
             xanchor = "right",
             yanchor = "bottom"
           )))

# Display (preview)
fig1

# Save the image with a scale of 2
save_image(fig1, file = "fig1/figure_1_california_population_by_national_origin.jpeg", 
           scale = 2)
save_image(fig1, file = "fig1/figure_1_california_population_by_national_origin.png", 
           scale = 2)

saveRDS(fig1, "fig1/fig1.RDS")

# Landscape version
# Save the image to a temporary file first
temp_file <- "temp_fig1.png"
save_image(fig1, file = temp_file, scale = 2)

# Read the image, rotate, and write the result
image_read(temp_file) %>%
  image_rotate(-90) %>%
  image_write("fig1/figure_1_california_population_by_national_origin_rotated.png")

# Do the same for the jpeg
temp_file_jpeg <- "temp_fig1.jpeg"
save_image(fig1, file = temp_file_jpeg, scale = 2)

image_read(temp_file_jpeg) %>%
  image_rotate(-90) %>%
  image_write("fig1/figure_1_california_population_by_national_origin_rotated.jpeg")


# Figure 2 ----------------------------------------------------------------
# Source: https://www.census.gov/dataviz/visualizations/051/
# Formatted with PowerPoint
print("The original source's image and PowerPoint were used for Figure 2.")

# Figure 3 ----------------------------------------------------------------
### Inflation (CPI)
## California
cpi.ca <- read.csv("tabula-EntireCCPI.csv", header = T)
cpi.ca <- cpi.ca[cpi.ca$Month=="Annual",]

## Federal
# Function to get annual CPI data for a specific range of years
get_annual_cpi_data <- function(startyear, endyear, api_key) {
  endpoint <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"
  payload  <- toJSON(list(
    seriesid        = list("CUUR0000SA0"),
    startyear       = startyear,
    endyear         = endyear,
    annualaverage   = TRUE,
    registrationKey = api_key))
  
  response <- POST(endpoint,
                   add_headers("Content-Type" = "application/json"),
                   body   = payload,
                   encode = "json"
  )
  
  parsed_response <- jsonlite::fromJSON(content(response, 
                                                "text", encoding = "UTF-8"))
  return(as.data.frame(parsed_response$Results$series$data))
}

# Initialize an empty data frame to store the combined results
combined_cpi <- data.frame()

# Set the initial start year and the final end year
initial_startyear <- 1955
final_endyear     <- 2022 # 2023 is not yet completed

# Loop through the years in 20-year increments
for (startyear in seq(initial_startyear, final_endyear, 20)) {
  endyear          <- min(startyear + 19, final_endyear)
  current_cpi_data <- get_annual_cpi_data(startyear, endyear, api_key.bls)
  combined_cpi     <- rbind(combined_cpi, current_cpi_data)
}

# Encode value as numeric
combined_cpi$value <- as.numeric(combined_cpi$value)

# Sort the data frame
cpi <- combined_cpi[combined_cpi$periodName == "Annual", ]
cpi <- cpi[order(cpi$year), ]

# Clear old row numbers (index)
row.names(cpi) <- NULL
print(cpi)

# Create new data frames for Federal and California CPI data
cpi_federal <- cpi
cpi_ca      <- cpi.ca

# Rename the columns for consistency
names(cpi_federal) <- c("Year", "period", "periodName", "All.Urban.Consumers",
                        "blank")
names(cpi_ca)      <- c("Year", "period", "All.Urban.Consumers", 
                        "Urban.Wage.Earners.and.Clerical.Workers")

# Extract the relevant columns
cpi_ca <- cpi_ca[, c("Year", "All.Urban.Consumers")]

# Add a column to indicate the location
cpi_federal$location <- "Federal"
cpi_ca$location      <- "California"

# Save files
saveRDS(cpi, "cpi.RDS")
saveRDS(cpi.ca, "cpi.ca.RDS")

# Combine the federal and California CPI data
combined_data <- rbind(cpi_federal[, c("Year","All.Urban.Consumers", "location")], 
                       cpi_ca[, c("Year", "All.Urban.Consumers", "location")])

names(combined_data) <- c("year", "cpi", "location")

# Convert the 'year' column to a numeric data type
combined_data$year <- as.numeric(combined_data$year)

# Filter data to start from 1995
combined_data <- combined_data[combined_data$year >= 1995,]

# Create the plot using Plotly
fig <- plot_ly() %>%
  add_lines(data = combined_data[combined_data$location == "Federal",], x = ~year, y = ~cpi, name = "Federal",
            line = list(color = 'blue', dash = "solid", width = 2)) %>%
  add_lines(data = combined_data[combined_data$location == "California",], x = ~year, y = ~cpi, name = "California",
            line = list(color = 'red', dash = "dash", width = 2)) %>%
  layout(
    title = "<b>Figure 3: Consumer Price Index (CPI), 1995 - 2022</b><br>California and Federal Annual Averages",
    xaxis = list(title = "Year", dtick = 5, showline = TRUE, linewidth = 2, 
                 linecolor = 'black', mirror = TRUE,titlefont = list(size = 12)),
    yaxis = list(title = "CPI", showline = TRUE, linewidth = 2, linecolor = 'black', mirror = TRUE),
    legend = list(orientation = "v", x = 0.01, y = 0.99, xanchor = "left", yanchor = "top", bgcolor = 'rgba(255, 255, 255, 0.5)', bordercolor = 'black', borderwidth = 1),
    margin = list(l = 60, r = 50, b = 100, t = 80, pad = 0),
    showlegend = TRUE,
    annotations = list(
      list(
        x = 0.5,
        y = -0.15,
        xref = "paper",
        yref = "paper",
        text = 'Source: <a href="https://www.bls.gov/cpi/">U.S. Bureau of Labor Statistics</a> and <a href="https://www.dir.ca.gov/oprl/cpi/entireccpi.pdf">State of California - DIR</a>',
        showarrow = FALSE,
        font = list(size = 16)
      )
    )
  )

# Set the font size of the x-axis label
fig <- fig %>% layout(
  xaxis = list(titlefont = list(size = 16),title_standoff=5),
  yaxis = list(titlefont = list(size = 16),title_standoff=5)
)

# Display (preview)
fig

# Save
plotly::save_image(fig, "fig3/Figure 3.png", scale = 2)


# Figure 4 ----------------------------------------------------------------
# https://usatrade.census.gov/
china_trade <- read.csv("fig4/chinese_imports.csv", header = T, skip = 2)

# Reformat column names
colnames(china_trade) <- c("Country", "Year", "Value in US Dollars", "Not_used")

# Exclude incomplete year (2023)
china_trade      <- china_trade[!china_trade$Year == "2023 through March",]
china_trade$Year <- as.numeric(china_trade$Year)

# Make dollars numeric
china_trade$Trade_Value <- as.numeric(gsub(",","", china_trade$`Value in US Dollars`))

# Create a Plotly line chart

p <- plot_ly(china_trade, x = ~Year, y = ~Trade_Value, type = 'scatter', mode = 'lines', name = 'China',
             line = list(color = 'red')) %>%
  layout(title = "<b>Figure 4: US Imports from China (1992 - 2022)</b>",
         xaxis = list(title = "Year", range = c(1992, 2022), tickvals = seq(1992, 2022, by = 2), 
                      showline = TRUE, linewidth = 2, linecolor = 'black', mirror = TRUE),
         yaxis = list(title = "Trade Value (USD)", showline = TRUE, linewidth = 2, linecolor = 'black', mirror = TRUE),
         shapes = list(
           list(type = "rect", x0 = 2018, x1 = 2021, y0 = 0, y1 = max(china_trade$Trade_Value),
                xref = "x", yref = "y", fillcolor = "gray", opacity = 0.3, line = list(width = 0))
         ),
         annotations = list(
           # Add a source label with hyperlink
           list(xref = "paper", yref = "paper", x = 0, y = -0.2, showarrow = FALSE,
                text = "Source: <a href='https://usatrade.census.gov/'>USA Trade Online</a>", font = list(size = 12), xanchor = "left"),
           # Add a label for COVID-19
           list(x = 2020, y = 0.75 * max(china_trade$Trade_Value), xref = "x", yref = "y", showarrow = FALSE,
                text = "COVID-19 Impact", font = list(size = 12), bgcolor = "rgba(255, 255, 255, 0.7)")
         )
  )

# Print the plot
p

# Save
plotly::save_image(p, "fig4/Figure 4 - Chinese Imports.jpeg", scale = 2)
plotly::save_image(p, "fig4/Figure 4 - Chinese Imports.png", scale = 2)

# Figure 5 ----------------------------------------------------------------
# Figure 5: Foreign Direct Investment in the U.S.,Financial Inflow Transactions Without Current-Cost Adjustment 
# https://www.bea.gov/international/di1fdibal
print("Excel and PowerPoint were used for Figure 5.")

# Figure 6 ----------------------------------------------------------------
# Same as Fig 1 but national
# Foreign pop growth

# Data preparation
years <- c(2000, 2021)

foreign_population <- lapply(years, function(year) {
  if (year == 2000) {
    data <- get_decennial(geography   = "us", 
                          year        = year, 
                          table       = "P021",
                          cache_table = T, 
                          showll   = T, 
                          sumfile.    = "sf3")
  } else {
    data <- get_acs(geography = "us", variables = c("B05001_001", "B05001_002", 
                                                    "B05001_003", "B05001_004", 
                                                    "B05001_005", "B05001_006"),
                    year = year, cache_table = T)
  }
  data$year <- year
  data
}) %>% bind_rows()

foreign_population$estimate[is.na(foreign_population$estimate)] <- 
  foreign_population$value[is.na(foreign_population$estimate)]

foreign_population <- foreign_population %>%
  select(-value, -moe)

sf3  <- load_variables("sf3",  year = 2000)
acs5 <- load_variables("acs5", year = 2021)

pop_labeled <- merge(foreign_population,  sf3, by.x = "variable", by.y = "name", all.x = T)
pop_labeled <- merge(pop_labeled,        acs5, by.x = "variable", by.y = "name", all.x = T)

pop_labeled$label <- ifelse(is.na(pop_labeled$label.y),
                                  pop_labeled$label.x, pop_labeled$label.y)
pop_labeled$label <- gsub("!","-", pop_labeled$label)

pop_labeled <- pop_labeled[, !colnames(pop_labeled) %in% c("label.x",   "label.y",
                                                           "concept.x", "concept.y")]

# Filter relevant variables
foreigners <- pop_labeled[pop_labeled$variable %in% c("P021013",
                                                      "P021002",     "B05001_001",
                                                      "B05001_002",  "B05001_003", 
                                                      "B05001_004",  "B05001_005", 
                                                      "B05001_006"),]

# DC data
dc_2000 <- get_decennial(geography = "state", year = 2000, state = "DC", table = "P021",
                         cache_table = T, showll = T, sumfile = "sf3")
dc_2021 <- get_acs(geography = "state", variables = c("B05001_001", "B05001_002", 
                                                       "B05001_003", "B05001_004", 
                                                       "B05001_005", "B05001_006"),
                   year = 2021, state = "DC", cache_table = T)

dc_data <- rbind(
  data.frame(geo      = "District of Columbia",
             variable = dc_2000$variable,
             estimate = dc_2000$value,
             year     = 2000),
  data.frame(geo      = "District of Columbia",
             variable = dc_2021$variable,
             estimate = dc_2021$estimate,
             year     = 2021)
)

# Combine datasets
foreign_population$geo <- "United States"
foreign_population     <- bind_rows(foreign_population, dc_data)

# Calculate foreign population
foreign_population$foreign <- ifelse(foreign_population$variable %in% 
                                          c("P021013", "B05001_006", "B05001_005"),
                                        T, F)

foreign_population <- foreign_population[foreign_population$foreign %in% T,]

foreign_population  <- aggregate(estimate ~ year + geo, sum, 
                                    data = foreign_population)
foreign_population
saveRDS(foreign_population, "us_dc_foreign_population.RDS")

### Create the Plotly bar chart
# Create a custom function to round and format numbers with the appropriate suffix
format_number <- function(x) {
  if (x >= 1e6) {
    paste(round(x / 1e6, 1), "Mil.")
  } else {
    paste(round(x / 1e3, 0), "k")
  }
}

# Vectorize the format_number function
format_number_vec <- Vectorize(format_number)

# Define colors
old_glory_blue <- "#002147"
old_glory_red  <- "#BB133E"
  
# Sort the data by year and location
fig6_plot_data <- foreign_population %>%
  arrange(year, geo)

# Update the plot
fig6 <- plot_ly() %>%
  add_trace(data = fig6_plot_data %>% filter(geo == "United States"),
            x = ~paste(year, geo), y = ~(estimate),
            type = "bar", text = ~format_number_vec(estimate),
            hoverinfo = "text", textposition = "outside",
            insidetextanchor = "middle", textfont = list(size = 12),
            marker = list(color = old_glory_blue),
            name = "United States",
            yaxis = "y") %>%
  add_trace(data = fig6_plot_data %>% filter(geo == "District of Columbia"),
            x = ~paste(year, geo), y = ~(estimate), 
            type = "bar", text = ~format_number_vec(estimate),
            hoverinfo = "text", textposition = "outside",
            insidetextanchor = "middle", textfont = list(size = 12),
            marker = list(color = old_glory_red),
            name = "District of Columbia",
            yaxis = "y2") %>%
  layout(title = list(text = "<b>Figure 6: Foreign-born Population Growth in the<br>United States and District of Columbia (2000 - 2021)</b>",
                      font = list(size = 26, family = "Arial", face = "bold")),
         xaxis = list(title = list(text = "Year", font = list(size = 20),
                                   standoff = 20, x = 0.5, y = -0.1), 
                                   tickangle = 0, title_standoff = 20, 
                                   tickfont = list(size = 16),
                      tickvals = c("2000 United States", "2000 District of Columbia", 
                                   "2021 United States", "2021 District of Columbia"),
                      ticktext = c("2000", "2000", "2021", "2021")),
         yaxis = list(title = "Population (Millions)", titlefont = list(size = 15),
                      title_standoff = 5, tickfont = list(size = 14),
                      range = c(0, 1.4 * max(fig6_plot_data$estimate))),
         yaxis2 = list(title = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Population (Thousands)", overlaying = "y", 
                       side           = "right",
                       titlefont      = list(size = 14), 
                       title_standoff = 5, 
                       tickfont       = list(size = 12.5),
                       range          = c(0, 1.4 * max(
                                          fig6_plot_data$estimate[
                                            fig6_plot_data$geo == 
                                              "District of Columbia"]))),
         barmode = 'group',
         margin = list(l = 60, r = 50, b = 100, t = 125, pad = 0),
         legend = list(bgcolor = 'rgba(255, 255, 255, 0.5)', bordercolor = 'black', borderwidth = 1,
                       xanchor = "center", x = 0.75, y = 1.01, orientation = "v"), # Adjusted x and y values, and set orientation to vertical
         annotations = list(
           list(
             x = 1,
             y = -0.35,
             xref = "paper",
             yref = "paper",
             text = 'Source: <a href="https://data.census.gov/">U.S. Census Bureau</a>, Decennial Census and ACS',
             showarrow = FALSE,
             font      = list(size = 14),
             xanchor   = "right",
             yanchor   = "bottom"
           )))

# Display (preview)
fig6

# Save the image with a scale of 2
save_image(fig6, file = "fig6/figure6_us_dc_population_by_national_origin.jpeg", 
           scale = 2)
save_image(fig6, file = "fig6/figure6_us_dc_population_by_national_origin.png", 
           scale = 2)

saveRDS(fig6, "fig6/fig6.RDS")

# Figure 7 ----------------------------------------------------------------
wd <- getwd()
setwd("fig7")

## Raw data
# Cato, FreedomHouse, Fraser
cato   <- read.csv("cato/human-freedom-index-2022.csv")
fhouse <- read.csv("freedomhouse/All_data_FIW_2013-2023.csv")
fraser <- read.csv("fraser/human-freedom-index-2021-data-tables-figures.csv", skip = 3)
trans  <- read.csv("transparency/merged_cpi_data.csv")
  
cato   <- cato[cato$countries == "United States", colnames(cato) %in% c("year", "countries",
                                                                        "hf_rank")]
cato$Rank <- cato$hf_rank

fhouse$year  <- fhouse$Edition
fhouse_ranks <- fhouse %>%
  group_by(year) %>%
  mutate(Rank = rank(-Total, ties.method = "min")) %>%
  select(year, Rank, Country.Territory)
fhouse <- fhouse_ranks[fhouse_ranks$Country.Territory == "United States",  
                 colnames(fhouse_ranks) %in% c("year", "Country.Territory", "Rank")]

fraser <- fraser[fraser$ISO == "USA", colnames(fraser) %in% c("ISO", "Year",
                                                              "HUMAN.FREEDOM..RANK.")]
fraser$Rank <- fraser$HUMAN.FREEDOM..RANK.
fraser$year <- fraser$Year

trans <- trans[trans$Country == "United States of America", colnames(trans) %in%
                 c("Rank", "Year")]
trans <- rbind(trans, c(2020, 25)) # manual update
trans <- rbind(trans, c(2021, 27))
trans <- rbind(trans, c(2022, 24))

trans$year <- trans$Year
write.csv(trans, "transparency/trans_combined.csv")

# Heritage
heritage_files <- list.files("heritage", pattern = "index\\d+_data.xls", full.names = TRUE)
heritage_data  <- data.frame() # Holder

for (file_path in heritage_files) {
  year           <- gsub(".*index(\\d{4})_data\\.xls.*", "\\1", file_path) 
  temp_data      <- read_excel(file_path, sheet = 1, col_names = TRUE)
  temp_data      <- temp_data[, c("Country Name", "World Rank")]
  temp_data$year <- as.integer(year)
  cat("Columns in", file_path, ":")
  print(colnames(temp_data))
  heritage_data <- rbind(heritage_data, temp_data)
} # don't worry about the "new names" msg

heritage_data <- heritage_data[heritage_data$`Country Name` == "United States", ]
heritage_data <- heritage_data[!is.na(heritage_data$`Country Name`),]

heritage_data$Rank <- heritage_data$`World Rank`
write.csv(heritage_data, "heritage/heritage_combined.csv", row.names = FALSE)
str(heritage_data)

# Reporters without Borders
rwb_files      <- list.files("reporters_without_borders", pattern = "\\d{4}\\.csv", full.names = TRUE)
rwb_data_frame <- data.frame("ISO" = NA,"Rank" = NA, "year" = NA)  # Initialize a list to hold parsed data frame

for (file_path in rwb_files) {
  year <- gsub(".*reporters_without_borders/(\\d{4})\\.csv", "\\1", file_path)  # Extract the year from the file name
  lines <- readLines(file_path)
  
  # Parse data using "," delimiter
  data      <- read.delim2(text = lines, sep = ";")
  data$year <- year
  if (sum(colnames(data) == "Rank") == 0) {
    data$Rank <- data$Rank.N
  }
  data           <- data[data$ISO %in% c("USA", "USA2"), colnames(data) %in% c("ISO", "Rank", "year")]
  rwb_data_frame <- rbind(data, rwb_data_frame)
}

rwb_data_frame <- rwb_data_frame[!is.na(rwb_data_frame$ISO),]
rwb_data       <- rwb_data_frame[order(rwb_data_frame$year),]
rwb_data$ISO   <- "United States"
row.names(rwb_data) <- NULL
write.csv(rwb_data, "reporters_without_borders/rwb_combined.csv" )

# World Justice
wj      <- read.csv("world_justice/world_justice_historical.csv")
wj$Year <- as.integer(gsub(".*-(\\d{4})", "\\1", wj$Year))
wj_ranks <- wj %>%
  group_by(Year) %>%
  mutate(Rank = rank(-`WJP.Rule.of.Law.Index..Overall.Score`, ties.method = "min")) %>%
  filter(Country == "United States") %>%
  select(Year, Rank, Country)

wj_ranks$year <- wj_ranks$Year
print(wj_ranks)
write.csv(wj_ranks, "world_justice/world_justice_historical_final.csv")

## Combine data sources
cato$Source          <- "Cato Institute"
fhouse$Source        <- "FreedomHouse.org"
fraser$Source        <- "Fraser Institute"
heritage_data$Source <- "Heritage Foundation"
rwb_data$Source      <- "Reporters without Borders"
trans$Source         <- "Transparency International"
wj_ranks$Source      <- "World Justice Report"

cato$year          <- as.numeric(as.character(cato$year)) 
fhouse$year        <- as.numeric(as.character(fhouse$year))
fraser$year        <- as.numeric(as.character(fraser$year))
heritage_data$year <- as.numeric(as.character(heritage_data$year))
rwb_data$year      <- as.numeric(as.character(rwb_data$year))
trans$year         <- as.numeric(as.character(trans$year))
wj_ranks$year      <- as.numeric(as.character(wj_ranks$year))

combined <-  rbind(
  cato[,c('year', "Rank", "Source")],
  fhouse[,c("year", "Rank", "Source")],
  fraser[,c("year", "Rank", "Source")],
  heritage_data[,c("year", "Rank", "Source")],
  rwb_data[,c("year", "Rank", "Source")],
  trans[,c("year", "Rank", "Source")],
  wj_ranks[,c("year", "Rank", "Source")]
)

library(reshape2)
wide_data <- dcast(combined, year ~ Source, value.var = "Rank")

write.csv(wide_data, "combined.csv", row.names = F)

# Save
#plotly::save_image(p, "Figure 7.jpeg", scale = 2)
#plotly::save_image(p, "Figure 7.png", scale = 2)

setwd(wd)

# Figure 8 - Illegal Immigration -----------------------------------------------

### Recent data 
# CBP Data Portal - Southwest Land Border Encounters
sbo_encounters_fy19_fy22     <- read.csv("https://www.cbp.gov/sites/default/files/assets/documents/2022-Oct/sbo-encounters-fy19-fy22.csv")
sbo_encounters_fy20_fy23_jun <- read.csv("https://www.cbp.gov/sites/default/files/assets/documents/2023-Jul/sbo-encounters-fy20-fy23-jun.csv")

aggregate(sbo_encounters_fy19_fy22$Encounter.Count, by = list(
  sbo_encounters_fy19_fy22$Fiscal.Year
), sum)

fy19 <- sbo_encounters_fy19_fy22[sbo_encounters_fy19_fy22$Fiscal.Year == "2019", ]
fy19 <- aggregate(fy19$Encounter.Count, by = list(fy19$Encounter.Type,
                                                  fy19$Fiscal.Year,
                                                  fy19$Citizenship.Grouping), sum)

fy20_23_noinadmissibles <- sbo_encounters_fy20_fy23_jun[
                            !sbo_encounters_fy20_fy23_jun$Encounter.Type 
                            == "Inadmissibles",]

fy20_23 <- aggregate(fy20_23_noinadmissibles$Encounter.Count, 
                      by = list(fy20_23_noinadmissibles$Encounter.Type,
                                fy20_23_noinadmissibles$Fiscal.Year,
                                fy20_23_noinadmissibles$Citizenship.Grouping), sum)

fy19_23 <- rbind(fy20_23, fy19)

write.csv(fy19_23, "combined_fy19_23.csv", row.names = F)

# Historical data
url <- "https://www.cbp.gov/sites/default/files/assets/documents/2019-Mar/bp-southwest-border-sector-apps-fy1960-fy2018.pdf"

border_data <- tabulizer::extract_tables(url)
border_data <- as.data.frame(border_data)

colnames(border_data) <- gsub("\r", " ", gsub("\n", " ", border_data[1,]))
border_data <- border_data[-1,]


# Combine new and old data
new <- aggregate(fy19_23$x, by = list(fy19_23$Group.2), sum)

colnames(new) <- c("Fiscal Year", "Southwest Border Total")
border_data   <- border_data[, c("Fiscal Year", "Southwest Border Total")]

all <- rbind(new, border_data)

all$`Southwest Border Total` <- gsub(",", "", all$`Southwest Border Total`)

# Let's project 2023
sum(sbo_encounters_fy20_fy23_jun$Encounter.Count[
  sbo_encounters_fy20_fy23_jun$Fiscal.Year == "2022"
]) # Total for 2022

months_avail_in_2023 <- unique(sbo_encounters_fy20_fy23_jun$Month..abbv.[
                          sbo_encounters_fy20_fy23_jun$Fiscal.Year == "2023 (FYTD)"])

old_partial <- sum(
  sbo_encounters_fy20_fy23_jun$Encounter.Count[
    sbo_encounters_fy20_fy23_jun$Fiscal.Year == "2022" & 
      sbo_encounters_fy20_fy23_jun$Month..abbv. %in% 
      months_avail_in_2023]
) # total in 2022 by this point in the FY

old_total <- sum(
  sbo_encounters_fy20_fy23_jun$Encounter.Count[
    sbo_encounters_fy20_fy23_jun$Fiscal.Year == "2022"] 
) # total in 2022 

ratio <- old_partial / old_total # 0.7344217 - 73.4% complete

partial_2023 <- sum(sbo_encounters_fy20_fy23_jun$Encounter.Count[
  sbo_encounters_fy20_fy23_jun$Fiscal.Year == "2023 (FYTD)"
]) # Total for 2023 so far

projection_2023 <- round(partial_2023 + partial_2023 * (1 - ratio))
projection_2023 <- data.frame("Fiscal Year" = 2023, 
                               "Southwest Border Total" = projection_2023,
                              check.names = F)

# Replace the partial 2023 with the projection
all <- all[!all$`Fiscal Year` == "2023 (FYTD)",]
all <- rbind(all, projection_2023)
all <- all[order(all$`Fiscal Year`),]

# Save the data set
write.csv(all, "fig8/border_data.csv", row.names = F)

# Figure N ----------------------------------------------------------------
# Read in the data
surveillance <- read.csv("https://www.eff.org/files/2017/01/04/2016-us-govt-transparency-report-data.csv", stringsAsFactors = FALSE)

# Clean and format the data
surveillance_clean <- surveillance %>%
  filter(between(year, 2010, 2016)) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"), "%Y-%m-%d")) %>%
  select(date, type, company, total) %>%
  mutate(type = ifelse(type == "FISA", "FISA Requests", "NSLs"))

# Create a stacked area chart
ggplot(surveillance_clean, aes(x = date, y = total, fill = type)) +
  geom_area() +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  labs(title = "US Government Surveillance Requests to Tech Companies",
       subtitle = "2010-2016",
       x = "Year",
       y = "Number of Requests",
       fill = "Type of Request") +
  theme_tufte(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.box.background = element_rect(color = "black", fill = "white"),
        legend.title = element_text(face = "bold")) 


# Display (preview)
p

# Display (preview)
p

# Save
#plotly::save_image(p, "Figure 8.jpeg", scale = 2)
#plotly::save_image(p, "Figure 8.png", scale = 2)

# Figure 9 -----------------------------------------------------------------
h1b <- read.csv("fig9/h1b_datahubexport-2022.csv")

h1b$Employer[h1b$Employer == ""] <- "Missing / Unknown"

# Drop unnecessary columns
data <- h1b %>% select(-City, -State, -Tax.ID, -ZIP)

# Convert employer names to uppercase
data$Employer <- toupper(data$Employer)

# Aggregate by employer and sum approvals
agg_data <- data %>%
  group_by(Employer) %>%
  summarise_at(vars(starts_with("Initial"), starts_with("Continuing")), sum)

agg_data$total_approved <- agg_data$Initial.Approval +
                           agg_data$Continuing.Approval

# Sort by total approvals
sorted_data <- agg_data %>% arrange(desc(total_approved))

colnames(sorted_data) <- gsub(".", " ", colnames(sorted_data),
                              fixed = T)

# Dedupe / merge - the original data set is shockingly dirty
#     some of the distinctions between similarlly named enitities of the 
#     same effective company in the data set may have minor business
#     organizational significance (not relevant in this context) but
#     most cases involved things that were more clearly typographical errors.
#     For example, results split between rows for "Big Company Name, Inc." and
#     "Big Company Name Inc.", or "Big Company Name INc.".
#     Of course, I didn't apire to clean every record. I just focused on the 
#     top companies.
sorted_data$Employer[grepl("AMAZO", sorted_data$Employer)] <- "AMAZON"
sorted_data$Employer[grepl("PRICEWATERHOUSECOOPERS", sorted_data$Employer)] <- "PWC"
sorted_data$Employer[grepl("PWC", sorted_data$Employer)]    <- "PWC"
sorted_data$Employer[grepl("FACEBO", sorted_data$Employer)] <- "META"
sorted_data$Employer[grepl("META P", sorted_data$Employer)] <- "META"
sorted_data$Employer[grepl("PAYPAL", sorted_data$Employer)] <- "PAYPAL"
sorted_data$Employer[grepl("TATA CON", sorted_data$Employer)]  <- "TATA GROUP"
sorted_data$Employer[grepl("TATA TECH", sorted_data$Employer)] <- "TATA GROUP"
sorted_data$Employer[grepl("TATA COM", sorted_data$Employer)]  <- "TATA GROUP"
sorted_data$Employer[grepl("TATA AME", sorted_data$Employer)]  <- "TATA GROUP"
sorted_data$Employer[grepl("TATA ELX", sorted_data$Employer)]  <- "TATA GROUP"
sorted_data$Employer[grepl("INFOSYS L", sorted_data$Employer)] <- "INFOSYS"
sorted_data$Employer[grepl("INFOSYS B", sorted_data$Employer)] <- "INFOSYS"

final <- sorted_data %>%
  group_by(Employer) %>%
  summarise_at(vars(starts_with("Initial"), starts_with("Continuing"), 
                    total_approved), sum)  %>% 
  arrange(desc(total_approved))

# Save sorted data to a new CSV file
write.csv(final, file = 'fig9/fy2022_h1b.csv', row.names = FALSE)


# Figure 10 ----------------------------------------------------------------
# Figure 10 uses the EFF's spreadsheet + PowerPoint
print("Excel and PowerPoint were used for Figure 10.")

# Figure 11 ----------------------------------------------------------------
# This uses a Python script I originally developed for aliendb.org
# The name of the script is timeline_for_book.py in the fig11 folder
print("Python was used for Figure 11 (see the fig11 folder).")
