# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------
################################################################################
##################################   Packages   ################################
################################################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

# Loading Packages -------------------------------------------------------------

pacman::p_load(readxl,tidymodels,tidyverse,lubridate,dplyr,caret,janitor,tidyr, 
               readr, skimr,forecast,e1071,xgboost,patchwork,shiny, 
               caret, vip, Metrics, yardstick,tseries, ggrepel)

"
Fold All	Alt+O
Unfold All	Shift+Alt+O
"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------
################################################################################
##############################   Loading Datasets  #############################
################################################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

# Loading Datasets - Unpropagated ----------------------------------------------

load_unpro <- function(file_path) {
  df <- tibble(read.csv(file_path))
  
  df <- df %>%
    mutate(X = as.POSIXct(X, format = "%Y-%m-%d %H:%M:%S")) %>%
    rename(datetime = X) %>%
    filter(!is.na(datetime)) %>%
    clean_names()
  
  return(df)
}

cryosat_2<- load_unpro("unpropagated_elements_CryoSat-2.csv")
haiyang_2a<- load_unpro("unpropagated_elements_Haiyang-2A.csv")
jason_3 <- load_unpro("unpropagated_elements_Jason-3.csv")
saral<- load_unpro("unpropagated_elements_SARAL.csv")
sentinel_3a<- load_unpro("unpropagated_elements_Sentinel-3A.csv")

cryosat_2_raw <- load_unpro("unpropagated_elements_CryoSat-2.csv")
haiyang_2a_raw <- load_unpro("unpropagated_elements_Haiyang-2A.csv")
jason_3_raw <- load_unpro("unpropagated_elements_Jason-3.csv")
saral_raw <- load_unpro("unpropagated_elements_SARAL.csv")
sentinel_3a_raw <- load_unpro("unpropagated_elements_Sentinel-3A.csv")

# Loading Datasets - Manoeuvres ------------------------------------------------

load_man <- function(filepath) {
  read.table(filepath, header = FALSE, sep = "\t", stringsAsFactors = FALSE) %>%
    mutate(Extracted = str_extract(V1, "(\\d{4} \\d{3} \\d{2} \\d{2} \\d{4} \\d{3} \\d{2} \\d{2})")) %>%
    select(Extracted) %>%
    separate(Extracted, into = c("b_y", "b_doy", "b_h", "b_m", "e_y", "e_doy", "e_h", "e_m"), sep = " ") %>%
    mutate(across(everything(), as.numeric)) %>%
    mutate(
      begin_date = as.Date(paste(b_y, b_doy, sep = " "), format = "%Y %j"),
      end_date = as.Date(paste(e_y, e_doy, sep = " "), format = "%Y %j"),
      begin_datetime = make_datetime(b_y, month(begin_date), day(begin_date), b_h, b_m),
      end_datetime = make_datetime(e_y, month(end_date), day(end_date), e_h, e_m)) %>%
    select(begin_datetime, end_datetime) %>%
    mutate(gap_hours = as.numeric(difftime(end_datetime, begin_datetime, units = "hour"))) %>%
    tibble() 
}

cryosat_2_man <- load_man("cs2man.txt")
haiyang_2a_man <- load_man("h2aman.txt")
jason_3_man <- load_man("ja3man.txt")
saral_man <- load_man("srlman.txt")
sentinel_3a_man <- load_man("s3aman.txt")

cryosat_2_man_raw <- load_man("cs2man.txt")
haiyang_2a_man_raw <- load_man("h2aman.txt")
jason_3_man_raw <- load_man("ja3man.txt")
saral_man_raw <- load_man("srlman.txt")
sentinel_3a_man_raw <- load_man("s3aman.txt")

# Checking duration

max_duration_table <- function() {
  # List of datasets loaded with load_man
  datasets <- list(
    cryosat_2_man = cryosat_2_man_raw,
    haiyang_2a_man= haiyang_2a_man_raw,
    jason_3_man = jason_3_man_raw,
    saral_man = saral_man_raw,
    sentinel_3a_man = sentinel_3a_man_raw
  )
  
  # Calculate the max duration for each dataset
  max_durations <- sapply(datasets, function(data) {
    max(data$gap_hours, na.rm = TRUE)
  })
  
  # Create the result table
  result_table <- tibble(
    dataset = names(max_durations),
    max_gap_hours = round(max_durations, 2)
  )
  
  return(result_table)
}

max_duration_table()

"
no man duration is longer than a day, 
therefore, all begin and end datetime will be merged into one datetime
"
# Finalizing man data

finalize_man <- function(data) {
  data %>%
    select(datetime = !!names(.)[1])
}

cryosat_2_man <- finalize_man(cryosat_2_man)
haiyang_2a_man <- finalize_man(haiyang_2a_man)
jason_3_man <- finalize_man(jason_3_man)
saral_man <- finalize_man(saral_man)
sentinel_3a_man <- finalize_man(sentinel_3a_man)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------
################################################################################
####################################   EDA   ###################################
################################################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

# EDA - Orbital Elements -------------------------------------------------------

# Orbital Element Summary Function
unpro_summary <- function(data) {
  data %>%
    summarize(
      start_date = as.Date(min(datetime)),
      end_date = as.Date(max(datetime)),
      man_total = n(),
      year = as.numeric(difftime(max(datetime), min(datetime), units = "days")) / 365,
      month = as.numeric(difftime(max(datetime), min(datetime), units = "days")) / 30
    ) %>%
    mutate(
      year = round(year),
      month = round(month),
    )
}

# Apply the function to each dataset
cryosat_2_unpro_summary <- unpro_summary(cryosat_2_raw)
haiyang_2a_unpro_summary <- unpro_summary(haiyang_2a_raw)
jason_3_unpro_summary <- unpro_summary(jason_3_raw)
saral_unpro_summary <- unpro_summary(saral_raw)
sentinel_3a_unpro_summary <- unpro_summary(sentinel_3a_raw)

# Combine the results into one table
combined_unpro_summary <- bind_rows(
  cryosat_2_unpro_summary %>% mutate(dataset = "CryoSat-2"),
  haiyang_2a_unpro_summary %>% mutate(dataset = "Haiyang-2A"),
  jason_3_unpro_summary %>% mutate(dataset = "Jason-3"),
  saral_unpro_summary %>% mutate(dataset = "SARAL"),
  sentinel_3a_unpro_summary %>% mutate(dataset = "Sentinel-3A")
)

combined_unpro_summary <- 
  combined_unpro_summary %>% 
  select(dataset, everything())

# Line plots for only first 2 years

line_plots_combined <- function(dataset, title = "Dataset Plot") {
  
  # Filter the dataset to include only the first three years of data
  start_date <- min(dataset$datetime)
  end_date <- start_date + years(2)
  dataset_filtered <- dataset %>%
    filter(datetime >= start_date & datetime < end_date)
  
  p_right_ascension <- ggplot(dataset_filtered, aes(x = datetime, y = right_ascension)) +
    geom_line() +
    labs(title = "Right Ascension") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 16, face = "bold")) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
  
  p_brouwer_mean_motion <- ggplot(dataset_filtered, aes(x = datetime, y = brouwer_mean_motion)) +
    geom_line() +
    labs(title = "Brouwer Mean Motion") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 16, face = "bold")) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
  
  p_eccentricity <- ggplot(dataset_filtered, aes(x = datetime, y = eccentricity)) +
    geom_line() +
    labs(title = "Eccentricity") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 16, face = "bold")) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
  
  p_argument_of_perigee <- ggplot(dataset_filtered, aes(x = datetime, y = argument_of_perigee)) +
    geom_line() +
    labs(title = "Argument of Perigee") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 16, face = "bold")) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
  
  p_inclination <- ggplot(dataset_filtered, aes(x = datetime, y = inclination)) +
    geom_line() +
    labs(title = "Inclination") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 16, face = "bold")) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
  
  p_mean_anomaly <- ggplot(dataset_filtered, aes(x = datetime, y = mean_anomaly)) +
    geom_line() +
    labs(title = "Mean Anomaly") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 16, face = "bold")) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
  
  plot_combined <- 
    (p_eccentricity | p_inclination | p_right_ascension) / 
    (p_argument_of_perigee | p_mean_anomaly | p_brouwer_mean_motion)
  
  plot_combined + 
    plot_annotation(
      title = title,
      theme = theme(
        plot.title = element_text(size = 24, face = "bold")
      )
    )
}

line_plot <- function(dataset, element, title = "Dataset Plot") {
  
  plot <- ggplot(dataset, aes(x = datetime, y = .data[[element]])) +
    geom_line() +
    labs(title = title) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_blank(),
          axis.text.x = element_text(size = 20, face = "bold"),
          axis.text.y = element_text(size = 18, face = "bold")) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 2))
  
  return(plot)
  
}

line_cryosat_2_eda <- line_plots_combined(cryosat_2_raw, "CryoSat-2 Orbital Elements Line Plot")
line_haiyang_2a_eda <- line_plots_combined(haiyang_2a_raw, "Haiyang-2A Orbital Elements Line Plot")
line_jason_3_eda <- line_plots_combined(jason_3_raw, "Jason-3 Orbital Elements Line Plot")
line_saral_eda <- line_plots_combined(saral_raw, "SARAL Orbital Elements Line Plot")
line_sentinel_3a_eda <- line_plots_combined(sentinel_3a_raw, "Sentinel-3A Orbital Elements Line Plot")

# CryoSat-2:
line_plot(cryosat_2_raw, "argument_of_perigee", "CryoSat 2 - Argument of Perigee")
line_plot(cryosat_2_raw, "brouwer_mean_motion", "CryoSat 2 - Brouwer Mean Motion")
line_plot(cryosat_2_raw, "eccentricity", "CryoSat 2 - Eccentricity")
line_plot(cryosat_2_raw, "inclination", "CryoSat 2 - Inclination")
line_plot(cryosat_2_raw, "mean_anomaly", "CryoSat 2 - Mean Anomaly")
line_plot(cryosat_2_raw, "right_ascension", "CryoSat 2 - Right Ascension")

# Haiyang-2A:
line_plot(haiyang_2a_raw, "argument_of_perigee", "Haiyang 2A - Argument of Perigee")
line_plot(haiyang_2a_raw, "brouwer_mean_motion", "Haiyang 2A - Brouwer Mean Motion")
line_plot(haiyang_2a_raw, "eccentricity", "Haiyang 2A - Eccentricity")
line_plot(haiyang_2a_raw, "inclination", "Haiyang 2A - Inclination")
line_plot(haiyang_2a_raw, "mean_anomaly", "Haiyang 2A - Mean Anomaly")
line_plot(haiyang_2a_raw, "right_ascension", "Haiyang 2A - Right Ascension")

# Jason-3:
line_plot(jason_3_raw, "argument_of_perigee", "Jason 3 - Argument of Perigee")
line_plot(jason_3_raw, "brouwer_mean_motion", "Jason 3 - Brouwer Mean Motion")
line_plot(jason_3_raw, "eccentricity", "Jason 3 - Eccentricity")
line_plot(jason_3_raw, "inclination", "Jason 3 - Inclination")
line_plot(jason_3_raw, "mean_anomaly", "Jason 3 - Mean Anomaly")
line_plot(jason_3_raw, "right_ascension", "Jason 3 - Right Ascension")

# SARAL:
line_plot(saral_raw, "argument_of_perigee", "SARAL - Argument of Perigee")
line_plot(saral_raw, "brouwer_mean_motion", "SARAL - Brouwer Mean Motion")
line_plot(saral_raw, "eccentricity", "SARAL - Eccentricity")
line_plot(saral_raw, "inclination", "SARAL - Inclination")
line_plot(saral_raw, "mean_anomaly", "SARAL - Mean Anomaly")
line_plot(saral_raw, "right_ascension", "SARAL - Right Ascension")

# Sentinel-3A:
line_plot(sentinel_3a_raw, "argument_of_perigee", "Sentinel 3A - Argument of Perigee")
line_plot(sentinel_3a_raw, "brouwer_mean_motion", "Sentinel 3A - Brouwer Mean Motion")
line_plot(sentinel_3a_raw, "eccentricity", "Sentinel 3A - Eccentricity")
line_plot(sentinel_3a_raw, "inclination", "Sentinel 3A - Inclination")
line_plot(sentinel_3a_raw, "mean_anomaly", "Sentinel 3A - Mean Anomaly")
line_plot(sentinel_3a_raw, "right_ascension", "Sentinel 3A - Right Ascension")

### export at W:1050 H:525 ###

# EDA - Manoeuvres -------------------------------------------------------------

# Manoeuvres Summary Function
man_summary <- function(data) {
  data %>%
    summarize(
      start_date = as.Date(min(datetime)),
      end_date = as.Date(max(datetime)),
      man_total = n(),
      year = as.numeric(difftime(max(datetime), min(datetime), units = "days")) / 365,
      month = as.numeric(difftime(max(datetime), min(datetime), units = "days")) / 30
    ) %>%
    mutate(
      year = round(year),
      month = round(month),
      man_per_year = round((man_total / year),1)
    )
}

# Generating summary for each man data
cryosat_2_man_summary <- man_summary(cryosat_2_man)
haiyang_2a_man_summary <- man_summary(haiyang_2a_man)
jason_3_man_summary <- man_summary(jason_3_man)
saral_man_summary <- man_summary(saral_man)
sentinel_3a_man_summary <- man_summary(sentinel_3a_man)

# Combine each summary into one table summary
combined_man_summary <- bind_rows(
  cryosat_2_man_summary %>% mutate(dataset = "CryoSat-2"),
  haiyang_2a_man_summary %>% mutate(dataset = "Haiyang-2A"),
  jason_3_man_summary %>% mutate(dataset = "Jason-3"),
  saral_man_summary %>% mutate(dataset = "SARAL"),
  sentinel_3a_man_summary %>% mutate(dataset = "Sentinel-3A")
)

combined_man_summary <- 
  combined_man_summary %>% 
  select(dataset, everything())

# Create month bar plots


cryosat_2_man_barplot_m <- bar_m_plots(cryosat_2_man)
haiyang_2a_man_barplot_m <- bar_m_plots(haiyang_2a_man)
jason_3_man_barplot_m <- bar_m_plots(jason_3_man)
saral_man_barplot_m <- bar_m_plots(saral_man)
sentinel_3a_man_barplot_m <- bar_m_plots(sentinel_3a_man)

# Create year bar plots
bar_y_plots  <- function(data, title) {
  data %>%
    mutate(year = format(datetime, "%Y")) %>%  # Extract year
    count(year, name = "count") %>%  # Count occurrences per year
    ggplot(aes(x = year, y = count)) +
    geom_bar(stat = "identity", colour = "black", fill = "grey") +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 18, face = "bold"),
      axis.text.y = element_text(size = 18, face = "bold"),
      axis.title.x = element_text(size = 18, face = "bold"),
      plot.title = element_blank()
    ) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1))
}

cryosat_2_man_barplot_y <- bar_y_plots(cryosat_2_man, "CryoSat-2 Manoeuvres by Year & Month")
haiyang_2a_man_barplot_y <- bar_y_plots(haiyang_2a_man, "Haiyang-2A Manoeuvres by Year & Month")
jason_3_man_barplot_y <- bar_y_plots(jason_3_man, "Jason-3 Manoeuvres by Year & Month")
saral_man_barplot_y <- bar_y_plots(saral_man, "SARAL Manoeuvres by Year & Month")
sentinel_3a_man_barplot_y <- bar_y_plots(sentinel_3a_man, "Sentinel-3A Manoeuvres by Year & Month")

# Create month / year bar plots

cryosat_2_man_barplot <- cryosat_2_man_barplot_y / cryosat_2_man_barplot_m
haiyang_2a_man_barplot <- haiyang_2a_man_barplot_y / haiyang_2a_man_barplot_m
jason_3_man_barplot <- jason_3_man_barplot_y / jason_3_man_barplot_m
saral_man_barplot <-  saral_man_barplot_y/ saral_man_barplot_m
sentinel_3a_man_barplot <- sentinel_3a_man_barplot_y / sentinel_3a_man_barplot_m

# export at W:1050 H:525

# EDA - Orbital Elements VS Manoeuvres -----------------------------------------

# Line Plot Function
line_plot_with_man <- function(dataset, dataset_man, beg_date, end_date, element,
                               title = "Dataset Plot") {
  
  # Filter dataset based on the date range
  dataset <- dataset %>% 
    filter(datetime >= beg_date & datetime <= end_date)
  
  # Get min and max datetime for the dataset
  min_datetime <- min(dataset$datetime)
  max_datetime <- max(dataset$datetime)
  
  # Filter maneuver dataset based on the same date range
  dataset_man <- dataset_man %>%
    filter(datetime >= min_datetime & datetime <= max_datetime)
  
  # Plot the data
  ggplot() +
    # Solid line for the specified orbital element
    geom_line(data = dataset, aes(x = datetime, y = !!sym(element), color = element), 
              size = 1) +
    # Dashed line for maneuver events
    geom_vline(data = dataset_man, aes(xintercept = as.numeric(datetime), linetype = "Manoeuvre"), 
               color = "red3", size = 1) +
    labs(title = paste(title, "-", element), color = "Legend", linetype = "Legend") +  # Include element name in the title
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 18, face = "bold"),
          axis.text.y = element_text(size = 18, face = "bold"), 
          legend.text = element_text(size = 14, face = "bold"),
          plot.title = element_blank(),  
          legend.position = "bottom",     # Position the legend at the bottom
          legend.title = element_blank(),  # Remove the legend title
          legend.key.size = unit(1.5, "cm")) +  # Increase legend key size
    scale_x_datetime(date_breaks = "2 months", date_labels = "%y-%b") +  # Correct date breaks
    scale_y_continuous(labels = label_number(accuracy = 0.0001)) +     
    scale_color_manual(values = setNames("black", element)) +  # Set color for the orbital element
    scale_linetype_manual(values = c("Manoeuvre" = "dashed"))
}


# Gnerating Line Plots
line_plot_with_man(cryosat_2, cryosat_2_man,"2011-01-01", "2011-04-30",
                   "brouwer_mean_motion", title = "CryoSat-2 Brouwer Mean Motion")
line_plot_with_man(cryosat_2, cryosat_2_man,"2011-01-01", "2011-12-31",
                   "mean_anomaly", title = "CryoSat-2 Mean Anomaly")
line_plot_with_man(cryosat_2, cryosat_2_man,"2011-01-01", "2011-12-31",
                   "inclination", title = "CryoSat-2 Inclination")
line_plot_with_man(jason_3, jason_3_man,"2017-04-01", "2017-05-01",
                   "brouwer_mean_motion", title = "CryoSat-2 Brouwer Mean Motion")
line_plot_with_man(saral, saral_man,"2015-01-01", "2015-03-01",
                   "brouwer_mean_motion", title = "CryoSat-2 Brouwer Mean Motion")
line_plot_with_man(sentinel_3a, sentinel_3a_man,"2017-01-01", "2017-12-31",
                   "brouwer_mean_motion", title = "Sentinel-3A Brouwer Mean Motion")
line_plot_with_man(sentinel_3a, sentinel_3a_man,"2017-01-01", "2017-12-31",
                   "mean_anomaly", title = "Sentinel-3A Mean Anomaly")
line_plot_with_man(sentinel_3a, sentinel_3a_man,"2017-01-01", "2017-12-31",
                   "argument_of_perigee", title = "Sentinel-3A Argument of Perigee")
line_plot_with_man(sentinel_3a, sentinel_3a_man,"2017-01-01", "2017-12-31",
                   "inclination", title = "Sentinel-3A Inclination")
line_plot_with_man(sentinel_3a, sentinel_3a_man,"2017-01-01", "2017-12-31",
                   "eccentricity", title = "Sentinel-3A Eccentricity")
line_plot_with_man(sentinel_3a, sentinel_3a_man,"2017-01-01", "2017-12-31",
                   "right_ascension", title = "Sentinel-3A Right Ascension")




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------
################################################################################
###############################    Pre-processing    ###########################
################################################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

# Pre-processing - Treating Unstable Period ------------------------------------

# Funtion
filtering_data <- function(dataset, start_date, end_date) {

  filtered_data <- dataset %>%
    filter(datetime >= as.Date(start_date) & datetime <= as.Date(end_date))
  
  return(filtered_data)
}

# Applying
cryosat_2 <- filtering_data(cryosat_2, "2010-07-01", "2020-06-01")
jason_3 <- filtering_data(jason_3, "2016-08-01", "2022-10-03")
saral <- filtering_data(saral, "2013-11-01", "2022-09-14")
sentinel_3a <- filtering_data(sentinel_3a, "2016-08-01", "2021-12-01")

cryosat_2_man <- filtering_data(cryosat_2_man, "2010-07-01", "2020-06-01")
jason_3_man <- filtering_data(jason_3_man, "2016-08-01", "2022-10-03")
saral_man <- filtering_data(saral_man, "2013-11-01", "2022-09-14")
sentinel_3a_man <- filtering_data(sentinel_3a_man, "2016-08-01", "2021-12-01")

# Pre-processing - Extreme outliers --------------------------------------------

# Funtions
find_outliers <- function(dataset, extreme = 3) {
  
  # Internal function to find outliers in a vector
  find_outliers <- function(data) {
    data <- data[!is.na(data)]
    lowerq = quantile(data, probs = 0.25)
    upperq = quantile(data, probs = 0.75)
    iqr = upperq - lowerq # Or use IQR(data)
    # Identify extreme outliers
    extreme.threshold.upper = (iqr * extreme) + upperq
    extreme.threshold.lower = lowerq - (iqr * extreme)
    result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
    
    return(result)
  }
  
  # List to store outlier indices
  outlier_indices <- integer(0)
  
  # Iterate over each column except the datetime
  for (col in names(dataset)[-1]) {
    outliers <- find_outliers(dataset[[col]])
    outlier_indices <- c(outlier_indices, outliers)
  }
  
  # Get unique outlier indices
  outlier_indices <- unique(outlier_indices)
  
  # Return the datetimes corresponding to outlier indices
  result <- tibble(dataset$datetime[outlier_indices]) %>% 
    rename(datetime = `dataset$datetime[outlier_indices]`)

  return(result)
}
outliers_percentage <- function() {
  table_data <- data.frame(
    Dataset = c("CryoSat-2", 
                "Sentinel-3A", 
                "SARAL",
                "Haiyang-2A",
                "Jason-3"
                ),
    Original = c(nrow(cryosat_2),
                 nrow(sentinel_3a),
                 nrow(saral),
                 nrow(haiyang_2a),
                 nrow(jason_3)
                 ),
    Outliers = c(nrow(find_outliers(cryosat_2)),
                 nrow(find_outliers(sentinel_3a)),
                 nrow(find_outliers(saral)),
                 nrow(find_outliers(haiyang_2a)),
                 nrow(find_outliers(jason_3))
                 ),
    Percentage = c(round(nrow(find_outliers(cryosat_2))/nrow(cryosat_2) * 100,2), 
                   round(nrow(find_outliers(sentinel_3a))/nrow(sentinel_3a) * 100,2),
                   round(nrow(find_outliers(saral))/nrow(saral) * 100,2),
                   round(nrow(find_outliers(haiyang_2a))/nrow(haiyang_2a) * 100,2),
                   round(nrow(find_outliers(jason_3))/nrow(jason_3) * 100,2)
    )
  )
  return(table_data)
}
remove_outliers <- function(dataset, extreme = 3) {
  outlier_datetimes <- find_outliers(dataset, extreme)
  cleaned_dataset <- dataset %>% filter(!datetime %in% outlier_datetimes$datetime)
  return(cleaned_dataset)
}


# Finding Outliers
outliers_percentage()

# Removing Outliers
cryosat_2 <- remove_outliers(cryosat_2)
haiyang_2a <- remove_outliers(haiyang_2a)
jason_3 <- remove_outliers(jason_3)


# Pre-processing - Data Normalization ------------------------------------------

# Function
data_norm <- function(df) {
  
  # Combine columns, scale variables, and convert to data frame
  temp <- as.data.frame(cbind(
    eccentricity = scale(df$eccentricity),
    argument_of_perigee = scale(df$argument_of_perigee),
    inclination = scale(df$inclination),
    mean_anomaly = scale(df$mean_anomaly),
    brouwer_mean_motion = scale(df$brouwer_mean_motion),
    right_ascension = scale(df$right_ascension)
  ))
  
  colnames(temp) <- c("eccentricity", "argument_of_perigee", "inclination", 
                      "mean_anomaly", "brouwer_mean_motion", "right_ascension")
  data <- df %>% 
    select(datetime) %>%
    cbind(temp) %>%
    as_tibble()
  
  return(data)
}

# Appliying
cryosat_2 <- data_norm(cryosat_2)
haiyang_2a <- data_norm(haiyang_2a)
jason_3 <- data_norm(jason_3)
saral <- data_norm(saral)
sentinel_3a <- data_norm(sentinel_3a)

# Pre-processing - Data Engineering - lag & diff terms -------------------------

# Original VS. Diff-1

par(mfrow = c(1, 2))

acf(haiyang_2a_bmm$brouwer_mean_motion, type = "correlation", 
    main = "Original", 
    xlim = c(1, 30), ylim = c(-1, 1), 
    cex.main = 1.5,   # Increase title size
    cex.lab = 1.5,    # Increase x and y labels size
    cex.axis = 1.5)   # Increase axis text size

acf(haiyang_2a_bmm$diff_1, type = "correlation", 
    main = "Diff-1", 
    xlim = c(1, 30), ylim = c(-1, 1), 
    cex.main = 1.5, 
    cex.lab = 1.5, 
    cex.axis = 1.5) 

par(mfrow = c(1, 1))


# Function of create lag and diff terms
create_lag_diff_terms <- function(data, column, max_lag = 2) {
  result <- data %>%
    select(datetime, all_of(column))
  
  for (i in 1:max_lag) {
    lag_col <- paste0("lag_", i)
    diff_col <- paste0("diff_", i)
    result <- result %>%
      mutate(!!lag_col := lag(!!as.name(column), n = i)) %>%
      mutate(!!diff_col := !!as.name(column) - !!as.name(lag_col))
  }
  
  result <- result %>% na.omit()
  
  return(result)
}

# Applying - CryoSat-2
cryosat_2_ecc <- create_lag_diff_terms(cryosat_2, "eccentricity")
cryosat_2_aop <- create_lag_diff_terms(cryosat_2, "argument_of_perigee")
cryosat_2_inc <- create_lag_diff_terms(cryosat_2, "inclination")
cryosat_2_ma <- create_lag_diff_terms(cryosat_2, "mean_anomaly")
cryosat_2_bmm <- create_lag_diff_terms(cryosat_2, "brouwer_mean_motion")
cryosat_2_ra  <- create_lag_diff_terms(cryosat_2, "right_ascension")

# Applying - Haiyang-2A
haiyang_2a_ecc <- create_lag_diff_terms(haiyang_2a, "eccentricity")
haiyang_2a_aop <- create_lag_diff_terms(haiyang_2a, "argument_of_perigee")
haiyang_2a_inc <- create_lag_diff_terms(haiyang_2a, "inclination")
haiyang_2a_ma  <- create_lag_diff_terms(haiyang_2a, "mean_anomaly")
haiyang_2a_bmm <- create_lag_diff_terms(haiyang_2a, "brouwer_mean_motion")
haiyang_2a_ra  <- create_lag_diff_terms(haiyang_2a, "right_ascension")

# Applying - Jason-3
jason_3_ecc <- create_lag_diff_terms(jason_3, "eccentricity")
jason_3_aop <- create_lag_diff_terms(jason_3, "argument_of_perigee")
jason_3_inc <- create_lag_diff_terms(jason_3, "inclination")
jason_3_ma <- create_lag_diff_terms(jason_3, "mean_anomaly")
jason_3_bmm <- create_lag_diff_terms(jason_3, "brouwer_mean_motion")
jason_3_ra <- create_lag_diff_terms(jason_3, "right_ascension")

# Applying - SARAL
saral_ecc <- create_lag_diff_terms(saral, "eccentricity")
saral_aop <- create_lag_diff_terms(saral, "argument_of_perigee")
saral_inc <- create_lag_diff_terms(saral, "inclination")
saral_ma  <- create_lag_diff_terms(saral, "mean_anomaly")
saral_bmm <- create_lag_diff_terms(saral, "brouwer_mean_motion")
saral_ra  <- create_lag_diff_terms(saral, "right_ascension")

# Applying - Sentinel-3A
sentinel_3a_ecc <- create_lag_diff_terms(sentinel_3a, "eccentricity")
sentinel_3a_aop <- create_lag_diff_terms(sentinel_3a, "argument_of_perigee")
sentinel_3a_inc <- create_lag_diff_terms(sentinel_3a, "inclination")
sentinel_3a_ma  <- create_lag_diff_terms(sentinel_3a, "mean_anomaly")
sentinel_3a_bmm <- create_lag_diff_terms(sentinel_3a, "brouwer_mean_motion")
sentinel_3a_ra  <- create_lag_diff_terms(sentinel_3a, "right_ascension")


# Pre-processing - Results - Summary -------------------------------------------

# Functions
elements_summary <- function() {
  table_data <- data.frame(
    Dataset = c("CryoSat-2", 
                "Sentinel-3A", 
                "SARAL",
                "Haiyang-2A",
                "Jason-3"
    ),
    Before = c(nrow(cryosat_2_raw),
                 nrow(sentinel_3a_raw),
                 nrow(saral_raw),
                 nrow(haiyang_2a_raw),
                 nrow(jason_3_raw)
    ),
    After = c(nrow(cryosat_2),
                 nrow(sentinel_3a),
                 nrow(saral),
                 nrow(haiyang_2a),
                 nrow(jason_3)
    ),
    per_exl = c(round(100 - nrow(cryosat_2)/nrow(cryosat_2_raw) * 100,2), 
                   round(100 - nrow(sentinel_3a)/nrow(sentinel_3a_raw) * 100,2),
                   round(100 - nrow(saral)/nrow(saral_raw) * 100,2),
                   round(100 - nrow(haiyang_2a)/nrow(haiyang_2a_raw) * 100,2),
                   round(100 - nrow(jason_3)/nrow(jason_3_raw) * 100,2)
    )
  )
  return(table_data)
}
filter_by_datetime <- function(man_data, reference_data) {
  
  start_date <- min(reference_data$datetime)
  end_date <- max(reference_data$datetime)
  
  filtered_data <- man_data %>%
    filter(datetime >= start_date & datetime <= end_date)
  
  return(filtered_data)
}
man_summary <- function() {
  
  cryosat_2_man_filtered <- filter_by_datetime(cryosat_2_man, cryosat_2)
  sentinel_3a_man_filtered <- filter_by_datetime(sentinel_3a_man, sentinel_3a)
  saral_man_filtered <- filter_by_datetime(saral_man, saral)
  haiyang_2a_man_filtered <- filter_by_datetime(haiyang_2a_man, haiyang_2a)
  jason_3_man_filtered <- filter_by_datetime(jason_3_man, jason_3)
  
  
  table_data <- data.frame(
    Dataset = c("CryoSat-2", 
                "Sentinel-3A", 
                "SARAL",
                "Haiyang-2A",
                "Jason-3"
    ),
    Before = c(nrow(cryosat_2_man),
               nrow(sentinel_3a_man),
               nrow(saral_man),
               nrow(haiyang_2a_man),
               nrow(jason_3_man)
    ),
    After = c(nrow(cryosat_2_man_filtered),
              nrow(sentinel_3a_man_filtered),
              nrow(saral_man_filtered),
              nrow(haiyang_2a_man_filtered),
              nrow(jason_3_man_filtered)
    ),
    per_exl = c(round(100 - nrow(cryosat_2_man_filtered)/nrow(cryosat_2_man) * 100,2), 
                round(100 - nrow(sentinel_3a_man_filtered)/nrow(sentinel_3a_man) * 100,2),
                round(100 - nrow(saral_man_filtered)/nrow(saral_man) * 100,2),
                round(100 - nrow(haiyang_2a_man_filtered)/nrow(haiyang_2a_man) * 100,2),
                round(100 - nrow(jason_3_man_filtered)/nrow(jason_3_man) * 100,2)
    )
  )
  return(table_data)
}

# Orbital Elements Datasets Summary
elements_summary()

# Manoeuvres Datasets Summary
man_summary()


# Pre-processing - Results - Plots ---------------------------------------------

# Function
line_plot <- function(dataset, element, title = "Dataset Plot") {
  
  plot <- ggplot(dataset, aes(x = datetime, y = .data[[element]])) +
    geom_line() +
    labs(title = title) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 20, face = "bold"),
          axis.text.x = element_text(size = 18, face = "bold"),
          axis.text.y = element_text(size = 18, face = "bold")) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5),
                       labels = scales::number_format(accuracy = 0.0001))
  
  return(plot)
  
}

# Applying - Before Pre-process
line_plot(sentinel_3a_raw, "argument_of_perigee", "Sentinel-3A Argument of Perigee (before preprocessing)")
line_plot(sentinel_3a_raw, "brouwer_mean_motion", "Sentinel-3A Brouwer Mean Motion (before preprocessing)")
line_plot(sentinel_3a_raw, "eccentricity", "Sentinel-3A Eccentricity (before preprocessing)")
line_plot(sentinel_3a_raw, "inclination", "Sentinel-3A Inclination (before preprocessing)")
line_plot(sentinel_3a_raw, "mean_anomaly", "Sentinel-3A Mean Anomaly (before preprocessing)")
line_plot(sentinel_3a_raw, "right_ascension", "Sentinel-3A Right Ascension (before preprocessing)")

line_plot(jason_3_raw, "argument_of_perigee", "Jason-3 Argument of Perigee (before preprocessing)")
line_plot(jason_3_raw, "brouwer_mean_motion", "Jason-3 Brouwer Mean Motion (before preprocessing)")
line_plot(jason_3_raw, "eccentricity", "Jason-3 Eccentricity (before preprocessing)")
line_plot(jason_3_raw, "inclination", "Jason-3 Inclination (before preprocessing)")
line_plot(jason_3_raw, "mean_anomaly", "Jason-3 Mean Anomaly (before preprocessing)")
line_plot(jason_3_raw, "right_ascension", "Jason-3 Right Ascension (before preprocessing)")


# Applying - After Pre-process
line_plot(sentinel_3a, "argument_of_perigee", "Sentinel-3A Argument of Perigee (after preprocessing)")
line_plot(sentinel_3a, "brouwer_mean_motion", "Sentinel-3A Brouwer Mean Motion (after preprocessing)")
line_plot(sentinel_3a, "eccentricity", "Sentinel-3A Eccentricity (after preprocessing)")
line_plot(sentinel_3a, "inclination", "Sentinel-3A Inclination (after preprocessing)")
line_plot(sentinel_3a, "mean_anomaly", "Sentinel-3A Mean Anomaly (after preprocessing)")
line_plot(sentinel_3a, "right_ascension", "Sentinel-3A Right Ascension (after preprocessing)")

line_plot(jason_3, "argument_of_perigee", "Jason-3 Argument of Perigee (after preprocessing)")
line_plot(jason_3, "brouwer_mean_motion", "Jason-3 Brouwer Mean Motion (after preprocessing)")
line_plot(jason_3, "eccentricity", "Jason-3 Eccentricity (after preprocessing)")
line_plot(jason_3, "inclination", "Jason-3 Inclination (after preprocessing)")
line_plot(jason_3, "mean_anomaly", "Jason-3 Mean Anomaly (after preprocessing)")
line_plot(jason_3, "right_ascension", "Jason-3 Right Ascension (after preprocessing)")


# Combined Plots
line_plot(jason_3_raw, "mean_anomaly", "Before") / line_plot(jason_3, "mean_anomaly", "After")

line_plot(jason_3_raw, "brouwer_mean_motion", "Before") / line_plot(jason_3, "brouwer_mean_motion", "After")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------
################################################################################
####################################    ARIMA    ###############################
################################################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

# ARIMA - Extracting Diff Term -------------------------------------------------

# Extract Diff-1 term

extract_columns <- function(df, column) {
  name_1 <- deparse(substitute(df))
  name_2 <- deparse(substitute(column))
  assign(paste0(name_1, "_", name_2), df[[name_2]], envir = .GlobalEnv)
}

# For CryoSat-2
extract_columns(cryosat_2_ecc, diff_1)
extract_columns(cryosat_2_aop, diff_1)
extract_columns(cryosat_2_inc, diff_1)
extract_columns(cryosat_2_ma, diff_1)
extract_columns(cryosat_2_bmm, diff_1)
extract_columns(cryosat_2_ra, diff_1)

# For Haiyang-2A
extract_columns(haiyang_2a_ecc, diff_1)
extract_columns(haiyang_2a_aop, diff_1)
extract_columns(haiyang_2a_inc, diff_1)
extract_columns(haiyang_2a_ma, diff_1)
extract_columns(haiyang_2a_bmm, diff_1)
extract_columns(haiyang_2a_ra, diff_1)

# For Jason-3
extract_columns(jason_3_ecc, diff_1)
extract_columns(jason_3_aop, diff_1)
extract_columns(jason_3_inc, diff_1)
extract_columns(jason_3_ma, diff_1)
extract_columns(jason_3_bmm, diff_1)
extract_columns(jason_3_ra, diff_1)

# For SARAL
extract_columns(saral_ecc, diff_1)
extract_columns(saral_aop, diff_1)
extract_columns(saral_inc, diff_1)
extract_columns(saral_ma, diff_1)
extract_columns(saral_bmm, diff_1)
extract_columns(saral_ra, diff_1)

# For Sentinel-3A
extract_columns(sentinel_3a_ecc, diff_1)
extract_columns(sentinel_3a_aop, diff_1)
extract_columns(sentinel_3a_inc, diff_1)
extract_columns(sentinel_3a_ma, diff_1)
extract_columns(sentinel_3a_bmm, diff_1)
extract_columns(sentinel_3a_ra, diff_1)

# ARIMA - Tuning matrix --------------------------------------------------------

# Setting parameter names

datasets_cryosat_2 <- c("cryosat_2_ecc_diff_1", "cryosat_2_aop_diff_1", "cryosat_2_inc_diff_1",
                        "cryosat_2_ma_diff_1", "cryosat_2_bmm_diff_1", "cryosat_2_ra_diff_1")
datasets_haiyang_2a <- c("haiyang_2a_ecc_diff_1", "haiyang_2a_aop_diff_1", "haiyang_2a_inc_diff_1",
                         "haiyang_2a_ma_diff_1", "haiyang_2a_bmm_diff_1", "haiyang_2a_ra_diff_1")
datasets_jason_3 <- c("jason_3_ecc_diff_1", "jason_3_aop_diff_1", "jason_3_inc_diff_1",
                      "jason_3_ma_diff_1", "jason_3_bmm_diff_1", "jason_3_ra_diff_1")
datasets_saral <- c("saral_ecc_diff_1", "saral_aop_diff_1", "saral_inc_diff_1",
                    "saral_ma_diff_1", "saral_bmm_diff_1", "saral_ra_diff_1")
datasets_sentinel_3a <- c("sentinel_3a_ecc_diff_1", "sentinel_3a_aop_diff_1", "sentinel_3a_inc_diff_1",
                          "sentinel_3a_ma_diff_1", "sentinel_3a_bmm_diff_1", "sentinel_3a_ra_diff_1")

# ARIMA - Tuning models --------------------------------------------------------

fit_arima_and_summary <- function(x, xreg, seasonal = TRUE) {
  
  # Fit ARIMA model
  arima_model <- auto.arima(x, xreg = xreg,
                            seasonal = seasonal, 
                            approximation = FALSE)
  
  # Perform Ljung-Box test on residuals
  lb_p_value <- Box.test(residuals(arima_model), type = "Ljung-Box")$p.value
  aic_value <- AIC(arima_model)
  
  # Return results as a list
  result <- list(
    ARIMA_model = paste0("(", arima_model$arma[1],",",arima_model$arma[6]+1,",",arima_model$arma[2],")"),
    Ljung_Box_p_value = lb_p_value,
    AIC = aic_value
  )
  
  return(result)
}

fit_arima_combinations <- function(datasets) {
  # Initialize an empty results data frame
  results_df <- data.frame(
    Dataset = character(),
    Xreg_Dataset = character(),
    ARIMA_model = character(),
    p_value = numeric(),
    AIC = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Iterate through each dataset as x
  for (x_name in datasets) {
    x <- get(x_name)  # Get x dataset
    other_datasets <- datasets[datasets != x_name]  # Get rest of datasets as xreg
    
    # Iterate through each xreg dataset
    for (xreg_name in other_datasets) {
      xreg <- get(xreg_name)  # Get xreg dataset
      
      # Fit ARIMA model and retrieve results
      result <- fit_arima_and_summary(x, xreg, seasonal = TRUE)
      
      # Append results to results_df
      results_df <- rbind(results_df, data.frame(
        X = x_name,
        Xreg = xreg_name,
        ARIMA = result$ARIMA_model,
        P_value = result$Ljung_Box_p_value,
        AIC = result$AIC,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  results_df_sorted <- results_df[order(results_df$AIC), ] %>%
    tibble()
  
  return(results_df_sorted)
}

results_cryosat_2 <- fit_arima_combinations(datasets_cryosat_2)
results_haiyang_2a <- fit_arima_combinations(datasets_haiyang_2a)
results_jason_3 <- fit_arima_combinations(datasets_jason_3)
results_saral <- fit_arima_combinations(datasets_saral)
results_sentinel_3a <- fit_arima_combinations(datasets_sentinel_3a)

# ARIMA - Tuning results  ------------------------------------------------------

# Tuning results in total
print(results_cryosat_2 %>% arrange(X, AIC), n=30)
print(results_haiyang_2a %>% arrange(X, AIC), n=30)
print(results_jason_3 %>% arrange(X, AIC), n=30)
print(results_saral %>% arrange(X, AIC), n=30)
print(results_sentinel_3a %>% arrange(X, AIC), n=30)

# Best tuning results for each element
cryosat_2_arima_result <- results_cryosat_2 %>% group_by(X) %>% filter(AIC == min(AIC)) %>%  ungroup() %>% select(-P_value)
haiyang_2a_arima_result <- results_haiyang_2a %>% group_by(X) %>% filter(AIC == min(AIC)) %>% ungroup() %>% select(-P_value)
jason_3_arima_result <- results_jason_3 %>% group_by(X) %>% filter(AIC == min(AIC)) %>% ungroup() %>% select(-P_value)
saral_arima_result <- results_saral %>% group_by(X) %>% filter(AIC == min(AIC)) %>% ungroup() %>% select(-P_value)
sentinel_3a_arima_result <- results_sentinel_3a %>% group_by(X) %>% filter(AIC == min(AIC)) %>% ungroup() %>% select(-P_value)

# Choose the best parameters for each element
generate_param_lists <- function(arima_results, prefix) {
  # Loop over each row of the results table
  for (i in 1:nrow(arima_results)) {
    sat_x_name <- arima_results$X[i]
    sat_xreg_name <- arima_results$Xreg[i]
    
    # Extract base names for sat_data_name
    sat_data_name <- gsub("_diff_1", "", sat_x_name)
    
    # Create the list for each row
    param_list <- list(
      sat_x_name = sat_x_name,
      sat_xreg_name = sat_xreg_name,
      sat_data_name = sat_data_name,
      sat_man_name = paste0(prefix, "_man")
    )
    
    # Create the list name dynamically based on the prefix and data name
    list_name <- paste0(prefix, "_arima_params_", gsub(paste0(prefix, "_"), "", sat_data_name))
    
    # Use assign to create separate lists in the global environment
    assign(list_name, param_list, envir = .GlobalEnv)
  }
}

generate_param_lists(cryosat_2_arima_result, "cryosat_2")
generate_param_lists(haiyang_2a_arima_result, "haiyang_2a")
generate_param_lists(jason_3_arima_result, "jason_3")
generate_param_lists(saral_arima_result, "saral")
generate_param_lists(sentinel_3a_arima_result, "sentinel_3a")


# ARIMA - Prediction Qualitative Analysis - CryoSat-2 --------------------------

# Arima Plot function
arima_pred_plot <- function(param, m, delay, title) {

  # set parameters
  set_parameters <- function(params) {
    sat_data <<- get(params$sat_data_name)
    sat_x <<- get(params$sat_x_name)
    sat_xreg <<- get(params$sat_xreg_name)
    sat_man <<- get(params$sat_man)
  }
  
  set_parameters(param)
  
  # Fit ARIMA model
  arima_model <- auto.arima(x = sat_x,
                            xreg = sat_xreg,
                            seasonal = TRUE,
                            approximation = FALSE)
  
  # Create pred table
  pred_data <- sat_data %>%
    select(datetime,
           actual = diff_1) %>%
    mutate(pred = as.numeric(fitted(arima_model))) %>%
    mutate(residual = abs(actual - pred))
  
  # Define the time range for the plot
  start_time <- min(pred_data$datetime)
  end_time <- start_time + months(m)
  
  # Filter the data based on the time range
  result_data_filtered <- pred_data %>%
    filter(datetime >= start_time & datetime <= end_time)
  
  # Assume `man_filtered` is from another dataset and contains the event dates
  man_filtered <- sat_man %>%
    filter(datetime >= start_time & datetime <= end_time) %>%
    mutate(datetime = datetime + days(delay))  # Adjust if needed
  
  # Plot using ggplot2
  plot <- ggplot(result_data_filtered, aes(x = datetime)) +
    # geom_line(aes(y = actual), color = "black", size = 0.5, linetype = "solid") +
    # geom_line(aes(y = pred), color = "blue", size = 0.5, linetype = "solid") +
    geom_line(aes(y = residual), color = "black", size = 0.6, linetype = "solid") +
    geom_vline(data = man_filtered,
               aes(xintercept = as.numeric(datetime)), size = 0.8,
               linetype = "dashed", color = "red3") +
    labs(x = "Datetime", y = "Residuls",
         title = title) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 20, face = "bold")) +
    scale_x_datetime(date_breaks = "2 month", date_labels = "%y-%b")
    
  return(plot)

}

arima_line_cryosat_2_aop <- arima_pred_plot(cryosat_2_arima_params_aop, 12, 0, "Argument of Perigee")
arima_line_cryosat_2_bmm <- arima_pred_plot(cryosat_2_arima_params_bmm, 12, 0, "Brouwer Mean Motion")
arima_line_cryosat_2_ecc <- arima_pred_plot(cryosat_2_arima_params_ecc, 12, 0, "Eccentricity")
arima_line_cryosat_2_inc <- arima_pred_plot(cryosat_2_arima_params_inc, 12, 0, "Inclination")
arima_line_cryosat_2_ma <- arima_pred_plot(cryosat_2_arima_params_ma, 12, 0, "Mean Anomaly")
arima_line_cryosat_2_ra <- arima_pred_plot(cryosat_2_arima_params_ra, 12, 0, "Right Ascension")

arima_line_cryosat_2_combined <- 
  (arima_line_cryosat_2_aop / 
     arima_line_cryosat_2_bmm / 
     arima_line_cryosat_2_inc / 
     arima_line_cryosat_2_ecc /
     arima_line_cryosat_2_ma / 
     arima_line_cryosat_2_ra) + 
  plot_annotation(
    title = "CryoSat-2 ARIMA Prediction Residuals",
    theme = theme(
      plot.title = element_text(size = 28, face = "bold")))


# ARIMA - Prediction Qualitative Analysis Haiyang-2A ---------------------------

# Arima Plot function
arima_pred_plot <- function(param, m, delay, title) {
  
  # set parameters
  set_parameters <- function(params) {
    sat_data <<- get(params$sat_data_name)
    sat_x <<- get(params$sat_x_name)
    sat_xreg <<- get(params$sat_xreg_name)
    sat_man <<- get(params$sat_man)
  }
  
  set_parameters(param)
  
  # Fit ARIMA model
  arima_model <- auto.arima(x = sat_x,
                            xreg = sat_xreg,
                            seasonal = TRUE,
                            approximation = FALSE)
  
  # Create pred table
  pred_data <- sat_data %>%
    select(datetime,
           actual = diff_1) %>%
    mutate(pred = as.numeric(fitted(arima_model))) %>%
    mutate(residual = abs(actual - pred))
  
  
  # Define the time range for the plot
  start_time <- min(pred_data$datetime)
  end_time <- start_time + months(m)
  
  # Filter the data based on the time range
  result_data_filtered <- pred_data %>%
    filter(datetime >= start_time & datetime <= end_time)
  
  # Assume `man_filtered` is from another dataset and contains the event dates
  man_filtered <- sat_man %>%
    filter(datetime >= start_time & datetime <= end_time) %>%
    mutate(datetime = datetime + days(delay))  # Adjust if needed
  
  # Plot using ggplot2
  plot <- ggplot() +
    # Solid line for residuals
    geom_line(data = result_data_filtered, aes(x = datetime, y = residual, color = "Residuals"), 
              size = 0.6) +
    # Dashed line for maneuver events
    geom_vline(data = man_filtered, aes(xintercept = as.numeric(datetime), linetype = "Manoeuvre"), 
               color = "red3", size = 0.8) +
    labs(title = title, color = "Legend", linetype = "Legend") +  
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 14, face = "bold"), 
          legend.text = element_text(size = 14, face = "bold"),
          plot.title = element_blank(),  
          legend.position = "bottom",     
          legend.title = element_blank(),  
          legend.key.size = unit(1.5, "cm")) +  
    scale_x_datetime(date_breaks = "2 months", date_labels = "%y-%b") +  
    scale_y_continuous(labels = label_number(accuracy = 0.01)) +     
    scale_color_manual(values = c("Residuals" = "black")) +  
    scale_linetype_manual(values = c("Manoeuvre" = "dashed"))
  
  return(plot)
  
}

arima_line_haiyang_2a_aop <- arima_pred_plot(haiyang_2a_arima_params_aop, 12, 0, "Argument of Perigee")
arima_line_haiyang_2a_bmm <- arima_pred_plot(haiyang_2a_arima_params_bmm, 12, 0, "Brouwer Mean Motion")
arima_line_haiyang_2a_inc <- arima_pred_plot(haiyang_2a_arima_params_inc, 12, 0, "Inclination")
arima_line_haiyang_2a_ecc <- arima_pred_plot(haiyang_2a_arima_params_ecc, 12, 0, "Eccentricity")
arima_line_haiyang_2a_ma <- arima_pred_plot(haiyang_2a_arima_params_ma, 12, 0, "Mean Anomaly")
arima_line_haiyang_2a_ra <- arima_pred_plot(haiyang_2a_arima_params_ra, 12, 0, "Right Ascension")

arima_line_haiyang_2a_combined <- 
  (arima_line_haiyang_2a_aop / 
     arima_line_haiyang_2a_bmm / 
     arima_line_haiyang_2a_inc / 
     arima_line_haiyang_2a_ecc /
     arima_line_haiyang_2a_ma / 
     arima_line_haiyang_2a_ra) + 
  plot_annotation(
    title = "haiyang_2a ARIMA Prediction Residuals",
    theme = theme(
      plot.title = element_text(size = 28, face = "bold")))

# ARIMA - Prediction Qualitative Analysis Jason-3 -------------------------------------

# ARIMA residual plot function
arima_pred_plot <- function(param, m, delay, title) {
  
  # set parameters
  set_parameters <- function(params) {
    sat_data <<- get(params$sat_data_name)
    sat_x <<- get(params$sat_x_name)
    sat_xreg <<- get(params$sat_xreg_name)
    sat_man <<- get(params$sat_man)
  }
  
  set_parameters(param)
  
  # Fit ARIMA model
  arima_model <- auto.arima(x = sat_x,
                            xreg = sat_xreg,
                            seasonal = TRUE,
                            approximation = FALSE)
  
  # Create pred table
  pred_data <- sat_data %>%
    select(datetime,
           actual = diff_1) %>%
    mutate(pred = as.numeric(fitted(arima_model))) %>%
    mutate(residual = abs(actual - pred))
  
  
  # Define the time range for the plot
  start_time <- min(pred_data$datetime)
  end_time <- start_time + months(m)
  
  # Filter the data based on the time range
  result_data_filtered <- pred_data %>%
    filter(datetime >= start_time & datetime <= end_time)
  
  # Assume `man_filtered` is from another dataset and contains the event dates
  man_filtered <- sat_man %>%
    filter(datetime >= start_time & datetime <= end_time) %>%
    mutate(datetime = datetime + days(delay))  # Adjust if needed
  
  # Plot using ggplot2
  plot <- ggplot(result_data_filtered, aes(x = datetime)) +
    # geom_line(aes(y = actual), color = "black", size = 0.5, linetype = "solid") +
    # geom_line(aes(y = pred), color = "blue", size = 0.5, linetype = "solid") +
    geom_line(aes(y = residual), color = "black", size = 0.6, linetype = "solid") +
    geom_vline(data = man_filtered,
               aes(xintercept = as.numeric(datetime)), size = 0.8,
               linetype = "dashed", color = "red3") +
    labs(x = "Datetime", y = "Residuls",
         title = title) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 20, face = "bold")) +
    scale_x_datetime(date_breaks = "2 month", date_labels = "%y-%b")
  
  return(plot)
  
}

arima_line_jason_3_aop <- arima_pred_plot(jason_3_arima_params_aop, 12, 3, "Argument of Perigee")
arima_line_jason_3_bmm <- arima_pred_plot(jason_3_arima_params_bmm, 12, 3, "Brouwer Mean Motion")
arima_line_jason_3_inc <- arima_pred_plot(jason_3_arima_params_inc, 12, 3, "Inclination")
arima_line_jason_3_ecc <- arima_pred_plot(jason_3_arima_params_ecc, 12, 3, "Eccentricity")
arima_line_jason_3_ma <- arima_pred_plot(jason_3_arima_params_ma, 12, 3, "Mean Anomaly")
arima_line_jason_3_ra <- arima_pred_plot(jason_3_arima_params_ra, 12, 3, "Right Ascension")

arima_resid_jason_3_combined <- 
  (arima_line_jason_3_aop / 
     arima_line_jason_3_bmm / 
     arima_line_jason_3_inc / 
     arima_line_jason_3_ecc /
     arima_line_jason_3_ma / 
     arima_line_jason_3_ra) + 
  plot_annotation(
    title = "Jason-3 ARIMA Prediction Residuals",
    theme = theme(
      plot.title = element_text(size = 28, face = "bold")))

# ARIMA - Prediction Qualitative Analysis SARAL ---------------------------------------

# ARIMA residual plot function
arima_pred_plot <- function(param, m, delay, title) {
  
  # set parameters
  set_parameters <- function(params) {
    sat_data <<- get(params$sat_data_name)
    sat_x <<- get(params$sat_x_name)
    sat_xreg <<- get(params$sat_xreg_name)
    sat_man <<- get(params$sat_man)
  }
  
  set_parameters(param)
  
  # Fit ARIMA model
  arima_model <- auto.arima(x = sat_x,
                            xreg = sat_xreg,
                            seasonal = TRUE,
                            approximation = FALSE)
  
  # Create pred table
  pred_data <- sat_data %>%
    select(datetime,
           actual = diff_1) %>%
    mutate(pred = as.numeric(fitted(arima_model))) %>%
    mutate(residual = abs(actual - pred))
  
  
  # Define the time range for the plot
  start_time <- min(pred_data$datetime)
  end_time <- start_time + months(m)
  
  # Filter the data based on the time range
  result_data_filtered <- pred_data %>%
    filter(datetime >= start_time & datetime <= end_time)
  
  # Assume `man_filtered` is from another dataset and contains the event dates
  man_filtered <- sat_man %>%
    filter(datetime >= start_time & datetime <= end_time) %>%
    mutate(datetime = datetime + days(delay))  # Adjust if needed
  
  # Plot using ggplot2
  plot <- ggplot(result_data_filtered, aes(x = datetime)) +
    # geom_line(aes(y = actual), color = "black", size = 0.5, linetype = "solid") +
    # geom_line(aes(y = pred), color = "blue", size = 0.5, linetype = "solid") +
    geom_line(aes(y = residual), color = "black", size = 0.6, linetype = "solid") +
    geom_vline(data = man_filtered,
               aes(xintercept = as.numeric(datetime)), size = 0.8,
               linetype = "dashed", color = "red3") +
    labs(x = "Datetime", y = "Residuls",
         title = title) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 20, face = "bold")) +
    scale_x_datetime(date_breaks = "2 month", date_labels = "%y-%b")
  
  return(plot)
  
}

arima_line_saral_aop <- arima_pred_plot(saral_arima_params_aop, 12, 3, "Argument of Perigee")
arima_line_saral_bmm <- arima_pred_plot(saral_arima_params_bmm, 12, 3, "Brouwer Mean Motion")
arima_line_saral_inc <- arima_pred_plot(saral_arima_params_inc, 12, 3, "Inclination")
arima_line_saral_ecc <- arima_pred_plot(saral_arima_params_ecc, 12, 3, "Eccentricity")
arima_line_saral_ma <- arima_pred_plot(saral_arima_params_ma, 12, 3, "Mean Anomaly")
arima_line_saral_ra <- arima_pred_plot(saral_arima_params_ra, 12, 3, "Right Ascension")

arima_resid_saral_combined <- 
  (arima_line_saral_aop / 
     arima_line_saral_bmm / 
     arima_line_saral_inc / 
     arima_line_saral_ecc /
     arima_line_saral_ma / 
     arima_line_saral_ra) + 
  plot_annotation(
    title = "SARAL ARIMA Prediction Residuals",
    theme = theme(
      plot.title = element_text(size = 28, face = "bold")))

# ARIMA - Prediction Qualitative Analysis Sentinel-3A ---------------------------------

# ARIMA residual plot function
arima_pred_plot <- function(param, m, delay, title) {
  
  # set parameters
  set_parameters <- function(params) {
    sat_data <<- get(params$sat_data_name)
    sat_x <<- get(params$sat_x_name)
    sat_xreg <<- get(params$sat_xreg_name)
    sat_man <<- get(params$sat_man)
  }
  
  set_parameters(param)
  
  # Fit ARIMA model
  arima_model <- auto.arima(x = sat_x,
                            xreg = sat_xreg,
                            seasonal = TRUE,
                            approximation = FALSE)
  
  # Create pred table
  pred_data <- sat_data %>%
    select(datetime,
           actual = diff_1) %>%
    mutate(pred = as.numeric(fitted(arima_model))) %>%
    mutate(residual = abs(actual - pred))
  
  
  # Define the time range for the plot
  start_time <- min(pred_data$datetime)
  end_time <- start_time + months(m)
  
  # Filter the data based on the time range
  result_data_filtered <- pred_data %>%
    filter(datetime >= start_time & datetime <= end_time)
  
  # Assume `man_filtered` is from another dataset and contains the event dates
  man_filtered <- sat_man %>%
    filter(datetime >= start_time & datetime <= end_time) %>%
    mutate(datetime = datetime + days(delay))  # Adjust if needed
  
  # Plot using ggplot2
  plot <- ggplot() +
    # Solid line for residuals
    geom_line(data = result_data_filtered, aes(x = datetime, y = residual, color = "Residuals"), 
              size = 0.6) +
    # Dashed line for maneuver events
    geom_vline(data = man_filtered, aes(xintercept = as.numeric(datetime), linetype = "Manoeuvre"), 
               color = "red3", size = 0.8) +
    labs(title = title, color = "Legend", linetype = "Legend") +  
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 18, face = "bold"),
          axis.text.y = element_text(size = 18, face = "bold"), 
          legend.text = element_text(size = 18, face = "bold"),
          plot.title = element_blank(),  
          legend.position = "bottom",     
          legend.title = element_blank(),  
          legend.key.size = unit(1.5, "cm")) +  
    scale_x_datetime(date_breaks = "2 months", date_labels = "%y-%b") +  
    scale_y_continuous(labels = label_number(accuracy = 0.01)) +     
    scale_color_manual(values = c("Residuals" = "black")) +  
    scale_linetype_manual(values = c("Manoeuvre" = "dashed"))
  
  return(plot)
  
}

arima_line_sentinel_3a_aop <- arima_pred_plot(sentinel_3a_arima_params_aop, 12, 0, "Argument of Perigee")
arima_line_sentinel_3a_bmm <- arima_pred_plot(sentinel_3a_arima_params_bmm, 12, 0, "Brouwer Mean Motion")
arima_line_sentinel_3a_inc <- arima_pred_plot(sentinel_3a_arima_params_inc, 12, 0, "Inclination")
arima_line_sentinel_3a_ecc <- arima_pred_plot(sentinel_3a_arima_params_ecc, 12, 0, "Eccentricity")
arima_line_sentinel_3a_ma <- arima_pred_plot(sentinel_3a_arima_params_ma, 12, 0, "Mean Anomaly")
arima_line_sentinel_3a_ra <- arima_pred_plot(sentinel_3a_arima_params_ra, 12, 0, "Right Ascension")

arima_resid_sentinel_3a_combined <- 
  (arima_line_sentinel_3a_aop / 
     arima_line_sentinel_3a_ecc /
     arima_line_sentinel_3a_bmm /
     arima_line_sentinel_3a_inc /
     arima_line_sentinel_3a_ma / 
     arima_line_sentinel_3a_ra) + 
  plot_annotation(
    title = "Sentinel-3A ARIMA Prediction Residuals",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold")))



# ARIMA - prediction Precision-Recall Analysis ---------------------------------

# Function to compute precision and recall
precision_recall <- function(param,element = "element",ground_truth,threshold_days = 5) {
  
  # prediction
  prediction <- function(param) {
    # Set parameters
    set_parameters <- function(param) {
      sat_data <<- get(param$sat_data_name)
      sat_x <<- get(param$sat_x_name)
      sat_xreg <<- get(param$sat_xreg_name)
      sat_man <<- get(param$sat_man)
    }
    
    set_parameters(param)
    
    # Fit ARIMA model
    arima_model <- auto.arima(x = sat_x,
                              xreg = sat_xreg,
                              seasonal = TRUE,
                              approximation = FALSE)
    
    # Create pred table
    pred_data <- sat_data %>%
      select(datetime,
             actual = diff_1) %>%
      mutate(pred = as.numeric(fitted(arima_model))) %>%
      mutate(residual = abs(actual - pred)) %>%
      select(-actual, -pred) 
    
    return(pred_data)
  }
  
  pred_data <- prediction(param)
  
  # Initialize a list to store results
  results_list <- list()
  
  thresholds <- sort(pred_data$residual, decreasing = TRUE)
  
  # Loop through all possible thresholds
  for (i in 1:length(thresholds)) {
    threshold_value <- thresholds[i]
    
    # Find the closest ground-truth event within the threshold
    results <- pred_data %>% 
      filter(residual >= threshold_value) %>%
      mutate(match_index = findInterval(datetime,ground_truth$datetime,all.inside = TRUE)) %>%
      mutate(ground_truth_timestamp = ground_truth$datetime[match_index]) %>%
      mutate(time_diff = abs(difftime(datetime,ground_truth_timestamp,units = "days")))
    
    TP <- n_distinct(results %>% filter(time_diff <= threshold_days) %>% pull(match_index))
    FP <- nrow(results %>% filter(time_diff > threshold_days))
    FN <- nrow(ground_truth) - TP

    # Calculate Precision and Recall
    precision <- ifelse(TP + FP > 0, TP / (TP + FP), 0)
    recall <- ifelse(TP + FN > 0, TP / (TP + FN), 0)
    
    # Calculate F1 Score
    F1_score <- ifelse(precision + recall > 0, 
                       2 * (precision * recall) / (precision + recall), 
                       0)
    # Calculate F0.5 score
    F0.5_score <- ifelse(precision + recall > 0, 
                         (1 + 0.5^2) * (precision * recall) / ((0.5^2 * precision) + recall), 
                         0)
    
    # Calculate F2 score
    F2_score <- ifelse(precision + recall > 0, 
                       (1 + 2^2) * (precision * recall) / ((2^2 * precision) + recall), 
                       0)
    
    # Store the result in the list
    results_list[[i]] <- data.frame(Element = element,
                                    Threshold = threshold_value,
                                    Precision = precision,
                                    Recall = recall,
                                    F0.5_score = F0.5_score,
                                    F1_score = F1_score,
                                    F2_score = F2_score)
  }
  
  # Combine the list into a tibble
  results_table <- do.call(rbind, results_list)
  return(as_tibble(results_table))
}

# Precision and recall calculation
cryosat_2_arima_pr_re <- bind_rows(precision_recall(cryosat_2_arima_params_aop, "Argument of Perigee", cryosat_2_man),
                                   precision_recall(cryosat_2_arima_params_bmm, "Brouwer Mean Motion", cryosat_2_man),
                                   precision_recall(cryosat_2_arima_params_inc, "Inclination", cryosat_2_man),
                                   precision_recall(cryosat_2_arima_params_ecc, "Eccentricity", cryosat_2_man),
                                   precision_recall(cryosat_2_arima_params_ma, "Mean Anomaly", cryosat_2_man),
                                   precision_recall(cryosat_2_arima_params_ra, "Right Ascension", cryosat_2_man))

haiyang_2a_arima_pr_re <- bind_rows(precision_recall(haiyang_2a_arima_params_aop, "Argument of Perigee", haiyang_2a_man),
                                    precision_recall(haiyang_2a_arima_params_bmm, "Brouwer Mean Motion", haiyang_2a_man),
                                    precision_recall(haiyang_2a_arima_params_inc, "Inclination", haiyang_2a_man),
                                    precision_recall(haiyang_2a_arima_params_ecc, "Eccentricity", haiyang_2a_man),
                                    precision_recall(haiyang_2a_arima_params_ma, "Mean Anomaly", haiyang_2a_man),
                                    precision_recall(haiyang_2a_arima_params_ra, "Right Ascension", haiyang_2a_man))

jason_3_arima_pr_re <- bind_rows(precision_recall(jason_3_arima_params_aop, "Argument of Perigee", jason_3_man),
                                 precision_recall(jason_3_arima_params_bmm, "Brouwer Mean Motion", jason_3_man),
                                 precision_recall(jason_3_arima_params_inc, "Inclination", jason_3_man),
                                 precision_recall(jason_3_arima_params_ecc, "Eccentricity", jason_3_man),
                                 precision_recall(jason_3_arima_params_ma, "Mean Anomaly", jason_3_man),
                                 precision_recall(jason_3_arima_params_ra, "Right Ascension", jason_3_man))

saral_arima_pr_re <- bind_rows(precision_recall(saral_arima_params_aop, "Argument of Perigee", saral_man),
                               precision_recall(saral_arima_params_bmm, "Brouwer Mean Motion", saral_man),
                               precision_recall(saral_arima_params_inc, "Inclination", saral_man),
                               precision_recall(saral_arima_params_ecc, "Eccentricity", saral_man),
                               precision_recall(saral_arima_params_ma, "Mean Anomaly", saral_man),
                               precision_recall(saral_arima_params_ra, "Right Ascension", saral_man))

sentinel_3a_arima_pr_re <- bind_rows(precision_recall(sentinel_3a_arima_params_aop, "Argument of Perigee", sentinel_3a_man),
                                     precision_recall(sentinel_3a_arima_params_bmm, "Brouwer Mean Motion", sentinel_3a_man),
                                     precision_recall(sentinel_3a_arima_params_ecc, "Eccentricity", sentinel_3a_man),
                                     precision_recall(sentinel_3a_arima_params_inc, "Inclination", sentinel_3a_man),
                                     precision_recall(sentinel_3a_arima_params_ma, "Mean Anomaly", sentinel_3a_man),
                                     precision_recall(sentinel_3a_arima_params_ra, "Right Ascension", sentinel_3a_man))

# Precision and Recall Curve
precision_recall_curve <- function(results, title = "Precision-Recall Curves by Element") {
  ggplot(results, aes(x = Recall, y = Precision, color = Element, group = Element)) +
    geom_line(size = 0.8) +
    labs(title = title,
         x = "Recall",
         y = "Precision") +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.title.x = element_text(size = 18, face = "bold"),
          axis.title.y = element_text(size = 18, face = "bold"),
          axis.text.x = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 20, face = "bold"),
          legend.text = element_text(size = 12, face = "bold"),
          legend.title = element_text(size = 14, face = "bold"))
}

precision_recall_curve(cryosat_2_arima_pr_re, "Cryosat-2 ARIMA Precision-Recall Curves by Element")
precision_recall_curve(haiyang_2a_arima_pr_re, "Haiyang-2A ARIMA Precision-Recall Curves by Element")
precision_recall_curve(jason_3_arima_pr_re, "Jason-3 ARIMA Precision-Recall Curves by Element")
precision_recall_curve(saral_arima_pr_re, "SARAL ARIMA Precision-Recall Curves by Element")
precision_recall_curve(sentinel_3a_arima_pr_re, "Sentinel-3A ARIMA Precision-Recall Curves by Element")

# Optimal F1 score
optimal_f1_score <- function(results) {
  
  output <- results %>% 
    group_by(Element) %>%
    filter(F1_score == max(F1_score, na.rm = TRUE)) %>%
    slice_max(F1_score, with_ties = FALSE) %>%
    select(Element, Threshold, Precision, Recall, F1_score) %>%
    arrange(desc(F1_score))
  
  return(output)
}

optimal_f1_score(cryosat_2_arima_pr_re)
optimal_f1_score(haiyang_2a_arima_pr_re)
optimal_f1_score(jason_3_arima_pr_re)
optimal_f1_score(saral_arima_pr_re)
optimal_f1_score(sentinel_3a_arima_pr_re)

# Optimal F0.5 score
optimal_f0.5_score <- function(results) {
  
  output <- results %>% 
    group_by(Element) %>%
    filter(F0.5_score == max(F0.5_score, na.rm = TRUE)) %>%
    slice_max(F0.5_score, with_ties = FALSE) %>%
    select(Element, Threshold, Precision, Recall, F0.5_score) %>%
    arrange(desc(F0.5_score))
  
  return(output)
}

optimal_f0.5_score(cryosat_2_arima_pr_re)
optimal_f0.5_score(haiyang_2a_arima_pr_re)
optimal_f0.5_score(jason_3_arima_pr_re)
optimal_f0.5_score(saral_arima_pr_re)
optimal_f0.5_score(sentinel_3a_arima_pr_re)

# Optimal F2 score
optimal_f2_score <- function(results) {
  
  output <- results %>% 
    group_by(Element) %>%
    filter(F2_score == max(F2_score, na.rm = TRUE)) %>%
    slice_max(F2_score, with_ties = FALSE) %>%
    select(Element, Threshold, Precision, Recall, F2_score) %>%
    arrange(desc(F2_score))
  
  return(output)
}

optimal_f2_score(cryosat_2_arima_pr_re)
optimal_f2_score(haiyang_2a_arima_pr_re)
optimal_f2_score(jason_3_arima_pr_re)
optimal_f2_score(saral_arima_pr_re)
optimal_f2_score(sentinel_3a_arima_pr_re)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------
################################################################################
###################################    XGboost    ##############################
################################################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

# XGBoost - Tuning Models ------------------------------------------------------

# Function
xgb_modeling <- function(xgb_target, xgb_actual) {
  
  # XGboost - Split dataset
  xgb_data <- xgb_target %>% 
    rename(actual = xgb_actual)
  xgb_split <- initial_time_split(xgb_data, prop = 0.8)
  xgb_train <- training(xgb_split)
  xgb_test <- testing(xgb_split)
  
  # XGboost - Cv_folds
  cv_folds <- vfold_cv(xgb_train, v = 20)
  
  # XGboost - Recipe
  xgb_recipe <- 
    recipe(actual ~ ., data = xgb_train) %>%
    update_role(datetime, new_role = "id") %>% # Exclude datetime from predictors
    update_role(starts_with("lag_"),  starts_with("diff_"), new_role = "predictor") %>%
    step_normalize(all_predictors()) %>%
    step_pca()

  
  # XGBoost - Model specification
  xgb_spec <- boost_tree(
    trees = 1000,
    tree_depth = tune(),
    min_n = tune(),
    loss_reduction = tune(),
    sample_size = 1,
    mtry = 0.8,
    learn_rate = tune(),
    stop_iter = 10
  ) %>% 
    set_engine("xgboost",
               counts=FALSE) %>% 
    set_mode("regression")
  
  xgb_grid <- grid_latin_hypercube(
    tree_depth(range = c(1, 10)),
    min_n(range = c(1, 10)),
    loss_reduction(range = c(0, 10)),
    learn_rate(range = c(0.001, 0.1)),
    size = 20
  )
  
  # XGBoost - Workflow 
  xgb_wf <- workflow() %>%
    add_recipe(xgb_recipe) %>%
    add_model(xgb_spec)
  
  # XGBoost - Tuning
  doParallel::registerDoParallel()
  xgb_tune <- tune_grid(
    xgb_wf,
    resamples = cv_folds,
    grid = xgb_grid,
    control = control_grid(save_pred = TRUE))
  
  # XGBoost - Tuning result metrics
  best_results <- show_best(xgb_tune, metric = "rmse")
  print(best_results)
  
  # XGBoost - Finalizing
  xgb_final <- xgb_wf %>%
    finalize_workflow(select_best(xgb_tune, metric = "rmse")) %>%
    fit(xgb_data)
  
  return(xgb_final)
  
}

# CryoSat-2
xgb_model_cryosat_2_aop <- xgb_modeling(cryosat_2_aop, "argument_of_perigee")
xgb_model_cryosat_2_bmm <- xgb_modeling(cryosat_2_bmm, "brouwer_mean_motion")
xgb_model_cryosat_2_ecc <- xgb_modeling(cryosat_2_ecc, "eccentricity")
xgb_model_cryosat_2_inc <- xgb_modeling(cryosat_2_inc, "inclination")
xgb_model_cryosat_2_ma  <- xgb_modeling(cryosat_2_ma, "mean_anomaly")
xgb_model_cryosat_2_ra  <- xgb_modeling(cryosat_2_ra, "right_ascension")

# Haiyang-2A
xgb_model_haiyang_2a_aop <- xgb_modeling(haiyang_2a_aop, "argument_of_perigee")
xgb_model_haiyang_2a_bmm <- xgb_modeling(haiyang_2a_bmm, "brouwer_mean_motion")
xgb_model_haiyang_2a_ecc <- xgb_modeling(haiyang_2a_ecc, "eccentricity")
xgb_model_haiyang_2a_inc <- xgb_modeling(haiyang_2a_inc, "inclination")
xgb_model_haiyang_2a_ma  <- xgb_modeling(haiyang_2a_ma, "mean_anomaly")
xgb_model_haiyang_2a_ra  <- xgb_modeling(haiyang_2a_ra, "right_ascension")

# Jason-3
xgb_model_jason_3_aop <- xgb_modeling(jason_3_aop, "argument_of_perigee")
xgb_model_jason_3_bmm <- xgb_modeling(jason_3_bmm, "brouwer_mean_motion")
xgb_model_jason_3_ecc <- xgb_modeling(jason_3_ecc, "eccentricity")
xgb_model_jason_3_inc <- xgb_modeling(jason_3_inc, "inclination")
xgb_model_jason_3_ma <- xgb_modeling(jason_3_ma, "mean_anomaly")
xgb_model_jason_3_ra <- xgb_modeling(jason_3_ra, "right_ascension")

# SARAL
xgb_model_saral_aop <- xgb_modeling(saral_aop, "argument_of_perigee")
xgb_model_saral_bmm <- xgb_modeling(saral_bmm, "brouwer_mean_motion")
xgb_model_saral_ecc <- xgb_modeling(saral_ecc, "eccentricity")
xgb_model_saral_inc <- xgb_modeling(saral_inc, "inclination")
xgb_model_saral_ma  <- xgb_modeling(saral_ma, "mean_anomaly")
xgb_model_saral_ra  <- xgb_modeling(saral_ra, "right_ascension")

# Sentinel-3A
xgb_model_sentinel_3a_aop <- xgb_modeling(sentinel_3a_aop, "argument_of_perigee")
xgb_model_sentinel_3a_bmm <- xgb_modeling(sentinel_3a_bmm, "brouwer_mean_motion")
xgb_model_sentinel_3a_ecc <- xgb_modeling(sentinel_3a_ecc, "eccentricity")
xgb_model_sentinel_3a_inc <- xgb_modeling(sentinel_3a_inc, "inclination")
xgb_model_sentinel_3a_ma  <- xgb_modeling(sentinel_3a_ma, "mean_anomaly")
xgb_model_sentinel_3a_ra  <- xgb_modeling(sentinel_3a_ra, "right_ascension")


# Saving the results
save(xgb_model_cryosat_2_aop, xgb_model_cryosat_2_bmm, xgb_model_cryosat_2_ecc,
     xgb_model_cryosat_2_inc, xgb_model_cryosat_2_ma, xgb_model_cryosat_2_ra, 
     file = "xgb_models_cryosat_2.RData")

save(xgb_model_haiyang_2a_aop, xgb_model_haiyang_2a_bmm, xgb_model_haiyang_2a_ecc,
     xgb_model_haiyang_2a_inc, xgb_model_haiyang_2a_ma, xgb_model_haiyang_2a_ra, 
     file = "xgb_models_haiyang_2a.RData")

save(xgb_model_jason_3_aop, xgb_model_jason_3_bmm, xgb_model_jason_3_ecc,
     xgb_model_jason_3_inc, xgb_model_jason_3_ma, xgb_model_jason_3_ra, 
     file = "xgb_models_jason_3.RData")

save(xgb_model_saral_aop, xgb_model_saral_bmm, xgb_model_saral_ecc,
     xgb_model_saral_inc, xgb_model_saral_ma, xgb_model_saral_ra, 
     file = "xgb_models_saral.RData")

save(xgb_model_sentinel_3a_aop, xgb_model_sentinel_3a_bmm, xgb_model_sentinel_3a_ecc,
     xgb_model_sentinel_3a_inc, xgb_model_sentinel_3a_ma, xgb_model_sentinel_3a_ra, 
     file = "xgb_models_sentinel_3a.RData")


# Loading the results
load("xgb_models_cryosat_2.RData")
load("xgb_models_haiyang_2a.RData")
load("xgb_models_jason_3.RData")
load("xgb_models_saral.RData")
load("xgb_models_sentinel_3a.RData")

# XGBoost - Tuning results -----------------------------------------------------

# Function
extract_model_result<- function(model, model_name) {
  # Extract the final XGBoost model
  xgb_model <- extract_fit_parsnip(model)$fit
  
  # Create a tibble with key parameters and performance metrics
  result_table <- tibble(
    model = model_name,                              # Add model name to distinguish
    min_n = xgb_model$params$min_child_weight,       # Extract min_n (min_child_weight)
    tree_depth = xgb_model$params$max_depth,         # Extract tree_depth
    learn_rate = xgb_model$params$eta,               # Extract learn_rate (eta)
    loss_reduction = xgb_model$params$gamma,         # Extract loss_reduction (gamma)
    n = length(xgb_model$evaluation_log$iter),       # Number of iterations
    metric = "rmse",                                # Assuming we are using RMSE metric
    mean = xgb_model$evaluation_log$training_rmse[xgb_model$best_iteration]  # Best RMSE score
  )
  
  return(result_table)
}

# CryoSat-2
xgb_model_cryosat_2_results <- 
  bind_rows(extract_model_result(xgb_model_cryosat_2_aop, "cryosat_2_aop"),
            extract_model_result(xgb_model_cryosat_2_bmm, "cryosat_2_bmm"),
            extract_model_result(xgb_model_cryosat_2_ecc, "cryosat_2_ecc"),
            extract_model_result(xgb_model_cryosat_2_inc, "cryosat_2_inc"),
            extract_model_result(xgb_model_cryosat_2_ma, "cryosat_2_ma"),
            extract_model_result(xgb_model_cryosat_2_ra, "cryosat_2_ra")) %>%
  arrange(mean)

# Haiyang-2A
xgb_model_haiyang_2a_results <- 
  bind_rows(extract_model_result(xgb_model_haiyang_2a_aop, "haiyang_2a_aop"),
            extract_model_result(xgb_model_haiyang_2a_bmm, "haiyang_2a_bmm"),
            extract_model_result(xgb_model_haiyang_2a_ecc, "haiyang_2a_ecc"),
            extract_model_result(xgb_model_haiyang_2a_inc, "haiyang_2a_inc"),
            extract_model_result(xgb_model_haiyang_2a_ma, "haiyang_2a_ma"),
            extract_model_result(xgb_model_haiyang_2a_ra, "haiyang_2a_ra")) %>%
  arrange(mean)

# Jason-3
xgb_model_jason_3_results <- 
  bind_rows(extract_model_result(xgb_model_jason_3_aop, "jason_3_aop"),
            extract_model_result(xgb_model_jason_3_bmm, "jason_3_bmm"),
            extract_model_result(xgb_model_jason_3_ecc, "jason_3_ecc"),
            extract_model_result(xgb_model_jason_3_inc, "jason_3_inc"),
            extract_model_result(xgb_model_jason_3_ma, "jason_3_ma"),
            extract_model_result(xgb_model_jason_3_ra, "jason_3_ra")) %>%
  arrange(mean)

# SARAL
xgb_model_saral_results <- 
  bind_rows(extract_model_result(xgb_model_saral_aop, "saral_aop"),
            extract_model_result(xgb_model_saral_bmm, "saral_bmm"),
            extract_model_result(xgb_model_saral_ecc, "saral_ecc"),
            extract_model_result(xgb_model_saral_inc, "saral_inc"),
            extract_model_result(xgb_model_saral_ma, "saral_ma"),
            extract_model_result(xgb_model_saral_ra, "saral_ra")) %>%
  arrange(mean)

# Sentinel-3A
xgb_model_sentinel_3a_results <- 
  bind_rows(extract_model_result(xgb_model_sentinel_3a_aop, "sentinel_3a_aop"),
            extract_model_result(xgb_model_sentinel_3a_bmm, "sentinel_3a_bmm"),
            extract_model_result(xgb_model_sentinel_3a_ecc, "sentinel_3a_ecc"),
            extract_model_result(xgb_model_sentinel_3a_inc, "sentinel_3a_inc"),
            extract_model_result(xgb_model_sentinel_3a_ma, "sentinel_3a_ma"),
            extract_model_result(xgb_model_sentinel_3a_ra, "sentinel_3a_ra")) %>%
  arrange(mean)

# XGBoost - Prediction Qualitative Analysis Function ---------------------------

xgb_pred_plot <- function(xgb_target, xgb_actual, xgb_man, xgb_final,m,delay, title) {
  
  xgb_data <- xgb_target %>% 
    rename(actual = xgb_actual)
  xgb_split <- initial_time_split(xgb_data, prop = 0.8)
  xgb_train <- training(xgb_split)
  xgb_test <- testing(xgb_split)
  
  # Combine predictions and actual values into a data frame
  pred_data <- data.frame(
    datetime = xgb_data$datetime,
    actual = xgb_data$actual,
    pred = predict(xgb_final, new_data = xgb_data)) %>% 
    rename(pred = .pred) %>% 
    mutate(residual = abs(actual - pred)) %>%
    tibble()
  
  # Define the time range for the plot
  start_time <- min(pred_data$datetime)
  end_time <- start_time + months(m)
  
  # Filter the data based on the time range
  result_data_filtered <- pred_data %>%
    filter(datetime >= start_time & datetime <= end_time)
  
  # Filter the man_filtered
  man_filtered <- xgb_man %>%
    filter(datetime >= start_time & datetime <= end_time) %>%
    mutate(datetime = datetime + days(delay))
  
  # Plot using ggplot2
  plot <- ggplot() +
    # Solid line for residuals
    geom_line(data = result_data_filtered, aes(x = datetime, y = residual, color = "Residuals"), 
              size = 0.6) +
    # Dashed line for maneuver events
    geom_vline(data = man_filtered, aes(xintercept = as.numeric(datetime), linetype = "Manoeuvre"), 
               color = "red3", size = 0.8) +
    labs(title = title, color = "Legend", linetype = "Legend") +  
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 18, face = "bold"),
          axis.text.y = element_text(size = 18, face = "bold"), 
          legend.text = element_text(size = 18, face = "bold"),
          plot.title = element_blank(),  
          legend.position = "bottom",     
          legend.title = element_blank(),  
          legend.key.size = unit(1.5, "cm")) +  
    scale_x_datetime(date_breaks = "2 months", date_labels = "%y-%b") +  
    scale_y_continuous(labels = label_number(accuracy = 0.01)) +     
    scale_color_manual(values = c("Residuals" = "black")) +  
    scale_linetype_manual(values = c("Manoeuvre" = "dashed"))
  
  
  return(plot)
  
}

xgb_pred_table <- function(xgb_target, xgb_actual, xgb_final, factor = 3) {
  
  # Prepare the data
  xgb_data <- xgb_target %>% 
    rename(actual = xgb_actual)
  
  # Combine predictions and actual values into a data frame
  pred_data <- data.frame(
    datetime = xgb_data$datetime,
    actual = xgb_data$actual,
    pred = predict(xgb_final, new_data = xgb_data)
  ) %>% 
    rename(pred = .pred) %>% 
    mutate(residual = abs(actual - pred))
  
  # Calculate threshold for maneuver probability
  residuals_mean <- mean(pred_data$residual)
  residuals_sd <- sd(pred_data$residual)
  threshold <- residuals_mean + (factor * residuals_sd)
  
  # Calculate maneuver probability as a ratio of residual to threshold
  pred_data <- pred_data %>%
    mutate(man_pro = pmin(residual / threshold, 1))
  
  # Create a simplified output with only datetime and maneuver probability
  output_table <- pred_data %>%
    select(datetime, man_pro)
  
  return(tibble(output_table))
}

# XGBoost - Prediction Qualitative Analysis CryoSat-2 --------------------------

# Each element plots
xbg_line_cryosat_2_aop <- xgb_pred_plot(cryosat_2_aop, "argument_of_perigee",cryosat_2_man,xgb_model_cryosat_2_aop, 24,0,"Argument of Perigee")
xbg_line_cryosat_2_bmm <- xgb_pred_plot(cryosat_2_bmm, "brouwer_mean_motion",cryosat_2_man,xgb_model_cryosat_2_bmm, 24,0,"Brouwer Mean Motion")
xbg_line_cryosat_2_ecc <- xgb_pred_plot(cryosat_2_ecc, "eccentricity",cryosat_2_man, xgb_model_cryosat_2_ecc, 24,0,"Eccentricity")
xbg_line_cryosat_2_inc <- xgb_pred_plot(cryosat_2_inc, "inclination",cryosat_2_man,xgb_model_cryosat_2_inc, 24,0,"Inclination")
xbg_line_cryosat_2_ma <- xgb_pred_plot(cryosat_2_ma, "mean_anomaly",cryosat_2_man,xgb_model_cryosat_2_ma, 24,0,"Mean Anomaly")
xbg_line_cryosat_2_ra <- xgb_pred_plot(cryosat_2_ra, "right_ascension",cryosat_2_man,xgb_model_cryosat_2_ra, 24,0,"Right Ascension")

# Combined plot
xbg_line_cryosat_2_plot_combined <- 
  (xbg_line_cryosat_2_aop / 
     xbg_line_cryosat_2_bmm / 
     xbg_line_cryosat_2_inc / 
     xbg_line_cryosat_2_ecc /
     xbg_line_cryosat_2_ma / 
     xbg_line_cryosat_2_ra) + 
  plot_annotation(
    title = "CryoSat-2 XGBoost Prediction Residuals",
    theme = theme(
      plot.title = element_text(size = 28, face = "bold")))



# XGBoost - Prediction Qualitative Analysis Haiyang-2A -------------------------

# Each element plots
xbg_line_haiyang_2a_aop <- xgb_pred_plot(haiyang_2a_aop, "argument_of_perigee",haiyang_2a_man,xgb_model_haiyang_2a_aop, 12,0,"Argument of Perigee")
xbg_line_haiyang_2a_bmm <- xgb_pred_plot(haiyang_2a_bmm, "brouwer_mean_motion",haiyang_2a_man,xgb_model_haiyang_2a_bmm, 12,0,"Brouwer Mean Motion")
xbg_line_haiyang_2a_ecc <- xgb_pred_plot(haiyang_2a_ecc, "eccentricity",haiyang_2a_man, xgb_model_haiyang_2a_ecc, 12,0,"Eccentricity")
xbg_line_haiyang_2a_inc <- xgb_pred_plot(haiyang_2a_inc, "inclination",haiyang_2a_man,xgb_model_haiyang_2a_inc, 12,0,"Inclination")
xbg_line_haiyang_2a_ma <- xgb_pred_plot(haiyang_2a_ma, "mean_anomaly",haiyang_2a_man,xgb_model_haiyang_2a_ma, 12,0,"Mean Anomaly")
xbg_line_haiyang_2a_ra <- xgb_pred_plot(haiyang_2a_ra, "right_ascension",haiyang_2a_man,xgb_model_haiyang_2a_ra, 12,0,"Right Ascension")

# Combined plot
xbg_line_haiyang_2a_plot_combined <- 
  (xbg_line_haiyang_2a_aop / 
     xbg_line_haiyang_2a_bmm / 
     xbg_line_haiyang_2a_inc / 
     xbg_line_haiyang_2a_ecc /
     xbg_line_haiyang_2a_ma / 
     xbg_line_haiyang_2a_ra) + 
  plot_annotation(
    title = "Haiyang-2A XGBoost Prediction Residuals",
    theme = theme(
      plot.title = element_text(size = 28, face = "bold")))

# XGBoost - Prediction Qualitative Analysis Jason-3 --------------------------------------------

# Each element plots
xbg_line_jason_3_aop <- xgb_pred_plot(jason_3_aop, "argument_of_perigee",jason_3_man,xgb_model_jason_3_aop, 12,3,"Argument of Perigee")
xbg_line_jason_3_bmm <- xgb_pred_plot(jason_3_bmm, "brouwer_mean_motion",jason_3_man,xgb_model_jason_3_bmm, 12,3,"Brouwer Mean Motion")
xbg_line_jason_3_ecc <- xgb_pred_plot(jason_3_ecc, "eccentricity",jason_3_man, xgb_model_jason_3_ecc, 12,3,"Eccentricity")
xbg_line_jason_3_inc <- xgb_pred_plot(jason_3_inc, "inclination",jason_3_man,xgb_model_jason_3_inc, 12,3,"Inclination")
xbg_line_jason_3_ma <- xgb_pred_plot(jason_3_ma, "mean_anomaly",jason_3_man,xgb_model_jason_3_ma, 12,3,"Mean Anomaly")
xbg_line_jason_3_ra <- xgb_pred_plot(jason_3_ra, "right_ascension",jason_3_man,xgb_model_jason_3_ra, 12,3,"Right Ascension")

# Combined plot
xbg_line_jason_3_plot_combined <- 
  (xbg_line_jason_3_aop / 
     xbg_line_jason_3_bmm / 
     xbg_line_jason_3_inc / 
     xbg_line_jason_3_ecc /
     xbg_line_jason_3_ma / 
     xbg_line_jason_3_ra) + 
  plot_annotation(
    title = "Jason-3 XGBoost Prediction Residuals",
    theme = theme(
      plot.title = element_text(size = 28, face = "bold")))


# XGBoost - Prediction Qualitative Analysis SARAL ----------------------------------------------

# Each element plots
xbg_line_saral_aop <- xgb_pred_plot(saral_aop, "argument_of_perigee",saral_man,xgb_model_saral_aop, 12,0,"Argument of Perigee")
xbg_line_saral_bmm <- xgb_pred_plot(saral_bmm, "brouwer_mean_motion",saral_man,xgb_model_saral_bmm, 12,0,"Brouwer Mean Motion")
xbg_line_saral_ecc <- xgb_pred_plot(saral_ecc, "eccentricity",saral_man, xgb_model_saral_ecc, 12,0,"Eccentricity")
xbg_line_saral_inc <- xgb_pred_plot(saral_inc, "inclination",saral_man,xgb_model_saral_inc, 12,0,"Inclination")
xbg_line_saral_ma <- xgb_pred_plot(saral_ma, "mean_anomaly",saral_man,xgb_model_saral_ma, 12,0,"Mean Anomaly")
xbg_line_saral_ra <- xgb_pred_plot(saral_ra, "right_ascension",saral_man,xgb_model_saral_ra, 12,0,"Right Ascension")

# Combined plot
xbg_line_saral_plot_combined <- 
  (xbg_line_saral_aop / 
     xbg_line_saral_bmm / 
     xbg_line_saral_inc / 
     xbg_line_saral_ecc /
     xbg_line_saral_ma / 
     xbg_line_saral_ra) + 
  plot_annotation(
    title = "SARAL XGBoost Prediction Residuals",
    theme = theme(
      plot.title = element_text(size = 28, face = "bold")))

# XGBoost - Prediction Qualitative Analysis Sentinel-3A ------------------------

# Each element plots
xbg_line_sentinel_3a_aop <- xgb_pred_plot(sentinel_3a_aop, "argument_of_perigee",sentinel_3a_man,xgb_model_sentinel_3a_aop, 12,0,"Argument of Perigee")
xbg_line_sentinel_3a_bmm <- xgb_pred_plot(sentinel_3a_bmm, "brouwer_mean_motion",sentinel_3a_man,xgb_model_sentinel_3a_bmm, 12,0,"Brouwer Mean Motion")
xbg_line_sentinel_3a_ecc <- xgb_pred_plot(sentinel_3a_ecc, "eccentricity",sentinel_3a_man, xgb_model_sentinel_3a_ecc, 12,0,"Eccentricity")
xbg_line_sentinel_3a_inc <- xgb_pred_plot(sentinel_3a_inc, "inclination",sentinel_3a_man,xgb_model_sentinel_3a_inc, 12,0,"Inclination")
xbg_line_sentinel_3a_ma <- xgb_pred_plot(sentinel_3a_ma, "mean_anomaly",sentinel_3a_man,xgb_model_sentinel_3a_ma, 12,0,"Mean Anomaly")
xbg_line_sentinel_3a_ra <- xgb_pred_plot(sentinel_3a_ra, "right_ascension",sentinel_3a_man,xgb_model_sentinel_3a_ra, 12,0,"Right Ascension")

# Combined plot
xbg_line_sentinel_3a_plot_combined <- 
  (
     xbg_line_sentinel_3a_ecc /
     xbg_line_sentinel_3a_ma / 
     xbg_line_sentinel_3a_ra) + 
  plot_annotation(
    title = "Sentinel-3A XGBoost Prediction Residuals",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold")))

# XGBoost - prediction Precision-Recall Analysis -------------------------------

# Precision and recall calculation
precision_recall_xgb <- function(xgb_target, xgb_actual, xgb_man, 
                                 xgb_final,threshold_days = 5) {
  
  xgb_data <- xgb_target %>% 
    rename(actual = xgb_actual)
  xgb_split <- initial_time_split(xgb_data, prop = 0.8)
  xgb_train <- training(xgb_split)
  xgb_test <- testing(xgb_split)
  
  # Combine predictions and actual values into a data frame
  pred_data <- data.frame(
    datetime = xgb_data$datetime,
    actual = xgb_data$actual,
    pred = predict(xgb_final, new_data = xgb_data)) %>% 
    rename(pred = .pred) %>% 
    mutate(residual = abs(actual - pred)) %>%
    select(-actual, -pred)

  # Initialize a list to store results
  results_list <- list()

  thresholds <- sort(pred_data$residual, decreasing = TRUE)

  # Loop through all possible thresholds
  for (i in 1:length(thresholds)) {
    threshold_value <- thresholds[i]

    # Find the closest ground-truth event within the threshold
    results <- pred_data %>%
      filter(residual >= threshold_value) %>%
      mutate(match_index = findInterval(datetime,xgb_man$datetime,all.inside = TRUE)) %>%
      mutate(ground_truth_timestamp = xgb_man$datetime[match_index]) %>%
      mutate(time_diff = abs(difftime(datetime,ground_truth_timestamp,units = "days")))

    TP <- n_distinct(results %>% filter(time_diff <= threshold_days) %>% pull(match_index))
    FP <- nrow(results %>% filter(time_diff > threshold_days))
    FN <- nrow(xgb_man) - TP

    # Calculate Precision and Recall
    precision <- ifelse(TP + FP > 0, TP / (TP + FP), 0)
    recall <- ifelse(TP + FN > 0, TP / (TP + FN), 0)

    # Calculate F1 Score
    F1_score <- ifelse(precision + recall > 0, 
                       2 * (precision * recall) / (precision + recall),
                       0)

    # Store the result in the list
    results_list[[i]] <- data.frame(Element = xgb_actual,
                                    Threshold = threshold_value,
                                    Precision = precision,
                                    Recall = recall,
                                    F1_score = F1_score)
  }

  # Combine the list into a tibble
  results_table <- do.call(rbind, results_list)
  return(as_tibble(results_table))
}

cryosat_2_xgb_pr <- bind_rows(precision_recall_xgb(cryosat_2_aop, "argument_of_perigee", cryosat_2_man,xgb_model_cryosat_2_aop),
                          precision_recall_xgb(cryosat_2_bmm, "brouwer_mean_motion", cryosat_2_man,xgb_model_cryosat_2_bmm),
                          precision_recall_xgb(cryosat_2_ecc, "eccentricity", cryosat_2_man,xgb_model_cryosat_2_ecc),
                          precision_recall_xgb(cryosat_2_inc, "inclination", cryosat_2_man,xgb_model_cryosat_2_inc),
                          precision_recall_xgb(cryosat_2_ma, "mean_anomaly", cryosat_2_man,xgb_model_cryosat_2_ma),
                          precision_recall_xgb(cryosat_2_ra, "right_ascension", cryosat_2_man,xgb_model_cryosat_2_ra))

haiyang_2a_xgb_pr <- bind_rows(precision_recall_xgb(haiyang_2a_aop, "argument_of_perigee", haiyang_2a_man,xgb_model_haiyang_2a_aop),
                          precision_recall_xgb(haiyang_2a_bmm, "brouwer_mean_motion", haiyang_2a_man,xgb_model_haiyang_2a_bmm),
                          precision_recall_xgb(haiyang_2a_ecc, "eccentricity", haiyang_2a_man,xgb_model_haiyang_2a_ecc),
                          precision_recall_xgb(haiyang_2a_inc, "inclination", haiyang_2a_man,xgb_model_haiyang_2a_inc),
                          precision_recall_xgb(haiyang_2a_ma, "mean_anomaly", haiyang_2a_man,xgb_model_haiyang_2a_ma),
                          precision_recall_xgb(haiyang_2a_ra, "right_ascension", haiyang_2a_man,xgb_model_haiyang_2a_ra))

jason_3_xgb_pr <- bind_rows(precision_recall_xgb(jason_3_aop, "argument_of_perigee", jason_3_man,xgb_model_jason_3_aop),
                            precision_recall_xgb(jason_3_bmm, "brouwer_mean_motion", jason_3_man,xgb_model_jason_3_bmm),
                            precision_recall_xgb(jason_3_ecc, "eccentricity", jason_3_man,xgb_model_jason_3_ecc),
                            precision_recall_xgb(jason_3_inc, "inclination", jason_3_man,xgb_model_jason_3_inc),
                            precision_recall_xgb(jason_3_ma, "mean_anomaly", jason_3_man,xgb_model_jason_3_ma),
                            precision_recall_xgb(jason_3_ra, "right_ascension", jason_3_man,xgb_model_jason_3_ra))

saral_xgb_pr <- bind_rows(precision_recall_xgb(saral_aop, "argument_of_perigee", saral_man,xgb_model_saral_aop),
                          precision_recall_xgb(saral_bmm, "brouwer_mean_motion", saral_man,xgb_model_saral_bmm),
                          precision_recall_xgb(saral_ecc, "eccentricity", saral_man,xgb_model_saral_ecc),
                          precision_recall_xgb(saral_inc, "inclination", saral_man,xgb_model_saral_inc),
                          precision_recall_xgb(saral_ma, "mean_anomaly", saral_man,xgb_model_saral_ma),
                          precision_recall_xgb(saral_ra, "right_ascension", saral_man,xgb_model_saral_ra))

sentinel_3a_xgb_pr <- bind_rows(precision_recall_xgb(sentinel_3a_aop, "argument_of_perigee", sentinel_3a_man,xgb_model_sentinel_3a_aop),
                                precision_recall_xgb(sentinel_3a_bmm, "brouwer_mean_motion", sentinel_3a_man,xgb_model_sentinel_3a_bmm),
                                precision_recall_xgb(sentinel_3a_ecc, "eccentricity", sentinel_3a_man,xgb_model_sentinel_3a_ecc),
                                precision_recall_xgb(sentinel_3a_inc, "inclination", sentinel_3a_man,xgb_model_sentinel_3a_inc),
                                precision_recall_xgb(sentinel_3a_ma, "mean_anomaly", sentinel_3a_man,xgb_model_sentinel_3a_ma),
                                precision_recall_xgb(sentinel_3a_ra, "right_ascension", sentinel_3a_man,xgb_model_sentinel_3a_ra))

# Precision and recall Curve
precision_recall_curve <- function(results, title = "Precision-Recall Curves by Element") {
  ggplot(results, aes(x = Recall, y = Precision, color = Element, group = Element)) +
    geom_line(size = 0.8) +
    labs(title = title,
         x = "Recall",
         y = "Precision") +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.title.x = element_text(size = 18, face = "bold"),
          axis.title.y = element_text(size = 18, face = "bold"),
          axis.text.x = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 20, face = "bold"),
          legend.text = element_text(size = 12, face = "bold"),
          legend.title = element_text(size = 14, face = "bold"))
}

precision_recall_curve(cryosat_2_xgb_pr, "CryoSat-2 XGBoost Precision-Recall Curves by Element")
precision_recall_curve(haiyang_2a_xgb_pr, "Haiyang-2A XGBoost Precision-Recall Curves by Element")
precision_recall_curve(jason_3_xgb_pr, "Jason-3 XGBoost Precision-Recall Curves by Element")
precision_recall_curve(saral_xgb_pr, "SARAL XGBoost Precision-Recall Curves by Element")
precision_recall_curve(sentinel_3a_xgb_pr, "Sentinel-3A XGBoost Precision-Recall Curves by Element")

# Optimal F1 score
optimal_f1_score <- function(results) {
  
  output <- results %>% 
    group_by(Element) %>%
    filter(F1_score == max(F1_score, na.rm = TRUE)) %>%
    slice_max(F1_score, with_ties = FALSE) %>%
    select(Element, Threshold, Precision, Recall, F1_score) %>%
    arrange(desc(F1_score))
  
  return(output)
}

optimal_f1_score(cryosat_2_xgb_pr)
optimal_f1_score(haiyang_2a_xgb_pr)
optimal_f1_score(jason_3_xgb_pr)
optimal_f1_score(saral_xgb_pr)
optimal_f1_score(sentinel_3a_xgb_pr)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------



