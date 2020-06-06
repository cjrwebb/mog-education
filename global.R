
# Mapping Overlaps Gadget: Education GLOBAL FUNCTIONS ---------------------

# Packages
# There are probably some functions and packages 
# here that are no longer used and could be removed

library(shiny)
library(tidyverse)
library(plotly)
library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(sf)
library(lazyeval)
library(rlang)
library(mltools)
library(RColorBrewer)
library(gsubfn)
library(shinycssloaders)
library(shinyjs)
library(waiter)
library(psych)
library(broom)
library(DT)
library(ggeffects)


# This tool is designed to map the data from many administrative sources
# into univariate and bivariate maps, as well as to provide an interface 
# for various basic statistical tests and linear models


# Setup ---------------------------------------------------------------

# Loading page function:

spinner <- tagList(
  spin_pixel(),
  span("Loading the MOG...", style="color:white;")
)

# You will need to register for a mapbox token and save it as "mapbox_token.txt" run the app
# https://www.mapbox.com 
mapbox_access_token = read_file("mapbox_token.txt")
Sys.setenv('MAPBOX_TOKEN' = mapbox_access_token)


# Load Pre-prepared data -------------------------------------------------------
lsoa_data_spatial <- read_rds("data/mog_datav2.Rds")
labels_lsoa_data <- read_csv("data/lsoa_dataset_labelsv2.csv")

# Tidy labels
labels_lsoa_data <- labels_lsoa_data %>% mutate(
  label = paste(label, " - (", var_name, ")", sep = "")
)

# Make some copies of labels for dropdowns
labels_lsoa_data_univ <- labels_lsoa_data
labels_lsoa_data_2 <- labels_lsoa_data

eng_wales_checker <- labels_lsoa_data$label[4:nrow(labels_lsoa_data_univ)] %>% str_detect("England & Wales")


# Functions ------------------------------------------------

### Create functions for automating bivariate maps

# Function for calculating tertiles.
tertile_values <- function(data, variable, la) {
  require("dplyr")
  require("lazyeval")
  variable = enquo(variable)
  
  filtered_data <- data %>% filter(LA_name %in% la)
  var_quantile <- filtered_data %>% pull(!!variable) %>%
    quantile(probs = seq(0, 1, length.out = 4), na.rm = TRUE)
  
  if (sum(duplicated(as.numeric(var_quantile))) == 0) {
    print("Unique quantiles detected - Returning regular tertiles")
    return(var_quantile)
  } else {
    print("Non-unique quantiles detected - Setting tertile 1 as 0, tertile 2 as less than median, tertile 3 as greater than median.")
    
    rare_quantile <- filtered_data %>% filter(!!variable > 0) %>% 
                        pull(!!variable) %>% 
                        quantile(probs = seq(0, 1, length.out = 3))
    
    rare_quantile <- c(0, rare_quantile)
    
    names(rare_quantile) <- c("0%", "33.3333%", "66.66667%", "100%")
    
    return(rare_quantile)
  }
  
}


# Calculate cuts and join to colour scale
bivar_cuts <- function(data, x, y, la) {
  require(dplyr)
  require(lazyeval)
  require(rlang)
  
  # enquote the variables
  x <- enquo(x)
  y <- enquo(y)
  
  x_st <- get_expr(x)
  y_st <- get_expr(y)
  
  bivariate_colour_scale <- tibble(
    "3 - 3" = "#3F2949", # high IMD, high LAC
    "2 - 3" = "#435786",
    "1 - 3" = "#4885C1", # low IMD, high LAC
    "3 - 2" = "#77324C",
    "2 - 2" = "#806A8A", # medium IMD, medium LAC
    "1 - 2" = "#89A1C8",
    "3 - 1" = "#AE3A4E", # high IMD, low LAC
    "2 - 1" = "#BC7C8F",
    "1 - 1" = "#CABED0" # low IMD, low LAC
  ) %>%
    gather("group", "fill")
  
  # Colour scale
  
  bivariate_colour_scale$bivar_key <- c(
    paste("<br>", "3 (High) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""), # 3 (High) X, 3 (High) Y
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""),
    paste("<br>", "1 (Low) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""), # 1 (Low) X, 3 (High) Y
    paste("<br>", "3 (High) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""),
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""), # medium X, medium Y
    paste("<br>", "1 (Low) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""),
    paste("<br>", "3 (High) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = ""), # 3 (High) X, 1 (Low) Y
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = ""),
    paste("<br>", "1 (Low) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = "") # 1 (Low) X, 1 (Low) Y
  )
  
  # Filter data
  fil_data <- data %>% filter(LA_name %in% la)


# Return data with tertile combinations linked to colour scale  
mutate(fil_data,
      X_tertile = cut(
          # remember bang bangs for evaluation
          !!x,
          breaks = tertile_values(data, !!x, la),
          include.lowest = TRUE
          ),
       Y_tertile = cut(
         # remember bang bangs for evaluation
         !!y,
         breaks = tertile_values(data, !!y, la),
         include.lowest = TRUE
       ),
      bivar_group = paste(
        as.numeric(X_tertile), "-",
        as.numeric(Y_tertile)
      )
) %>%
  left_join(bivariate_colour_scale, by = c("bivar_group" = "group")
  )  

  
}


# Function for drawing bivariate maps:
draw_bivariate_map <- function(data, x, y, la) {
  require(dplyr)
  require(lazyeval)
  require(rlang)
  require(plotly)
  
  x <- enquo(x)
  y <- enquo(y)

  
  x_st <- get_expr(x)
  y_st <- get_expr(y)
  
  # Colours lookup
  
  bivariate_scale <- c(
    "3 - 3" = "#3F2949", # high IMD, high LAC
    "2 - 3" = "#435786",
    "1 - 3" = "#4885C1", # low IMD, high LAC
    "3 - 2" = "#77324C",
    "2 - 2" = "#806A8A", # medium IMD, medium LAC
    "1 - 2" = "#89A1C8",
    "3 - 1" = "#AE3A4E", # high IMD, low LAC
    "2 - 1" = "#BC7C8F",
    "1 - 1" = "#CABED0" # low IMD, low LAC
  )
  
  names(bivariate_scale) <- c(
    paste("<br>", "3 (High) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""), # 3 (High) X, 3 (High) Y
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""),
    paste("<br>", "1 (Low) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""), # 1 (Low) X, 3 (High) Y
    paste("<br>", "3 (High) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""),
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""), # medium X, medium Y
    paste("<br>", "1 (Low) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""),
    paste("<br>", "3 (High) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = ""), # 3 (High) X, 1 (Low) Y
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = ""),
    paste("<br>", "1 (Low) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = "") # 1 (Low) X, 1 (Low) Y
  )
  

  # Drop unnecessary variables
  data_lite <- select(data, LSOA11CD, msoa11hclnm, !!x, !!y, LA_name)
  
  # Data transformations
  map_data <- bivar_cuts(data_lite, !!x, !!y, la)
  
  
  plot_mapbox(map_data,
              alpha = 0.6,
              alpha_stroke = 0.1,
              color = ~bivar_key,
              colors = bivariate_scale,
              text = ~paste(x_st, ": ", round(eval(parse(text = x_st)), 2), "<br>",
                            y_st, ": ", round(eval(parse(text = y_st)), 2), "<br>",
                            "<b>", msoa11hclnm, "</b>", sep = ""),
              hovertemplate = paste("%{text}<extra></extra>"),
              showlegend = FALSE
  ) %>%
    layout(
      mapbox = list(zoom = 8,
                    style = "mapbox://styles/drcalumwebb/ck55r34nt00yh1dol4wfi3xi4",
                    accesstoken = mapbox_access_token),
      legend = list(orientation = "h",   # show entries horizontally
                    xanchor = "center",  # use center of legend as anchor
                    x = 0.5)
    )
  

  
}

 
# Create function for key for Shiny App
draw_bivar_legend <- function(x, y) {
  require(rlang)
  require(dplyr)
  require(tidyverse)
  
  x <- enquo(x)
  y <- enquo(y)
  x_st <- get_expr(x)
  y_st <- get_expr(y)
  
  # Bivariate colour scale
  bivariate_scale <- tibble(
    "3 - 3" = "#3F2949", # high IMD, high LAC
    "2 - 3" = "#435786",
    "1 - 3" = "#4885C1", # low IMD, high LAC
    "3 - 2" = "#77324C",
    "2 - 2" = "#806A8A", # medium IMD, medium LAC
    "1 - 2" = "#89A1C8",
    "3 - 1" = "#AE3A4E", # high IMD, low LAC
    "2 - 1" = "#BC7C8F",
    "1 - 1" = "#CABED0" # low IMD, low LAC
  ) %>% 
    gather(group, fill)
  
  
  bivariate_scale2 <- bivariate_scale %>%
    separate(group, into = c(as_string(x_st), as_string(y_st)), sep = " - ") %>%
    mutate(x_st = as.integer(!!x), y_st = as.integer(!!y))
  
  legend <- ggplot() +
    geom_tile(data = bivariate_scale2, mapping = aes(x = !!x, y = !!y, fill = fill, alpha = 0.75)) +
    scale_fill_identity() +
    labs(x = paste("Higher", as_string(x_st), "\u2192"), y = paste("Higher", as_string(y_st), "\u2192")) +
    theme_minimal() +
    theme(    
      axis.line = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_text(size = rel(1)),
      legend.position = "none"
      ) +
    coord_fixed()
  
  legend
  
}

# Create quantiles key
# for red
draw_red_scale <- function(data, x, la) {
  require(rlang)
  require(dplyr)
  require(tidyverse)
  
  x <- enquo(x)
  x_st <- get_expr(x)
  
  # Scale values as table
    red_scale <- tibble(
      "3 - 1" = "#AE3A4E", # high IMD, low LAC
      "2 - 1" = "#BC7C8F",
      "1 - 1" = "#CABED0" # low IMD, low LAC
    ) %>% 
      gather(group, fill)
    
    # Name columns name of variable x
    red_scale <- red_scale %>%
      separate(group, into = c(as_string(x_st), "y", sep = " - ")) %>%
      mutate(x_st = as.integer(!!x),
             y = as.integer(y))
    
    first_tertile <- paste(round(tertile_values(data, !!x, la)[1], 1), 
                            "-", 
                            round(tertile_values(data, !!x, la)[2], 1))
    second_tertile <- paste(round(tertile_values(data, !!x, la)[2], 1), 
                           "-", 
                           round(tertile_values(data, !!x, la)[3], 1))
    third_tertile <- paste(round(tertile_values(data, !!x, la)[3], 1), 
                            "-", 
                            round(tertile_values(data, !!x, la)[4], 1))
    
    red_scale <- red_scale %>% arrange(x_st)
    
    red_scale$tertile_vals <- c(first_tertile, 
                                second_tertile, 
                                third_tertile)
    
    ggplot() +
      geom_tile(data = red_scale, mapping = aes(x = !!x, y = y, fill = fill, alpha = 0.75)) +
      geom_text(data = red_scale, aes(x = !!x, y = y, label = tertile_vals)) +
      scale_fill_identity() +
      labs(x = paste("Higher", as_string(x_st), "\u2192"), y = "") +
      theme_minimal() +
      theme(    
        axis.line = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = rel(1)),
        legend.position = "none"
      ) +
      coord_fixed(1/6)
}



# for blue
draw_blue_scale <- function(data, x, la) {
  require(rlang)
  require(dplyr)
  require(tidyverse)
  
  x <- enquo(x)
  x_st <- get_expr(x)
  
  # Scale values as table
  red_scale <- tibble(
    "3 - 1" = "#4885C1", # high IMD, low LAC
    "2 - 1" = "#89A1C8",
    "1 - 1" = "#CABED0" # low IMD, low LAC
  ) %>% 
    gather(group, fill)
  
  # Name columns name of variable x
  red_scale <- red_scale %>%
    separate(group, into = c(as_string(x_st), "y", sep = " - ")) %>%
    mutate(x_st = as.integer(!!x),
           y = as.integer(y))
  
  first_tertile <- paste(round(tertile_values(data, !!x, la)[1], 1), 
                         "-", 
                         round(tertile_values(data, !!x, la)[2], 1))
  second_tertile <- paste(round(tertile_values(data, !!x, la)[2], 1), 
                          "-", 
                          round(tertile_values(data, !!x, la)[3], 1))
  third_tertile <- paste(round(tertile_values(data, !!x, la)[3], 1), 
                         "-", 
                         round(tertile_values(data, !!x, la)[4], 1))
  
  red_scale <- red_scale %>% arrange(x_st)
  
  red_scale$tertile_vals <- c(first_tertile, 
                              second_tertile, 
                              third_tertile)
  
  ggplot() +
    geom_tile(data = red_scale,
              mapping = aes(x = !!x, y = y, fill = fill, alpha = 0.75)) +
    geom_text(data = red_scale, aes(x = !!x, y = y,label = tertile_vals)) +
    scale_fill_identity() +
    labs(x = paste("Higher", as_string(x_st), "\u2192"), y = "") +
    theme_minimal() +
    theme(    
      axis.line = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_text(size = rel(1)),
      legend.position = "none"
    ) +
    coord_fixed(1/6)
}



# Univariate maps ---------------------------------------------------------

# Function to convert into ntiles and assign custom colour scale called fill
# This is still not working perfectly - the way bins are split - needs to be changed, maybe using cut_interval. Low priority

bin_variable <- function(data, variable, la, nbins = 10) {
  require("dplyr")
  require("lazyeval")
  require("mltools")
  
  variable = enquo(variable)
  variable_st = get_expr(variable)
  nbins = nbins
  
  # filter data
  filtered_data <- data %>% filter(LA_name %in% la)
  
  var_binned <- filtered_data %>% mutate(
    binned_var = bin_data(round(!!variable, 3), bins = nbins, binType = "quantile")
  ) %>%
    mutate(
      binned_var = str_replace_all(binned_var, "\\,", " -")
    ) %>%
    mutate(
      binned_var = str_remove(binned_var, "\\[")
    ) %>%
    mutate(
      binned_var = str_remove(binned_var, "\\]")
    ) %>%
    mutate(
      binned_var = str_remove(binned_var, "\\(|\\)")
    ) %>%
    mutate(
      binned_var = paste0(variable_st, ": ", binned_var)
    )
  
  # Write 10 bins version of bins
  var_binned  <- var_binned %>% mutate(
    bin_upper = as.numeric(str_extract(binned_var, "(?<=- )[0-9]*.*"))
  )

  # Join numerically ordered bins (for sorting)
  
  bin_lth_lookup <- tibble("bin_upper" = unique(sort(var_binned$bin_upper))) %>%
    arrange(bin_upper)
  
  bin_lth_lookup <- bin_lth_lookup %>% mutate(bin_low_high = as.numeric(rownames(bin_lth_lookup)))

  var_binned <- var_binned %>% left_join(bin_lth_lookup, by = "bin_upper")

  # Create dynamic colour lookup with rcolorbrewer and ramp
  bin_col_lookup <- tibble(
    "bin_low_high" = seq(1, length(unique(var_binned$bin_upper)), 1),
    "bin_colour" = colorRampPalette(brewer.pal(3, "OrRd"))(length(unique(var_binned$bin_upper)))
  )

  var_binned <- var_binned %>% left_join(bin_col_lookup,
                                         by = "bin_low_high") %>%
    mutate(
      binned_var = paste0("<br>Bin ", bin_low_high, ": <br>", binned_var, "<br>")
    )

 return(var_binned)

}



# Custom univariate colour scale (remember needs number of bins)
univariate_scale <- function(nbins) {
  nbins = nbins
  
  univariate_scale <- tibble(
    bin_low_high = seq(1, nbins, 1),
    bin_col_scale = colorRampPalette(brewer.pal(3, "OrRd"))(nbins)
  )
  
  return(univariate_scale)
  
}


# Function for drawing univariate plot ------
draw_univariate_map  <- function(data, x, nbins, la) {
  require("dplyr")
  require("lazyeval")
  require("mltools")
  require("plotly")
  
  x <- enquo(x)
  x_st <- get_expr(x)
    
  # Create bins
  data <- bin_variable(data, !!x, la, nbins)
    
    
    # Create colour scale for bins
    univariate_col_lookup_dt <- left_join(univariate_scale(nbins), 
                                        data %>%
                                          group_by(binned_var) %>%
                                          summarise(
                                            bin_low_high = first(bin_low_high)
                                          ),
                                        by = "bin_low_high"
                                      ) %>%
                                drop_na()
    
    
    # Create lookup vector for colour scale that can be read by Plotly
    univariate_col_lookup <- setNames(c(univariate_col_lookup_dt$bin_col_scale), 
                                      univariate_col_lookup_dt$binned_var)
    
    # Create plot - could do quantile version but wouldn't work with some data
    plot_mapbox(data,
                alpha = 0.6,
                alpha_stroke = 0.1,
                color = ~binned_var,
                colors = univariate_col_lookup,
                text = ~paste(x_st, ": ", round(eval(parse(text = x_st)), 2), "<br>",
                              "<b>", msoa11hclnm, "</b>", sep = ""),
                hovertemplate = paste("%{text}<extra></extra>"),
                showlegend = FALSE
    ) %>%
      layout(
        mapbox = list(zoom = 8,
                      style = "mapbox://styles/drcalumwebb/ck55r34nt00yh1dol4wfi3xi4",
                      accesstoken = mapbox_access_token)
      )

}
    

    
# Function for ggplot univariate map legend
draw_univariate_legend  <- function(data, x, nbins, la) {
  require("dplyr")
  require("lazyeval")
  require("mltools")
  require("plotly")
  
  x <- enquo(x)
  x_st <- get_expr(x)
  
  # Create bins
  data <- bin_variable(data, !!x, la, nbins)
  
  
  # Create colour scale for bins
  univariate_col_lookup_dt <- left_join(univariate_scale(nbins), 
                                        data %>%
                                          group_by(binned_var) %>%
                                          mutate(
                                            bin_var_raw = bin_data(round(!!x, 3), bins = nbins+1, boundaryType = "lcro)", binType = "quantile")
                                          ) %>%
                                          summarise(
                                            bin_low_high = first(bin_low_high),
                                            bin_var_raw = first(bin_var_raw)
                                          ),
                                        by = "bin_low_high"
  ) %>%
    drop_na() %>%
    mutate(
      bin_var_raw = str_replace(bin_var_raw, "\\,", " -")
    )  %>%
    mutate(
      bin_var_raw = str_remove_all(bin_var_raw, "\\[|\\)")
    ) %>% mutate(
      bin_var_raw = gsubfn("([0-9.]+)", ~format(round(as.numeric(bin_var_raw), 2), nsmall=2), bin_var_raw)
    )
  
  univariate_col_lookup_dt$y <- rep(1, times = nrow(univariate_col_lookup_dt))
  
  univariate_col_lookup_dt <- univariate_col_lookup_dt %>% arrange(bin_low_high) %>%
    mutate(bin_low_high_valid = seq(1, nrow(univariate_col_lookup_dt), 1)
           )

  ggplot() +
    geom_tile(
      data = univariate_col_lookup_dt,
      mapping = aes(
        x = bin_low_high_valid,
        y = y,
        fill = bin_col_scale,
        alpha = 0.75)
    ) +
    geom_text(
      data = univariate_col_lookup_dt,
      aes(
        x = bin_low_high_valid,
        y = y,
        label = bin_var_raw
      )) +
    scale_fill_identity() +
    labs(x = paste("Higher", as_string(x_st), "\u2192"),
         y = "") +
    theme_minimal(
    ) +
    theme(
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_text(size = rel(1)),
      legend.position = "none"
    ) +
    coord_fixed(1/8)

  
}


# Plotly bivariate legend

draw_bivar_legend_plotly <- function(x, y) {
  require(rlang)
  require(dplyr)
  require(tidyverse)
  
  x <- enquo(x)
  y <- enquo(y)
  x_st <- get_expr(x)
  y_st <- get_expr(y)
  
  # Bivariate colour scale
  bivariate_scale <- tibble(
    "3 - 3" = "#3F2949", # high IMD, high LAC
    "2 - 3" = "#435786",
    "1 - 3" = "#4885C1", # low IMD, high LAC
    "3 - 2" = "#77324C",
    "2 - 2" = "#806A8A", # medium IMD, medium LAC
    "1 - 2" = "#89A1C8",
    "3 - 1" = "#AE3A4E", # high IMD, low LAC
    "2 - 1" = "#BC7C8F",
    "1 - 1" = "#CABED0" # low IMD, low LAC
  ) %>% 
    gather(group, fill)
  
  
  bivariate_scale2 <- bivariate_scale %>%
    separate(group, into = c(as_string(x_st), as_string(y_st)), sep = " - ") %>%
    mutate(x_st = as.integer(!!x),
           y_st = as.integer(!!y))
  
  # Add interpretable names
   bivariate_scale2$tilenames <- c(
    paste("<br>", "3 (High) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""), # 3 (High) X, 3 (High) Y
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""),
    paste("<br>", "1 (Low) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""), # 1 (Low) X, 3 (High) Y
    paste("<br>", "3 (High) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""),
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""), # medium X, medium Y
    paste("<br>", "1 (Low) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""),
    paste("<br>", "3 (High) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = ""), # 3 (High) X, 1 (Low) Y
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = ""),
    paste("<br>", "1 (Low) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = "") # 1 (Low) X, 1 (Low) Y
  )
   
   # Need to create numeric codes for fills and then can use colors argument linked with names
   bivariate_scale2 <- bivariate_scale2 %>%
     mutate(x_st = as.numeric(x_st),
            y_st = as.numeric(y_st),
            colkey = paste(x_st, " - ", y_st, sep = ""),
            colkey_numeric = seq(1, nrow(.), 1))
   
   colkey = bivariate_scale2$fill
   
   colkey <- setNames(colkey, bivariate_scale2$colkey_numeric)
   
   
   plot_ly(bivariate_scale2,
           x = ~x_st,
           y = ~y_st,
           z = ~colkey_numeric,
           text = ~tilenames,
           hoverinfo = "text",
           colors = colkey,
           type = "heatmap",
           showscale = FALSE
   ) %>% 
     layout(
       xaxis = list(title = paste0("Higher ", x_st, " \u2192"),
                    showline = FALSE,
                    showticklabels = FALSE,
                    showgrid = FALSE,
                    zeroline = FALSE,
                    showticks = FALSE,
                    autotick = FALSE,
                    # Fix coords equal
                    scaleanchor = "y", 
                    scaleratio = 1),
       yaxis = list(title = paste0("Higher ", y_st, " \u2192"),
                    showline = FALSE,
                    showticklabels = FALSE,
                    showgrid = FALSE,
                    zeroline = FALSE,
                    autotick = FALSE,
                    scaleanchor = "x", 
                    scaleratio = 1)
     )
  
  
}


draw_bivariate_map_coupled <- function(data, x, y, la) {
  require(dplyr)
  require(lazyeval)
  require(rlang)
  require(plotly)
  
  x <- enquo(x)
  y <- enquo(y)
  
  
  x_st <- get_expr(x)
  y_st <- get_expr(y)
  
  # Colours lookup
  bivariate_scale <- c(
    "3 - 3" = "#3F2949", # high IMD, high LAC
    "2 - 3" = "#435786",
    "1 - 3" = "#4885C1", # low IMD, high LAC
    "3 - 2" = "#77324C",
    "2 - 2" = "#806A8A", # medium IMD, medium LAC
    "1 - 2" = "#89A1C8",
    "3 - 1" = "#AE3A4E", # high IMD, low LAC
    "2 - 1" = "#BC7C8F",
    "1 - 1" = "#CABED0" # low IMD, low LAC
  )

  names(bivariate_scale) <- c(
    paste("<br>", "3 (High) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""), # 3 (High) X, 3 (High) Y
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""),
    paste("<br>", "1 (Low) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""), # 1 (Low) X, 3 (High) Y
    paste("<br>", "3 (High) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""),
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""), # medium X, medium Y
    paste("<br>", "1 (Low) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""),
    paste("<br>", "3 (High) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = ""), # 3 (High) X, 1 (Low) Y
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = ""),
    paste("<br>", "1 (Low) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = "") # 1 (Low) X, 1 (Low) Y
  )

  
  plot_mapbox(data,
              alpha = 0.6,
              alpha_stroke = 0.1,
              color = ~bivar_key,
              colors = bivariate_scale,
              text = ~paste(x_st, ": ", round(eval(parse(text = x_st)), 2), "<br>",
                            y_st, ": ", round(eval(parse(text = y_st)), 2), "<br>",
                            "<b>", msoa11hclnm, "</b>", sep = ""),
              hovertemplate = paste("%{text}<extra></extra>"),
              showlegend = FALSE
  ) %>%
    layout(
      mapbox = list(zoom = 8,
                    style = "mapbox://styles/drcalumwebb/ck55r34nt00yh1dol4wfi3xi4",
                    accesstoken = mapbox_access_token),
      legend = list(orientation = "h",   # show entries horizontally
                    xanchor = "center",  # use center of legend as anchor
                    x = 0.5)
    )
  
  
  
}


draw_bivar_legend_plotly_coupled <- function(x, y) {
  require(rlang)
  require(dplyr)
  require(tidyverse)
  
  x <- enquo(x)
  y <- enquo(y)
  x_st <- get_expr(x)
  y_st <- get_expr(y)
  
  # Bivariate colour scale
  bivariate_scale <- tibble(
    "3 - 3" = "#3F2949", # high IMD, high LAC
    "2 - 3" = "#435786",
    "1 - 3" = "#4885C1", # low IMD, high LAC
    "3 - 2" = "#77324C",
    "2 - 2" = "#806A8A", # medium IMD, medium LAC
    "1 - 2" = "#89A1C8",
    "3 - 1" = "#AE3A4E", # high IMD, low LAC
    "2 - 1" = "#BC7C8F",
    "1 - 1" = "#CABED0" # low IMD, low LAC
  ) %>% 
    gather(group, fill)
  
  
  bivariate_scale2 <- bivariate_scale %>%
    separate(group, into = c(as_string(x_st), as_string(y_st)), sep = " - ") %>%
    mutate(x_st = as.integer(!!x),
           y_st = as.integer(!!y))
  
  # Add interpretable names
  bivariate_scale2$tilenames <- c(
    paste("<br>", "3 (High) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""), # 3 (High) X, 3 (High) Y
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""),
    paste("<br>", "1 (Low) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""), # 1 (Low) X, 3 (High) Y
    paste("<br>", "3 (High) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""),
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""), # medium X, medium Y
    paste("<br>", "1 (Low) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""),
    paste("<br>", "3 (High) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = ""), # 3 (High) X, 1 (Low) Y
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = ""),
    paste("<br>", "1 (Low) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = "") # 1 (Low) X, 1 (Low) Y
  )
  
  # Need to create numeric codes for fills and then can use colors argument linked with names
  bivariate_scale2 <- bivariate_scale2 %>%
    mutate(x_st = as.numeric(x_st),
           y_st = as.numeric(y_st),
           colkey = paste(x_st, " - ", y_st, sep = ""),
           colkey_numeric = seq(1, nrow(.), 1))
  
  colkey = bivariate_scale2$fill
  
  colkey <- setNames(colkey, bivariate_scale2$colkey_numeric)
  

  plot_ly(
    bivariate_scale2,
          x = ~x_st,
          y = ~y_st,
          z = ~colkey_numeric,
          text = ~tilenames,
          hoverinfo = "none",
          colors = colkey,
          alpha = 0.6,
          type = "heatmap",
          showscale = FALSE,
          key = ~fill
  ) %>%
    add_trace(bivariate_scale2,
              type = "scatter",
                x = ~x_st,
                y = ~y_st,
                text = ~tilenames,
                hoverinfo = "text",
                color = ~colkey_numeric,
                colors = colkey,
                showlegend = FALSE,
                showscale = FALSE,
                marker = list(size = 40),
                source = "subset",
                key = ~fill) %>%
    hide_colorbar() %>%
    layout(
      xaxis = list(title = paste0("Higher ", x_st, " \u2192"),
                   showline = FALSE,
                   showticklabels = FALSE,
                   showgrid = FALSE,
                   zeroline = FALSE,
                   showticks = FALSE,
                   autotick = FALSE,
                   # Fix coords equal
                   scaleanchor = "y", 
                   scaleratio = 1),
      yaxis = list(title = paste0("Higher ", y_st, " \u2192"),
                   showline = FALSE,
                   showticklabels = FALSE,
                   showgrid = FALSE,
                   zeroline = FALSE,
                   autotick = FALSE,
                   scaleanchor = "x", 
                   scaleratio = 1),
      clickmode = "select+event",
      hovermode = "closest",
      showscale = FALSE
    ) 

  
}



# Function for Scatterplot/Correlation Tests ------------------------------

# Function for Scatterplot

plot_corr <- function(data, x, y, la) {
  
  require(dplyr)
  require(lazyeval)
  require(rlang)
  require(plotly)
  
  x <- enquo(x)
  y <- enquo(y)
  
  lookup <- labels_lsoa_data
  
  x_st <- get_expr(x)
  y_st <- get_expr(y)
  
  x_label <- lookup[lookup$var_name == x_st,][[2]]
  y_label <- lookup[lookup$var_name == y_st,][[2]]
  
  data <- data %>% filter(LA_name %in% la)
  
  data %>% ggplot() +
    geom_point(aes(x = !!x, y = !!y, col = !!y), size = 3) +
    geom_smooth(aes(x = !!x, y = !!y), method = "lm", alpha = 0.3,
                col = "black") +
    ylab(str_wrap(y_label, 60)) +
    xlab(str_wrap(x_label, 60)) +
    theme_minimal() +
    scale_color_viridis_c() +
    theme(legend.position = "none",
          text = element_text(size = 16))
  
  
}


corr_test <- function(data, x, y, la) {
  
  require(dplyr)
  require(lazyeval)
  require(rlang)
  require(plotly)
  
  x <- enquo(x)
  y <- enquo(y)
  
  x_st <- get_expr(x)
  y_st <- get_expr(y)
  
  lookup <- labels_lsoa_data
  
  x_label <- str_remove_all(lookup[lookup$var_name == x_st,][[2]], "\\(.*?\\)")
  y_label <- str_remove_all(lookup[lookup$var_name == y_st,][[2]], "\\(.*?\\)")
  
  x_label <- str_remove_all(x_label, "\\- ")
  y_label <- str_remove_all(y_label, "\\- ")
  
  data <- data %>% filter(LA_name %in% la)
  
  correlation <- corr.test(data[[x_st]], data[[y_st]])
  
  paste("The correlation between ", x_label, "and", y_label, "is", round(correlation$r, 3), ".<br><br>",
        ifelse(correlation$r > 0, paste("This is a positive correlation, meaning that as", x_label, "increases", y_label, "also tends to increase."),
               paste("This is a negative correlation, meaning that as", x_label, "increases", y_label, "tends to decrease.")), "<br><br>",
        ifelse(correlation$p < 0.05, "The p-value for this correlation is less than 0.05, meaning that the relationship is statistically significant and we have evidence to reject the idea of no correlation in the wider population.",
               "The p-value for this correlation is greater than 0.05, meaning that the relationship is not statistically significant and we do not have strong enough evidence to reject the idea of no correlation in the wider population."),
        "<br><br>The exact p-value is: ", format(correlation$p, scientific = F)
               )
  

}

corr_test(lsoa_data_spatial, LvlOfNtrgD, 	LEAB2009_2, "Sheffield")



# ANOVA plots -------------------------------------------------------------

plot_anova <- function(data, x, la) {
  
  require(dplyr)
  require(lazyeval)
  require(rlang)
  require(plotly)
  
  x <- enquo(x)
  
  lookup <- labels_lsoa_data
  
  x_st <- get_expr(x)
  
  x_label <- lookup[lookup$var_name == x_st,][[2]]
  
  data <- data %>% filter(LA_name %in% la)
  
  data %>% ggplot() +
    geom_density(aes(x = !!x, fill = LA_name, col = LA_name),
                 alpha = 0.2, size = 1.5) +
    xlab(str_wrap(x_label, 60)) +
    theme_minimal() +
    theme(text = element_text(size = 16)) 
  
  
}

plot_anova(lsoa_data_spatial, LvlOfNtrgD, c("Sheffield", "York"))

# Function for table of means

means_table <- function(data, x, la) {
  
  require(dplyr)
  require(lazyeval)
  require(rlang)
  
  x <- enquo(x)
  
  lookup <- labels_lsoa_data
  
  x_st <- get_expr(x)
  
  x_label <- lookup[lookup$var_name == x_st,][[2]]
  
 data <- data %>% filter(LA_name %in% la) %>%
    select(LA_name, !!x) %>%
    group_by(LA_name) %>%
    summarise(
      !!x_st := round(mean(!!x, na.rm = TRUE), 3)
    )

  st_set_geometry(data, NULL)
  
}


means_table(lsoa_data_spatial, LvlOfNtrgD, c("Sheffield", "York"))


# Function for ANOVA test with TukeyHSD

anova_tidy <- function(data, x, la) {
  
  require(dplyr)
  require(lazyeval)
  require(rlang)
  
  x <- enquo(x)
  
  lookup <- labels_lsoa_data
  
  x_st <- get_expr(x)
  
  x_label <- lookup[lookup$var_name == x_st,][[2]]
  
  data <- data %>% filter(LA_name %in% la)
  
  aov_model <- aov(data = data,
      formula = eval(parse(text = paste(x_st))) ~ LA_name)
  
  tidy(TukeyHSD(aov_model)) %>%
    select(Comparison = comparison,
           Mean_Diff = estimate,
           P_Value = adj.p.value) %>%
    mutate(
      Mean_Diff = round(Mean_Diff, 3),
      P_Value = ifelse(round(P_Value, 3) < 0.001, "<0.001", round(P_Value, 3))
    )
  
  
}

anova_tidy(lsoa_data_spatial, LvlOfNtrgD, c("Sheffield", "York"))





# Function for regression and marginal effects  ---------------------------

# Function for regression model

regress <- function(data, x, y, la) {
  
  require(dplyr)
  require(lazyeval)
  require(rlang)
  require(plotly)
  
  y <- enquo(y)
  
  y_st <- get_expr(y)
  
  lookup <- labels_lsoa_data
  
  y_label <- str_remove_all(lookup[lookup$var_name == y_st,][[2]], "\\(.*?\\)")
  
  y_label <- str_remove_all(y_label, "\\- ")
  
  data <- data %>% filter(LA_name %in% la)
  
  if (length(la) > 1) {
    
    data <- data %>%
      mutate(LA_name = relevel(factor(LA_name), ref = la[1]))
    
    model <- lm(data = data,
                formula = paste(y_st, "~", ifelse(!x == "", paste(x, collapse = "+"), paste(1, collapse = "+")), "+ I(LA_name)"))
    
    return(model)
    
  } else {
  
  model <- lm(data = data,
              formula = paste(y_st, "~", paste(x, collapse = "+")))
  
  return(model)
  }
  
}

regress(lsoa_data_spatial, c(""), 
        LEAB2009_2, c("Sheffield", "York"))

# Standardised estimates

regress_std <- function(data, x, y, la) {
  
  require(dplyr)
  require(lazyeval)
  require(rlang)
  require(plotly)
  require(broom)
  
  y <- enquo(y)
  
  y_st <- get_expr(y)
  
  lookup <- labels_lsoa_data
  
  y_label <- str_remove_all(lookup[lookup$var_name == y_st,][[2]], "\\(.*?\\)")
  
  y_label <- str_remove_all(y_label, "\\- ")
  
  data <- data %>% filter(LA_name %in% la)
  
  if (length(la) > 1) {
    
    data <- data %>%
      mutate(LA_name = relevel(factor(LA_name), ref = la[1]))
    
    model <- lm(data = data,
                formula = paste("scale(", y_st, ")", "~", ifelse(!x == "", paste("scale(", paste(x), ")", collapse = " + "), paste(1, collapse = " + ")), paste0("+ I(LA_name)")))
    
    return(model)
    
  } else {
  
  model <- lm(data = data,
              formula = paste("scale(", y_st, ")", "~", paste0("scale(", x, ")", collapse = " + ")))
  
  return(model)
  
  }
  
}

regress_std(lsoa_data_spatial, c("LvlOfNtrgD", "IncmDpACIS"), 
        LEAB2009_2, c("Sheffield", "York", "Barnsley"))

str_remove(string = "I(relevel(factor(LA_name), ref = 'Sheffield'))York", pattern = "\\(.*?\\)")
gsub(".*\\)\\)", x = "I(relevel(factor(LA_name), ref = 'Sheffield'))York", replacement = "")

# Function for creating and tidying regression models

regress_tidy <- function(data, x, y, la) {
  require(dplyr)
  require(lazyeval)
  require(rlang)
  require(plotly)
  
  y <- enquo(y)
  y_st <- get_expr(y)
  lookup <- labels_lsoa_data
  y_label <- str_remove_all(lookup[lookup$var_name == y_st,][[2]], "\\(.*?\\)")
  y_label <- str_remove_all(y_label, "\\- ")
  data <- data %>% filter(LA_name %in% la)
  
  model_unst <- regress(data = data, x = x, y = !!y, la = la)
  model_std <- regress_std(data, x, !!y, la)
  
  tidymodel <- tidy(model_unst) %>%
                 mutate_at(vars(estimate:p.value), ~round(., 3)) %>%
                 select(Term = term, Coefficient = estimate, Std.Error = std.error, t = statistic, P.Value = p.value) %>%
    mutate(Term = ifelse(str_detect(Term, "I\\(LA_name"), paste(Term, "=", row_number(Term)), Term))

  tidy_std <- tidy(model_std) %>%
    mutate_at(vars(estimate:p.value), ~round(., 3)) %>%
    select(Term = term, Beta = estimate, Std.Error = std.error, t = statistic, P.Value = p.value)

  tidymodel %>% mutate(
    Beta = tidy_std$Beta
  ) %>% select(Term, Coefficient, Beta, everything()) %>%
    left_join(lookup, by = c("Term" = "var_name")) %>%
    mutate(label = str_replace_all(str_remove_all(str_remove_all(label, "\\(.*?\\)"), "\\- "), "  ", " ")
           ) %>%
    mutate(
      Term = ifelse(is.na(label), Term, label),
      P.Value = ifelse(P.Value < 0.001, "<0.001", P.Value)
    ) %>%
    mutate(
      Term = str_remove_all(Term, "I\\(LA_name\\)")
    ) %>%
    mutate(
      Term = str_trim(Term)
    ) %>%
    select(-label)

  
}

regress_tidy(lsoa_data_spatial, c("LvlOfNtrgD", "IncmDpACIS"), 
            LEAB2009_2, c("Sheffield", "York", "Barnsley"))


tidy_rsq <- function(data, x, y, la) {
  
  require(dplyr)
  require(lazyeval)
  require(rlang)
  require(plotly)
  
  y <- enquo(y)
  y_st <- get_expr(y)
  lookup <- labels_lsoa_data
  y_label <- str_remove_all(lookup[lookup$var_name == y_st,][[2]], "\\(.*?\\)")
  y_label <- str_remove_all(y_label, "\\- ")
  data <- data %>% filter(LA_name %in% la)
  
  model_unst <- regress(data = data, x = x, y = !!y, la = la)
  
  round(summary(model_unst)$r.squared, 3)
  
}


# R squared


tidy_rsq(lsoa_data_spatial, c("LvlOfNtrgD", "IncmDpACIS"), 
            LEAB2009_2, "Sheffield")


# Plot partial regression

partial_regress <- function(data, x, y, la) {
  require(dplyr)
  require(lazyeval)
  require(rlang)
  require(plotly)
  
  y <- enquo(y)
  y_st <- get_expr(y)
  lookup <- labels_lsoa_data
  y_label <- str_remove_all(lookup[lookup$var_name == y_st,][[2]], "\\(.*?\\)")
  y_label <- str_remove_all(y_label, "\\- ")
  data <- data %>% filter(LA_name %in% la)
  
  model_unst <- regress(data = data, x = x, y = !!y, la = la)
  
  ggpredict(model_unst) %>% plot(rawdata = FALSE,
                                 jitter = FALSE,
                                 grid = TRUE,
                                 show.x.title = FALSE,
                                 colors = "eight",
                                 dot.alpha = 0.2)
  
}


partial_regress(lsoa_data_spatial, 
                c("LvlOfNtrgD", "IncmDpACIS", "AccssbltGP", "AccssbltyT"), 
                LEAB2009_2, c("Sheffield", "York", "Barnsley", "Camden"))




datatable(regress_tidy(lsoa_data_spatial, c("LvlOfNtrgD", "IncmDpACIS"), 
            LEAB2009_2, "Sheffield"))


