library(shiny)
library(shinydashboard)
library(reticulate)
library(DT) # interactive tables
library(ggplot2) # plots
library(dplyr)
library(readr) # for writing CSV with list columns
library(dotenv)
library(shinyjs)
library(plotly)
library(shinymanager)
library(tidyr)

enableBookmarking("server")

load_dot_env(".env")

r_working_dir <- Sys.getenv("R_WORKING_DIR")
python_path <- Sys.getenv("PYTHON_PATH")

# Python path
use_python(python_path)

# Set working directory
setwd(r_working_dir)

# Source Python modules
reticulate::source_python("python/data_functions.py")
reticulate::source_python("python/plot_functions.py")

# Load and convert data properly
food_data <- load_food_data("data/processed/view_food.csv")
food_data <- as.data.frame(py_to_r(food_data))
