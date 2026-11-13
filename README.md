# Food App Dashboard 🥗📊

## Overview 👀
Academic team project developed at UCLL in collaboration with ['Purva Patil', 'Jonathan Vermeulen', 'Timothy De Jaeger'].

The **Food App Dashboard** is an interactive Shiny application designed to explore, analyze, and manage food product data efficiently. It is intended for data analysts, nutritionists, or product managers who need a user-friendly interface to examine food datasets, verify new products, compare items, and maintain data integrity.

---

## Purpose 🎯

The application provides a streamlined workflow for managing a food database:

1. **Data Exploration**: Browse, filter, and sort food product data.
2. **New Product Verification**: Identify and validate new products that require review.
3. **Search & Compare**: Find specific products, compare nutrition information, and manage duplicate entries.
4. **Product Management**: Track recently added products, check nutrition completeness, and update product information.
5. **Editing**: Modify product details directly through the app, with updates saved automatically.

The app simplifies the process of maintaining an accurate, up-to-date, and comprehensive food dataset.

---

## Features ✨

### 1. Home

- **What is shown:** A welcoming interface with an overview of total items and a guide to app workflow.
- **Reason:** Helps users understand how to navigate the app and what they can achieve with it.

### 2. Data View

- **What is shown:** The full food dataset in an interactive table.
- **Reason:** Allows users to explore, filter, and sort data to gain insights or select items for further actions.

### 3. New Products

- **What is shown:** List of new products that need verification.
- **Reason:** Helps users quickly identify and review newly added products that may require attention or linking.

### 4. Nutrient Variance Brand & Category

- **What is shown:** Interactive boxplots of nutrient distributions across the top (currently all brands/categories) brands and categories. Users can select a nutrient from a dropdown to view its variability among brands or categories.

- **Reason:** Helps users quickly understand how nutrient values vary across different brands or categories, identify outliers, and spot patterns in nutrition data

### 5. Search & Compare

- **What is shown:** Search functionality, comparison tables, similarity heatmaps, cluster visualizations, and missing nutritional fields.
- **Reason:** Enables users to find specific products, compare nutritional values, link duplicates, or create new entries while ensuring data completeness.

### 6. Added Products

- **What is shown:** Recently added products, charts showing products added per day, and nutritional data completeness.
- **Reason:** Helps users track product addition trends and evaluate the completeness of newly added data.

### 7. Edit Products

- **What is shown:** Interactive table to edit product details.
- **Reason:** Allows users to update product information directly, keeping the dataset accurate and up-to-date.

---

## Future Improvements 🚀

- **Analytics on Product Trends:** Include dashboards for categories, trends, and popular products over time.
- **Decision Support Chatbot:**
  - Integrate a chatbot to assist clients by reviewing similarity metrics (name, nutrition, clusters) and suggesting possible links or actions.
  - Provides context for recommendations so the client can make informed decisions.
  - Could track metrics such as suggestions provided, actions accepted, and time saved per product.
- **Enhanced Similarity Analysis:** Incorporate advanced machine learning models to improve product similarity matching for both active and non-active products.
- **Notification System:** Alert users when a new product is added, or a product requires immediate attention.

---

# Food App Dashboard Setup Guide ⚙️

## Step 1: Install R

1. Download and install R from the official CRAN website: [cran.r-project.org](https://cran.r-project.org/).
2. Choose the latest version for Windows (e.g., R 4.3+). Install to the default location (e.g., `C:\Program Files\R\R-4.3.2`).

## Step 2: Configure R in VS Code

To enable R support in VS Code, update the extension settings with your R installation paths:

1. Open VS Code.
2. Go to **File > Preferences > Settings** (or press `Ctrl+,`).
3. Search for "r.rpath" in the settings search bar.
4. Set **R > Rpath: Windows** to the path of `R.exe` (adjust based on your R version):
   ```
   C:\Program Files\R\R-4.3.2\bin\x64\R.exe
   ```
5. Set **R > Rterm: Windows** to the path of `Rterm.exe`:
   ```
   C:\Program Files\R\R-4.3.2\bin\x64\Rterm.exe
   ```
6. Save the changes and restart VS Code.

**Tip**: If paths don't work, add your R `bin\x64` folder to the system PATH (search "Environment Variables" in Windows Start menu).

## Step 3: Set Up Data Folders

1. Navigate to your cloned repo folder (e.g., `Food_app`).
2. Create a `data` folder inside it.
3. Inside `data`, create two subfolders: `raw` and `processed`.
4. Add your data file `view_food.csv` to the `data/raw` folder.

**Download Data**: If you don't have the data file, you can download it here:
[Google Drive - view_food.csv](https://drive.google.com/file/d/1w_NwlZwmyEe1siE9drnh8_E9zuJCJtUj/view?usp=sharing)

**Note**: Ensure the CSV has the expected columns (e.g., 'category', 'calories') as per the app's logic.

## Step 4: Install R Packages

Open the R terminal in VS Code (see Step 5) and run:

```
install.packages(c("shiny", "shinydashboard", "reticulate", "DT", "ggplot2", "plotly", "dotenv", "shinymanager", "shinyjs"))
```

This installs all required packages. If prompted, select a CRAN mirror.

## Step 5: Install Python Packages

1. Ensure Python 3.8+ is installed: [https://www.python.org/](https://www.python.org/)
2. Create a `requirements.txt` in the project root with the following content:

```
pandas
numpy
scikit-learn
plotly
python-dotenv
```

3. Open a terminal in your project folder and run:

```
pip install -r requirements.txt
```

## Step 6: Run the Dashboard Locally

1. Open your project folder in VS Code.
2. Open the R terminal:

   - Press `Ctrl+Shift+P` to open the Command Palette.
   - Type and select "R: Create R Terminal".

3. In the R terminal, set the working directory to your project root (adjust the path):

   ```
   setwd("C:\\path_to_repo\\Data-Visualisation-Analytics--Group-F2\\Food_app")
   ```

4. Source the global file:

   ```
   source("global.R")
   ```

5. Run the app:

   ```
   shiny::runApp()
   ```

6. The dashboard should open. Press `Ctrl+C` in the terminal to stop it.

---

## Data Documentation 🗂️

### Source of Data

- The data for the Food App Dashboard is stored in a CSV file: `view_food.csv`.
- The CSV is located in the `data/raw` folder of the project repository.
- **Processed Version:** On first run, the raw CSV is copied to `data/processed/view_food.csv`. The app reads and writes to this processed file to avoid altering the original data.
- **Note:** No pre-existing database is used; all data is read directly from the CSV.

### Data Loading

- The app loads the CSV using standard R functions (e.g., `read.csv`) or Python functions (`pandas.read_csv`).
- Raw data is loaded at runtime and converted into a suitable DataFrame for processing.

### Data Cleaning

- No explicit cleaning is applied to the raw data before loading.
- Implicit cleaning occurs during processing:
  - Non-numeric nutritional values are coerced to `NA`.
  - Date strings are parsed to POSIXct format.
  - `id` and `active` fields are coerced to integers.

### Data Processing

#### Python Functions (`python/data_functions.py`)

- **`load_food_data()`**: Loads the CSV in low-memory mode.
- **`process_food_data()`**: Ensures the data is a valid DataFrame.
- **`cluster_foods()`**:
  - Clusters products using **Spectral Clustering** with KNN affinity.
  - Uses nutritional fields: `energy`, `protein`, `fat`, `carbohydrates`, `sugar`, `starch`, `dietary_fiber`.
  - Applies **PCA** for 3D visualization.
- **`highlight_missing()`**: Counts missing numeric fields per product.
- **`compute_similarity()`**: Combines:
  - 50% name similarity
  - 50% nutrition similarity
- **`compute_name_similarity()`**: TF-IDF based similarity using name, brand, and category.
- **`compute_variance_by_category` & `compute_variance_by_category`**:
  - Computes nutrient variability across the top brands or categories.
  - Returns data for interactive boxplots showing distributions of selected nutrients.
  - Input: full processed dataset

#### R Processing (`server.R`)

- **Numeric Coercion**: Converts nutritional fields (`energy`, `protein`, `fat`, etc.) to numeric (non-numeric entries become `NA`).
- **Active/ID Coercion**: Ensures `id` and `active` columns are integers.
- **Date Handling**: Parses `created` timestamps to POSIXct format.
- **PCA + K-Means Clustering**: Applied separately for active and non-active products for 3D visualization.
- **Distance Calculations**: Computes Euclidean distances in scaled nutritional space for "closest products" tables.
- **Linked Items Management**: Tracks linked products in the `linked_items` column as semicolon-separated strings.
- **Variance tracking of nutrients**:
  - Prepares data for interactive Plotly boxplots to visualize distributions, identify outliers, and compare nutrient profiles.

### Nutritional Fields Used

- `energy`, `protein`, `fat`, `carbohydrates`, `sugar`, `starch`, `dietary_fiber`

### Summary

- Raw CSV data is read and minimally cleaned during processing.
- Processing includes numeric coercion, clustering, similarity computation, and linked item management.
- Python functions handle clustering, similarity, and missing values, while R handles reactive data transformations, PCA visualization, and distance calculations.

---
