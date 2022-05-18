# Connect back to DataRobot again if needed
datarobot::ConnectToDataRobot(
   token = "put your token here",
   endpoint = "https://app.datarobot.com/api/v2")

# Loading packages --------------------------------------------------------

library(datarobot)
library(reshape)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(readr)
library(tibble)

# Loading functions for class ---------------------------------------------

source("functions_for_class.R")

# Data preparation --------------------------------------------------------

# Loading dataset with created binary target
wine_data <- read_delim("C:/Users/misherman/Documents/Data Robot/wine_data.csv", delim = ";") 

wine_data <- wine_data %>% 
  rowid_to_column() %>% 
  mutate(wine_is_good = as.factor(if_else(
    quality >= 7,
    "good",
    "bad"
  )))

# Add noise for reference
set.seed(10)
wine_data_with_noise <- wine_data %>% 
  mutate(X1 = rnorm(n = nrow(.)),
         X2 = rnorm(n = nrow(.)),
         X3 = rnorm(n = nrow(.)),
         X4 = rnorm(n = nrow(.)),
         X5 = rnorm(n = nrow(.)))

# Start of Project --------------------------------------------------------

ConnectToDataRobot(endpoint = "https://app.datarobot.com/api/v2", token = "NWU5OGMwNzg2MTdmMjMyOTA0NjkxY2UxOkMrTUxQaTVGZEE0ZnM4ZWlUQTJEOThNYjIyNE91bjlxSTBETnplQUV6YTQ9")

project_object <- SetupProject(
  dataSource = wine_data_with_noise, 
  projectName = paste("Wine Quality - Feature Impact / Partial Dependence",
                      Sys.Date())
)

# Creating specifying custom partition as a list of settings
# "TVH" corresponds to Training-Validation-Holdout partitioning
# 30% of the data will be held for validation
# Set percentage to be held out to 0 here so the problem essentially
# turns back into cross validation for simplicity going forward.
# Could set `holdoutPct` to a different value though.
my_partition <- CreateStratifiedPartition(validationType = "TVH", 
                                          validationPct = 30, 
                                          holdoutPct = 0)

# Using TVH partitioning 
SetTarget(project = project_object, 
          target = "wine_is_good", 
          mode = "manual",
          positiveClass = "good",
          partition = my_partition)

# Set to maximum number of workers
UpdateProject(project = project_object, workerCount = -1)

# Choosing some blueprints from repository
bps <- ListBlueprints(project_object)

# Can investigate the names of the different blueprints by looking
# at the `modelType` slot in each of the 69 list values

## Look at one name
pluck(bps[[1]], "modelType") # or pluck(bps[[1]]$modelType)
## Get all names
bps_names <- map_chr(bps, pluck, "modelType")
# This `map_chr()` is same thing as the following
# No need to run
for(i in seq_len(length(bps))) {
  bps_names[i] <- pluck(bps[[i]]$modelType)
}

# Select blueprints corresponding to Random Forest, Neural Network, 
# and Logistic Regression
selected_bps <- bps[str_detect(bps_names, "Random|Neural|Logistic")]

# Train models from these blueprints
## Initialize model_job_ids vector
model_job_ids <- vector(mode = "integer", 
                        length = length(selected_bps))
## Assign the model_job_ids to correspond to the models built from blueprints
model_job_ids <- map_chr(.x = selected_bps, 
                         .f = RequestNewModel, 
                         project = project_object)

# If running this as a script and not interactively, it ensures that lines
# following aren't run until these complete
# Wait for models
invisible(
  walk(.x = model_job_ids, 
       .f = WaitForJobToComplete,
       project = project_object)
)

# Collect models
my_models <- ListModels(project_object)



# Inspect feature importance across models -------------------------------

# Function to plot feature impacts (either normalized or unnormalized).
# Main inputs are the project object and list of models to use.
# Returns the plot and selected features based on cutoff.
# Cutoff is simply which features to choose based on median of the feature
# impact scores, e.g. a cutoff of 0.05 means eliminating any feature with a
# normalized median value less than 0.05.
# Output is a graph visualizing the distribution of feature impact scores.
# Normalized turned on by default.
# `plot_feature_impacts()` is in the functions_for_class.R file
norm_plot <- plot_feature_impacts(project = project_object, 
                                  my_models, 
                                  cutoff = 0.1, 
                                  normalized = TRUE)
norm_plot

# Using unnormalized feature impacts scores (raw difference in logloss here)
unnorm_plot <- plot_feature_impacts(project_object, 
                                    my_models, 
                                    cutoff = 0.1, 
                                    normalized = FALSE)
unnorm_plot

# Make new feature list ---------------------------------------------------

# Creating new feature list
featurelist_job <- CreateFeaturelist(project_object, 
                                     listName = "Selected Features", 
                                     featureNames = norm_plot$selected_features)

# Retrain on best model
#as_tibble(my_models) %>% View()

## Top performing model is the first list entry by default
best_model <- my_models[[1]]
best_model_reduced_id <- RequestNewModel(project_object, 
                                         featurelist = featurelist_job, 
                                         blueprint = best_model)
# Wait for models
invisible(
  WaitForJobToComplete(project = project_object,
                       jobId = best_model_reduced_id)
)

best_model_reduced <- GetModelFromJobId(project_object, best_model_reduced_id)

# Inspect leaderboard
as_tibble(ListModels(project_object)) %>% View()

# Partial Dependence ------------------------------------------------------

# Downloading training data predictions from DataRobot 
# to grab validation fold rowID
validation_fold_id <- RequestTrainingPredictions(best_model_reduced, 
                                                 dataSubset = DataSubset$ValidationAndHoldout)
validation_fold <- GetTrainingPredictionsFromJobId(project_object, 
                                                   validation_fold_id)
validation <- wine_data[validation_fold$rowId, ]

# Function for generating feature effects from DataRobot manually
# Inputs are project object, model, desired dataset, and feature of interest.
# Returns the plot and plot data.
# Sampling done by default for n > 1000 for speed, but can be turned off.
# size_of_grid is the number of feature values to use. Default is 25.
# grid is automatically created (quantiles for numerics, 
# random sampling for categoricals). 
# size_of_grid can also take a vector of values for custom grids.
# Can plot ICE curves but turned off by default. 
#    Best to leave off for larger samples.
# Can also plot +/- 1 standard deviation around the average prediction.
# Can only be used for categoricals and numerics in the requested dataset 
#    (no DR derived features).
# Computing partial dependence for alcohol
alcohol_pd <- partial_dependence(project = project_object, 
                                 model = best_model_reduced, 
                                 data = validation, 
                                 feature = "alcohol")
alcohol_pd

# Computing partial dependence, ICE, and standard deviation for alcohol
alcohol_ice <- partial_dependence(project = project_object, 
                                  model = best_model_reduced, 
                                  data = validation, 
                                  feature = "alcohol",
                                  ice_plot = TRUE, 
                                  std_dev_plot = TRUE, 
                                  sample_size = nrow(validation))
alcohol_ice 
