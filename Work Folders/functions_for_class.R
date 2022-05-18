

# Creates training and testing dataset (both random and stratified random)
# Inputs are the target feature
# Return row index of the sample
# Can specify sample size
create_train_test <- function(target, sample_size = 0.5){
  # dplyr required
  require(dplyr)
  
  # Adding rowID
  out <- cbind.data.frame(rowID = 1:length(target), y = target) 

  # If categorical, do stratified. If not, do random.
  if(is.character(target) | is.factor(target)){
    out <- out %>%
      group_by_at(vars(y)) %>%
      mutate(num_rows=n()) %>%
      sample_frac(size = sample_size, weight = num_rows, replace = FALSE) %>%
      ungroup
  }else{
    out <- out %>% sample_frac(size = sample_size, replace = FALSE)
  }
 
  return(out$rowID)
}

# Function to plot feature impacts (either normalized or unnormalized)
# Inputs are the project object and list of models to use
# Returns the plot and selected features based on cutoff
# Cutoff is simply which features to choose based on median of the feature impact scores
# e.g. a cutoff of 0.05 means eliminate any feature with a normalized median value less than 0.05
# Output is a graph visualizing the distribution of feature impact scores
# Normalized turned on by default
plot_feature_impacts <- function(project, models, cutoff = 0.05, 
                               normalized = TRUE){
  # Loading required package
  pkgs <- c("ggplot2", "reshape", "dplyr")
  sapply(pkgs, require, character.only = TRUE)
  
  if(cutoff > 1){
    stop("Cutoff should be smaller than 1.")
  }
  
  # Finding which models need to feature impact to be computed
  ids <- which(sapply(models, 
                      function(x) 
                        !any(tryCatch(
                          GetFeatureImpactForModel(x),
                          error = function(e){"ERROR"}) == "ERROR") == FALSE))
  # Requesting needed feature impacts
  if(length(ids) != 0){
    cat("Computing Feature Impacts ...\n")
    fi_job_ids <- rep(NA, length(ids))
    for (i in ids){
      fi_job_ids[i] <- RequestFeatureImpact(models[[i]])
    }
    fi_job_ids <- as.vector(na.omit(fi_job_ids))
    for(i in 1:length(fi_job_ids)){
      WaitForJobToComplete(project, fi_job_ids[i], maxWait = 600000)
    }
  }
  # Collecting impacts
  cat("Collecting Feature Impacts ...\n")
  impacts <- lapply(models, function(x) GetFeatureImpactForModel(x))
  impacts <- melt(impacts, id = "featureName")
  impacts <- subset(impacts, variable == "impactUnnormalized")
  impacts$norm_value <- impacts$value / max(impacts$value)
  
  # Calculating average feature impact score
  feature_impact_medians <- impacts %>% 
    select(featureName, value, norm_value) %>%
    group_by(featureName) %>% 
    summarize(norm_median_value = median(norm_value),
              unnorm_median_value = median(value))
  
  # Selecting features that meet the cutoff
  selected_features <- feature_impact_medians %>%
    filter(norm_median_value > cutoff) %>%
    select(featureName) %>%
    pull(featureName)
  
  # Creating Plot
  if(normalized){
    out_plot <- ggplot(impacts, 
                       aes(x = reorder(featureName, norm_value, median), 
                           y = norm_value)) + 
      geom_boxplot() +
      geom_hline(aes(yintercept = cutoff, colour = "red")) + 
      theme(legend.position="none") +
      ylab("Normalized Importance Score") + 
      xlab("") + 
      coord_flip()
  } else {
    # Determine which unnormalized values meets cutoff
    norm_ecdf <- ecdf(feature_impact_medians$norm_median_value)
    norm_quant <- norm_ecdf(cutoff)
    unnorm_cutoff <- quantile(feature_impact_medians$unnorm_median_value, 
                              norm_quant)
    out_plot <- ggplot(impacts, 
                       aes(x = reorder(featureName, value, median), 
                           y = value)) + 
      geom_boxplot() +
      geom_hline(aes(yintercept = unnorm_cutoff, colour = "red")) + 
      theme(legend.position="none") +
      ylab("Unnormalized Importance Score") + 
      xlab("") + 
      coord_flip()
  }
  return(list(plot = out_plot, selected_features = selected_features))
}

# Function for generating feature effects from DataRobot manually
# Inputs are project object, model, desired dataset, and feature of interest.
# Returns the plot and plot data
# Sampling done by default for n > 1000 for speed purposes but can be turned off.
# size_of_grid is the number of feature values to use. Default is 25.
# grid is automatically created (quantiles for numerics, random sampling for categoricals). 
# size_of_grid can also take a vector of values for custom grids.
# Can plot ICE curves but turned off by default. Best to leave off for larger samples.
# Can also plot +/- 1 standard deviation around the average prediction.
# Can only be used for categoricals and numerics in the requested dataset (no DR derived features).
partial_dependence <- function(project, model, data, feature, size_of_grid = 25, 
                              sample_size = 1000, ice_plot = FALSE, 
                              std_dev_plot = FALSE){
  # Loading required ackages
  pkgs <- c("ggplot2", "reshape2", "dplyr")
  sapply(pkgs, require, character.only = TRUE)
  
  # For reproducibility
  set.seed(10)
  
  # Get needed info
  project_info <- GetProject(project)
  data <- as.data.frame(data)
  
  # No Multiclass
  if(project_info$targetType == "Multiclass"){
    stop("Feature Effects is not support for Multiclass yet.")
  }
  
  # If feature is not
  if(!(feature %in% colnames(data))){
    stop("Specified feature is not found in dataset.")
  }
  
  # Can sample to a smaller size for speed
  if(!is.null(sample_size)){
    # Random sample if regression or if target is not included in dataset
    if(project_info$targetType == "Regression" | 
       !project_info$target %in% colnames(data)){
      # Random sample
      data <- data %>% sample_n(size = min(nrow(data), sample_size),
                                replace = FALSE)
    } else {
      # Stratified random sample
      data <- data %>%
        group_by_at(vars(project_info$target)) %>%
        mutate(num_rows = n()) %>%
        sample_frac(size = min(1, sample_size/nrow(data)), weight = num_rows, 
                    replace = FALSE) %>%
        ungroup() %>%
        as.data.frame()
    }
  }
  
  # Defining feature type
  if(is.character(data[, feature]) | is.factor(data[, feature])){
    feature_type <- "categorical"
  }else{
    feature_type <- "numeric"
  }
  
  # If scalar, create grid. If more than one value, assume it's a supplied grid
  if(length(size_of_grid) == 1){
    cats <- unique(data[, feature])
    if(length(cats) > size_of_grid){
      if(feature_type == "numeric"){
        sampled_values <- quantile(data[, feature], 
                                   probs = seq(0.05, 0.95, 
                                               length.out = size_of_grid), 
                                   na.rm = TRUE)
      }else{
        sampled_values <- sample(cats, size = size_of_grid, replace = FALSE)
      }
    }else{
      sampled_values <- cats
    }    
  }else{
    sampled_values <- size_of_grid
  }
  
  # Creating augmented dataset (function of sample_size and size_of_grid)
  data$rowID <- 1:nrow(data)
  augmented_dataset <- bind_rows(lapply(sampled_values, function(x){
    data[, feature] <- x
    return(data)
  }))
  
  # Uploading augmented dataset
  cat("Uploading augmented dataset of size", 
      format(object.size(augmented_dataset), units = "Mb"), 
      "with", nrow(augmented_dataset), "rows.",  "\n")
  augmented_dataset_id <- UploadPredictionDataset(project, augmented_dataset)
  
  # Requestion predictions on augmented dataset
  cat("Requesting predictions on augmented dataset", '\n')
  pred_job_id <- RequestPredictions(project, 
                                              model$modelId, 
                                              augmented_dataset_id$id)
  
  # Adding back predictions
  if(project_info$targetType == "Regression"){
    augmented_dataset$predictions <- GetPredictions(project, pred_job_id,
                                                    type = "response", 
                                                    maxWait = 600000)
  }else{
    augmented_dataset$predictions <- GetPredictions(project, pred_job_id,
                                                    type = "probability", 
                                                    maxWait = 600000)
  }
  
  # Preparing data for plotting
  cat("Preparing plots", '\n')
  
  # Collecting needed info
  ice_plot_data <- augmented_dataset[, c(feature, "rowID", "predictions")]
  
  # Calculating partial dependence
  pd_plot_data <- ice_plot_data %>%
    group_by_at(vars(feature)) %>%
    summarise(mean_pred = mean(predictions),
              sd = sd(predictions),
              mean_minus_sd = mean_pred - sd,
              mean_plus_sd = mean_pred + sd)
  
  # Plotting partial dependence
  if(feature_type == "numeric"){
    pd_plot <- ggplot() +
      geom_line(data = pd_plot_data, aes(x = !!ensym(feature), 
                                         y = mean_pred, group = 1), 
                size = 2.5, color = "black") +
      geom_line(data = pd_plot_data, 
                aes(x = !!ensym(feature), 
                    y = mean_pred, 
                    group = 1), 
                size = 2, 
                color = "gold") +
      xlab(noquote(feature)) + 
      ylab(paste0("Target (", project_info$target, ")")) + 
      theme_bw() +
      ggtitle("Partial dependence")
  }else{
    pd_plot <- ggplot() +
      geom_point(data = pd_plot_data, 
                 aes(x = !!ensym(feature), y = mean_pred, group = 1), 
                 size = 4.5, color = "black") +
      geom_point(data = pd_plot_data, 
                 aes(x = !!ensym(feature), 
                     y = mean_pred, group = 1), 
                 size = 4, color = "gold") +
      xlab(noquote(feature)) + 
      ylab(paste0("Target (", project_info$target, ")")) + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle("Partial dependence")
  }
  
  # Making plots
  if(ice_plot){
    if(feature_type == "numeric"){
      pd_plot$layers <- c(geom_line(data = ice_plot_data, 
                                    aes(x = !!ensym(feature), 
                                        y = predictions, 
                                        group = rowID), 
                                    size = 1, color = "darkgray", alpha = 0.25), 
                          pd_plot$layers)
    }else{
      pd_plot$layers <- c(geom_point(data = ice_plot_data, 
                                     aes(x = !!ensym(feature), y = predictions, 
                                         group = rowID), 
                                     size = 1, 
                                     color = "darkgray", 
                                     alpha = 0.25), 
                          pd_plot$layers)
    }
  }
  
  if(std_dev_plot){
    if(feature_type == "numeric"){
      pd_plot <- pd_plot +
        geom_line(data = pd_plot_data, 
                  aes(x = !!ensym(feature), y = mean_minus_sd, group = 1), 
                  size= 1, 
                  color = "darkorchid") +
        geom_line(data = pd_plot_data, 
                  aes(x = !!ensym(feature), y = mean_plus_sd, group = 1), 
                  size = 1, 
                  color = "darkorchid")
    }else{
      pd_plot <- pd_plot + 
        geom_point(data = pd_plot_data, 
                   aes(x = !!ensym(feature), y = mean_minus_sd, group = 1), 
                   size = 2, color = "darkorchid") +
        geom_point(data = pd_plot_data,
                   aes(x = !!ensym(feature), 
                       y = mean_plus_sd, 
                       group = 1), 
                   size = 2, color = "darkorchid")
    }
  }
  
  return(list(pd_plot = pd_plot, pd_data = pd_plot_data))
}

# Adapted from the split conformal interval function in the conformalInference package
# Inputs are project object, model, project data, and new dataset to create intervals on
# Returns rowID, model predictions, and their upper / lower bound.
# Alpha is the miscoverage level (aka rejection region)
# So if you want 90% prediction intervals, set alpha = 0.1
# Locally_weighted boolean - if TRUE function will build 2nd model to construct intervals (see Conformal Inference paper)
prediction_intervals = function(project, model, data, 
                               newdata, alpha = 0.1, locally_weighted = FALSE) {
  #Loading required package
  require(stringr)
  
  # Set up data
  data <- as.data.frame(data)
  new_data <- as.data.frame(newdata)
  target_index <- which(colnames(data) == model$projectTarget)
  y <- data[, target_index]
  project_info <- GetProject(project)
  # If holdout, unlock
  UpdateProject(project, holdoutUnlocked = TRUE)
  
  # Check if training predictions have already been extracted
  train_ids <- ListTrainingPredictions(project)
  if(length(train_ids) != 0){
    model_ids <- sapply(train_ids, '[[', "modelId")
    
    train_data_ids <- sapply(train_ids, '[[', "id")
    # If already done, extract. If not, go get them
    if(model$modelId %in% model_ids){
      id <- train_data_ids[which(model_ids == model$modelId)]
      fit <- GetTrainingPredictions(project, id)$prediction
    } else{
      fit <- RequestTrainingPredictions(model, DataSubset$All)
      fit <- GetTrainingPredictionsFromJobId(project, fit)$prediction
    }
  }
  else{
    fit <- RequestTrainingPredictions(model, DataSubset$All)
    fit <- GetTrainingPredictionsFromJobId(project, fit)$prediction
  }
  
  # Get residuals
  res <- abs(y - fit)
  
  # If local weighting, need to train model to learn MAD residuals
  if(locally_weighted) {
    # For splitting Eureqa GAM
    require(stringr)
    # Create a new target
    data[, target_index] <- res
    mad_project <- SetupProject(dataSource = data, projectName = "MAD Model")
    # If user partition specified, use the same one
    if(!is.null(project_info$partition$userPartitionCol)){
      mad_partition <- CreateUserPartition(
        validationType = project_info$partition$validationType, 
        userPartitionCol = project_info$partition$userPartitionCol, 
        trainingLevel = project_info$partition$trainingLevel, 
        validationLevel = project_info$partition$validationLevel)
      SetTarget(project = mad_project, target = colnames(data)[target_index], 
                mode = "manual", metric = "MAE",
                partition = mad_partition)
    } else{
      SetTarget(project = mad_project, target = colnames(data)[target_index],
                mode = "manual", metric = "MAE")
    }
    # Find a Eureqa model
    bps <- ListBlueprints(mad_project)
    eureqa <- bps[grep("Eureqa Generalized Additive Model", 
                       sapply(bps, '[[', "modelType"))]
    # Choosing the Eureqa GAM with less generations
    chosen <- which.min(as.numeric(str_extract(sapply(
      eureqa, '[[', "modelType"), "\\-*\\d+\\.*\\d*"))
      )
    # In case a model was built not on the informative features
    new_list <- CreateFeaturelist(mad_project, "new_list", 
                                  ListModelFeatures(model))
    # Train on all data
    mad_model_id <- RequestNewModel(mad_project, 
                                    blueprint = eureqa[[chosen]], 
                                    featurelist = new_list, samplePct = 100)
    mad_model <- GetModelFromJobId(mad_project, modelJobId = mad_model_id, 
                                   maxWait = 600000)
    mad_out <- RequestTrainingPredictions(mad_model, DataSubset$All)
    mad_out <- GetTrainingPredictionsFromJobId(mad_project, mad_out)$prediction
    mad_test <- UploadPredictionDataset(mad_project, newdata)
    mad_pred <- RequestPredictions(mad_project, mad_model$modelId, 
                                             mad_test$id)
    mad_pred <- GetPredictions(mad_project, mad_pred)
    # Don't need project anymore
    DeleteProject(mad_project)
    # Scale predictions by predicted MAD deviations on training
    res <- res / mad_out
  } else{
    mad_out <- rep(1, length(res))
    mad_pred <- rep(1, nrow(newdata))
  }
  
  # Taking the (1 - alpha) percentile for desired coverage
  d <- as.vector(quantile(res, (1 - alpha)))
  
  # Training coverage
  coverage <- sum(ifelse(y >= (fit - d * mad_out) & y <= (fit + d * mad_out), 
                         1, 0)) / nrow(data)
  
  # Make predictions on new data
  test <- UploadPredictionDataset(project, newdata)
  pred <- RequestPredictions(project, model$modelId, test$id)
  pred <- GetPredictions(project, pred)
  
  # Create bounds
  lower <- pred - d * mad_pred
  upper <- pred + d * mad_pred
  
  # Displaying coverage on training data
  cat("Coverage on training data =", 
      coverage, 
      "with average interval length of", 2 * mean(d * mad_out), '\n')
  
  # Row ID
  rowID <- seq(0, nrow(newdata) - 1, by = 1)
  
  # Format result
  out <- cbind.data.frame(rowID, pred, lower, upper)
  colnames(out) <- c("rowID", "predictions", "lower_bound", "upper_bound")
  
  return(out)
}

# Function to optimize binary classification thresholds
# Input is the project object, the model, and the target
# Can specify whether to maximize or minimize the objection function
# Need to specify costs for each outcome to a meaningful analysis
find_threshold <- function(project, model, target, maximize = FALSE, 
                          TNC = -1, TPC = -1, FNC = 1, FPC = 1){
  # Loading required package
  require(MLmetrics)
  
  # Set up data
  y <- as.factor(target)
  event_class <- GetProject(project)$positiveClass
  
  # Check if training predictions have already been extracted
  train_ids <- ListTrainingPredictions(project)
  if(length(train_ids) != 0){
    model_ids <- sapply(train_ids, '[[', 
                        which(names(train_ids[[1]]) == "modelId"))
    train_data_ids <- sapply(train_ids, '[[', 
                             which(names(train_ids[[1]]) == "id"))
    #If already done, extract. If not, go get them
    if(model$modelId %in% model_ids){
      id <- train_data_ids[which(model_ids == model$modelId)]
      fit <- GetTrainingPredictions(project, id)
    }else{
      fit <- RequestTrainingPredictions(model, DataSubset$All)
      fit <- GetTrainingPredictionsFromJobId(project, fit)
    }
  }else{
    fit <- RequestTrainingPredictions(model, DataSubset$All)
    fit <- GetTrainingPredictionsFromJobId(project, fit)
  }
  
  # Setting up objective function
  cost_fun <- function(actuals, probs, threshold){
    levs <- levels(actuals)
    predicted_labels <- as.factor(ifelse(probs > threshold, event_class, 
                                         levs[which(levs != event_class)]))
    
    ConfusionMatrix(y_pred = predicted_labels, y_true = actuals)
    cm <- ConfusionMatrix(y_pred = predicted_labels, y_true = actuals)
    TNs <- cm[1,1]
    TPs <- cm[2,2]
    FPs <- cm[1,2]
    FNs <- cm[2,1]
    cost <- TNC*TNs + TPC*TPs + FPC*FPs + FNC*FNs
    return(cost)
  }
  
  # Doing the optimization
  obj_fun <- function(x) cost_fun(actuals = y, 
                                  probs = fit[ , paste0("class_", event_class)], 
                                  threshold = x)
  out <- optimize(obj_fun, interval = c(0, 1), maximum = maximize)
  
  # Attach names
  out <- list(model_name = model$modelType, 
              best_threshold = out[[1]], 
              optimized_value = out$objective)
  return(out)
}

# FOR APPENDIX
# Feature impact with heurstic p-values
# Calculate feature impact like normal
# Shuffle the target column
# Create a new project and do feature impact with the same model
# Do this for S amount of times
# Compute the number of times your S importance scores are greater (unnormalized) than the true ones
# In this way, if S=10 and there's only 1 times out of 10 that yields a higher score, then p-value is 1/10=.1
# Based on Altmann et al. (2010), suggests setting S to 50 or 100
# Input is project object, dataset, model, and a number for S
# Works for blenders as well
# Can pass additional arguments from SetTarget for null projects
importance_test <- function(project, data, model, S, workers = -1,
                           seed = 0, partition = NULL, weights = NULL, 
                           responseCap = NULL,
                           smartDownsampled = NULL, 
                           majorityDownsamplingRate = NULL, 
                           scaleoutModelingMode = NULL, 
                           offset = NULL, exposure = NULL,
                           eventsCount = NULL, maxWait = 600000){
  # Collecting information
  p.info <- GetProject(project)
  target.index <- which(colnames(data) == model$projectTarget)
  true.model.id <- model$modelId
  true.model <- GetModel(project, true.model.id)
  model.features <- ListModelFeatures(true.model)
  bps <- ListBlueprints(project)
  # tryCatching to see if feature impact has been completed
  if(as.vector(unique(tryCatch(GetFeatureImpactForModel(true.model), 
                               error=function(e){"ERROR"})[1]=="ERROR"))){
    fi.id <- RequestFeatureImpact(true.model)
    fi <- GetFeatureImpactForJobId(project, fi.id)
  } else{
    fi <- GetFeatureImpactForModel(true.model)
  }
  set.seed(seed)
  # To store results
  result <- matrix(rep(0), ncol = S, nrow = nrow(fi))
  for(i in 1:S){
    cat('Computing Null Case', i, '\n')
    # Creating a new project for the null cases (where target is shuffled)
    data[, target.index] <- sample(data[, target.index], replace = FALSE)
    null.project <- SetupProject(dataSource = data, 
                                 projectName = "Feature Impact Null",
                                 maxWait = maxWait)
    SetTarget(project = null.project, target = colnames(data)[target.index], 
              mode = "manual",
              metric = p.info$metric, targetType = p.info$targetType,
              positiveClass = p.info$positiveClass,
              partition = partition, weights = weights, seed = seed, 
              responseCap = responseCap,
              smartDownsampled = smartDownsampled, 
              majorityDownsamplingRate = majorityDownsamplingRate, 
              scaleoutModelingMode = scaleoutModelingMode,
              offset = offset, exposure = exposure, eventsCount = eventsCount, 
              maxWait = maxWait)
    UpdateProject(null.project, workerCount = workers)
    if(model$modelCategory == "blend"){
      models.to.blend <- model$modelIds
      m <- as.character(model$blenderMethod)
      models.to.blend.bps <- sapply(models.to.blend, 
                                    function(x) 
                                      GetModel(project, x)$blueprintId)
      f.lists <- list()
      for(j in 1:length(models.to.blend)){
        f.lists[[j]] <- CreateFeaturelist(null.project, paste0("model_", j), 
                                          ListModelFeatures(
                                            GetModel(project,
                                                     models.to.blend[j])))
      }
      # Requesting null models
      model.job.ids <- rep(0, length(models.to.blend))
      for (i in 1:length(models.to.blend)){
        model.job.ids[i] <- RequestNewModel(
          null.project, 
          blueprint = list(
            projectId = null.project$projectId,
            blueprintId = as.vector(models.to.blend.bps[i])),
          featurelist = f.lists[[i]])
      }
      for (i in 1:length(model.job.ids)){
        WaitForJobToComplete(null.project, model.job.ids[i])
      }
      mods <- ListModels(null.project)
      models.to.blend <- sapply(mods, '[[', "modelId")
      null.model.id <- RequestBlender(null.project, 
                                      modelIds = models.to.blend, 
                                      blendMethod = m)
      null.model <- GetModelFromJobId(null.project, null.model.id)
    } else {
      # In case a model was built not on the informative features
      new.list <- CreateFeaturelist(null.project, "new.list", model.features)
      null.model.id <- RequestNewModel(null.project, 
                                       blueprint = true.model, 
                                       featurelist = new.list)
      null.model <- GetModelFromJobId(null.project, modelJobId = null.model.id)
    }
    null.feature.impact.id <- RequestFeatureImpact(null.model)
    null.feature.impact <- GetFeatureImpactForJobId(null.project, 
                                                    null.feature.impact.id)
    # Delete null project (prevents clutter of projects in your account)
    DeleteProject(null.project)
    # Merging true importance and null importance
    both <- merge(fi, null.feature.impact, by = "featureName", all.x=TRUE)
    both[is.na(both)] <- 0
    both <- both[order(both$featureName),]
    # Recording if the null importance is greater than the true one
    result[,i] <- matrix(ifelse(
      both$impactUnnormalized.y >= both$impactUnnormalized.x, 
      1, 
      0), 
      ncol = 1)
    rownames(result) <- both$featureName
    # In case connection is interrupted, saving results
    null.importances <<- result
  }
  # Sorting so everything matches
  out <- fi[order(fi$featureName),]
  # Calculating the fraction of times null was greater (non-parametric strategy)
  out$p.value <- rowMeans(result)
  out <- out[order(out$impactNormalized, decreasing = TRUE),]
  return(out)
}
