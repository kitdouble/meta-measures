meta_measures <- function(response = "response", accuracy = "correct", confidence = "confidence", subject = "participant_id", target_left = "target_left", data = mydata, sse = T, mle = F, hier = T){
  
  
  
  ## SETUP ----------------------------------------------------------------
  
  # packages
  require(metaSDT)
  
  # set up data frame
  if(sse | mle | hier) {
      data = data.frame(response = data[,response], accuracy = data[,accuracy], confidence = data[,confidence], subject = data[,subject], target_left = data[,target_left])
    } else {
    # Data frame if no SDT measures (e.g not 2AFC)
      data = data.frame(accuracy = data[,accuracy], confidence = data[,confidence], subject = data[,subject])
    }
  
  
  
  # Create an empty data frame to store the results
  meta_results <- data.frame(subject = character(), stringsAsFactors = FALSE)
  
  

  ## kendall Correlation ----------------------------------------------------------------
  
  # Loop through each subject
  for (subject_id in unique(data$subject)) {
    # Subset data for the current subject
    subject_data <- subset(data, data$subject == subject_id)
    
    # Calculate gamma correlation
    gamma <- cor(subject_data$confidence, subject_data$accuracy , method = "kendall")
    
    # Add the result to the results data frame
    meta_results <- rbind(meta_results, data.frame(subject = subject_id, kendall_correlation = gamma))
  }
  
  ## Discrimination ----------------------------------------------------------------
  
  # Aggregate the data
  discim_data <- aggregate(confidence ~ subject*accuracy, data, mean)
  
  # Spreads the data
  discim_data$accuracy <- ifelse(discim_data$accuracy == 1, "correct", "incorrect")
  discim_data <- spread(discim_data, accuracy, confidence)
  discim_data$discrimination <- (discim_data$correct - discim_data$incorrect)
  
  # Merge
  meta_results <- merge(meta_results, discim_data[,c("subject", "discrimination")], by = "subject")
  
  
  
  ## sensitivity (SSE) ----------------------------------------------------------------
  if (sse) {
    
    tryCatch({ 
      
      for (subject_id in unique(data$subject)) {
        
        # Subset data for the current subject
        subject_data <- subset(data, data$subject == subject_id)
        
        # Frequency table
        y <- table(factor(subject_data$confidence, levels = 1:6), subject_data$response, subject_data$target_left)
        nR_S1 <- c(rev(y[,1,1]),y[,2,1])
        nR_S2 <- c(rev(y[,1,2]),y[,2,2])
        
        # SDT meta d' using SSE
        fit <- fit_meta_d_SSE(nr_s1 = nR_S1, nr_s2 = nR_S2, add_constant = T)
        
        # Add the result to the results data frame
        meta_results[meta_results$subject == subject_id, "sensitivity_sse"] <- fit$meta_da[1]
        meta_results[meta_results$subject == subject_id, "da"] <- fit$da[1]
        
      }
      
    }, error = function(e) {
    })
  }
  
  
  
  ## sensitivity (MLE) ----------------------------------------------------------------
  if (mle) {
    
    tryCatch({ 
      
      for (subject_id in unique(data$subject)) {
        
        # Subset data for the current subject
        subject_data <- subset(data, data$subject == subject_id)
        
        # Frequency table
        y <- table(factor(subject_data$confidence, levels = 1:6), subject_data$response, subject_data$target_left)
        nR_S1 <- c(rev(y[,1,1]),y[,2,1])
        nR_S2 <- c(rev(y[,1,2]),y[,2,2])
        
        # SDT meta d' using MLE
        fit <- fit_meta_d_MLE(nr_s1 = nR_S1, nr_s2 = nR_S2, add_constant = T)
        
        # Add the result to the results data frame
        meta_results[meta_results$subject == subject_id, "sensitivity_mle"] <- fit$meta_da[1]
        meta_results[meta_results$subject == subject_id, "da"] <- fit$da[1]
        
      }
      
    }, error = function(e) {
    })
  }
  
  
  
  ## sensitivity (hierarchical) ----------------------------------------------------------------
  if (hier) {
    
    tryCatch({ 
      
      for (subject_id in unique(data$subject)) {
        
        # Subset data for the current subject
        subject_data <- subset(data, data$subject == subject_id)
        
        # Frequency table
        y <- table(factor(subject_data$confidence, levels = 1:6), subject_data$response, subject_data$target_left)
        nR_S1 <- c(rev(y[,1,1]),y[,2,1])
        nR_S2 <- c(rev(y[,1,2]),y[,2,2])
        
        # SDT meta d' using  hierarchical Bayesian framework
        bfit <- fit_metad_indiv(nR_S1 = nR_S1, nR_S2 = nR_S2)
        output = bfit[[1]]
        d1 = bfit[[2]]$d1
        
        # Save mean values 
        Value <- summary(output)
        stat <- data.frame(mean = Value[["statistics"]][, "Mean"])
        stat %<>%
          rownames_to_column(var = "name")
        
        # Add the result to the results data frame
        meta_results[meta_results$subject == subject_id, "sensitivity_hier"] <- stat$mean[stat$name == "meta_d"]
        meta_results[meta_results$subject == subject_id, "da"] <- bfit[[2]]$d1
        
      }
      
    }, error = function(e) {
    })
  }
  
  
  
  ## Mean Confidence ----------------------------------------------------------------
  
  # Aggregate the data
  conf_data <- setNames(aggregate(confidence ~ subject, data, mean),c("subject", "mean_conf"))
  
  # Merge
  meta_results <- merge(meta_results, conf_data[,c("subject", "mean_conf")], by = "subject")
  
  
  
  ## Variance Confidence ----------------------------------------------------------------
  
  # Aggregate the data
  conf_data <- setNames(aggregate(confidence ~ subject, data, var),c("subject", "var_conf"))
  
  # Merge
  meta_results <- merge(meta_results, conf_data[,c("subject", "var_conf")], by = "subject")
  
  
  
  ## Mean Accuracy ----------------------------------------------------------------
  
  # Aggregate the data
  acc_data <- setNames(aggregate(accuracy ~ subject, data, mean),c("subject", "mean_acc"))
  
  # Merge
  meta_results <- merge(meta_results, acc_data[,c("subject", "mean_acc")], by = "subject")
  
  
  # Print the results
  return(meta_results)
  
  
}
