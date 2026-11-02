
run_simulation_compare_tt_wmw <- function(
    num_sim = 1000,
    alpha_level = .05,
    context1 = NULL,
    context2 = NULL, 
    adjust_parameter1 = NULL,
    increase_mean_by = NULL,
    DEBUG = FALSE,
    verbose = FALSE,
    show_progress = TRUE,
    n1 = 50,
    n2 = 50,
    dist1 = NULL,
    dist1args = NULL,
    dist2 = NULL,
    dist2args = NULL,
    silence_intermediate_for_multisim = TRUE,
    ...
){
  
  if (!is.null(context1)){
    dumpstats1 <- generate_data(context = context1, dump_stats = TRUE, ...)
  }
  else if (!is.null(dist1)){
    ds_gd_string_x <- paste("generate_data(distribution = ", paste("'", dist1, "'", sep = ""), ", n = ", n1, ", ", dist1args, ",  dump_stats = TRUE, ...)")
    dumpstats1 <- eval(parse(text = ds_gd_string_x))
  }
  if (!is.null(context2)){
    dumpstats2 <- generate_data(context = context2, adjust_parameter1 = adjust_parameter1, dump_stats = TRUE, ...)
  }
  else if (!is.null(dist2)){
    ds_gd_string_y <- paste("generate_data(distribution = ", paste("'", dist2, "'", sep = ""), ", n = ", n2, ", ", dist2args, ",  dump_stats = TRUE, ...)")
    dumpstats2 <- eval(parse(text = ds_gd_string_y))
  }
  else if (is.null(context2) & is.numeric(increase_mean_by)){
    context2 <- context1
    adjust_parameter1 <- adjust1_from_context(context = context2, increase_mean_by = increase_mean_by, ...)
    dumpstats2 <- generate_data(context = context2, adjust_parameter1 = adjust_parameter1, dump_stats = TRUE, ...)
    
  }
  
  # add in logic for checking distributions
  
  mean_x <- mean_from_dumpstats(dumpstats1)
  mean_y <- mean_from_dumpstats(dumpstats2)
  #browser()
  parameter1 <- mean_x - mean_y
  median_x <- median_from_dumpstats(dumpstats1)
  median_y <- median_from_dumpstats(dumpstats2)
  parameter2 <- median_x - median_y
  #browser()
  if (parameter1 == 0){
    H0 <- TRUE
  }
  # there's a niche/rare circumstance where one could choose two different contexts and then use adjust_parameter1 to alter the parameters so that context2 has the same mean as context1...
  # ... don't do that
  else if (parameter1 != 0){
    H0 <- FALSE    
  }
  else{
    print("Something went wrong.")
  }
  if (DEBUG){
    print(paste("H0: ", H0, sep = ""))
  }
  
  if (verbose){
    cat("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")
    cat("\n")
    print("SIMULATION INFO")
    cat("\n")
    print(paste("Number of simulations (n_sim): ", num_sim, sep = ""))
    cat("\n")
    print(paste("Mean of X: ", round(mean_x,3), sep = ""))
    print(paste("Mean of Y: ", round(mean_y,3), sep = ""))
    print(paste("Difference of X - Y is: ", round(parameter1), sep = ""))
    print(paste("For this simulation, the null hypothesis is ", H0, sep = ""))
    
    cat("\n")
    print(paste("Median of X: ", round(median_x,3), sep = ""))
    print(paste("Median of Y: ", round(median_y,3), sep = ""))
    print(paste("Difference of X - Y is: ", round(parameter2), sep = ""))
    cat("\n")
    cat("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")
    cat("\n")
  }
  
  # set up output objects
  pval_tt <- c()
  pval_wmw <- c()
  estimate_tt <- c()
  estimate_wmw <- c() # Hodges-Lehmann from wilcox.test
  estimate_wmw2 <- c() # Difference in sample medians
  estimate_wmw3 <- c() # Lehmann Estimator for Symmetric Dists. (HWC Comment 4.17)
  ci_lower_tt <- c()
  ci_lower_wmw <- c()
  ci_upper_tt <- c()
  ci_upper_wmw <- c()
  
  progress_points <- floor(quantile(1:num_sim, probs = seq(from = .1, to = .9, by = .1)))
  progress_points <- c(progress_points, num_sim)
  progress_points_pcts <- seq(from = 10, to = 100, by = 10)
  
  if (show_progress){
    cat("Running Simulations. Current Progress:\n")
    cat(" ")
  }
  for (i in 1:num_sim){
    if (show_progress){
      if (i %in% progress_points){
        # improve this by not printing ... if it is 100%
        cat(paste(progress_points_pcts[which(progress_points == i)], "% ... "))
      }
    }
    if (is.null(dist1)){
      dat_x <- generate_data(context = context1, n = n1, ...)
    }
    else {
      gd_string_x <- paste("generate_data(distribution = ", paste("'", dist1, "'", sep = ""), ", n = ", n1, ", ", dist1args, ", ...)")
      dat_x <- eval(parse(text = gd_string_x))
    }
    if (is.null(dist2)){
      dat_y <- generate_data(context = context2, adjust_parameter1 = adjust_parameter1, n = n2, ...)
    }
    else{
      gd_string_y <- paste("generate_data(distribution = ", paste("'", dist2, "'", sep = ""), ", n = ", n2, ", ", dist2args, ", ...)")
      dat_y <- eval(parse(text = gd_string_y))
    }
    out_tt <- t.test(dat_x, dat_y)
    out_wmw <- wilcox.test(dat_x, dat_y, conf.int = TRUE)
    pval_tt[i] <- out_tt$p.value
    estimate_tt[i] <- out_tt$estimate[1]-out_tt$estimate[2]
    ci_lower_tt[i] <- out_tt$conf.int[1]
    ci_upper_tt[i] <- out_tt$conf.int[2]
    pval_wmw[i] <- out_wmw$p.value
    estimate_wmw[i] <- out_wmw$estimate
    estimate_wmw2[i] <- median(dat_x) - median(dat_y)
    estimate_wmw3[i] <- lehmann_estimator(dat_x, dat_y)
    ci_lower_wmw[i] <- out_wmw$conf.int[1]
    ci_upper_wmw[i] <- out_wmw$conf.int[2]
  }
  if (DEBUG){
    print(paste("i: ", i, sep = ""))
  }
  if (show_progress){
    cat("\nAll simulations complete.\n")
  }
  
  method1_results <- 
    list(
      name = "Two-sample t-test",
      param_name = "MEAN1 - MEAN2",
      param_val = parameter1,
      H0 = H0,
      alpha_level = alpha_level,
      pvals = pval_tt,
      lowers = ci_lower_tt,
      uppers = ci_upper_tt, 
      estimates = estimate_tt,
      estimates_list = list(
        list(
          est_name = "Difference of Sample Means",
          est_val = estimate_tt
        )
      )
    )
  method2_results <- 
    list(
      name = "Wilcox-Mann-Whitney",
      param_name = "MEDIAN1 - MEDIAN2",
      param_val = parameter2,
      H0 = H0,
      alpha_level = alpha_level,
      pvals = pval_wmw,
      lowers = ci_lower_wmw,
      uppers = ci_upper_wmw,
      estimates = estimate_wmw,
      estimates_list = list(
        list(
          est_name = "Hodges-Lehmann Estimate",
          est_val = estimate_wmw
        ),
        list(
          est_name = "Difference of Sample Medians",
          est_val = estimate_wmw2
        ),
        list(
          est_name = "Lehmann Estimator for Symmetric Distributions",
          est_val = estimate_wmw3
        )
      )
    )
  
  if (silence_intermediate_for_multisim){
    ignored_messages <- 
      capture.output({
        suppressMessages({
          suppressWarnings({
            summarized_output <- summarize_simulation_compare(method1_results, method2_results, ...)
          })
        })
      })
    return(summarized_output)
  }
  else{
    return(summarize_simulation_compare(method1_results, method2_results, ...))
  }
}

# # Need to develop this in conjunction with a larger simulation wrapper
# multisim_summary_table_builder <- 
#   function(
    #     current_multisim_summary_table,
#     method_results
#   ){
#     
#     # fill
#     new_multisim_summary_table <- current_multisim_summary_table
#     return(new_multisim_summary_table)
#     
#   }

summarize_simulation_compare <- function(
    method1_results, 
    method2_results, 
    show_minimal_output = FALSE,
    return_lists = FALSE, 
    ...
){
  # add some extra spaces so output values align 
  # could use vector_of_strings %>% stringr::str_pad(., width = max(nchar(.)), side = "right", pad = " ") instead, but that introduces a dependency.
  method1_name_string <- paste(method1_results$name, ": ", paste(rep(" ", times = max(c(nchar(method2_results$name) - nchar(method1_results$name), 0))), collapse = ""), sep = "")
  method2_name_string <- paste(method2_results$name, ": ", paste(rep(" ", times = max(c(nchar(method1_results$name) - nchar(method2_results$name), 0))), collapse = ""), sep = "")
  method1_param_note_string <- paste("Note: ", method1_results$param_name, " for ", method1_results$name, "\n", sep = "")
  method2_param_note_string <- paste("Note: ", method2_results$param_name, " for ", method2_results$name, "\n", sep = "")
  # could be extra slick and add an extra space for positive values if there's a negative value for one of them
  # could extend this to trying to align the decimal point?
  
  method1_results$reject_rate <- check_sim_pval(pvals = method1_results$pvals, alpha_level = method1_results$alpha_level)
  method2_results$reject_rate <- check_sim_pval(pvals = method2_results$pvals, alpha_level = method2_results$alpha_level)
  method1_results$coverage_rate <- check_sim_ci_coverage(lowers = method1_results$lowers, uppers = method1_results$uppers, parameter = method1_results$param_val)
  method2_results$coverage_rate <- check_sim_ci_coverage(lowers = method2_results$lowers, uppers = method2_results$uppers, parameter = method2_results$param_val)
  method1_results$ci_width <- check_sim_ci_width(lowers = method1_results$lowers, uppers = method1_results$uppers)
  method2_results$ci_width <- check_sim_ci_width(lowers = method2_results$lowers, uppers = method2_results$uppers)
  
  # Might need to redo the logic of bias and MSE so that each estimator is evaluated against each parameter 
  # Update: the bias checking function now does this, but the output isn't yet stored
  method1_results$bias <- check_sim_est_bias(estimates = method1_results$estimates, parameter = method1_results$param_val)
  method2_results$bias <- check_sim_est_bias(estimates = method2_results$estimates, parameter = method2_results$param_val)
  
  # Note: need to update MSE in the same way as bias
  method1_results$mse <- check_sim_est_mse(estimates = method1_results$estimates, parameter = method1_results$param_val)
  method2_results$mse <- check_sim_est_mse(estimates = method2_results$estimates, parameter = method2_results$param_val)  
  
  if (method1_results$H0 == TRUE){
    
    cat("\n\nType I Error Rate\n")
    cat("-----------------\n")
    cat(paste(method1_name_string, round(method1_results$reject_rate, 3), "\n", sep = ""))
    cat(paste(method2_name_string, round(method2_results$reject_rate, 3), "\n", sep = ""))
    if (!show_minimal_output){
      cat("\n")
      cat(paste("Note: closest to ", method1_results$alpha_level, " is better (without being systematically above ", method1_results$alpha_level, ")\n"), sep = "")
      cat("\n")
    }
    
  }
  if (method1_results$H0 == FALSE){
    cat("\n\nEmpirical Power\n")
    cat("---------------\n")
    
    cat(paste(method1_name_string, round(method1_results$reject_rate, 3), "\n", sep = ""))
    cat(paste(method2_name_string, round(method2_results$reject_rate, 3), "\n", sep = ""))
    if (!show_minimal_output){
      cat("\n")
      cat("Note: higher is better")
      cat("\n")
    }
    
  } 
  {
    
    cat("\n\nConfidence Interval Coverage\n")
    cat("-----------------------------\n")
    
    cat(paste(method1_name_string, round(method1_results$coverage_rate, 3), "\n", sep = ""))
    cat(paste(method2_name_string, round(method2_results$coverage_rate, 3), "\n", sep = ""))
    if (!show_minimal_output){
      cat("\n")
      cat("Note: closest to ", 1-method1_results$alpha_level, " is better (without systematically going below ",  1-method1_results$alpha_level, ")\n")
      cat(method1_param_note_string)
      cat(method2_param_note_string)
      cat("\n")
      
    }
    
    
    
    cat("\n\nAverage Confidence Interval Width\n")
    cat("---------------------------------\n")
    
    cat(paste(method1_name_string, round(method1_results$ci_width, 2), "\n", sep = ""))
    cat(paste(method2_name_string, round(method2_results$ci_width, 2), "\n", sep = ""))
    if (!show_minimal_output){
      cat("\n")
      cat("Note: smaller is better")
      cat("\n")
      
    }
    
    # if (!show_minimal_output){
    #   cat("\n")
    #   cat("-----------------------")
    # }
    # cat("\nBias of Estimator\n")
    # if (show_minimal_output){
    #   cat("-----------------\n")
    # }
    # cat(paste(method1_name_string, round(method1_results$bias, 2), "\n", sep = ""))
    # cat(paste(method2_name_string, round(method2_results$bias, 2), "\n", sep = ""))
    # if (!show_minimal_output){
    #   cat("\n")
    #   cat("Note: smaller is better\n")
    #   cat(method1_param_note_string)
    #   cat(method2_param_note_string)
    #   cat("\n")
    # }
    
    
    
    
    cat("\n\nMean Squared Error\n")
    cat("------------------\n")
    
    cat(paste(method1_name_string, round(method1_results$mse, 2), "\n", sep = ""))
    cat(paste(method2_name_string, round(method2_results$mse, 2), "\n", sep = ""))
    if (!show_minimal_output){
      cat("\n")
      cat("Note: smaller is better\n")
      cat(method1_param_note_string)
      cat(method2_param_note_string)
      cat("\n")
    }
    
    bias_output_df <- check_sim_est_bias_for_param(method1_results, method2_results)
    
  }
  if (return_lists){
    #warning("Complete bias results are not currently returned.")
    return(list(method1_results = method1_results, method2_results = method2_results, bias_output_df = bias_output_df))
  }
}