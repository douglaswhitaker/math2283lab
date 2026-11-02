# Helper functions for simulations using generate_data()

# Currently designing this to specifically use a context1 + increase_mean_by approach
# Can be expanded later to other approaches
#' @export
multisim_wrapper <- function(
    context1,
    increase_mean_by_vec,
    n1_vec,
    n2_vec,
    num_sim = 1000,
    simplify_output_table = TRUE,
    show_progress = TRUE,
    ...
){

  if (!(length(n1_vec) == length(n2_vec))){
    stop("n1_vec and n2_vec must be the same length.")
  }




  # setup output table
  # this is being done manually for now, it ought to be expanded to something algorithmic later
  # --- HARD CODED --- update!

  multisim_output_df <-
    data.frame(

      # simulation info
      simulation_id = character(),
      num_sim = numeric(),
      context1 = character(),
      n1 = numeric(),
      n2 = numeric(),
      increase_mean_by = numeric(),

      # Two-Sample T-Test
      method1 = character(),
      param_name1 = character(),
      param_val1 = numeric(),
      alpha_level1 = numeric(), # probably not needed

      reject_rate1 = numeric(),
      coverage_rate1 = numeric(),
      ci_width1 = numeric(),

      # # These aren't needed but maybe return them anyway?
      # pvals1 = numeric(),
      # lowers1 = numeric(),
      # uppers1 = numeric(),
      # est_name_A1 = character(),
      # est_val1_A1 = numeric(),

      # bias_param_name1_est_name_A1 = character(),
      # bias_param_name1_est_name_A2 = character(),
      # bias_param_name1_est_name_B2 = character(),
      # bias_param_name1_est_name_C2 = character(),
      # bias_param_name1_est_val_A1 = numeric(),
      # bias_param_name1_est_val_A2 = numeric(),
      # bias_param_name1_est_val_B2 = numeric(),
      # bias_param_name1_est_val_C2 = numeric(),
      # mse_param_name1_est_val_A1 = numeric(),
      # mse_param_name1_est_val_A2 = numeric(),
      # mse_param_name1_est_val_B2 = numeric(),
      # mse_param_name1_est_val_C2 = numeric(),
      bias_param_name1_est_name_A1 = character(),
      bias_param_name1_est_val_A1 = numeric(),
      mse_param_name1_est_val_A1 = numeric(),

      bias_param_name1_est_name_A2 = character(),
      bias_param_name1_est_val_A2 = numeric(),
      mse_param_name1_est_val_A2 = numeric(),

      bias_param_name1_est_name_B2 = character(),
      bias_param_name1_est_val_B2 = numeric(),
      mse_param_name1_est_val_B2 = numeric(),

      bias_param_name1_est_name_C2 = character(),
      bias_param_name1_est_val_C2 = numeric(),
      mse_param_name1_est_val_C2 = numeric(),

      bias_param_name1_est_min = numeric(),
      mse_param_name1_est_min = numeric(),

      # Wilcoxon-Mann-Whitney
      method2 = character(),
      param_name2 = character(),
      param_val2 = numeric(),
      alpha_level2 = numeric(), # probably not needed

      reject_rate2 = numeric(),
      coverage_rate2 = numeric(),
      ci_width2 = numeric(), # add comma if continuing

      reject_rate_min = numeric(),
      reject_rate_max = numeric(),
      ci_width_min = numeric(),
      coverage_rate_min = numeric(),
      coverage_rate_max = numeric(),


      bias_param_name2_est_name_A1 = character(),
      bias_param_name2_est_val_A1 = numeric(),
      mse_param_name2_est_val_A1 = numeric(),

      bias_param_name2_est_name_A2 = character(),
      bias_param_name2_est_val_A2 = numeric(),
      mse_param_name2_est_val_A2 = numeric(),

      bias_param_name2_est_name_B2 = character(),
      bias_param_name2_est_val_B2 = numeric(),
      mse_param_name2_est_val_B2 = numeric(),

      bias_param_name2_est_name_C2 = character(),
      bias_param_name2_est_val_C2 = numeric(),
      mse_param_name2_est_val_C2 = numeric(),

      bias_param_name2_est_min = numeric(),
      mse_param_name2_est_min = numeric()

      # # These aren't needed but maybe return them anyway?
      # pvals2 = numeric(),
      # lowers2 = numeric(),
      # uppers2 = numeric(),
      # est_name_A2 = character(),
      # est_val1_A2 = numeric(),
      # est_name_B2 = character(),
      # est_val1_B2 = numeric(),
      # est_name_C2 = character(),
      # est_val1_C2 = numeric(),

      # # Bias and MSE

      #
      # mse_param_name1_est_name_A1 = numeric(),
      # mse_param_name1_est_name_A2 = numeric(),
      # mse_param_name1_est_name_B2 = numeric(),
      # mse_param_name1_est_name_C2 = numeric(),
      #
      # bias_param_name2_est_name_A1 = numeric(),
      # bias_param_name2_est_name_A2 = numeric(),
      # bias_param_name2_est_name_B2 = numeric(),
      # bias_param_name2_est_name_C2 = numeric(),
      #
      # mse_param_name2_est_name_A1 = numeric(),
      # mse_param_name2_est_name_A2 = numeric(),
      # mse_param_name2_est_name_B2 = numeric(),
      # mse_param_name2_est_name_C2 = numeric() # add comma if continuing
    )

  if (show_progress){
    cat("\nBeginning Simulation Loop\n\n")
  }
  for (i in 1:length(increase_mean_by_vec)){
    if(show_progress){
      cat("\n\nIncrementing increase_mean_by_vec\n")
      cat(paste("* i = ", i, "\n\n", sep = ""))
    }
    for (j in 1:length(n1_vec)){ # by design, n1_vec and n2_vec must be the same length in this version
      if(show_progress){
        cat(paste("* j = ", j, " (incrementing sample size)\n", sep = ""))
      }

      current_sim_output <-
        run_simulation_compare_tt_wmw(
          context1 = context1,
          increase_mean_by = increase_mean_by_vec[i],
          n1 = n1_vec[j],
          n2 = n2_vec[j],
          return_lists = TRUE,
          ...)
      #browser()
      multisim_output_df <-
        rbind(
          multisim_output_df,
          data.frame(
            # simulation info
            simulation_id = paste("i=",i,";j=",j,sep=""),
            num_sim = num_sim,
            context1 = context1,
            n1 = n1_vec[j],
            n2 = n2_vec[j],
            increase_mean_by = increase_mean_by_vec[i],

            # Two-Sample T-Test
            method1 = current_sim_output$method1_results$name,
            param_name1 = current_sim_output$method1_results$param_name,
            param_val1 = current_sim_output$method1_results$param_val,
            alpha_level1 = current_sim_output$method1_results$alpha_level, # probably not needed

            reject_rate1 = current_sim_output$method1_results$reject_rate,
            coverage_rate1 = current_sim_output$method1_results$coverage_rate,
            ci_width1 = current_sim_output$method1_results$ci_width,

            bias_param_name1_est_name_A1 = current_sim_output$bias_output_df[1, "est_name_A1"],
            bias_param_name1_est_val_A1 = current_sim_output$bias_output_df[1, "bias_est_A1"],
            mse_param_name1_est_val_A1 = current_sim_output$bias_output_df[1, "mse_est_A1"],

            bias_param_name1_est_name_A2 = current_sim_output$bias_output_df[1, "est_name_A2"],
            bias_param_name1_est_val_A2 = current_sim_output$bias_output_df[1, "bias_est_A2"],
            mse_param_name1_est_val_A2 = current_sim_output$bias_output_df[1, "mse_est_A2"],

            bias_param_name1_est_name_B2 = current_sim_output$bias_output_df[1, "est_name_B2"],
            bias_param_name1_est_val_B2 = current_sim_output$bias_output_df[1, "bias_est_B2"],
            mse_param_name1_est_val_B2 = current_sim_output$bias_output_df[1, "mse_est_B2"],

            bias_param_name1_est_name_C2 = current_sim_output$bias_output_df[1, "est_name_C2"],
            bias_param_name1_est_val_C2 = current_sim_output$bias_output_df[1, "bias_est_C2"],
            mse_param_name1_est_val_C2 = current_sim_output$bias_output_df[1, "mse_est_C2"],

            bias_param_name1_est_min = min(abs(c(
              current_sim_output$bias_output_df[1, "bias_est_A1"],
              current_sim_output$bias_output_df[1, "bias_est_A2"],
              current_sim_output$bias_output_df[1, "bias_est_B2"],
              current_sim_output$bias_output_df[1, "bias_est_C2"]
            ))),
            mse_param_name1_est_min = min(c(
              current_sim_output$bias_output_df[1, "mse_est_A1"],
              current_sim_output$bias_output_df[1, "mse_est_A2"],
              current_sim_output$bias_output_df[1, "mse_est_B2"],
              current_sim_output$bias_output_df[1, "mse_est_C2"]
            )),

            # Wilcoxon-Mann-Whitney
            method2 = current_sim_output$method2_results$name,
            param_name2 = current_sim_output$method2_results$param_name,
            param_val2 = current_sim_output$method2_results$param_val,
            alpha_level2 = current_sim_output$method2_results$alpha_level, # probably not needed

            reject_rate2 = current_sim_output$method2_results$reject_rate,
            coverage_rate2 = current_sim_output$method2_results$coverage_rate,
            ci_width2 = current_sim_output$method2_results$ci_width,

            reject_rate_min = min(current_sim_output$method1_results$reject_rate, current_sim_output$method2_results$reject_rate),
            reject_rate_max = max(current_sim_output$method1_results$reject_rate, current_sim_output$method2_results$reject_rate),
            ci_width_min = min(current_sim_output$method1_results$ci_width, current_sim_output$method2_results$ci_width),
            coverage_rate_min = min(current_sim_output$method1_results$coverage_rate, current_sim_output$method2_results$coverage_rate),
            coverage_rate_max = max(current_sim_output$method1_results$coverage_rate, current_sim_output$method2_results$coverage_rate),

            bias_param_name2_est_name_A1 = current_sim_output$bias_output_df[2, "est_name_A1"],
            bias_param_name2_est_val_A1 = current_sim_output$bias_output_df[2, "bias_est_A1"],
            mse_param_name2_est_val_A1 = current_sim_output$bias_output_df[2, "mse_est_A1"],

            bias_param_name2_est_name_A2 = current_sim_output$bias_output_df[2, "est_name_A2"],
            bias_param_name2_est_val_A2 = current_sim_output$bias_output_df[2, "bias_est_A2"],
            mse_param_name2_est_val_A2 = current_sim_output$bias_output_df[2, "mse_est_A2"],

            bias_param_name2_est_name_B2 = current_sim_output$bias_output_df[2, "est_name_B2"],
            bias_param_name2_est_val_B2 = current_sim_output$bias_output_df[2, "bias_est_B2"],
            mse_param_name2_est_val_B2 = current_sim_output$bias_output_df[2, "mse_est_B2"],

            bias_param_name2_est_name_C2 = current_sim_output$bias_output_df[2, "est_name_C2"],
            bias_param_name2_est_val_C2 = current_sim_output$bias_output_df[2, "bias_est_C2"],
            mse_param_name2_est_val_C2 = current_sim_output$bias_output_df[2, "mse_est_C2"],

            bias_param_name2_est_min = min(abs(c(
              current_sim_output$bias_output_df[2, "bias_est_A1"],
              current_sim_output$bias_output_df[2, "bias_est_A2"],
              current_sim_output$bias_output_df[2, "bias_est_B2"],
              current_sim_output$bias_output_df[2, "bias_est_C2"]
            ))),
            mse_param_name2_est_min = min(c(
              current_sim_output$bias_output_df[2, "mse_est_A1"],
              current_sim_output$bias_output_df[2, "mse_est_A2"],
              current_sim_output$bias_output_df[2, "mse_est_B2"],
              current_sim_output$bias_output_df[2, "mse_est_C2"]
            )) # add comma if continuing
          )
        )

    }
  }
  if (show_progress){
    cat("\nCompleted Simulation Loop\n\n")
  }
  #print(multisim_output_df) # debug
  if (!simplify_output_table){
    return(multisim_output_df)
  }
  if (simplify_output_table){
    return(multisim_output_df[,c("simulation_id",
                                 "num_sim",
                                 "context1",
                                 "method1",
                                 "method2",
                                 "n1",
                                 "n2",
                                 "increase_mean_by",
                                 "reject_rate1",
                                 "reject_rate2",
                                 "reject_rate_min",
                                 "reject_rate_max",
                                 "coverage_rate1",
                                 "coverage_rate2",
                                 "coverage_rate_min",
                                 "coverage_rate_max",
                                 "ci_width1",
                                 "ci_width2",
                                 "ci_width_min",
                                 "alpha_level1",
                                 "alpha_level2",
                                 "param_name1",
                                 "param_val1",
                                 "bias_param_name1_est_name_A1",
                                 "bias_param_name1_est_name_A2",
                                 "bias_param_name1_est_name_B2",
                                 "bias_param_name1_est_name_C2",
                                 "bias_param_name1_est_val_A1",
                                 "bias_param_name1_est_val_A2",
                                 "bias_param_name1_est_val_B2",
                                 "bias_param_name1_est_val_C2",
                                 "bias_param_name1_est_min",
                                 "mse_param_name1_est_val_A1",
                                 "mse_param_name1_est_val_A2",
                                 "mse_param_name1_est_val_B2",
                                 "mse_param_name1_est_val_C2",
                                 "mse_param_name1_est_min",
                                 "param_name2",
                                 "param_val2",
                                 "bias_param_name2_est_name_A1",
                                 "bias_param_name2_est_name_A2",
                                 "bias_param_name2_est_name_B2",
                                 "bias_param_name2_est_name_C2",
                                 "bias_param_name2_est_val_A1",
                                 "bias_param_name2_est_val_A2",
                                 "bias_param_name2_est_val_B2",
                                 "bias_param_name2_est_val_C2",
                                 "bias_param_name2_est_min",
                                 "mse_param_name2_est_val_A1",
                                 "mse_param_name2_est_val_A2",
                                 "mse_param_name2_est_val_B2",
                                 "mse_param_name2_est_val_C2",
                                 "mse_param_name2_est_min")])
  }

}


# Process dumpstats -------------------------------------------------------


mean_from_dumpstats <- function(ds){
  if (ds$distribution == "debug"){
    "Debug reached."
    return(0)
  }
  else if (ds$distribution == "lognormal"){
    return(mean_lognormal(meanlog = ds$meanlog, sdlog = ds$sdlog))
  }
  else if (ds$distribution == "normal"){
    return(ds$mean)
  }
  else if (ds$distribution == "weibull"){
    return(mean_weibull(shape = ds$shape, scale = ds$scale))
  }
  else if (ds$distribution == "binomial"){
    return(mean_binomial(size = ds$size, prob = ds$prob))
  }
  else if (ds$distribution == "exponential"){
    return(mean_exponential(rate = ds$rate))
  }
  else if (ds$distribution == "cauchy"){
    # THIS IS NOT THE MEAN but we handle this in the other function
    # Don't warn the students to not spoil the surprise, as it were
    return(median_cauchy(location = ds$location))
  }
}

median_from_dumpstats <- function(ds){
  if (ds$distribution == "debug"){
    "Debug reached."
    return(0)
  }
  else if (ds$distribution == "lognormal"){
    return(median_lognormal(meanlog = ds$meanlog))
  }
  else if (ds$distribution == "normal"){
    return(ds$mean)
  }
  else if (ds$distribution == "weibull"){
    return(median_weibull(shape = ds$shape, scale = ds$scale))
  }
  else if (ds$distribution == "binomial"){
    return(median_binomial(size = ds$size, prob = ds$prob))
  }
  else if (ds$distribution == "exponential"){
    return(median_exponential(rate = ds$rate))
  }
  else if (ds$distribution == "cauchy"){
    return(median_cauchy(location = ds$location))
  }
}


# Distribution mean, median, etc. -----------------------------------------


mean_exponential <- function(rate){
  1/rate
}
median_exponential <- function(rate){
  log(2)/rate
}

mean_binomial <- function(size, prob){
  size * prob
}
median_binomial <- function(size, prob){
  if (prob == 0.5 & !(size %% 2 == 0)){
    warning("The median is non-unique for Binomial dist. with p = 0.5 and odd n.")
  }
  qbinom(p = 0.5, size = size, prob = prob)
}
mean_weibull <- function(shape, scale){
  scale*gamma(1+1/shape)
}
median_weibull <- function(shape, scale){
  scale*(log(2)^(1/shape))
}
mean_lognormal <- function(meanlog, sdlog){
  exp(meanlog + (sdlog^2)/2)
}
median_lognormal <- function(meanlog){
  exp(meanlog)
}

median_cauchy <- function(location){
  location
}

lehmann_estimator <- function(group1, group2){
  g1 <- outer(group1, group1, FUN = "+")/2
  #g1
  g1[lower.tri(g1, diag = TRUE)]
  #median(g1[lower.tri(g1, diag = TRUE)])

  g2 <- outer(group2, group2, FUN = "+")/2
  #g2
  g2[lower.tri(g2, diag = TRUE)]
  #median(g2[lower.tri(g2, diag = TRUE)])

  median(g1[lower.tri(g1, diag = TRUE)]) - median(g2[lower.tri(g2, diag = TRUE)])

}


# Helpers for generate_data() ---------------------------------------------


adjust1_lognormal <- function(specified_mean, sdlog, meanlog = NULL){
  new_meanlog <- log(specified_mean)-(sdlog^2)/2
  if (!is.null(meanlog)){
    return(new_meanlog - meanlog)
  }
  else{
    return(new_meanlog)
  }
}

adjust1_exponential <- function(original_rate, increase_mean_by){
  original_mean <- 1/original_rate
  new_mean <- original_mean + increase_mean_by
  new_rate <- 1/new_mean
  adjust1 <- new_rate - original_rate
  return(adjust1)
}

adjust1_from_context <- function(context, increase_mean_by, ...){
  ds <- generate_data(context = context, dump_stats = TRUE, ...)
  #browser()
  if (ds$distribution == "lognormal"){
    return(adjust1_lognormal(
      specified_mean =
        mean_lognormal(meanlog = ds$meanlog, sdlog = ds$sdlog) +
        increase_mean_by,
      meanlog = ds$meanlog,
      sdlog = ds$sdlog
    ))
  }
  else if (ds$distribution == "exponential"){
    return(adjust1_exponential(original_rate = ds$rate, increase_mean_by = increase_mean_by))
  }
  else if (ds$distribution == "normal"){
    return(increase_mean_by)
  }
  else if (ds$distribution == "cauchy"){
    return(increase_mean_by) # not really the mean, we're just shifting the location for the simulation
  }
}


# Evaluating simulation results -------------------------------------------



check_sim_pval <- function(pvals, alpha_level){
  sum(pvals <= alpha_level)/length(pvals)
}

check_sim_ci_width <- function(lowers, uppers){
  mean(uppers-lowers)
}

check_sim_est_bias <- function(estimates, parameter){
  mean(estimates - parameter)
}

check_sim_est_mse <- function(estimates, parameter){
  var(estimates) + (check_sim_est_bias(estimates, parameter))^2
}

check_sim_ci_coverage <- function(lowers, uppers, parameter){
  mean((lowers < parameter) & (uppers > parameter))
}

# Going to start by only doing a cat version of this, then work on return later
check_sim_est_bias_for_param <-
  function(...){
    meth_res_arg_list <- list(...)

    parameters_vec <- c()
    parameters_vals_vec <- c()
    # Identify parameters of interest
    for (i in 1:length(meth_res_arg_list)){
      parameters_vec <- c(parameters_vec, meth_res_arg_list[[i]]$param_name)
      parameters_vals_vec <- c(parameters_vals_vec, meth_res_arg_list[[i]]$param_val)
    }
    # Check to see if we're dealing with different parameters or the same parameters across the methods
    if (!(length(parameters_vec) == length(unique(parameters_vec)))){
      if (length(unique(parameters_vec)) == length(unique(parameters_vals_vec))){
        # The parameter to estimate is the same in at least two of the methods,
        # and when we try to simplify the vectors the lengths match what we'd
        # expect, so we just do the naive thing. In theory some more error
        # checking here might be nice.
        parameters_vec <- unique(parameters_vec)
        parameters_vals_vec <- unique(parameters_vals_vec)
      }
      else{
        stop("LENGTH MISMATCH: parameters_vec and parameters_vals_vec")
      }
    }

    # hard coding this for now!
    # --- HARD CODED --- update!

    bias_output_df <-
      data.frame(
        parameter_name = character(),
        parameter_val = numeric(),

        est_name_A1 = character(),
        est_var_A1 = numeric(),
        bias_est_A1 = numeric(),
        mse_est_A1 = numeric(),

        est_name_A2 = character(),
        est_var_A2 = numeric(),
        bias_est_A2 = numeric(),
        mse_est_A2 = numeric(),

        est_name_B2 = character(),
        est_var_B2 = numeric(),
        bias_est_B2 = numeric(),
        mse_est_B2 = numeric(),

        est_name_C2 = character(),
        est_var_C2 = numeric(),
        bias_est_C2 = numeric(),
        mse_est_C2 = numeric() # add comma if continuing

        # est_name_A1 = character(),
        # est_name_A2 = character(),
        # est_name_B2 = character(),
        # est_name_C2 = character(),
        # bias_est_A1 = numeric(),
        # bias_est_A2 = numeric(),
        # bias_est_B2 = numeric(),
        # bias_est_C2 = numeric(),
        # mse_est_A1 = numeric(),
        # mse_est_A2 = numeric(),
        # mse_est_B2 = numeric(),
        # mse_est_C2 = numeric() # add comma if continuing

      )

    for (i in 1:length(parameters_vec)){
      cat("\n")
      cat(paste("Bias --- Parameter is ", parameters_vec[i], sep = ""))
      cat("\n---------------------------------------\n")
      output_for_current_method_df <-
        data.frame(
          parameter_name = parameters_vec[i],
          parameter_val = parameters_vals_vec[i]
        )
      for (cur_method in 1:length(meth_res_arg_list)){
        cat(paste("Current Method: ", meth_res_arg_list[[cur_method]]$name, "\n", sep = ""))

        # Older version
        # current_bias <-
        #   check_sim_est_bias(
        #     estimates = meth_res_arg_list[[cur_method]]$estimates,
        #     parameter = parameters_vals_vec[i]
        #   )
        # print(current_bias)

        # Newer version



        for (cur_est in 1:length(meth_res_arg_list[[cur_method]]$estimates_list)){
          current_bias <-
            check_sim_est_bias(
              estimates = meth_res_arg_list[[cur_method]]$estimates_list[[cur_est]]$est_val,
              parameter = parameters_vals_vec[i]
            )
          cat(paste("* ", meth_res_arg_list[[cur_method]]$estimates_list[[cur_est]]$est_name, ": ", round(current_bias, 3), "\n", sep = ""))

          current_estimate_variance <- var(meth_res_arg_list[[cur_method]]$estimates_list[[cur_est]]$est_val)
          output_for_current_method_df <-
            cbind(
              output_for_current_method_df,
              data.frame(
                meth_res_arg_list[[cur_method]]$estimates_list[[cur_est]]$est_name, # name
                current_estimate_variance, # variance of estimates
                current_bias, # bias for current parameter
                current_estimate_variance + (current_bias)^2 # MSE
              )
            )


        }

      }
      #browser()
      bias_output_df <-
        rbind(
          bias_output_df,
          output_for_current_method_df
        )
    }


    colnames(bias_output_df) <-     c("parameter_name",
                                      "parameter_val",

                                      "est_name_A1",
                                      "est_var_A1",
                                      "bias_est_A1",
                                      "mse_est_A1",

                                      "est_name_A2",
                                      "est_var_A2",
                                      "bias_est_A2",
                                      "mse_est_A2",

                                      "est_name_B2",
                                      "est_var_B2",
                                      "bias_est_B2",
                                      "mse_est_B2",

                                      "est_name_C2",
                                      "est_var_C2",
                                      "bias_est_C2",
                                      "mse_est_C2")
    #print(bias_output_df)
    return(bias_output_df)
  }
