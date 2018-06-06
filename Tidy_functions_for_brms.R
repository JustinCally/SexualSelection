
# Helper function for making supplementary tables of model results
make_text_summary <- function(model){
  posterior_samples(model) %>% 
    select(parnames(model), -starts_with("r"), -lp__) %>%
    as.mcmc() %>% posterior_summary() %>%
    round(3) %>% as.data.frame()
}

# For adding stars to the tables when the 95% CIs don't overlap zero
add_significance_stars <- function(posterior){
  posterior$xx <- " "
  posterior$xx[posterior[,3] < 0 & posterior[,4] < 0] <- "*"
  posterior$xx[posterior[,3] > 0 & posterior[,4] > 0] <- "*"
  names(posterior)[names(posterior) == "xx"] <- " "
  posterior
}

# Get the Bayes R^2 for a model, and it's 95% CIs, and neaten them for printing
neat.R2 <- function(model){
  R2 <- bayes_R2(model) %>% round(2)
  paste(R2[1,1], " (95% CIs = ", R2[1,3], "-", R2[1,4], ")", sep = "")
}