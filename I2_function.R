#Script for I2, taken and modified from Dan Nobles MetaAidR on github


round_df <- function(x, digits) {
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

I2 <- function(model, v, ME = FALSE, sims = 1500, phylo = FALSE){
  
  if(class(model) != "MCMCglmm" && class(model) != "rma.mv" && class(model) != "rma"){
    stop("The model object is not of class 'MCMCglmm' or 'metafor'")
  }
  
  wi <- 1/v  #weight
  Vw <- sum((wi) * (length(wi) - 1))  / (((sum(wi)^2) - sum((wi)^2)))
  
  if("MCMCglmm" %in% class(model)){
    # Get posterior distribution
    # TO DO : NEED TO MAKE THIS WORK WITH ginverse in MCMCglmm. Added a bit, but needs work.
    if(ME == FALSE){
      post <- model$VCV[,-match(c("sqrt(mev):sqrt(mev).meta"), colnames(model$VCV))]
    } else{
      post <- model$VCV[,-match(ME, colnames(model$VCV))]
    }
    #Calculate total variance
    VT <- rowSums(Matrix::cBind(post, Vw))
    Vt <- rowSums(post)  # remove Vw
    
    # For each variance component divide by the total variance. Note this needs to be fixed for phylo, but does deal with variable random effects.
    I2_re <- post / VT
    I2_total  <- Vt / VT
    
    if(phylo == FALSE){
      tmpMatrix <- Matrix::cBind(I2_re, total = I2_total)
    }else{
      I2_phylo <- post[,match(phylo, colnames(post))] / Vt
      tmpMatrix <- Matrix::cBind(I2_re, I2_phylo, total = I2_total)
    }
    
    mode <- MCMCglmm::posterior.mode(coda::as.mcmc(tmpMatrix))
    CI <- coda::HPDinterval(coda::as.mcmc(tmpMatrix))
    colnames(CI) <- c("2.5% CI", "97.5% CI")
    
    I2_Table <- as.data.frame(Matrix::cBind(I2_Est. = mode[-match("units", names(mode))], CI[-match("units", rownames(CI)),]))
    class(I2_Table) <- c("metaAidR", "data.frame")
    
    return(round_df(I2_Table, digits = 4))
  }
  
  if("rma.mv" %in% class(model) | "rma" %in% class(model)){
    #Monte Carlo Simulations
    # From metafor extract the important statistics
    sigma2 <- matrix(model$sigma2, nrow = 1, ncol = length(model$sigma2))
    colnames(sigma2) <- model$s.names
    sigmaN <- model$s.nlevels
    
    if("Observation.level" %in% colnames(sigma2) == FALSE){
      stop("The metafor object does not contain a residual variance estimate. Please include an observation-level random effect (~1|obs) when fitting model")
    }
    
    #For each variance estimate use Monte Carlo simulation of data
    Sims <- data.frame(mapply(function(x,y) simMonteCarlo(x, y, sims = sims), sigma2, sigmaN))
    colnames(Sims) <- colnames(sigma2) 
    
    #Calculate total variance
    VT <- rowSums(Matrix::cBind(Sims, Vw))
    Vt <- rowSums(Sims)  # remove Vw
    
    # For each variance component divide by the total variance. Note this needs to be fixed for phylo, but does deal with variable random effects.
    I2_re       <- (Sims / VT)*100
    I2_total   <- (Vt / VT)*100
    
    if(phylo == FALSE){
      tmpMatrix <- Matrix::cBind(I2_re[colnames(I2_re)], total = I2_total)
    }else{
      I2_phylo <- Sims[, match(phylo, colnames(sigma2))] / Vt
      tmpMatrix <- Matrix::cBind(I2_re[colnames(I2_re)], phylo = I2_phylo, total = I2_total)
    }
    
    CI <- lapply(tmpMatrix, function(x) stats::quantile(x, c(0.025, 0.975), na.rm = TRUE))
    I_CI <- as.data.frame(do.call(rbind, CI))
    colnames(I_CI) <- c("2.5% CI", "97.5% CI")
    I2_table <- Matrix::cBind(I2_Est. = colMeans(tmpMatrix), I_CI )
    
    class(I2_table) <- c("metaAidR", "data.frame")
    
    return(round_df(I2_table, digits = 4))
  }
  
}