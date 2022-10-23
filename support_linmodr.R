# supporting functions for mathmethr course


fix_aov_smr <- function(aov_obj, rown = NULL, coln = NULL){
  #' Function to assist with printing aov summary object with xtable
  #' Fixes p-values, allows to set rownames and colnames
  #' Author: Marina Varfolomeeva
  pvals <- aov_obj[[1]][, ncol(aov_obj[[1]])]
  pvals <- format.pval(pvals, eps = 0.01)
  pvals[pvals == "NA"] <- NA
  aov_obj[[1]][, ncol(aov_obj[[1]])] <- pvals
  aov_obj[[1]][, 1] <- format(aov_obj[[1]][, 1], digits = 0)
  if(!is.null(rown)){
    rownames(aov_obj[[1]]) <- rown
  }
  if(!is.null(coln)){
    colnames(aov_obj[[1]]) <- coln
  }
  return(aov_obj)
}

fix_Anova <- function(anova_obj, rown = NULL, coln = NULL){
  #' Function to assist with printing Anova{car} object with xtable
  #' Fixes p-values, allows to set rownames and colnames
  #' Author: Marina Varfolomeeva
  anova_obj <- data.frame(anova_obj)
  pvals <- anova_obj[, 4]
  pvals <- format.pval(pvals, eps = 0.01)
  pvals[pvals == "NA"] <- NA

  anova_obj[, 4] <- pvals
  anova_obj[, c(1, 3)] <- format(anova_obj[, c(1, 3)], digits = 1, nsmall = 1)
  anova_obj$F.value[nrow(anova_obj)] <- NA
  # anova_obj[, 2] <- format(anova_obj[, 2], digits = 0)
  if(!is.null(rown)){
    rownames(anova_obj) <- rown
  }
  if(!is.null(coln)){
    colnames(anova_obj) <- coln
  }
  return(anova_obj)
}


lm_equation <- function(fit, strict = TRUE, digits = 2, trim = TRUE){
  #' Function to produce equation of linear model
  #' given the model object. Intended for use with lm
  #' Author: Marina Varfolomeeva
  #' Arguments:
  #' fit - lm model object
  #' strict - shoud the variable names be replaced with Xn
  #'
  #   extracting call formula
  frml <- as.character(fit$call)[2]
  #   extract signs
  sign <- ifelse(grepl("-", coef(fit)[-1]), " - ", " + ")
  # extract coefficients
  coeffs <- format(abs(coef(fit)), digits = digits, trim = trim)
  if(strict == TRUE){
    i <- 1:(length(coeffs) - 1)
    vars <- c("Y", paste0(" X", i))

  } else {
    # extract vector of variable names
    vars <- unlist(strsplit(frml, "[~+]"))
    # combine everything
  }
  start <- ifelse(coef(fit)[1] > 0, paste(vars[1], coeffs[1], sep = " = "), paste(vars[1], coeffs[1], sep = " = - "))
  end <- paste(sign, coeffs[-1], vars[-1], sep = "", collapse = "")
  return(cat(start, end, sep = ""))
}
