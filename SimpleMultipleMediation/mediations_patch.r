########################################## BUG FIXES & EXTENSION ##############################################
# This is a bug fix for the mediations() function of the mediation package (v. 3.1.2) that fixes two issues.  #
# First, there was an error in line 17 of the function code that prevented the function from working unless   #
# covariates were specified. Second, for multiple mediator models, the mediations() function did not take into#
# account that the direct effects of each mediator should control for the direct effects of the other         #
# mediators. In addition, for personal convenience, the default for bootstrapping was changed to TRUE and the #
# labelling of the models in the output was altered to improve readability.                                   #
#                                                                                                             #
# An additional function was written for the class mediations called total_indirect. It calculates the size,  #
# standard error, and confidence interval for the total indirect effect, which provides a single test for the #
# the set of mediators (c.f., Preacher, K.J., & Hayes, A.F. (2008) Behavior Research Methods, 40, 879 - 891.).#
# It takes an object of class mediations as its first argument and optionally allows for the width of the     #
# confidence interval to be set with the "conf.level=" option (borrowing the syntax from mediations().        #
# conf.level takes a value between 0 and 1, and the default is .95 for a 95% confidence interval.             #
#                                                                                                             #
# Finally, an additional function was written for the mediation class called standard_errors. It prints the   #
# standard errors for the indirect, direct, and total effects.                                                #
###############################################################################################################

mediations <- function (datasets, treatment, mediators, outcome, covariates = NULL, 
    families = c("gaussian", "gaussian"), tau.m = 0.5, tau.y = 0.5, 
    LowerY = NULL, UpperY = NULL, interaction = FALSE, conf.level = 0.95, 
    sims = 50, boot = TRUE, weights = NULL, ...) 
{
    data <- names(datasets)
    labels <- c()
    out <- list()
    temp.out <- list()
    count <- 1
    weight.storage <- weights
    other.mediators <- c()
    for (j in 1:length(mediators)) {
      trimmed.mediators <- mediators[-j]
      other.mediators[j] <- paste(trimmed.mediators, collapse=" + ")
      }
    for (i in 1:length(treatment)) {
        d1 <- sprintf("datasets$%s", data[i])
        dataarg <- eval(parse(text = d1))
        for (o in 1:length(outcome)) {
            for (j in 1:length(mediators)) {
                if (is.null(covariates)) {
                  f1 <- sprintf("%s ~ %s", mediators[j], 
                    treatment[i])
                  if (interaction) {
                    f2 <- sprintf("%s ~ %s * %s + %s", outcome[o], 
                      treatment[i], mediators[j], other.mediators[j])
                    f2b <- sprintf("%s ~ %s * %s", outcome[o], 
                      treatment[i], mediators[j])
                    }
                  else {
                    f2 <- sprintf("%s ~ %s + %s + %s", outcome[o], 
                      treatment[i], mediators[j], other.mediators[j])
                    f2b <- sprintf("%s ~ %s + %s", outcome[o], 
                      treatment[i], mediators[j])
                    }
                }
                else {
                  f1 <- sprintf("%s ~ %s + %s", mediators[j], 
                    treatment[i], covariates)
                  if (interaction) {
                    f2 <- sprintf("%s ~ %s * %s + %s + s", outcome[o], 
                      treatment[i], mediators[j], covariates, other.mediators[j])
                    f2b <- sprintf("%s ~ %s * %s + %s", outcome[o], 
                      treatment[i], mediators[j], covariates)
                  }
                  else {
                    f2 <- sprintf("%s ~ %s + %s + %s + %s", outcome[o], 
                      treatment[i], mediators[j], covariates, other.mediators[j])
                    f2b <- sprintf("%s ~ %s + %s + %s", outcome[o], 
                      treatment[i], mediators[j], covariates)
                  }
                }
                if (!is.null(weights)) {
                  weight1 <- sprintf("dataarg$%s", weights)
                  weight <- as.data.frame(eval(parse(text = weight1)))
                }
                else {
                  dataarg$weight <- weight <- rep(1, nrow(dataarg))
                }
                if (families[1] == "binomial") {
                  result1 <- glm(f1, family = binomial("probit"), 
                    weights = weight, data = dataarg)
                }
                else if (families[1] == "quantile") {
                  if (!is.null(weights)) {
                    stop("Weights not supported with quantile regression")
                  }
                  else {
                    result1 <- rq(f1, data = dataarg, tau = tau.m)
                  }
                }
                else if (families[1] == "oprobit") {
                  result1 <- polr(f1, method = "probit", weights = weight, 
                    data = dataarg)
                }
                else if (families[1] == "gaussian") {
                  result1 <- glm(f1, family = "gaussian", weights = weight, 
                    data = dataarg)
                }
                else {
                  stop("mediations does not support this model for the mediator")
                }
                if (families[2] == "binomial") {
                  result2 <- glm(f2, family = binomial("probit"), 
                    weights = weight, data = dataarg)
                  result2b <- glm(f2b, family = binomial("probit"), 
                    weights = weight, data = dataarg)
                }
                else if (families[2] == "quantile") {
                  if (!is.null(weights)) {
                    stop("Weights not supported with quantile regression")
                  }
                  else {
                    result2 <- rq(f2, data = dataarg, tau = tau.y)
                    result2b <- rq(f2b, data = dataarg, tau = tau.y)
                  }
                }
                else if (families[2] == "tobit") {
                  result2 <- vglm(f2, tobit(Lower = LowerY, Upper = UpperY), 
                    weights = weight, data = dataarg)
                  result2b <- vglm(f2b, tobit(Lower = LowerY, Upper = UpperY), 
                    weights = weight, data = dataarg)
                }
                else if (families[2] == "oprobit") {
                  result2 <- polr(f2, method = "probit", weights = weight, 
                    data = dataarg)
                  result2b <- polr(f2b, method = "probit", weights = weight, 
                    data = dataarg)
                }
                else if (families[2] == "gaussian") {
                  result2 <- glm(f2, family = "gaussian", weights = weight, 
                    data = dataarg)
                  result2b <- glm(f2b, family = "gaussian", weights = weight, 
                    data = dataarg)
                }
                else {
                  print("mediations does not support this model for the outcome")
                }
                if (is.null(weight.storage)) {
                  out[[(count)]] <- mediate(result1, result2, 
                    sims = sims, treat = treatment[i], mediator = mediators[j], 
                    conf.level = conf.level, boot = boot, ...)
                  temp.out[[(count)]] <- mediate(result1, result2b,
                    sims = sims, treat = treatment[i], mediator = mediators[j], 
                    conf.level = conf.level, boot = boot, ...)
                  out[[(count)]]$tau.coef <- temp.out[[(count)]]$tau.coef
                  out[[(count)]]$tau.ci <- temp.out[[(count)]]$tau.ci
                  out[[(count)]]$tau.sims <- temp.out[[(count)]]$tau.sims
                  summary(out[[(count)]])
                }
                else {
                  out[[(count)]] <- mediate(result1, result2, 
                    sims = sims, treat = treatment[i], mediator = mediators[j], 
                    conf.level = conf.level, boot = boot, ...)
                  temp.out[[(count)]] <- mediate(result1, result2b,
                    sims = sims, treat = treatment[i], mediator = mediators[j], 
                    conf.level = conf.level, boot = boot, ...)
                  out[[(count)]]$tau.coef <- temp.out[[(count)]]$tau.coef
                  out[[(count)]]$tau.ci <- temp.out[[(count)]]$tau.ci
                  out[[(count)]]$tau.sims <- temp.out[[(count)]]$tau.sims
                  weights <- weight.storage
                }
                rm(result1, result2)
                labels[(count)] <- sprintf("Effects for the Path Through the Mediator: %s", mediators[j])
                count <- count + 1
            }
        }
        if (!is.null(weight.storage)) {
            weights <- weight.storage
        }
    }
    names(out) <- labels
    if (families[2] == "oprobit") {
        class(out) <- "mediations.order"
    }
    else {
        class(out) <- "mediations"
    }
    out
    }


total_indirect <- function (mediations.object, conf.level = .95) {
  
  model.names <- names(mediations.object)
  #Creating an empty list object to store the data from the resampling procedure of each model
  indirect.resamples <- list()
  
  #Provides the opportunity to alter the width of the confidence interval
  lower.proportion <- (1 - conf.level)/2
  upper.proportion <- conf.level + lower.proportion
  confidence.label <- conf.level * 100
  for (i in 1:length(model.names)) {
     indirect.resamples[[i]] <- mediations.object[[model.names[i]]]$d.avg.sims
  }
  indirect.resamples <- data.frame(indirect.resamples)
  total.indirect.resamples <- rowSums(indirect.resamples)

  cat("Total Indirect Effect: ", mean(total.indirect.resamples), ",    SE: ", sd(total.indirect.resamples), ",    ", confidence.label, "% CI: ", quantile(total.indirect.resamples, lower.proportion), " - ", quantile(total.indirect.resamples, upper.proportion), sep="")
}

standard_errors <- function( mediation.object ) {
  if (class(mediation.object) == "mediate")
  {
    cat("\n Standard Errors for Mediation Analysis\n",
      "\nSE of Mediation (Indirect) Effect:\t", sd(as.numeric(mediation.object$d.avg.sims)),
      "\nSE of Direct Effect:\t", sd(as.numeric(mediation.object$z.avg.sims)),
      "\nSE of Total Effect:\t", sd(as.numeric(mediation.object$tau.sims)),
      "\n\n")
  }
  else {
    model.names <- names(mediation.object)
    for (i in 1:length(model.names)) {
      cat(" Standard Errors for Path With Mediator: ", mediation.object[[i]]$mediator, "\n",
        "\nSE of Mediation (Indirect) Effect:\t", sd(mediation.object[[i]]$d.avg.sims),
        "\n")
      }
   }
}
  
augment_summary <- function(mediations.object, conf.level=.95) {
  #Provides the opportunity to alter the width of the confidence interval
  lower.proportion <- (1 - conf.level)/2
  upper.proportion <- conf.level + lower.proportion
  confidence.label <- conf.level * 100
  model.names <- names(mediations.object)
    total.effect.resamples <- c()
    direct.effect.resamples <- c()
    df <- 0
    for (i in 1:length(model.names)) {
      print(summary(mediations.object[[i]]))
      df <- as.numeric(mediations.object[[i]]$model.m$df.residual)
      standard_errors(mediations.object[[i]])
      total.effect.resamples <- c(total.effect.resamples, mediations.object[[i]]$tau.sims)
      direct.effect.resamples <- c(direct.effect.resamples, mediations.object[[i]]$z.avg.sims)
    }
    cat("\n\nTotal Effect:", mean(total.effect.resamples),
        ",  SE:", sd(total.effect.resamples),
        ", t:", mean(total.effect.resamples)/sd(total.effect.resamples),
        ", p=", (1 - pt(abs(mean(total.effect.resamples)/sd(total.effect.resamples)), df))*2,
        ",  ", confidence.label, "% CI:",
            quantile(total.effect.resamples, lower.proportion),
            "-",
            quantile(total.effect.resamples, upper.proportion)
        )

    cat("\nDirect Effect:", mean(direct.effect.resamples),
        ",  SE:", sd(direct.effect.resamples),
        ", t:", mean(direct.effect.resamples)/sd(direct.effect.resamples),
        ", p=", (1 - pt(abs(mean(direct.effect.resamples)/sd(direct.effect.resamples)), df))*2,
        ",  ", confidence.label, "% CI:",
            quantile(direct.effect.resamples, lower.proportion),
            "-",
            quantile(direct.effect.resamples, upper.proportion)
        )
  cat("\n")
  total_indirect(mediations.object)
}