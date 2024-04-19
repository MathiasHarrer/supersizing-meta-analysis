# resultExtractor --------------------------------------------------------------

resultExtractor = function(x) {
  cbind(
    .TE = x$model.threelevel.che$beta[,1],
    .seTE = x$model.threelevel.che$se,
    k = x$summary["Three-Level Model (CHE)",]$k,
    n = metafor::ranef(x$model.threelevel.che)$study %>% nrow,
    g = paste0(
      format(round(x$model.threelevel.che$beta, 2), nsmall = 2), " [",
      format(round(x$model.threelevel.che$ci.lb, 2), nsmall = 2),"; ",
      format(round(x$model.threelevel.che$ci.ub, 2), nsmall = 2),"]"),
    pi = x$summary["Three-Level Model (CHE)",]$prediction.ci,
    nnt = format(round(x$summary["Three-Level Model (CHE)",]$nnt,2), nsmall=2),
    tau.between = paste0(
      format(round(x$model.threelevel.che$variance.components[1,"tau2"], 3), 
             nsmall = 3), " [",
      format(round(x$model.threelevel.che$variance.components[1,"i2"], 1), 
             nsmall = 1),"%]"),
    tau.within = paste0(
      format(round(x$model.threelevel.che$variance.components[2,"tau2"], 3), 
             nsmall = 3), " [",
      format(round(x$model.threelevel.che$variance.components[2,"i2"], 1), 
             nsmall = 1),"%]")
  )
}

# makeFormula ------------------------------------------------------------------

makeFormula = function(x) {
  preds = names(x[x])
  f = paste("~", paste(preds, collapse = " + "))
  as.formula(f)
}
