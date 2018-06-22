#' multinom_sum
#'
#' This function produces cleaner output of the multinom function (nnet package)
#' @param x multinom model object
#' @keywords multinom, multinomial regression
#' @export
#' @examples
#' multinom_sum

multinom_sum <- function(x) {
  z.stat <- summary(x)$coefficients/summary(x)$standard.errors
  p.val <- (1 - pnorm(abs(z.stat), 0, 1))*2
  outtable <- data.frame(
    Outcome=melt(t(summary(x)$coefficients))[,2],
    Predictor=melt(t(summary(x)$coefficients))[,1],
    Odds=round(exp(melt(t(summary(x)$coefficients))[,3]),5),
    Coef=round(melt(t(summary(x)$coefficients))[,3],5),
    Std_Error=round(melt(t(summary(x)$standard.errors))[,3],5),
    CI_2.5=round(matrix(melt(confint(x))[order(melt(confint(x))$Var2),
                                         ]$value,
                        ncol=2),5)[,1],
    CI_97.5=round(matrix(melt(confint(x))[order(melt(confint(x))$Var2),
                                          ]$value,
                         ncol=2),5)[,2],
    z=round(melt(z.stat)[order(melt(z.stat)$Var1),]$value,3),
    p=round(melt(p.val)[order(melt(p.val)$Var1),]$value,3)
  )
  outtable$sig <- ifelse(outtable$p<=.001,'***',
                         ifelse(outtable$p<=.01,'**',
                                ifelse(outtable$p<=.05,'*',
                                       ifelse(outtable$p<=.1,'.',' '))))
  sink('file')
  lrt_r <- anova(update(x,.~-.),x)
  sink()
  mfit <- data.frame(
    n=nrow(residuals(x)),
    AIC=summary(x)$AIC,
    Deviance=summary(x)$deviance,
    Log_lik=logLik(x),
    LRT_df=lrt_r$`   Df`[2],
    LRT_chisq=round(lrt_r$`LR stat.`[2],4),
    LRT_p=round(lrt_r$`Pr(Chi)`[2],3),
    McF_R2=1-(lrt_r$`Resid. Dev`[2]/lrt_r$`Resid. Dev`[1]),
    Ref_Group=summary(x)$lev[
      !summary(x)$lev%in%rownames(summary(x)$coefficients)]
  )
  print(mfit)
  print(outtable)
  invisible(list(model=x,
                 z.stat=z.stat,
                 p.val=p.val,
                 fit=mfit,
                 table=outtable,
                 anova_table=lrt_r))
}
