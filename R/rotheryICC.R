#' Rothery's rank-based intraclass correlation
#'
#' This function computes a rank-based intraclass correlation per Rothery (1979).
#' The normal approximation is used to estimate the variance and the p-value,
#' assuming an ICC value under the null of 2/3. 
#' 
#' @param df Data frame containing var and id, in long form (many rows per id).
#' @param var Column containing the measure of interest.
#' @param id Column containing the cluster identifier.
#' @importFrom dplyr mutate summarize group_by
#' @return Data frame containing point estimate on native (r_c) and linear (rho) scales, standard error, 95% CI, and p-value of Rothery's ICC. 
#' @keywords emj, icc, intraclass, correlation
#' @references Rothery, P. (1979). A nonparametric measure of intraclass correlation. *Biometrika* **66**, 629-639. \href{doi:10.1093/biomet/66.3.629}{doi:10.1093/biomet/66.3.629}.
#' @export

rotheryICC <- function(df, var, id) {
  
  ## Helper functions, not for exporting
  C_alpha <- function(N, n_alpha, R) {
    const <- (1/2)*(N - n_alpha)*n_alpha*(n_alpha - 1) +
      (1/6)*n_alpha*(n_alpha^2 - 1)
    R_sort <- sort(R)
    sum <- 0
    for (i in 1:n_alpha) {
      for (j in 1:n_alpha) {
        term <- abs(R_sort[j] - R_sort[i])
        sum <- sum + term
      }
    }
    T <- 
      out <- const - (1/2)*abs(sum)
    return(out)
  }
  
  S2_alpha <- function(n_alpha) {
    out <- n_alpha*(n_alpha-1)
    return(out)
  }
  
  S3_alpha <- function(N, n_alpha) {
    out <- (1/2)*n_alpha*(n_alpha-1)*(N-n_alpha)
    return(out)
  }
  ## Rothery script proper begins here
  names(df)[which(colnames(df)==var)] <- "var"
  names(df)[which(colnames(df)==id)] <- "id"
  r_c  <- df %>%
    mutate(rank = rank(var)) %>%
    merge(summarize(., N=n())) %>%
    group_by(id) %>%
    summarize(N=first(N),
              n_alpha = n(),
              C_alpha = C_alpha(N=first(N), n_alpha=n_alpha, R=rank),
              S3_alpha = S3_alpha(N=first(N), n_alpha=n_alpha),
              S2_alpha = S2_alpha(n_alpha=n_alpha),
              .groups="drop") %>%
    summarize(r_c = sum(C_alpha)/sum(S3_alpha), 
              S2 = sum(S2_alpha),
              S3 = sum(S3_alpha),
              C_alpha = sum(C_alpha),
              r_c2 = C_alpha/S3,
              var_r_c = (1/4)*(S3^-2)*(1/45)*(first(N)+1)*(6*S3 - S2^2 + (first(N)-1)*sum(S2_alpha*n_alpha)),
              normal_lower = r_c2 - 1.96*sqrt(var_r_c),
              normal_upper = r_c2 + 1.96*sqrt(var_r_c),
              rho = 2*sin(pi*(r_c - 1/2))-1,
              p = pnorm(r_c, mean=(2/3), sd=sqrt(var_r_c), lower.tail=FALSE),
              .groups="drop")
  out <- data.frame(Estimate=r_c$r_c,
                    Std.Err.=sqrt(r_c$var_r_c),
                    ci.lower=r_c$normal_lower,
                    ci.upper=r_c$normal_upper,
                    p=r_c$p,
                    rho=r_c$rho,
                    method="normal approximation")
  return(out)
}