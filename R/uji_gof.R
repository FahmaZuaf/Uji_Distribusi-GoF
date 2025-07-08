#' Uji Goodness-of-Fit (GoF) untuk Data Simulasi
#'
#' Fungsi ini menguji kesesuaian data terhadap distribusi teoritis.
#' Untuk data dari distribusi kontinu (seperti normal, gamma, dll), digunakan uji Kolmogorov-Smirnov.
#' Untuk data dari distribusi diskrit (seperti binomial, poisson, dll), digunakan uji Chi-square.
#'
#' Distribusi kontinu yang didukung:
#' \itemize{
#'   \item \strong{"normal"}: pnorm
#'   \item \strong{"exponential"}: pexp
#'   \item \strong{"gamma"}: pgamma
#'   \item \strong{"uniform"}: punif
#'   \item \strong{"lognormal"}: plnorm
#' }
#'
#' Distribusi diskrit yang didukung:
#' \itemize{
#'   \item \strong{"binomial"}: dbinom
#'   \item \strong{"poisson"}: dpois
#'   \item \strong{"geometric"}: dgeom
#' }
#'
#' @param data Vektor numerik. Data yang akan diuji kesesuaiannya.
#' @param dist String. Nama distribusi teoritis yang digunakan.
#' @param params List. Parameter distribusi. Misalnya: list(mean = 0, sd = 1) untuk normal.
#'
#' @return Objek hasil uji dari `ks.test()` atau `chisq.test()`, tergantung distribusinya.
#'
#' @examples
#' x <- simulate_data(200, "normal", list(mean = 0, sd = 1))
#' uji_gof(x, dist = "normal", params = list(mean = 0, sd = 1))
#'
#' y <- simulate_data(200, "poisson", list(lambda = 3))
#' uji_gof(y, dist = "poisson", params = list(lambda = 3))
#'
#' @importFrom stats pnorm pexp pgamma punif plnorm
#' @importFrom stats dbinom dpois dgeom
#' @importFrom stats ks.test chisq.test
#' @export
uji_gof <- function(data, dist, params = list()) {
  if (dist %in% c("normal", "exponential", "gamma", "uniform", "lognormal")) {
    pfun <- switch(dist,
                   "normal" = function(x) pnorm(x, mean = params$mean, sd = params$sd),
                   "exponential" = function(x) pexp(x, rate = params$rate),
                   "gamma" = function(x) pgamma(x, shape = params$shape, rate = params$rate),
                   "uniform" = function(x) punif(x, min = params$min, max = params$max),
                   "lognormal" = function(x) plnorm(x, meanlog = params$meanlog, sdlog = params$sdlog)
    )
    return(ks.test(data, pfun))

  } else if (dist %in% c("binomial", "poisson", "geometric")) {
    tab <- table(factor(data, levels = 0:max(data)))
    expected_probs <- switch(dist,
                             "binomial" = dbinom(0:max(data), size = params$size, prob = params$prob),
                             "poisson" = dpois(0:max(data), lambda = params$lambda),
                             "geometric" = dgeom(0:max(data), prob = params$prob)
    )
    expected <- expected_probs * length(data)
    return(chisq.test(tab, p = expected / sum(expected), rescale.p = TRUE))

  } else {
    stop("Distribusi tidak dikenali untuk uji GoF.")
  }
}
