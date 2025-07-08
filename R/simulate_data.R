#' Simulasi Data dari Berbagai Distribusi Statistik
#'
#' Fungsi ini digunakan untuk mensimulasikan data dari berbagai distribusi statistik,
#' baik diskrit maupun kontinu. Distribusi yang didukung antara lain:
#' \itemize{
#'   \item \strong{"normal"}: distribusi Normal (mean, sd)
#'   \item \strong{"exponential"}: distribusi Eksponensial (rate)
#'   \item \strong{"gamma"}: distribusi Gamma (shape, rate)
#'   \item \strong{"uniform"}: distribusi Uniform (min, max)
#'   \item \strong{"lognormal"}: distribusi Log-normal (meanlog, sdlog)
#'   \item \strong{"binomial"}: distribusi Binomial (size, prob)
#'   \item \strong{"poisson"}: distribusi Poisson (lambda)
#'   \item \strong{"geometric"}: distribusi Geometrik (prob)
#' }
#'
#' @param n Integer. Jumlah data yang ingin disimulasikan.
#' @param dist String. Nama distribusi yang ingin digunakan.
#' @param params List. Parameter yang diperlukan untuk distribusi tersebut.
#'
#' @return Vektor numerik berisi data simulasi dari distribusi yang dipilih.
#'
#' @examples
#' simulate_data(100, "normal", list(mean = 0, sd = 1))
#' simulate_data(100, "poisson", list(lambda = 3))
#' simulate_data(100, "uniform", list(min = 0, max = 10))
#'
#' @importFrom stats rnorm rexp rgamma runif rlnorm rbinom rpois rgeom
#' @export
simulate_data <- function(n = 100, dist = "normal", params = list()) {
  switch(dist,
         "normal" = rnorm(n, mean = params$mean, sd = params$sd),
         "exponential" = rexp(n, rate = params$rate),
         "gamma" = rgamma(n, shape = params$shape, rate = params$rate),
         "uniform" = runif(n, min = params$min, max = params$max),
         "lognormal" = rlnorm(n, meanlog = params$meanlog, sdlog = params$sdlog),
         "binomial" = rbinom(n, size = params$size, prob = params$prob),
         "poisson" = rpois(n, lambda = params$lambda),
         "geometric" = rgeom(n, prob = params$prob),
         stop("Distribusi tidak dikenali. Gunakan salah satu dari: normal, exponential, gamma, uniform, lognormal, binomial, poisson, geometric.")
  )
}
