#' Visualisasi Distribusi Data vs Distribusi Teoritis
#'
#' Fungsi ini menampilkan histogram dari data yang disimulasikan, lalu menambahkan kurva fungsi kepadatan probabilitas (PDF) dari distribusi teoritis sebagai pembanding. Fungsi ini hanya berlaku untuk distribusi kontinu seperti:
#' \itemize{
#'   \item \strong{"normal"}: dnorm
#'   \item \strong{"exponential"}: dexp
#'   \item \strong{"gamma"}: dgamma
#'   \item \strong{"uniform"}: dunif
#'   \item \strong{"lognormal"}: dlnorm
#' }
#'
#' @param data Vektor numerik. Data hasil simulasi.
#' @param dist String. Nama distribusi yang digunakan.
#' @param params List. Parameter dari distribusi yang bersangkutan.
#'
#' @return Tidak mengembalikan objek. Fungsi menghasilkan grafik (histogram + PDF teoritis).
#'
#' @examples
#' data <- simulate_data(200, "normal", list(mean = 0, sd = 1))
#' visual_gof(data, "normal", list(mean = 0, sd = 1))
#'
#' @importFrom graphics hist curve
#' @importFrom stats dnorm dexp dgamma dunif dlnorm
#' @export
visual_gof <- function(data, dist, params) {
  hist(data,
       probability = TRUE,
       col = "lightblue",
       breaks = 30,
       main = paste("Histogram dan PDF -", dist),
       xlab = "Data")

  curve({
    switch(dist,
           "normal" = dnorm(x, mean = params$mean, sd = params$sd),
           "exponential" = dexp(x, rate = params$rate),
           "gamma" = dgamma(x, shape = params$shape, rate = params$rate),
           "uniform" = dunif(x, min = params$min, max = params$max),
           "lognormal" = dlnorm(x, meanlog = params$meanlog, sdlog = params$sdlog),
           NA  # fallback jika distribusi tidak dikenali
    )
  }, col = "red", lwd = 2, add = TRUE)
}
