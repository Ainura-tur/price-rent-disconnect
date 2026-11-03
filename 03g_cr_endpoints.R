# =============================================================================
# 03g_cr_endpoints.R -- MCUB valid-range endpoints not stated in the text
#
# Uses the ivcrtest package (github.com/Ainura-tur/ivcrtest) with the
# moving-block bootstrap covariance of the three correlations (blocks of eight
# quarters, cities kept together), as in Table D.1. Computes, for the
# national-timing instrument and prices, the upper end of the valid range at
# h = 4, 8, 12 and the lower end at h = 4, 8, 12.
# =============================================================================

src <- readLines("03f_missing_figures.R")
eval(parse(text = src[1:grep("^grid <- seq", src)]))   # data, prep(), cfgs, boot_cov()
suppressPackageStartupMessages({ library(ivcrtest); library(parallel) })

cell <- function(cf, h) {
  o <- prep(cf$y, cf$own, cf$z, h, cf$tw)
  rs <- function(v) as.numeric(residuals(plm(as.formula(paste(v, "~", paste(o$ctrl, collapse = "+"))),
                                             o$P, model = "within", effect = o$eff)))
  df <- data.frame(qid = as.integer(as.character(o$P$qid)), y = rs("Y"), x = rs("X"), z = rs(cf$z))
  if (cor(df$z, df$x) < 0) df$z <- -df$z
  set.seed(1000 + h); Vb <- boot_cov(df)
  list(df = df, S = Vb[c("xy", "zx", "zy"), c("xy", "zx", "zy")])   # order (xy, xz, yz)
}
hs <- c(4, 8, 12)
cells <- lapply(hs, function(h) cell(cfgs[[1]], h))   # national timing, prices
tasks <- list(list(c = 1, dir = "below", br = c(0.70, 0.799)),
              list(c = 2, dir = "below", br = c(0.70, 0.799)),
              list(c = 3, dir = "below", br = c(0.70, 0.799)),
              list(c = 1, dir = "above", br = NULL),
              list(c = 2, dir = "above", br = NULL),
              list(c = 3, dir = "above", br = c(0.05, 0.30)))
run <- function(t) {
  tk <- tasks[[t]]; cc <- cells[[tk$c]]
  sp <- try(cr_switch_point(cc$df, direction = tk$dir, bracket = tk$br, Sigma_rho = cc$S,
                            steps = 6, scan_grid = c(0.005, 0.01, 0.02, 0.05, 0.10)), silent = TRUE)
  if (inherits(sp, "try-error"))
    return(data.frame(h = hs[tk$c], direction = tk$dir, tau = NA, status = conditionMessage(attr(sp, "condition"))))
  data.frame(h = hs[tk$c], direction = tk$dir, tau = sp$tau, status = sp$status,
             bracket_lo = sp$bracket[1], bracket_hi = sp$bracket[2])
}
# Windows cannot fork, so the six endpoints run one after the other there.
out <- dplyr::bind_rows(mclapply(seq_along(tasks), run,
                                 mc.cores = if (.Platform$OS.type == "windows") 1L else min(6, detectCores())))
print(out)
write_csv(out, file.path(OUTDIR, "cr_mcub_endpoints.csv"))
