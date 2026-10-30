# =============================================================================
# 03e_cr_table_figure.R  --  paper table and figure for the CR test
#
# Turns output/cr_results.csv, written by 03d_cr_test_apra.R, and
# output/lpiv_results.csv, written by 03c_lp_iv_apra.R, into
#   output/table_cr_apra.tex       the compact table for the paper: for the two
#                                  headline designs, the IV and OLS responses,
#                                  the first-stage F, the MCUB interval for
#                                  corr(z, u) and p_adm at 4, 8 and 12 quarters
#                                  (main credit measure, full sample)
#   output/fig_cr_valid_range.png  the valid range of credit's correlation with
#   output/fig_cr_valid_range.pdf  the error, from the switching points, for the
#                                  same projections
# The valid ranges appear only in the figure, so no number is reported twice.
# To report them in the table instead, set VALID_RANGE_IN_TABLE to TRUE.
#
# Run from the repository root after 03d:
#   Rscript 03e_cr_table_figure.R
# It reads two CSV files, does not need 00_setup.R, and takes a few seconds.
#
# Suggested caption for the grouped figure (no title is drawn): "Valid ranges
# for credit's correlation with the error, by outcome, horizon and instrument.
# At each horizon, the upper bar is the national-timing instrument (either
# APRA measure in force) and the lower bar the exposure instrument (2014
# investor share times the interest-only cap). Each coloured bar shows the
# assumptions about rho_xu under which the instrument passes the
# correlation-restriction test: restricting rho_xu to (0, r) passes once r
# reaches the bar's left end, and restricting it to (r, 0.8) fails once r
# exceeds its right end. Grey marks the assumed domain, [0, 0.8]; a grey bar
# with no coloured part fails for every value."
# With FIG_LAYOUT <- "grid", rows are instruments and columns outcomes, and a
# row without a coloured bar fails for every value.
# =============================================================================

# Log this run in run_logs/ (see 00_run_log.R); the last line closes the log.
source("00_run_log.R")
start_run_log("03e_cr_table_figure")

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(ggplot2)
})
# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

# ---- Settings ----------------------------------------------------------------
OUTDIR  <- "output"
D_MAX   <- 0.8                      # upper end of the assumed domain for rho_xu
SCAN    <- c(0.005, 0.75)           # range scanned by cr_switch_point()
VALID_RANGE_IN_TABLE <- FALSE
FIG_LAYOUT <- "grouped"            # "grouped": instruments side by side; "grid": 2 x 2
# Grouped layout: one colour per instrument (indigo, amber), checked for
# colour-blind separation. Grid layout: the price and rent colours of the
# paper's Figure 1; the rent teal is a little more saturated so that it does
# not read as grey.
COL_INSTRUMENT <- c("National timing" = "#3F4E8C", "Exposure" = "#C8792B")
COL_OUTCOME <- c(Prices = "#8C2D3A", Rents = "#00897B")
COL_DOMAIN  <- "#E3E6E8"
COL_INK     <- "#33383C"

f_res  <- file.path(OUTDIR, "cr_results.csv")
f_lpiv <- file.path(OUTDIR, "lpiv_results.csv")
if (!file.exists(f_res))  stop(f_res, " not found. Run 03d_cr_test_apra.R first.")
if (!file.exists(f_lpiv)) stop(f_lpiv, " not found. Run 03c_lp_iv_apra.R first; the table reports its OLS responses.")
res  <- read_csv(f_res, show_col_types = FALSE)
lpiv <- read_csv(f_lpiv, show_col_types = FALSE) |>
  select(design, instrument, measure, outcome, sample, h, ols_b, fs_F_lpiv = fs_F)

DESIGNS <- c(national = "Panel A. National timing: either APRA measure in force",
             exposure = "Panel B. Exposure: 2014 investor share $\\times$ interest-only cap")
HEADLINE <- c(national = "any", exposure = "io_cap")

hd <- res |>
  filter(sample == "full", measure == "main", status == "ok",
         (design == "national" & instrument == HEADLINE[["national"]]) |
         (design == "exposure" & instrument == HEADLINE[["exposure"]])) |>
  left_join(lpiv, by = c("design", "instrument", "measure", "outcome", "sample", "h")) |>
  mutate(design = factor(design, levels = names(DESIGNS)),
         outcome = factor(outcome, levels = c("price", "rent"))) |>
  arrange(design, outcome, h)
if (nrow(hd) != 12) stop("Expected 12 headline rows (2 designs x 2 outcomes x h = 4, 8, 12); found ", nrow(hd))

# ---- Valid ranges from the switching points ----------------------------------
# Domains (0, r) pass once r reaches the lower end; domains (r, D_MAX) fail once
# r passes the upper end. A family that passes over the whole scan puts the end
# at the domain's edge; one that fails over the whole scan puts it at the scan's
# edge on the failing side.
vr <- hd |>
  mutate(lo = case_when(above_status == "interior" ~ abs(tau_above),
                        above_status == "accepts throughout" ~ 0,
                        TRUE ~ SCAN[2]),
         hi = case_when(below_status == "interior" ~ abs(tau_below),
                        below_status == "accepts throughout" ~ D_MAX,
                        TRUE ~ SCAN[1]),
         none = !zero_mcub | lo >= hi)

# ---- Table --------------------------------------------------------------------
num3 <- function(v) sprintf("%.3f", v)
num2 <- function(v) ifelse(v < 0, sprintf("$-$%.2f", abs(v)), sprintf("%.2f", v))
ci   <- function(lo, hi) paste0("[", num2(lo), ", ", num2(hi), "]")
padm <- function(lab, p) case_when(lab == "<=0.01" ~ "$\\le$0.01",
                                   lab == ">0.50"  ~ "$\\ge$0.50",
                                   TRUE ~ sprintf("%.2f", p))
vrng <- function(lo, hi, none) ifelse(none, "none", paste0("[", num2(lo), ", ", num2(hi), "]"))

row_tex <- function(label, cells) paste0(label, " & ", paste(cells, collapse = " & "), " \\\\")

lines <- c(
  "\\begin{table}[t]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{Instrumented responses and correlation-restriction test of the APRA instruments}",
  "\\label{tab:cr_apra}",
  "\\begin{tabular}{llccc}",
  "\\toprule",
  " & & \\multicolumn{3}{c}{Horizon $h$ (quarters)} \\\\",
  "\\cmidrule(lr){3-5}",
  " & & 4 & 8 & 12 \\\\",
  "\\midrule")
for (d in names(DESIGNS)) {
  lines <- c(lines, paste0("\\multicolumn{5}{l}{\\emph{", DESIGNS[[d]], "}} \\\\"))
  for (o in c("price", "rent")) {
    s <- vr |> filter(design == d, outcome == o) |> arrange(h)
    lines <- c(lines,
      row_tex(paste0(ifelse(o == "price", "Prices", "Rents"), " & IV response"), num3(s$beta_iv)),
      row_tex(" & OLS response", num3(s$ols_b)),
      row_tex(" & First-stage $F$", sprintf("%.1f", s$fs_F_lpiv)),
      row_tex(" & MCUB interval", ci(s$mcub_lo, s$mcub_hi)),
      row_tex(" & $p^{\\mathrm{adm}}_0$", padm(s$p_adm_label, s$p_adm)))
    if (VALID_RANGE_IN_TABLE)
      lines <- c(lines, row_tex(" & Valid range", vrng(s$lo, s$hi, s$none)))
    if (!(d == "exposure" && o == "rent")) lines <- c(lines, "\\addlinespace")
  }
}
lines <- c(lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste("\\item Instrumented local projections (Equation~\\ref{eq:lpiv}) and the correlation-restriction",
        "test of \\citet{dzhumashev2026test}, main credit measure, full sample. IV response:",
        "change in the log price or rent level from $t-1$ to $t+h$ per unit of cumulative credit",
        "growth over the same quarters; OLS response: the same projection estimated by OLS;",
        "first-stage $F$: Driscoll--Kraay Wald statistic of the instrument. MCUB interval: 95 percent",
        "modified conditional union-bound interval \\citep{bei2024inference} for the instrument's",
        "correlation with the error, $\\rho_{zu}$, when credit's correlation with the error,",
        "$\\rho_{xu}$, lies in $[0, 0.8]$; validity is rejected when the interval excludes zero.",
        "$p^{\\mathrm{adm}}_0$: smallest level in $(0.01, 0.5)$ at which zero leaves the interval,",
        "censored at both ends. Outcome, credit and instrument are residualised on the fixed effects",
        "and controls of each projection, and the instrument is signed so that it raises credit. The",
        "covariance of the three sample correlations comes from a moving block bootstrap over blocks",
        "of eight quarters, with the five cities in each quarter kept together",
        "\\citep{kunsch1989jackknife}. Imbens--Manski/Stoye and naive endpoint intervals",
        "\\citep{imbens2004confidence, stoye2009more} agree with the MCUB intervals on zero inclusion",
        "in every cell."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}")
writeLines(lines, file.path(OUTDIR, "table_cr_apra.tex"))

# ---- Figure -------------------------------------------------------------------
# FIG_LAYOUT "grouped" (default): one panel per outcome, and at each horizon
# the two instruments' bars side by side, so the comparison the text makes
# (the two instruments, within prices and within rents) sits on one axis.
# FIG_LAYOUT "grid": rows are the instruments, columns the outcomes, one bar
# per horizon. In both, grey = the assumed domain and coloured = the valid
# range; the bars float, because a range need not start at zero. Ends inside
# the domain carry their value; an end too close to zero for an outside label
# carries it inside the bar, in white.
fig <- vr |>
  mutate(instr = factor(ifelse(design == "national", "National timing", "Exposure"),
                        levels = c("National timing", "Exposure")),
         panel_col = factor(ifelse(outcome == "price", "Prices", "Rents"), levels = c("Prices", "Rents")),
         row = match(h, c(12, 8, 4)))           # 4 quarters at the top
if (FIG_LAYOUT == "grouped") {
  fig  <- fig |> mutate(y = row + ifelse(design == "national", 0.19, -0.19))
  half <- 0.15                                  # half the bar height, in row units
} else {
  fig  <- fig |> mutate(y = row)
  half <- 0.17
}

lab_hi <- fig |> filter(!none, hi < D_MAX - 0.02) |>
  transmute(instr, panel_col, y, x = hi + 0.012, label = sprintf("%.2f", hi), hjust = 0)
lab_lo <- fig |> filter(!none, lo >= 0.06) |>
  transmute(instr, panel_col, y, x = lo - 0.012, label = sprintf("%.2f", lo), hjust = 1)
lab_lo_in <- fig |> filter(!none, lo > 0.002, lo < 0.06) |>
  transmute(instr, panel_col, y, x = lo + 0.012, label = sprintf("%.2f", lo), hjust = 0)
lab_none <- fig |> filter(none) |>
  transmute(instr, panel_col, y, x = 0.015, label = "fails for every value", hjust = 0)

p <- ggplot(fig) +
  geom_rect(aes(xmin = 0, xmax = D_MAX, ymin = y - half, ymax = y + half), fill = COL_DOMAIN) +
  geom_rect(data = filter(fig, !none),
            aes(xmin = lo, xmax = hi, ymin = y - half, ymax = y + half,
                fill = if (FIG_LAYOUT == "grouped") instr else panel_col)) +
  geom_text(data = bind_rows(lab_hi, lab_lo), aes(x = x, y = y, label = label, hjust = hjust),
            size = 2.9, colour = COL_INK) +
  geom_text(data = lab_lo_in, aes(x = x, y = y, label = label, hjust = hjust),
            size = 2.9, colour = "white") +
  geom_text(data = lab_none, aes(x = x, y = y, label = label, hjust = hjust),
            size = 2.9, colour = COL_INK, fontface = "italic") +
  scale_x_continuous(limits = c(0, D_MAX), breaks = seq(0, D_MAX, 0.2),
                     labels = c("0", "0.2", "0.4", "0.6", "0.8"),
                     expand = expansion(add = c(0.004, 0.004))) +
  scale_y_continuous(breaks = 1:3, labels = c("12 quarters", "8 quarters", "4 quarters"),
                     limits = c(0.5, 3.5), expand = expansion(0)) +
  labs(x = expression(paste("Assumed correlation between credit and the error of the projection, ", rho[xu])),
       y = NULL, fill = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        panel.spacing.x = unit(22, "pt"),
        panel.spacing.y = unit(12, "pt"),
        strip.placement = "outside",
        strip.text.x = element_text(colour = COL_INK, face = "bold", hjust = 0),
        strip.text.y.left = element_text(colour = COL_INK, face = "bold", angle = 90),
        legend.text = element_text(colour = COL_INK),
        axis.text = element_text(colour = "grey35"),
        axis.title.x = element_text(colour = "grey25", margin = margin(t = 6)))
if (FIG_LAYOUT == "grouped") {
  p <- p + facet_wrap(~ panel_col, nrow = 1) +
    scale_fill_manual(values = COL_INSTRUMENT, breaks = names(COL_INSTRUMENT)) +
    theme(legend.position = "top", legend.justification = "left",
          legend.margin = margin(0, 0, 0, 0), legend.box.spacing = unit(4, "pt"),
          legend.key.height = unit(8, "pt"), legend.key.width = unit(16, "pt"))
  fig_h <- 3.6
} else {
  p <- p + facet_grid(instr ~ panel_col, switch = "y") +
    scale_fill_manual(values = COL_OUTCOME, guide = "none")
  fig_h <- 3.6
}

ggsave(file.path(OUTDIR, "fig_cr_valid_range.png"), p, width = 8, height = fig_h, dpi = 300, bg = "white")
pdf_dev <- if (capabilities("cairo")) cairo_pdf else pdf
ggsave(file.path(OUTDIR, "fig_cr_valid_range.pdf"), p, width = 8, height = fig_h, device = pdf_dev)

message("Wrote table_cr_apra.tex, fig_cr_valid_range.png and fig_cr_valid_range.pdf to ", OUTDIR, "/")
print(as.data.frame(transmute(vr, design, outcome, h, valid_range = vrng(lo, hi, none))), row.names = FALSE)

end_run_log()
