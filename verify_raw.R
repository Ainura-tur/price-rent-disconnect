# =============================================================================
# verify_raw.R  --  pre-run check that raw/ holds the expected input files
#
# Read-only. Renames NOTHING, reads NOTHING into the pipeline. It simply checks
# that every file 01_build_panel.R expects is present in raw/ under its canonical
# name, and flags:
#   - MISSING : an expected input is absent (pipeline will fail)
#   - PUNCT?  : an ABS file is absent but a punctuation-variant IS present (you
#               likely just need to rename it; see raw/README.md normalisation)
#   - STRAY   : an ABS-looking file in raw/ that is NOT an expected input
#               (e.g. the superseded all-groups OMAD export, or a wrong export)
#
# Usage (from project root):
#   Rscript verify_raw.R
# Exit status 0 if all expected inputs are present, 1 otherwise — so it can gate
# a run:  Rscript verify_raw.R && Rscript run_all.R
# =============================================================================

RAW <- "raw"

# --- The canonical input set the pipeline reads (13 files) -------------------
# Mirrors raw/README.md. Update here AND there if the inputs change.
expected <- c(
  # numbered ABS xlsx workbooks
  "641601.xlsx", "310102.xlsx", "87520039.xlsx",
  "6401010.xlsx", "634502b.xlsx", "62020010.xlsx",
  # ABS Data Explorer CSV exports (canonical underscore names)
  "ABS_RES_DWELL_ST_1_0_0__1_2_3_4_5_Q.csv",
  "ABS_LEND_HOUSING_1_1_______10_20_1_2_3_4_5_6_7_8_AUS_Q.csv",
  "ABS_OMAD_VISA___11_12_15_22_23_24_25_1009_1010_1020_1030_1040_1041_2203_2208_01_02_03_1_2_3_4_5_AUS_Q.csv",
  "ABS_ANA_SFD_1_0_0_C_FCE_RDSH__20_2_3_4_5_6_1_Q.csv",
  "ABS_ANA_SFD_1_0_0_C_GFC_PSS_GSS_20_2_3_4_5_6_1_Q.csv",
  # non-ABS
  "bis_dp_search_export_20260615-045639.csv",
  "d02hist.xlsx",
  "panel_main.csv"
)

# Known strays that may sit in raw/ but are NOT inputs (see raw/README.md).
# Listed so verify can name them explicitly rather than just flagging "STRAY".
known_strays <- c(
  "560114.xlsx",
  "f01dhist.xls",
  "f01dhist.xlsx"
)

# Punctuation-insensitive key: lowercases and maps every non-alphanumeric run to
# a single "_", so an ABS download name and its canonical form share a key. Used
# only to suggest "you probably need to rename this", never to rename.
punct_key <- function(x) {
  x <- sub("\\.[^.]*$", "", x)            # drop extension
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  sub("_+$", "", x)
}

if (!dir.exists(RAW)) {
  message("FAIL: no '", RAW, "/' directory. Create it and add the input files.")
  quit(save = "no", status = 1)
}

present <- list.files(RAW)
present_keys <- setNames(punct_key(present), present)

missing <- character(0)
punct   <- list()   # expected -> the punctuation-variant present
for (e in expected) {
  if (e %in% present) next
  k <- punct_key(e)
  hit <- names(present_keys)[present_keys == k & names(present_keys) != e]
  if (length(hit)) punct[[e]] <- hit[1] else missing <- c(missing, e)
}

# ABS-looking files present that are neither an expected input nor a known stray.
abs_present <- present[grepl("^ABS|^abs", present)]
matched_keys <- punct_key(expected)
stray_abs <- abs_present[!(abs_present %in% expected) &
                         !(punct_key(abs_present) %in% matched_keys)]

# --- Report ------------------------------------------------------------------
cat("verify_raw.R — checking", RAW, "/\n")
cat(strrep("-", 60), "\n")

ok_files <- intersect(expected, present)
cat(sprintf("Present and canonical : %d / %d expected inputs\n",
            length(ok_files), length(expected)))

if (length(punct)) {
  cat("\nRENAME NEEDED (punctuation variant found — see raw/README.md):\n")
  for (e in names(punct))
    cat(sprintf("  have: %s\n  ->    %s\n", punct[[e]], e))
}

if (length(missing)) {
  cat("\nMISSING (no file, no punctuation variant):\n")
  for (m in missing) cat("  ", m, "\n")
}

stray_known <- intersect(present, known_strays)
if (length(stray_known)) {
  cat("\nKNOWN NON-INPUTS present (safe to leave; NOT committed — raw/README.md):\n")
  for (s in stray_known) cat("  ", s, "\n")
}

stray_other <- setdiff(stray_abs, known_strays)
if (length(stray_other)) {
  cat("\nUNEXPECTED ABS files (verify these are not a wrong export):\n")
  for (s in stray_other) cat("  ", s, "\n")
}

cat(strrep("-", 60), "\n")
problems <- length(missing) + length(punct)
if (problems == 0) {
  cat("OK: all", length(expected), "expected inputs are present and canonical.\n")
  quit(save = "no", status = 0)
} else {
  cat(sprintf("ACTION NEEDED: %d missing, %d to rename. Fix before running.\n",
              length(missing), length(punct)))
  quit(save = "no", status = 1)
}
