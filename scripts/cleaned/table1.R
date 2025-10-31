#Creating Table 1 (Cohort Descriptions)
library(librarian)
shelf(table1)
shelf(dplyr)

## --------------------------------------------------------------
## Packages
## --------------------------------------------------------------
# install.packages(c("dplyr", "tidyr", "knitr", "kableExtra"))
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

## --------------------------------------------------------------
## 2. Data (exact numbers from your LaTeX table)
## --------------------------------------------------------------
tbl_data <- tribble(
  ~Regimen,      ~Assay, ~N,
  # Two Dose
  "1-1",        "ELISA", 287,
  "1-1",        "sVNT",   14,
  "4-4",        "ELISA",  58,
  "4-4",        "sVNT",    5,
  # Three Dose
  "1-1-1",      "ELISA", 105,
  "1-1-1",      "sVNT",   10,
  "4-4-4",      "ELISA",  17,
  "4-4-4",      "sVNT",    1,
  # Waning Analysis
  "After Dose 1","ELISA",  19,
  "After Dose 1","sVNT",   92,
  "After Dose 2","ELISA", 926,
  "After Dose 2","sVNT",  856,
  "After Dose 3","ELISA",   5,
  "After Dose 3","sVNT",    4
) %>%
  mutate(
    Section = case_when(
      Regimen %in% c("1-1","4-4")               ~ "Two Dose",
      Regimen %in% c("1-1-1","4-4-4")           ~ "Three Dose",
      TRUE                                      ~ "Waning Analysis"
    ),
    Order = case_when(
      Section == "Two Dose"      & Regimen == "1-1"   ~ 1,
      Section == "Two Dose"      & Regimen == "4-4"   ~ 2,
      Section == "Three Dose"    & Regimen == "1-1-1" ~ 3,
      Section == "Three Dose"    & Regimen == "4-4-4" ~ 4,
      Regimen == "After Dose 1"                       ~ 5,
      Regimen == "After Dose 2"                       ~ 6,
      Regimen == "After Dose 3"                       ~ 7
    )
  ) %>%
  arrange(Order, Assay) %>%
  select(-Order)

## --------------------------------------------------------------
## 3. Insert \multirow{2}{*} for the first row of each regimen
## --------------------------------------------------------------
add_multirow <- function(df) {
  df %>%
    mutate(
      Regimen = ifelse(
        Assay == "ELISA",
        # ---- NOTE: removed booktabs = TRUE (not a valid argument) ----
        cell_spec(Regimen, "latex", escape = FALSE,
                  extra = paste0("\\multirow{2}{*}{", Regimen, "}")),
        ""   # second row stays empty
      )
    )
}

tbl_ready <- tbl_data %>% add_multirow()

## --------------------------------------------------------------
## 4. Build the table with kableExtra
## --------------------------------------------------------------
tbl_latex <- kbl(
  tbl_ready %>% select(Regimen, Assay, N),
  format   = "latex",
  booktabs = TRUE,               # <-- correct place for booktabs
  align    = c("l", "l", "r"),
  escape   = FALSE,
  col.names = c("", "", "Participants (n)"),
  caption  = "Participant Counts and Waning Analysis for ELISA and sVNT"
) %>%
  kable_styling(latex_options = c("hold_position"),
                font_size = 10) %>%
  # Header row (bold)
  row_spec(0, bold = TRUE) %>%
  # Top rule (thin)
  row_spec(0, extra_latex_after = "\\toprule[0.8pt]") %>%
  # Thick separator after header
  row_spec(1, extra_latex_after = "\\specialrule{1.2pt}{0pt}{0pt}") %>%
  # Section headings (multicolumn) + thin rule before each
  pack_rows("Two Dose",      which(tbl_ready$Section == "Two Dose")[1],
            which(tbl_ready$Section == "Two Dose")[4],
            bold = TRUE, latex_gap_space = "0.5em") %>%
  pack_rows("Three Dose",    which(tbl_ready$Section == "Three Dose")[1],
            which(tbl_ready$Section == "Three Dose")[4],
            bold = TRUE, latex_gap_space = "0.5em") %>%
  pack_rows("Waning Analysis",which(tbl_ready$Section == "Waning Analysis")[1],
            which(tbl_ready$Section == "Waning Analysis")[6],
            bold = TRUE, latex_gap_space = "0.5em") %>%
  # Thin rule before each section (except the first)
  row_spec(c(0,
             which(tbl_ready$Assay == "ELISA" &
                     tbl_ready$Section != lag(tbl_ready$Section, default = "")) - 1),
           extra_latex_after = "\\specialrule{0.8pt}{0pt}{0pt}") %>%
  # Thin rule after every regimen pair
  row_spec(seq(2, nrow(tbl_ready), by = 2),
           extra_latex_after = "\\specialrule{0.8pt}{0pt}{0pt}") %>%
  # Thick separators between the three big blocks
  row_spec(c(4, 8), extra_latex_after = "\\specialrule{1.2pt}{0pt}{0pt}") %>%
  # Bottom rule
  row_spec(nrow(tbl_ready), extra_latex_after = "\\bottomrule") %>%
  # Row height (same as \renewcommand{\arraystretch}{1.5})
  kable_styling(latex_options = "repeat_header") %>%
  row_spec(0:nrow(tbl_ready), extra_latex_after = "\\arraystretch{1.5}")

## --------------------------------------------------------------
## 5. Print / save the LaTeX code
## --------------------------------------------------------------
cat(tbl_latex, file = "participant_table.tex")
cat(tbl_latex)   # prints to console






