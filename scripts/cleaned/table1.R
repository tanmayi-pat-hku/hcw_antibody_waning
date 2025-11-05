# Load libraries
library(librarian)
shelf(xtable)
shelf(dplyr)

# Create a structured data frame for the table
data <- data.frame(
  Vaccine_Regimen = c("1-1", "1-1", "4-4", "4-4", "1-1-1", "1-1-1", "4-4-4", "4-4-4", "After Dose 1", "After Dose 1", "After Dose 2", "After Dose 2", "After Dose 3", "After Dose 3"),
  Type = c("ELISA", "sVNT", "ELISA", "sVNT", "ELISA", "sVNT", "ELISA", "sVNT", "ELISA", "sVNT", "ELISA", "sVNT", "ELISA", "sVNT"),
  Participants = c(287, 14, 58, 5, 105, 10, 17, 1, 19, 92, 926, 856, 5, 4),
  stringsAsFactors = FALSE
)

# Create a LaTeX table string
table_latex <- "\\begin{table}[ht]\n\
\\centering\n\
\\caption{Participant Counts and Waning Analysis for ELISA and sVNT}\n\
\\begin{tabular}{@{}lll@{}}\n\
\\specialrule{0.8pt}{0pt}{0pt} % top line\n\
\\textbf{Vaccine Regimen} &  & \\textbf{Participants (n)} \\\\\n\
\\specialrule{1.2pt}{0pt}{0pt} % major separator\n\
\\specialrule{0.8pt}{0pt}{0pt}\n\
\\multicolumn{3}{l}{\\textbf{Two Dose}} \\\\\n\
\\specialrule{0.8pt}{0pt}{0pt}\n\
\\multirow{2}{*}{1-1} & ELISA & 287 \\\\\n\
 & sVNT & 14 \\\\\n\
\\specialrule{0.8pt}{0pt}{0pt}\n\
\\multirow{2}{*}{4-4} & ELISA & 58 \\\\\n\
 & sVNT & 5 \\\\\n\
\\specialrule{1.2pt}{0pt}{0pt}\n\
\\multicolumn{3}{l}{\\textbf{Three Dose}} \\\\\n\
\\specialrule{0.8pt}{0pt}{0pt}\n\
\\multirow{2}{*}{1-1-1} & ELISA & 105 \\\\\n\
 & sVNT & 10 \\\\\n\
\\specialrule{0.8pt}{0pt}{0pt}\n\
\\multirow{2}{*}{4-4-4} & ELISA & 17 \\\\\n\
 & sVNT & 1 \\\\\n\
\\specialrule{1.2pt}{0pt}{0pt}\n\
\\multicolumn{3}{l}{\\textbf{Waning Analysis}} \\\\\n\
\\specialrule{0.8pt}{0pt}{0pt}\n\
After Dose 1 & ELISA & 19 \\\\\n\
 & sVNT & 92 \\\\\n\
\\specialrule{0.8pt}{0pt}{0pt}\n\
After Dose 2 & ELISA & 926 \\\\\n\
 & sVNT & 856 \\\\\n\
\\specialrule{0.8pt}{0pt}{0pt}\n\
After Dose 3 & ELISA & 5 \\\\\n\
 & sVNT & 4 \\\\\n\
\\bottomrule\n\
\\end{tabular}\n\
\\label{tab:with_two_dose_label}\n\
\\end{table}\n"

# Write the LaTeX table to a .tex file
writeLines(table_latex, "table_output.tex")