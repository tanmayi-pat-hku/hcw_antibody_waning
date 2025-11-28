#Write a Table
# Define the LaTeX table code as a string with escaped backslashes
table1 <- "
\\begin{table}[!htbp]
\\centering
\\caption{Effect of vaccine regimen on anti-spike IgG (ELISA) and neutralizing antibody (sVNT) levels.}
\\label{tab:boosting-stats}
\\small
\\begin{tabular}{@{}l l r r l c@{}}
\\toprule
Assay & Comparison                           & n (Group 1) & n (Group 2) & Test Statistic & Significance \\\\
\\midrule
\\multicolumn{6}{c}{\\textit{One-dose Regimen (Wilcoxon rank-sum)}} \\\\
ELISA & B vs S           & 579         & 126         & \\$W$ = 69,599     & \\$P$ \\$<$ 0.001 \\\\
sVNT   & B vs S               & 520         & 17         & \\$W$ = 8,161    & \\$P$ \\$<$ 0.001 \\\\
\\midrule
\\multicolumn{6}{c}{\\textit{Two-dose Regimen (Wilcoxon rank-sum)}} \\\\
ELISA  & B-B vs S-S               & 883         & 231         & \\$W$ = 183,444    & \\$P$ \\$<$ 0.001 \\\\
sVNT  & B-B vs S-S               & 885         & 227         & \\$W$ = 193,315    & \\$P$ \\$<$ 0.001 \\\\
\\midrule
\\multicolumn{6}{c}{\\textit{Three-dose Regimen (Kruskal--Wallis \\$\\chi^2 = 153.2\\$, \\$P$ \\$<$ 0.001)}} \\\\
ELISA & B-B-B vs S-S-S                       & 447         & 114         & \\$Z$ = 12.31      & \\$P$ \\$<$ 0.001 \\\\
      & B-B-B vs S-S-B                       & 447         & 97          & \\$Z$ = 1.10       & ns  \\\\
      & S-S-S vs S-S-B                       & 114         & 97          & \\$Z$ = 8.46       & \\$P$ \\$<$ 0.001 \\\\
\\midrule
\\multicolumn{6}{c}{\\textit{Three-dose Regimen (Kruskal--Wallis \\$\\chi^2 = 146.3\\$, \\$P$ \\$<$ 0.001)}} \\\\
sVNT  & B-B-B vs S-S-S                       & 707         & 137         & \\$Z$ = 11.90      & \\$P$ \\$<$ 0.001 \\\\
      & B-B-B vs S-S-B                       & 707         & 105         & \\$Z$ = 0.41       & ns  \\\\
      & B-B-B vs B-B-S                       & 707         & 17          & \\$Z$ = 2.35       & ns  \\\\
      & S-S-S vs S-S-B                       & 137         & 105         & \\$Z$ = 8.24       & \\$P$ \\$<$ 0.001 \\\\
\\midrule
\\multicolumn{6}{c}{\\textit{Four-dose Regimen (Kruskal--Wallis \\$\\chi^2 = 45.5\\$, \\$P$ \\$<$ 0.001)}} \\\\
sVNT  & B-B-B-B vs S-S-S-S                   & 43          & 34          & \\$Z$ = 5.56       & \\$P$ \\$<$ 0.001 \\\\
      & S-S-B-B vs S-S-S-S                   & 29          & 34          & \\$Z$ = 5.56       & \\$P$ \\$<$ 0.001 \\\\
      & S-S-S-B vs S-S-S-S                   & 8           & 34          & \\$Z$ = 4.28       & \\$P$ \\$<$ 0.001 \\\\
      & All other pairs                      & —           & —           & —              & ns  \\\\
\\bottomrule
\\end{tabular}
\\end{table}
"

# Specify the file path
file_path <- "table1.tex"

# Write the LaTeX code to the .tex file
writeLines(table1, file_path)

# Inform the user
cat("LaTeX table has been written to", file_path, "\n")

