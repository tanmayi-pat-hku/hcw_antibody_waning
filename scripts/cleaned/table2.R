table2 <- "
\\begin{table}[!htbp]
\\centering
\\caption{Antibody Waning Rates After Second and Third Doses}
\\label{tab:waning-slopes}
\\small
\\begin{tabular}{@{}l l c c l c@{}}
\\toprule
     & Group & \\% Change/Day & Half-life (days) & 95\\% CI & Significant \\\\
\\midrule
\\multicolumn{6}{c}{\\textit{After Dose 2 (B-B vs S-S: \\$P\\$ \\$<\\$ 0.0001, ANOVA F-test)}} \\\\
 & B-B & -0.211 & 329 & (-0.001, -0.0008) & Yes \\\\
 & S-S & -0.429 & 161 & (-0.0023, -0.0015) & Yes \\\\
\\midrule
\\multicolumn{6}{c}{\\textit{After Dose 3 (B-B-B vs S-S-S: \\$P\\$ \\$<\\$ 0.0001, ANOVA F-test)}} \\\\
 & B-B-B & -0.017 & 4067 & (-0.0002, 0) & No \\\\
 & S-S-S & -0.201 & 344 & (-0.001, -0.0007) & Yes \\\\
\\bottomrule
\\end{tabular}
\\end{table}
"

# Specify the file path
file_path <- "table2.tex"

# Write the LaTeX code to the .tex file
writeLines(table2, file_path)

