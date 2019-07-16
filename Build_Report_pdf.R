# Set working directory
setwd("U:/models")

# Load packages
require(knitr)
require(markdown)

# Create .md, .html, and .pdf files
knit("DoW_Analysis.Rmd")
markdownToHTML('DoW_Analysis.md', 'DoW_Analysis.html', options=c("use_xhml"))
system("pandoc -s DoW_Analysis.html -o DoW_Analysis.pdf")