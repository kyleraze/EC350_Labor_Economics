# remotes::install_github("jhelvy/xaringanBuilder")
# remotes::install_github('rstudio/chromote')

library(xaringanBuilder)

build_pdf("11-Minimum_Wage_Debate.html", complex_slides = T, partial_slides = F)
