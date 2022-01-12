# remotes::install_github("jhelvy/xaringanBuilder")
# remotes::install_github('rstudio/chromote')

library(xaringanBuilder)

build_pdf("04-All_About_Regression.html", complex_slides = T, partial_slides = F)
