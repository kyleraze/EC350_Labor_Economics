# remotes::install_github("jhelvy/xaringanBuilder")
# remotes::install_github('rstudio/chromote')

library(xaringanBuilder)

build_pdf("09-Robots_Taxes.html", complex_slides = T, partial_slides = F)
