# remotes::install_github("jhelvy/xaringanBuilder")
# remotes::install_github('rstudio/chromote')

library(xaringanBuilder)

build_pdf("04-Observational_Data.html", complex_slides = T, partial_slides = F)
