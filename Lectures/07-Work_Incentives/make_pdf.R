# remotes::install_github("jhelvy/xaringanBuilder")
# remotes::install_github('rstudio/chromote')

library(xaringanBuilder)

build_pdf("07-Work_Incentives.html", complex_slides = T, partial_slides = F)
