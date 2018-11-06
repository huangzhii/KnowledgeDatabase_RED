module load rstudio
mkdir ~/r-packages
R -e "install.packages(c('data.table', 'dplyr', 'tidyr', 'digest', 'DT', 'jsonlite', 'withr'), lib='~/r-packages', repos='https://ftp.ussg.iu.edu/CRAN/')"

#R -e "library(withr)"
#R -e "with_libpaths('~/r-packages', devtools::install_github('dreamRs/shinyWidgets'))"
