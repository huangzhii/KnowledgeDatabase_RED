module load rstudio
mkdir ~/r-packages
R -e "install.packages(c('data.table', 'dplyr', 'tidyr', 'digest', 'DT', 'jsonlite', 'shinyWidgets'), lib='~/r-packages', repos='https://ftp.ussg.iu.edu/CRAN/')"
