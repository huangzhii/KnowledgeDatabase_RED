#ONCOKB
module load perl
module load rstudio
module load samtools
export VEP_PATH=/gpfs/home/z/h/zhihuan/Carbonate/Desktop/KnowledgeDatabase/vep
export VEP_DATA=/gpfs/home/z/h/zhihuan/Carbonate/Desktop/KnowledgeDatabase/.vep

#export PERL5LIB=/gpfs/home/z/h/zhihuan/Carbonate/Desktop/KnowledgeDatabase/vep:/opt/moab/lib/perl5:/opt/moab/lib/perl5

export PERL5LIB=$VEP_PATH:$PERL5LIB
export PATH=$VEP_PATH/htslib:$PATH

R -e "shiny::runApp(launch.browser=T)"
