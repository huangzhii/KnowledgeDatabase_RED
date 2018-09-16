perl mskcc-vcf2maf-decbf60/vcf2maf.pl --input-vcf ./shiny/data/vcffiles/R-HT77-p0--R-HT72.union.v5B.annotated.vcf --output-maf ./R-HT77-p0--R-HT72.union.v5B.annotated.maf

IMAF="R-HT77-p0--R-HT72.union.v5B.annotated.maf"
OMAF="R-HT77-p0--R-HT72.union.v5B.annotated.oncokb.txt"
python ./oncokb-annotator/MafAnnotator.py -i $IMAF -o $OMAF

