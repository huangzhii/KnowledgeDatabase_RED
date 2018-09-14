perl mskcc-vcf2maf-decbf60/vcf2maf.pl --input-vcf ./CureMatch_data/CM02/CNV.vcf --ref-fasta ~/.vep/homo_sapiens/86_GRCh37/Homo_sapiens.GRCh37.75.dna.primary_assembly.fa.gz --output-maf ./CureMatch_data/CM02/CNV.maf

perl mskcc-vcf2maf-decbf60/vcf2maf.pl --input-vcf ./CureMatch_data/CM02/EXP.vcf --ref-fasta ~/.vep/homo_sapiens/86_GRCh37/Homo_sapiens.GRCh37.75.dna.primary_assembly.fa.gz --output-maf ./CureMatch_data/CM02/EXP.maf

perl mskcc-vcf2maf-decbf60/vcf2maf.pl --input-vcf ./CureMatch_data/CM02/INDEL.vcf --ref-fasta ~/.vep/homo_sapiens/86_GRCh37/Homo_sapiens.GRCh37.75.dna.primary_assembly.fa.gz --output-maf ./CureMatch_data/CM02/INDEL.maf

perl mskcc-vcf2maf-decbf60/vcf2maf.pl --input-vcf ./CureMatch_data/CM02/SNP.vcf --ref-fasta ~/.vep/homo_sapiens/86_GRCh37/Homo_sapiens.GRCh37.75.dna.primary_assembly.fa.gz --output-maf ./CureMatch_data/CM02/SNP.maf

perl mskcc-vcf2maf-decbf60/vcf2maf.pl --input-vcf ./CureMatch_data/CM02/SV.vcf --ref-fasta ~/.vep/homo_sapiens/86_GRCh37/Homo_sapiens.GRCh37.75.dna.primary_assembly.fa.gz --output-maf ./CureMatch_data/CM02/SV.maf


IMAF="./CureMatch_data/CM02/SNP.maf"
OMAF="./CureMatch_data/CM02/SNP.oncokb.txt"
python ./oncokb-annotator/MafAnnotator.py -i $IMAF -o $OMAF


IMAF="./pediatric_data/R-HT72-p0--R-HT72.union.v5B.annotated.zhi.maf"
OMAF="./pediatric_data/R-HT72-p0--R-HT72.union.v5B.annotated.oncokb.txt"
python ./oncokb-annotator/MafAnnotator.py -i $IMAF -o $OMAF


IMAF="./test.zhi.maf"
OMAF="./test.oncokb.txt"
python ./oncokb-annotator/MafAnnotator.py -i $IMAF -o $OMAF


