foundationXMLparser2 <- function(file){
  xmllist = xmlToList(file, addAttributes = TRUE, simplify = FALSE)
  xml.finalreport = xmllist[['ResultsPayload']][['FinalReport']]
  xml.variantreport = xmllist[['ResultsPayload']][['variant-report']]
  
  #--------------------
  #    PMI
  #--------------------
  PMI_list = NULL;
  PMI_list = data.frame(matrix(ncol=18, nrow=0))
  name_list = c("PMI.reportId", "SampleId", "PMI.MRN", "PMI.Gender", "PMI.FirstName", "PMI.LastName",
                "PMI.Gender", "PMI.DOB", "PMI.Diagnosis", "PMI.Physician", "PMI.PhysicianId",
                "PMI.PhysicianNPI", "PMI.Pathologist", "PMI.Facility", "PMI.FacilityId",
                "PMI.CollDate", "PMI.ReceivedDate")
  colnames(PMI_list) = name_list
  
  PMI.reportId = xml.finalreport[["PMI"]]$ReportId
  SampleId = xml.finalreport[["Sample"]]$SampleId
  
  PMI.MRN = xml.finalreport[["PMI"]]$MRN
  PMI.Gender = xml.finalreport[["PMI"]]$Gender
  PMI.FirstName = xml.finalreport[["PMI"]]$FirstName
  PMI.LastName = xml.finalreport[["PMI"]]$LastName
  PMI.Gender = xml.finalreport[["PMI"]]$Gender
  PMI.DOB = xml.finalreport[["PMI"]]$DOB
  PMI.Diagnosis = xml.finalreport[["PMI"]]$SubmittedDiagnosis
  
  PMI.Physician = xml.finalreport[["PMI"]]$OrderingMD
  PMI.PhysicianId = xml.finalreport[["PMI"]]$OrderingMDId
  PMI.PhysicianNPI = xmllist[["CustomerInformation"]]$NPI
  PMI.Pathologist = xml.finalreport[["PMI"]]$Pathologist
  PMI.Facility = xml.finalreport[["PMI"]]$MedFacilName
  PMI.FacilityId = xml.finalreport[["PMI"]]$MedFacilID
  PMI.SpecSite = xml.finalreport[["PMI"]]$SpecSite
  PMI.CollDate = xml.finalreport[["PMI"]]$CollDate
  PMI.ReceivedDate = xml.finalreport[["PMI"]]$ReceivedDate
  
  biomarkers.microsatellite.instability.status = xml.variantreport[["biomarkers"]]$`microsatellite-instability`[["status"]]
  biomarkers.TMB.score = xml.variantreport[["biomarkers"]]$`tumor-mutation-burden`[["score"]]
  biomarkers.TMB.status = xml.variantreport[["biomarkers"]]$`tumor-mutation-burden`[["status"]]
  biomarkers.TMB.unit = xml.variantreport[["biomarkers"]]$`tumor-mutation-burden`[["unit"]]
  
  if (is.null(PMI.reportId)) {PMI.reportId = NA}
  if (is.null(SampleId)) {SampleId = NA}
  if (is.null(PMI.MRN)) {PMI.MRN = NA}
  if (is.null(PMI.Gender)) {PMI.Gender = NA}
  if (is.null(PMI.FirstName)) {PMI.FirstName = NA}
  if (is.null(PMI.LastName)) {PMI.LastName = NA}
  if (is.null(PMI.Gender)) {PMI.Gender = NA}
  if (is.null(PMI.DOB)) {PMI.DOB = NA}
  if (is.null(PMI.Diagnosis)) {PMI.Diagnosis = NA}
  if (is.null(PMI.Physician)) {PMI.Physician = NA}
  if (is.null(PMI.PhysicianId)) {PMI.PhysicianId = NA}
  if (is.null(PMI.PhysicianNPI)) {PMI.PhysicianNPI = NA}
  if (is.null(PMI.Pathologist)) {PMI.Pathologist = NA}
  if (is.null(PMI.Facility)) {PMI.Facility = NA}
  if (is.null(PMI.FacilityId)) {PMI.FacilityId = NA}
  if (is.null(PMI.SpecSite)) {PMI.SpecSite = NA}
  if (is.null(PMI.CollDate)) {PMI.CollDate = NA}
  if (is.null(PMI.ReceivedDate)) {PMI.ReceivedDate = NA}
  if (is.null(PMI.Facility)) {PMI.Facility = NA}
  if (is.null(biomarkers.microsatellite.instability.status)) {biomarkers.microsatellite.instability.status = NA}
  if (is.null(biomarkers.TMB.score)) {biomarkers.TMB.score = NA}
  if (is.null(biomarkers.TMB.status)) {biomarkers.TMB.status = NA}
  if (is.null(biomarkers.TMB.unit)) {biomarkers.TMB.unit = NA}
  
  PMI_list = data.frame(cbind(PMI.reportId, SampleId, PMI.MRN, PMI.Gender, PMI.FirstName, PMI.LastName,
                              PMI.Gender, PMI.DOB, PMI.Diagnosis, PMI.Physician, PMI.PhysicianId,
                              PMI.PhysicianNPI, PMI.Pathologist, PMI.Facility, PMI.FacilityId,
                              PMI.CollDate, PMI.ReceivedDate, biomarkers.microsatellite.instability.status,
                              biomarkers.TMB.score, biomarkers.TMB.status, biomarkers.TMB.unit));
  
  #--------------------
  #    Sample_list
  #--------------------
  Sample_list = NULL;
  Sample_list = data.frame(matrix(ncol=8, nrow=0))
  name_list = c("SampleId", "PMI.reportId", "PertinentNegatives.Gene",
                "Summaries.alterationCount", "Summaries.clinicalTrialCount",
                "Summaries.resistiveCount", "Summaries.sensitizingCount",
                "VariantProperties.geneName_isVUS_variantName")
  colnames(Sample_list) = name_list
  
  PertinentNegatives.Gene = xml.finalreport[["PertinentNegatives"]]$PertinentNegative$Gene
  Summaries.alterationCount = xml.finalreport[["Summaries"]][["alterationCount"]]
  Summaries.clinicalTrialCount = xml.finalreport[["Summaries"]][["clinicalTrialCount"]]
  Summaries.resistiveCount = xml.finalreport[["Summaries"]][["resistiveCount"]]
  Summaries.sensitizingCount = xml.finalreport[["Summaries"]][["sensitizingCount"]]
  VP = data.frame(t(data.frame(xml.finalreport[["VariantProperties"]])))
  VP$merge = paste(VP$geneName, VP$isVUS, VP$variantName, sep="_")
  
  VariantProperties.geneName_isVUS_variantName = paste(VP$merge, collapse=";")
  
  if (is.null(PertinentNegatives.Gene)) {PertinentNegatives.Gene = NA}
  if (is.null(Summaries.alterationCount)) {Summaries.alterationCount = NA}
  if (is.null(Summaries.clinicalTrialCount)) {Summaries.clinicalTrialCount = NA}
  if (is.null(Summaries.resistiveCount)) {Summaries.resistiveCount = NA}
  if (is.null(Summaries.sensitizingCount)) {Summaries.sensitizingCount = NA}
  
  
  Sample_list = data.frame(cbind(SampleId, PMI.reportId, PertinentNegatives.Gene,
                                 Summaries.alterationCount, Summaries.clinicalTrialCount,
                                 Summaries.resistiveCount, Summaries.sensitizingCount,
                                 VariantProperties.geneName_isVUS_variantName))
  
  
  #--------------------
  #    Gene_list
  #--------------------
  Gene_list = NULL;
  Gene_list = data.frame(matrix(ncol=7, nrow=0))
  name_list = c("Genes.geneName", "Genes.alteration.name", "Genes.alteration.dnaFraction",
                "Genes.alteration.isEquivocal", "Genes.alteration.isSubclonal",
                "Genes.alteration.interpretation", "Genes.alteration.clinicalTrialNote")
  colnames(Gene_list) = name_list
  
  i = 1;
  while(i<=length(xml.finalreport[["Genes"]])){
    Genes.geneName = xml.finalreport[["Genes"]][i]$Gene$Name
    Genes.alteration.alterationName = xml.finalreport[["Genes"]][i]$Gene$Alterations$Alteration$Name
    G.A = xml.finalreport[["Genes"]][i]$Gene$Alterations$Alteration$AlterationProperties$AlterationProperty
    # print(Genes.geneName)
    if ("dnaFraction" %in% names(G.A)){
      Genes.alteration.dnaFraction = G.A[["dnaFraction"]]
    } else{
      Genes.alteration.dnaFraction = NA
    }
    Genes.alteration.isEquivocal = G.A[["isEquivocal"]]
    Genes.alteration.isSubclonal = G.A[["isSubclonal"]]
    Genes.alteration.name = G.A[["name"]]
    
    Genes.alteration.interpretation = xml.finalreport[["Genes"]][i]$Gene$Alterations$Alteration$Interpretation;
    Genes.alteration.ClinicalTrialNote = xml.finalreport[["Genes"]][i]$Gene$Alterations$Alteration$ClinicalTrialNote
    
    if (is.null(Genes.geneName)) { Genes.geneName = NA }
    if (is.null(Genes.alteration.name)) { Genes.alteration.name = NA}
    if (is.null(Genes.alteration.isEquivocal)) { Genes.alteration.isEquivocal = NA}
    if (is.null(Genes.alteration.isSubclonal)) { Genes.alteration.isSubclonal = NA}
    if (is.null(Genes.alteration.name)) { Genes.alteration.name = NA}
    if (is.null(Genes.alteration.interpretation)) { Genes.alteration.interpretation = NA}
    if (is.null(Genes.alteration.ClinicalTrialNote)) { Genes.alteration.ClinicalTrialNote = NA}
    
    x = NULL
    x = cbind(Genes.geneName, Genes.alteration.name, Genes.alteration.dnaFraction,
              Genes.alteration.isEquivocal, Genes.alteration.isSubclonal,
              Genes.alteration.interpretation, Genes.alteration.ClinicalTrialNote)
    Gene_list =rbind(Gene_list, x)
    if (length(xml.finalreport[["Genes"]])==0) {break}
    i = i + 1;
  }
  
  #--------------------
  #    Trial_list
  #--------------------
  Trial_list = NULL
  Trial_list = data.frame(matrix(ncol=8, nrow=0))
  name_list = c("Trial.Gene", "Trial.Title", "Trial.StudyPhase", "Trial.Target",
                "Trial.Locations", "Trial.NCTID", "Trial.Note", "Trial.Include")
  colnames(Trial_list) = name_list
  
  i = 1;
  while(i<=length(xml.finalreport[["Trials"]])){
    
    Trial = xml.finalreport[["Trials"]][i]$Trial
    
    Trial.Gene = Trial$Gene
    Trial.Title = Trial$Title
    Trial.StudyPhase = Trial$StudyPhase
    Trial.Target = Trial$Target
    Trial.Locations = Trial$Locations
    Trial.NCTID = Trial$NCTID
    Trial.Note = Trial$Note
    Trial.Include = Trial$Include
    
    if (is.null(Trial.Gene)) {Trial.Gene = NA}
    if (is.null(Trial.Title)) {Trial.Title = NA}
    if (is.null(Trial.StudyPhase)) {Trial.StudyPhase = NA}
    if (is.null(Trial.Target)) {Trial.Target = NA}
    if (is.null(Trial.Locations)) {Trial.Locations = NA}
    if (is.null(Trial.NCTID)) {Trial.NCTID = NA}
    if (is.null(Trial.Note)) {Trial.Note = NA}
    if (is.null(Trial.Include)) {Trial.Include = NA}
    
    x = NULL
    x = cbind(Trial.Gene, Trial.Title, Trial.StudyPhase, Trial.Target,
              Trial.Locations, Trial.NCTID, Trial.Note, Trial.Include)
    
    Trial_list = rbind(Trial_list, x)
    if (length(xml.finalreport[["Trials"]])==0) {break}
    i = i + 1
  }
  
  
  #--------------------
  #    Reference_list
  #--------------------
  References_list = NULL
  References_list = data.frame(matrix(ncol=2, nrow=0))
  name_list = c("PMID", "FullCitation")
  colnames(References_list) = name_list
  
  i = 1;
  while(i<=length(xml.finalreport[["References"]])){
    PMID = xml.finalreport[["References"]][i]$Reference$ReferenceId
    FullCitation = xml.finalreport[["References"]][i]$Reference$FullCitation
    
    if (is.null(PMID)) {PMID = NA}
    if (is.null(FullCitation)) {FullCitation = NA}
    
    x = NULL
    x = cbind(PMID, FullCitation)
    References_list = rbind(References_list, x)
    if (length(xml.finalreport[["References"]])==0) {break}
    i = i + 1
  }
  
  
  #=====================================
  #    Start working on xml.variantreport
  #    there could be DNA and RNA samples; 
  #=====================================
  variant.sample = NULL
  variant.sample = data.frame(matrix(ncol=5, nrow=0))
  name_list = c("bait-set", "mean-exon-depth", "name", "nucleic-acid-type", "qualitycontrol")
  colnames(variant.sample) = name_list
  
  
  i = 1;
  while(i<=length(xml.variantreport[["samples"]])){
    if (xml.variantreport[["samples"]][[i]][["nucleic-acid-type"]]=="DNA") {i_DNA = i}
    i = i + 1
  }
  
  variant.sample.bait.set = xml.variantreport[["samples"]][[i_DNA]][["bait-set"]]
  
  variant.sample.mean.exon.depth = xml.variantreport[["samples"]][[i_DNA]][["mean-exon-depth"]]
  variant.sample.name = xml.variantreport[["samples"]][[i_DNA]][["name"]]
  variant.sample.nucleic.acid.type = xml.variantreport[["samples"]][[i_DNA]][["nucleic-acid-type"]]
  variant.qualitycontrol = xml.variantreport[["quality-control"]][["status"]]
  
  if (is.null(variant.sample.bait.set)) {variant.sample.bait.set =NA}
  if (is.null(variant.sample.mean.exon.depth)) {variant.sample.mean.exon.depth = NA}
  if (is.null(variant.sample.name)) {variant.sample.name = NA}
  if (is.null(variant.sample.nucleic.acid.type)) {variant.sample.nucleic.acid.type = NA}
  if (is.null(variant.qualitycontrol)) {variant.qualitycontrol = NA}
  
  x = NULL
  x = cbind(variant.sample.bait.set, variant.sample.mean.exon.depth,
            variant.sample.name, variant.sample.nucleic.acid.type,
            variant.qualitycontrol)
  variant.sample = cbind(PMI.reportId, SampleId, rbind(variant.sample, x))
  
  
  
  #--------------------
  #    short.variant.list
  #--------------------
  
  short.variant.list = NULL
  
  short.variant.list = data.frame(matrix(ncol=12, nrow=0))
  name_list = c("short.variant.dna.evidence",    "allele.fraction", "cds.effect", "depth", "functional.effect",
                "gene", "percent.reads", "position", "protein.effect", "status", "strand", "transcript")
  colnames(short.variant.list) = name_list
  
  i = 1;
  while(i<=length(xml.variantreport[["short-variants"]])){
    short.variant.dna.evidence = xml.variantreport[["short-variants"]][i]$`short-variant`$`dna-evidence`[["sample"]]
    
    if (is.null(xml.variantreport[["short-variants"]][i]$`short-variant`)) {
      short.variant.attrs = list(allele.fraction=NA, cds.effect=NA, depth=NA, functional.effect=NA,
                                 gene=NA, percent.reads=NA, position=NA, protein.effect=NA, status=NA, 
                                 strand=NA, transcript=NA)
    } else {
      short.variant.attrs = data.frame(t(xml.variantreport[["short-variants"]][i]$`short-variant`$.attrs))
    }
    
    if (is.null(short.variant.dna.evidence)) {short.variant.dna.evidence = NA}
    if (is.null(short.variant.attrs$allele.fraction)) {short.variant.attrs$allele.fraction = NA}
    if (is.null(short.variant.attrs$cds.effect)) {short.variant.attrs$cds.effect = NA}
    if (is.null(short.variant.attrs$depth)) {short.variant.attrs$depth = NA}
    if (is.null(short.variant.attrs$functional.effect)) {short.variant.attrs$functional.effect =NA}
    if (is.null(short.variant.attrs$gene)) {short.variant.attrs$gene = NA}
    if (is.null(short.variant.attrs$percent.reads)) {short.variant.attrs$percent.reads = NA}
    if (is.null(short.variant.attrs$position)) {short.variant.attrs$position = NA}
    if (is.null(short.variant.attrs$protein.effect)) {short.variant.attrs$protein.effect = NA}
    if (is.null(short.variant.attrs$status)) {short.variant.attrs$status = NA}
    if (is.null(short.variant.attrs$strand)) {short.variant.attrs$strand = NA}
    if (is.null(short.variant.attrs$transcript)) {short.variant.attrs$transcript = NA}
    
    x = NULL
    x = cbind(short.variant.dna.evidence, data.frame(short.variant.attrs))
    
    short.variant.list = rbind(short.variant.list, x)
    if (length(xml.variantreport[["short-variants"]])==0) {break}
    i = i + 1
  }
  
  
  #--------------------
  #    CNA.list
  #--------------------
  CNA.list = NULL
  
  CNA.list = data.frame(matrix(ncol=9, nrow=0))
  name_list = c("CNA.dna.evidence", "copy.number", "equivocal", "gene", "number.of.exons", "position", "ratio", "status", "type")
  colnames(CNA.list) = name_list
  
  i = 1;
  while(i<=length(xml.variantreport[["copy-number-alterations"]])){
    CNA.dna.evidence = xml.variantreport[["copy-number-alterations"]][i]$`copy-number-alteration`$`dna-evidence`[["sample"]]
    
    if (is.null(xml.variantreport[["copy-number-alterations"]][i]$'copy-number-alteration')) {
      CNA.attrs = list(copy.number=NA, equivocal=NA, gene=NA, number.of.exons=NA,
                       position=NA, ratio=NA, status=NA, type=NA)
    } else {
      CNA.attrs = data.frame(t(xml.variantreport[["copy-number-alterations"]][i]$`copy-number-alteration`$.attrs))
    }
    
    if (is.null(CNA.dna.evidence)) {CNA.dna.evidence = NA}
    if (is.null(CNA.attrs$copy.number)) {CNA.attrs$copy.number = NA}
    if (is.null(CNA.attrs$equivocal)) {CNA.attrs$equivocal = NA}
    if (is.null(CNA.attrs$gene)) {CNA.attrs$gene = NA}
    if (is.null(CNA.attrs$number.of.exons)) {CNA.attrs$number.of.exons = NA}
    if (is.null(CNA.attrs$position)) {CNA.attrs$position = NA}
    if (is.null(CNA.attrs$ratio)) {CNA.attrs$ratio = NA}
    if (is.null(CNA.attrs$status)) {CNA.attrs$status = NA}
    if (is.null(CNA.attrs$type)) {CNA.attrs$type = NA}
    
    x = NULL
    x = cbind(CNA.dna.evidence, data.frame(CNA.attrs))
    
    CNA.list = rbind(CNA.list, x)
    if (length(xml.variantreport[["copy-number-alterations"]])==0) {break}
    i = i + 1
  }
  
  
  #--------------------
  #    rearrangement.list
  #--------------------
  rearrangement.list = NULL
  
  rearrangement.list = data.frame(matrix(ncol=10, nrow=0))
  name_list = c("rearrangement.dna.evidence", "description", "in.frame", "other.gene", "pos1", "pos2", "status", "supporting.read.pairs", "targeted.gene", "type")
  colnames(rearrangement.list) = name_list
  
  i = 1;
  while(i<=length(xml.variantreport[["rearrangements"]])){
    rearrangement.dna.evidence = xml.variantreport[["rearrangements"]][i]$rearrangement$`dna-evidence`[["sample"]]
    
    if (is.null(xml.variantreport[["copy-number-alterations"]][i]$'copy-number-alteration')) {
      rearrangement.attrs = list(description=NA, in.frame=NA, other.gene=NA, pos1=NA, pos2=NA, status=NA, supporting.read.pairs=NA, targeted.gene=NA, type=NA)
    } else {
      rearrangement.attrs = data.frame(t(xml.variantreport[["rearrangements"]][i]$rearrangement$.attrs))
    }
    
    
    if (is.null(rearrangement.dna.evidence)) {rearrangement.dna.evidence = NA}
    if (is.null(rearrangement.attrs$description)) {rearrangement.attrs$description = NA}
    if (is.null(rearrangement.attrs$in.frame)) {rearrangement.attrs$in.frame = NA}
    if (is.null(rearrangement.attrs$other.gene)) {rearrangement.attrs$other.gene = NA}
    if (is.null(rearrangement.attrs$pos1)) {rearrangement.attrs$pos1 = NA}
    if (is.null(rearrangement.attrs$pos2)) {rearrangement.attrs$pos2 = NA}
    if (is.null(rearrangement.attrs$status)) {rearrangement.attrs$status = NA}
    if (is.null(rearrangement.attrs$supporting.read.pairs)) {rearrangement.attrs$supporting.read.pairs = NA}
    if (is.null(rearrangement.attrs$targeted.gene)) {rearrangement.attrs$targeted.gene = NA}
    if (is.null(rearrangement.attrs$type)) {rearrangement.attrs$type = NA}
    
    x = NULL
    x = cbind(rearrangement.dna.evidence, data.frame(rearrangement.attrs))
    
    rearrangement.list = rbind(rearrangement.list, x)
    if (length(xml.variantreport[["rearrangements"]])==0) {break}
    i = i + 1
  }
  
  #----------------------------------------
  #    4 tables: clinical/variants/CNA/SV
  #----------------------------------------
  variants = NULL
  variants = cbind.fill(variant.sample, short.variant.list)
  
  CNA = NULL    
  
  CNA = data.frame(matrix(ncol=14, nrow=0))
  CNA = cbind.fill(variant.sample, CNA.list)
  
  SV = NULL
  SV = cbind.fill(variant.sample, rearrangement.list)
  
  out = NULL;
  out = list(PMI_list, variants, CNA, SV, References_list, Gene_list);
}