library(XML)
foundationXMLparser <- function(file){
  xmllist <- xmlToList(file, addAttributes = TRUE, simplify = FALSE)
  xmllist <- structure(xmllist)
  xml.finalreport = xmllist[['ResultsPayload']][['FinalReport']]
  xml.variantreport = xmllist[['ResultsPayload']][['variant-report']]
  #--------------------
  #  Sample_list
  #--------------------
  sampleId = xml.finalreport[["Sample"]]$SampleId
  PMI.reportId = xml.finalreport[["PMI"]]$ReportId
  PMI.Gender = xml.finalreport[["PMI"]]$Gender
  PMI.DOB = xml.finalreport[["PMI"]]$DOB
  PertinentNegatives.Gene = xml.finalreport[["PertinentNegatives"]]$PertinentNegative$Gene
  Summaries.alterationCount = xml.finalreport[["Summaries"]][["alterationCount"]]
  Summaries.clinicalTrialCount = xml.finalreport[["Summaries"]][["clinicalTrialCount"]]
  Summaries.resistiveCount = xml.finalreport[["Summaries"]][["resistiveCount"]]
  Summaries.sensitizingCount = xml.finalreport[["Summaries"]][["sensitizingCount"]]
  VP = data.frame(t(data.frame(xml.finalreport[["VariantProperties"]])))
  VP$merge <- paste(VP$geneName, VP$isVUS, VP$variantName, sep="_")
  
  VariantProperties.geneName_isVUS_variantName = paste(VP$merge, collapse=";")
  
  Sample_list = data.frame(cbind(sampleId, PMI.reportId, PMI.Gender, PMI.DOB, PertinentNegatives.Gene,
                                 Summaries.alterationCount, Summaries.clinicalTrialCount,
                                 Summaries.resistiveCount, Summaries.sensitizingCount,
                                 VariantProperties.geneName_isVUS_variantName))
  
  #--------------------
  #  Gene_list
  #--------------------
  Gene_list = NULL
  for (i in 1:length(xml.finalreport[["Genes"]])){
    Genes.geneName = xml.finalreport[["Genes"]][i]$Gene$Name
    Genes.alteration.alterationName = xml.finalreport[["Genes"]][i]$Gene$Alterations$Alteration$Name
    G.A = xml.finalreport[["Genes"]][i]$Gene$Alterations$Alteration$AlterationProperties$AlterationProperty
    # print(Genes.geneName)
    if ("dnaFraction" %in% names(G.A)){
      Genes.alteration.dnaFraction = G.A[["dnaFraction"]]
    }
    else{
      Genes.alteration.dnaFraction = NA
    }
    Genes.alteration.isEquivocal = G.A[["isEquivocal"]]
    Genes.alteration.isSubclonal = G.A[["isSubclonal"]]
    Genes.alteration.name = G.A[["name"]]
    Genes.alteration.interpretation = G.A[["isSubclonal"]]
    Genes.alteration.clinicalTrialNote = G.A[["name"]]
    Gene_list = rbind(Gene_list, c(Genes.geneName, Genes.alteration.dnaFraction, Genes.alteration.isEquivocal,
                                   Genes.alteration.isSubclonal, Genes.alteration.name,
                                   Genes.alteration.interpretation, Genes.alteration.clinicalTrialNote))
  }
  Gene_list = data.frame(Gene_list)
  colnames(Gene_list) = c("Genes.geneName", "Genes.alteration.dnaFraction", "Genes.alteration.isEquivocal",
                          "Genes.alteration.isSubclonal", "Genes.alteration.name",
                          "Genes.alteration.interpretation", "Genes.alteration.clinicalTrialNote")
  #--------------------
  #  Trial_list
  #--------------------
  Trial_list = NULL
  for (i in 1:length(xml.finalreport[["Trials"]])){
    Trial = xml.finalreport[["Trials"]][i]$Trial
    Trial.Gene = Trial$Gene
    Trial.Title = Trial$Title
    Trial.StudyPhase = Trial$StudyPhase
    Trial.Target = Trial$Target
    Trial.Locations = Trial$Locations
    Trial.NCTID = Trial$NCTID
    Trial.Note = Trial$Note
    Trial.Include = Trial$Include
    Trial_list = rbind(Trial_list, c(Trial.Gene, Trial.Title, Trial.StudyPhase, Trial.Target,
                                     Trial.Locations, Trial.NCTID, Trial.Note, Trial.Include))
  }
  Trial_list = data.frame(Trial_list)
  colnames(Trial_list) = c("Trial.Gene", "Trial.Title", "Trial.StudyPhase", "Trial.Target",
                           "Trial.Locations", "Trial.NCTID", "Trial.Note", "Trial.Include")
  
  #--------------------
  #  Reference_list
  #--------------------
  References_list = NULL
  for (i in 1:length(xml.finalreport[["References"]])){
    PMID = xml.finalreport[["References"]][i]$Reference$ReferenceId
    FullCitation = xml.finalreport[["References"]][i]$Reference$FullCitation
    References_list = rbind(References_list, c(PMID, FullCitation))
  }
  References_list = data.frame(References_list)
  colnames(References_list) = c("PMID", "FullCitation")
  
  
  
  #--------------------
  #  Merging xml.finalreport
  #--------------------
  Gene.Trial.List = merge(Gene_list, Trial_list, by.x = "Genes.geneName", by.y = "Trial.Gene",
                          all=T, sort = F)
  Sample.Gene.Trial.List = merge(Sample_list, Gene.Trial.List)
  
  
  #=====================================
  #  Start working on xml.variantreport
  #=====================================
  
  
  variant.sample.bait.set = xml.variantreport[["samples"]][["sample"]][["bait-set"]]
  variant.sample.mean.exon.depth = xml.variantreport[["samples"]][["sample"]][["mean-exon-depth"]]
  variant.sample.name = xml.variantreport[["samples"]][["sample"]][["name"]]
  variant.sample.nucleic.acid.type = xml.variantreport[["samples"]][["sample"]][["nucleic-acid-type"]]
  variant.qualitycontrol = xml.variantreport[["quality-control"]][["status"]]
  
  variant.sample = data.frame(cbind(variant.sample.bait.set, variant.sample.mean.exon.depth,
                                    variant.sample.name, variant.sample.nucleic.acid.type,
                                    variant.qualitycontrol))
  
  #--------------------
  #  short.variant.list
  #--------------------
  short.variant.list = NULL
  for (i in 1:length(xml.variantreport[["short-variants"]])){
    short.variant.dna.evidence = xml.variantreport[["short-variants"]][i]$`short-variant`$`dna-evidence`[["sample"]]
    short.variant.attrs = data.frame(t(xml.variantreport[["short-variants"]][i]$`short-variant`$.attrs))
    short.variant.list = rbind(short.variant.list, cbind(short.variant.dna.evidence, short.variant.attrs))
  }
  
  #--------------------
  #  CNA.list
  #--------------------
  CNA.list = NULL
  for (i in 1:length(xml.variantreport[["copy-number-alterations"]])){
    CNA.dna.evidence = xml.variantreport[["copy-number-alterations"]][i]$`copy-number-alteration`$`dna-evidence`[["sample"]]
    CNA.attrs = data.frame(t(xml.variantreport[["copy-number-alterations"]][i]$`copy-number-alteration`$.attrs))
    CNA.list = rbind(CNA.list, cbind(CNA.dna.evidence, CNA.attrs))
  }
  
  #--------------------
  #  rearrangement.list
  #--------------------
  rearrangement.list = NULL
  for (i in 1:length(xml.variantreport[["rearrangements"]])){
    rearrangement.dna.evidence = xml.variantreport[["rearrangements"]][i]$rearrangement$`dna-evidence`[["sample"]]
    rearrangement.attrs = data.frame(t(xml.variantreport[["rearrangements"]][i]$rearrangement$.attrs))
    rearrangement.list = rbind(rearrangement.list, cbind(rearrangement.dna.evidence, rearrangement.attrs))
  }
  
  biomarkers.microsatellite.instability.status = xml.variantreport[["biomarkers"]]$`microsatellite-instability`[["status"]]
  biomarkers.TMB.score = xml.variantreport[["biomarkers"]]$`tumor-mutation-burden`[["score"]]
  biomarkers.TMB.status = xml.variantreport[["biomarkers"]]$`tumor-mutation-burden`[["status"]]
  biomarkers.TMB.unit = xml.variantreport[["biomarkers"]]$`tumor-mutation-burden`[["unit"]]
  
  
  biomarker_list = data.frame(cbind(biomarkers.microsatellite.instability.status,
                                    biomarkers.TMB.score, biomarkers.TMB.status,
                                    biomarkers.TMB.unit))
  variant.list1 = merge(variant.sample, short.variant.list, by.x = "variant.sample.name",
                        by.y = "short.variant.dna.evidence", all=T, sort = F)
  
  variant.list2 = merge(variant.list1, CNA.list, by.x = "variant.sample.name",
                        by.y = "CNA.dna.evidence", all=T, sort = F)
  
  variant.list3 = merge(variant.list2, rearrangement.list, by.x = "variant.sample.name",
                        by.y = "rearrangement.dna.evidence", all=T, sort = F)
  
  variant.list4 = merge(variant.list3, biomarker_list, all=T, sort = F)
  
  variant.list5 = merge(Sample_list, variant.list4, all=T, sort = F)
  
  
  return_object[[1]] = Sample.Gene.Trial.List
  return_object[[2]] = References_list
  return_object[[3]] = variant.list5
  
  return(return_object)
}