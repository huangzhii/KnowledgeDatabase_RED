# Zhi Huang 08/23/2018
library(data.table)
library(dplyr)
library(tidyr)
library(digest)
library(DT)
library(jsonlite)
<<<<<<< HEAD
library(XML)
library(plyr)
library(rowr)
# library(shinyWidgets)
=======
#library(shinyWidgets)
>>>>>>> 181274d618d6caa134a9fbb0ec4a7fa5c742bc6c
source("utils.R")
source("foundationXMLparser2.R")
# output <- foundationXMLparser('~/Desktop/shared_with_Jeremy/QRF024108.xml')
# View(output[[3]])


options(shiny.maxRequestSize=300*1024^2) # to the top of server.R would increase the limit to 300MB
options(shiny.sanitize.errors = FALSE)
options(stringsAsFactors = FALSE)

function(input, output, session) {
  #-------------------
  #  Foundation Data
  #-------------------
  observeEvent(input$action1,{
    smartModal(error=F, title = "Converting Uploaded Data", content = "Converting uploaded data, please wait for a little while...")
    input.xml = input$xml_file$datapath
    foundationRes <<- foundationXMLparser2(input.xml)
    foundationRes[[5]]$PMID = sapply(foundationRes[[5]]$PMID, function(x) paste(createPUBMEDLink(x), sep="", collapse=""))
    output$resultsUI <- renderUI({
      tagList(
        h4("Foundation Database Output", style="color: STEELBLUE; font-size: 22px"),
        downloadButton("download_foundation_all", "Download all tables"),
        tabsetPanel(
          tabPanel("PMI", DT::dataTableOutput("foundation_output_1")),
          tabPanel("Variants", DT::dataTableOutput("foundation_output_2")),
          tabPanel("CNA", DT::dataTableOutput("foundation_output_3")),
          tabPanel("SV", DT::dataTableOutput("foundation_output_4")),
          tabPanel("References", DT::dataTableOutput("foundation_output_5")),
          tabPanel("Gene list", DT::dataTableOutput("foundation_output_6"))
        )
      )
    })
    output$foundation_output_1 <- DT::renderDataTable({foundationRes[[1]]}, selection="none", escape = F,
                                                  options=list(searching=T,pageLength = 100, ordering=T, autoWidth = TRUE, columnDefs = list(list(width = '100px', targets = "_all" ))))
    
    output$foundation_output_2 <- DT::renderDataTable({foundationRes[[2]]}, selection="none", escape = F,
                                                  options=list(searching=T,pageLength = 100, ordering=T, autoWidth = TRUE, columnDefs = list(list(width = '100px', targets = "_all" ))))
    
    output$foundation_output_3 <- DT::renderDataTable({foundationRes[[3]]}, selection="none", escape = F,
                                                  options=list(searching=T,pageLength = 100, ordering=T, autoWidth = TRUE, columnDefs = list(list(width = '100px', targets = "_all" ))))
    
    output$foundation_output_4 <- DT::renderDataTable({foundationRes[[4]]}, selection="none", escape = F,
                                                  options=list(searching=T,pageLength = 100, ordering=T, autoWidth = TRUE, columnDefs = list(list(width = '100px', targets = "_all" ))))
    
    output$foundation_output_5 <- DT::renderDataTable({foundationRes[[5]]}, selection="none", escape = F,
                                                  options=list(searching=T,pageLength = 100, ordering=T, autoWidth = TRUE, columnDefs = list(list(width = '100px', targets = "_all" ))))
    
    output$foundation_output_6 <- DT::renderDataTable({foundationRes[[6]]}, selection="none", escape = F,
                                                  options=list(searching=T,pageLength = 100, ordering=T, autoWidth = TRUE, columnDefs = list(list(width = '100px', targets = "_all" ))))
    
    
    
    removeModal()
    session$sendCustomMessage("buttonCallbackHandler", "tab2")
      
      
  })
  
  #-------------------
  #   Nantomic Data
  #-------------------
  observeEvent(input$action2,{
    smartModal(error=F, title = "Converting Uploaded Data", content = "Converting uploaded data, please wait for a little while...")
    
    output$resultsUI <- renderUI({
      tagList(
        h4("NantOmics Database Output", style="color: STEELBLUE; font-size: 22px"),
        downloadButton("download_MAF", "Download MAF data"),
        downloadButton("download_oncokb_selected", "Download oncokb.txt (Selected)"),
        downloadButton("download_oncokb_all", "Download oncokb.txt (All)"),
        DT::dataTableOutput("database_output")
      )
    })
    vep2maf.pl = "/gpfs/home/z/h/zhihuan/Carbonate/Desktop/KnowledgeDatabase/mskcc-vcf2maf-decbf60/vcf2maf.pl"
    input.vcf = input$vcf_file$datapath
    vcf <<- read.table(input.vcf)
    ref.fasta = "/gpfs/home/z/h/zhihuan/Carbonate/Desktop/KnowledgeDatabase/.vep/homo_sapiens/86_GRCh37/Homo_sapiens.GRCh37.75.dna.primary_assembly.fa.gz"
    md5 = digest(Sys.time(), "md5")
    if (!file.exists('tempstorage')){
        dir.create(file.path('tempstorage'))
    }
    output.maf <<- sprintf("tempstorage/output_%s.maf", md5)
    # oncoKB annotator
    OMAF <<- sprintf("tempstorage/output_%s.oncokb.txt", md5)
    python = "python"
    MafAnnotator = "/gpfs/home/z/h/zhihuan/Carbonate/Desktop/KnowledgeDatabase/oncokb-annotator/MafAnnotator.py"
    log1 = system(sprintf("export VEP_PATH=$HOME/vep &&
                          export VEP_DATA=$HOME/.vep &&
                          export PERL5LIB=$VEP_PATH:$PERL5LIB &&
                          export PATH=$VEP_PATH/htslib:$PATH &&
                          export PATH=/usr/include:$PATH &&
                          export PATH=/usr/local:$PATH &&
                          export LD_LIBRARY_PATH=/usr/include:$LD_LIBRARY_PATH &&
                          export LD_LIBRARY_PATH=/usr/local:$LD_LIBRARY_PATH &&
                          export LD_LIBRARY_PATH=\"/usr/include/:/usr/local/include/:$LD_LIBRARY_PATH\" &&
                          export PERL_BASE=\"$HOME/perl\" &&
                          export PERL5LIB=\"$PERL_BASE/perl-5.22.2/lib/perl5:$PERL_BASE/perl-5.22.2/lib/perl5/x86_64-linux:$PERL5LIB\" &&
                          export PATH=\"$PERL_BASE/perl-5.22.2/bin:$PATH\" &&
                          #export PATH=$HOME/vep/samtools/bin:$PATH &&
                          module load samtools &&
                          perl %s --input-vcf %s --ref-fasta %s --output-maf %s",
                          vep2maf.pl, input.vcf, ref.fasta, output.maf),
                  intern = T)
    
    # maf = read.table(output.maf, sep = "\t", header = T)
    # maf = select(maf, -cDNA_position)
    # maf = select(maf, -CDS_position)
    # write.table(maf, file=output.maf, sep = "\t")
    
    log2 = system(sprintf("export VEP_PATH=$HOME/vep &&
                          export VEP_DATA=$HOME/.vep &&
                          export PERL5LIB=$VEP_PATH:$PERL5LIB &&
                          export PATH=$VEP_PATH/htslib:$PATH &&
                          export PATH=/usr/include:$PATH &&
                          export PATH=/usr/local:$PATH &&
                          export LD_LIBRARY_PATH=/usr/include:$LD_LIBRARY_PATH &&
                          export LD_LIBRARY_PATH=/usr/local:$LD_LIBRARY_PATH &&
                          export LD_LIBRARY_PATH=\"/usr/include/:/usr/local/include/:$LD_LIBRARY_PATH\" &&
                          export PERL_BASE=\"$HOME/perl\" &&
                          export PERL5LIB=\"$PERL_BASE/perl-5.22.2/lib/perl5:$PERL_BASE/perl-5.22.2/lib/perl5/x86_64-linux:$PERL5LIB\" &&
                          export PATH=\"$PERL_BASE/perl-5.22.2/bin:$PATH\" &&
                          #export PATH=$HOME/vep/samtools/bin:$PATH &&
                          module load samtools &&
                          %s %s -i %s -o %s",
                          python, MafAnnotator, output.maf, OMAF),
                  intern = F)
    
    output$logs <- renderText({
      paste(log1, "\n", sep="")
      paste(log2, "\n", sep="")
    })
    oncokb.txt <<- read.delim(OMAF, header = T)
    # add drugs
    drugs = select(oncokb.txt, matches("LEVEL_."))
    drugs[is.na(drugs)] <- ''
    drugs2 = unite(drugs, sep=',')
    for (i in 1:dim(drugs)[2]){
      drugs2 = matrix(apply(drugs2, 1, function(x) gsub(',$','',x)))
      drugs2 = matrix(apply(drugs2, 1, function(x) gsub('^,','',x)))
    }
    oncokb.txt$drugs = drugs2
    if (dim(oncokb.txt)[1] == 0){
      removeModal()
      print("Error occured")
      sendSweetAlert(session, title = "Error occured", "MAF file return 0 rows of information.", type = "error",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    } else{
      oncokb.txt$t_rate = oncokb.txt$t_alt_count/oncokb.txt$t_depth
      
      if (sum(!is.na(oncokb.txt$t_rate)) == 0){ # based on Mark data
        # all t_depth and t_alt_count are missing
        # use information from vcf file instead
        Tumor_R_HT77_p0 = vcf[,11] # based on Mark data
        Tumor_R_HT77_p0 = strsplit(Tumor_R_HT77_p0, ":")
        for (i in 1:length(Tumor_R_HT77_p0)){
          mutect_AD = Tumor_R_HT77_p0[[i]][2]
          ref = as.numeric(strsplit(mutect_AD,",")[[1]][1])
          alt = as.numeric(strsplit(mutect_AD,",")[[1]][2])
          oncokb.txt$t_depth[i] = ref+alt
          oncokb.txt$t_rate[i] = alt/(ref+alt)
        }
      }
      fileoutput = oncokb.txt[, c("Hugo_Symbol","Variant_Classification","HGVSc","HGVSp_Short",
                                  "t_depth", "t_rate", "BIOTYPE", "IMPACT", "oncogenic",
                                  "Highest_level") ]
      if(!is.null(oncokb.txt$drugs)){
        fileoutput$drugs = oncokb.txt$drugs
      }
      fileoutput$Mutation_Effect = ""
      fileoutput$PUBMED = ""
      for (i in 1:dim(fileoutput)[1]){
        if(nchar(fileoutput$HGVSp_Short[i]) > 0 && !is.na(nchar(fileoutput$HGVSp_Short[i])) ){
          HGVSp_Short = unlist(strsplit(fileoutput$HGVSp_Short[i], split='p.', fixed=TRUE))[2]
          jsonlist=fromJSON(sprintf("http://oncokb.org/api/private/search/variants/biological?hugoSymbol=%s", fileoutput$Hugo_Symbol[i]))
          #jsonlist=fromJSON(sprintf("http://oncokb.org/api/private/search/variants/biological?hugoSymbol=%s", "PIK3CA"))
          if(length(jsonlist$variant$alteration==HGVSp_Short) > 0){
            mutation.effect = jsonlist[jsonlist$variant$alteration==HGVSp_Short,]$mutationEffect
            if(length(mutation.effect) > 0){
              fileoutput$Mutation_Effect[i] = mutation.effect
            }
            pubmedlinks = unlist(jsonlist[jsonlist$variant$alteration==HGVSp_Short,]$mutationEffectPmids)
            if(!is.null(pubmedlinks)){
              fileoutput$PUBMED[i] = paste(createPUBMEDLink(pubmedlinks), sep="", collapse="")
            }
          }
          
        }
      }
      
      # fileoutput$PUBMED = createPUBMEDLink(fileoutput$PUBMED)
      # fileoutput$PUBMED[is.na(oncokb.txt$PUBMED)] = ""
      fileoutput <<- fileoutput
      output$database_output <- DT::renderDataTable({
        fileoutput
      }, selection="none", escape = F, options=list(searching=T, pageLength = 100, ordering=T,
                                                    autoWidth = TRUE,
                                                    columnDefs = list(list(width = '100px', targets = "_all" )))
      )#,extensions = 'Responsive')
      session$sendCustomMessage("buttonCallbackHandler", "tab2")
      removeModal()
    }
  })
  
  # download
  
  output$download_foundation_all <- downloadHandler(
    filename = 'Results_Foundation.zip',
    content = function(fname) {
      filenames = c("PMI", "Variants", "CNA", "SV", "References", "Gene list")
      fs <- paste0(filenames, '.tsv')
      for(i in 1:length(foundationRes)){
        write.table(foundationRes[[i]], file = fs[i], sep = '\t', col.names = NA)
        # print(fs[i])
      }
      zip(zipfile=fname, files=fs)
      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
    },
    contentType = "application/zip"
  )
  
  output$download_MAF <- downloadHandler(
    filename = function() { "output.maf" },
    content = function(file) {
      file.copy(output.maf, file)
    })
  output$download_oncokb_all <- downloadHandler(
    filename = function() { "output_all.oncokb.txt" },
    content = function(file) {
      file.copy(OMAF, file)
    })
  output$download_oncokb_selected <- downloadHandler(
    filename = function() { "output_selected.oncokb.txt" },
    content = function(file) {
      write.table(fileoutput, file, row.names=TRUE, sep = "\t", quote = F)
    })
}

