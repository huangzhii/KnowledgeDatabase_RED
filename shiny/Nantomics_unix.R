  library(xlsx); #install.packages("rJava"); install.packages("xlsxjars"); install.packages("xlsx");
  library(data.table);
  library(doParallel);
  library(foreach); 

  base =32;

  #working diretory;
  type = "APG"
  db = "Nantomics";
  home = "/N/dc2/scratch/zyizhang/";
  dir = paste(home, db, "/", sep="");
  setwd(dir);

  #---------------;
  #unzipped VCF files, filemapping.xlsx and Nantomics_xxx.R are under dir;
  #---------------;

  #mapping file;
  filemapping = paste(dir, "filemapping.xlsx", sep="");
  patients = paste(dir, "patients_", db, "_", type, ".txt", sep="");

  #directories for saving files;

  dir.db = paste(dir, db, "_", type, "/", sep="");
  dir.Var = paste(dir.db, "Variants/", sep="");
  dir.ISOFORM = paste(dir.db, "ISOFORM/", sep="");
  dir.GENE = paste(dir.db, "GENE/", sep="");
  dir.CNA = paste(dir.db, "CNA/", sep="");
  dir.SV = paste(dir.db, "SV/", sep="");

  if (!dir.exists(dir.db)) {dir.create(dir.db)};  
  if (!dir.exists(dir.Var)) {dir.create(dir.Var)};
  if (!dir.exists(dir.ISOFORM)) {dir.create(dir.ISOFORM)};
  if (!dir.exists(dir.GENE)) {dir.create(dir.GENE)};
  if (!dir.exists(dir.CNA)) {dir.create(dir.CNA)};
  if (!dir.exists(dir.SV)) {dir.create(dir.SV)};

  #----------------------------
  # Input data
  #----------------------------
  #read mapping files;
  file.tmp = read.xlsx(filemapping, sheetName=paste(db, "-", type, sep=""), header=F);
  file = as.data.frame(file.tmp[file.tmp[, 1] %like% ".som.vcf.gz", ]);
  names(file) = "file.name";

  file = within(file, {file.name = as.character(sort(file.name))});

  if (file.exists(patients)) {
    patient.ID = read.delim(patients, sep="\t", fill=TRUE);
  } else {
    patient.ID = NULL;
  }
  
  #to create unique patient ID applied to data_file name;
  max.ID = dim(patient.ID)[1];
  if (is.null(max.ID)) {
    max.ID = 0;
  }

  file.new.1 = do.call(gsub, list(".gz", "", file$file.name))
  file.new = setdiff(file.new.1, patient.ID$file.name);


  if (length(file.new)==0) {patient.ID.new = NULL;
    } else {
    patient.ID.1 = as.data.frame(cbind(file.new, 1:length(file.new)));
    colnames(patient.ID.1) = c("file.name", "no");

    patient.ID.new = within(patient.ID.1, {
      file.name = file.name;
      patientID = paste(type, formatC(as.numeric(as.character(no)) + max.ID, width=5, format="d", flag="0"), sep="")})
  }

  patient.ID = rbind(patient.ID, patient.ID.new[, -2]);

  write.table(patient.ID, file=patients, row.names=F, sep="\t");

  if (!is.null(patient.ID.new)) {
  cl = makeCluster(base);
  registerDoParallel(cl);
  foreach(i=1:dim(patient.ID.new)[1], .combine=c, .packages=c("plyr", "data.table")) %dopar%  {
    #Read vcf file (.txt format) of each patient to R and combind the files;
    file = paste(dir, as.character(patient.ID.new[i, ]$file.name), sep="");
    file = "/N/u/zhihuan/Carbonate/Desktop/KnowledgeDatabase/CureMatch_data/CM02/CM02_CF0773_Tumor_WG_DNA_vs_20090227JS_2.5mL_Normal_WG_DNA_vs_CF0773_Tumor_RNA-Seq--20161102052233.som.vcf.txt.gz"
    patientID = patient.ID.new[i, ]$patientID;

    names.col = c("CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT", "NORMAL", "TUMOR", "TUMOR_RNA");
    if (!file.exists(file)) {
      data.In = NULL;
    } else {
      data.In = read.delim(file, sep="\t", fill=TRUE, header = F, col.names=names.col);

      #----------------------------
      # Manipulate data
      #----------------------------
      no.row = max(which(substr(data.In[, 1], 1, 1) == '#'));
      header = data.In[1:no.row, ];

      data = data.In[(no.row+1):dim(data.In)[1], ];
      data[, 1] = as.vector(data[, 1]);
      rownames(data) = NULL;

      #----------------------------
      # Variants
      #----------------------------
      #for variants, CHROM in (1-22, X, Y) and FILTER = "PASS";

      #CHROM in 1:22 or X or Y and FILTER="PASS" and  POS!=".";
      data.work = NULL;
      data.work = data[data$CHROM %in% c(1:22, "X", "Y") & data$FILTER=="PASS" & data$POS!='.', ]; 

      #FORMAT="GT:DP:..." and "INFO!="NN....";
      data.work = data.work[substr(data.work$FORMAT, 4, 5)=="DP" & substr(data.work$INFO,1, 2)!="NN", ]; 

      data.var = data.work[, -which(names(data.work) %in% ("TUMOR_RNA"))];

      #combine header to the variant table;
      data.var = rbind(header[, c(1:dim(data.var)[2])], data.var);

      rownames(data.var) = NULL;
      colnames(data.var) = NULL;


      write.table(data.var, file=paste(dir.Var, patientID, "_VCF.vcf", sep=""), row.names=F, sep="\t");
      # write.table(data.var, file=paste("~/Desktop/1_VCF.vcf", sep=""), row.names=F, sep="\t");
      
      #----------------------------
      # Expression (ISOFORM and gene)
      #----------------------------
      # variabls are extracted from FORMAT and values are extracted from TUMOR_RNA;
      
      #for expression, CHROM in (1-22, X, Y);

      #CHROM in 1:22 or X or Y and FILTER="PASS" and      POS!=".";
      data.work = NULL;
      data.work = data[data$CHROM %in% c(1:22, "X", "Y") & data$POS!='.', ]; 

      #FORMAT="GT:EXP_I_LEN:...";
      data.work = data.work[substr(data.work$FORMAT, 4, 8)=="EXP_I", ]; 
      rownames(data.work) = NULL;

      #---------gene info table---------;
      start = Sys.time();

      Gene.info = NULL;

      Gene.info = data.work[, 1:2];

      END = NULL;
      ISO = NULL;
      GENE = NULL;
      NOTE = NULL;

      if (dim(data.work)[1]!=0) {
        #to obtain the places of "=", ";", and "/" and length of INFO in list;
        loc.1 = lapply(data.work$INFO, function(x) unlist(gregexpr("=", x)));
        loc.2 = lapply(data.work$INFO, function(x) unlist(gregexpr(";", x)));
        loc.3 = lapply(data.work$INFO, function(x) unlist(gregexpr("/", x)));
        loc.4 = lapply(as.character(data.work$INFO), nchar);

        #to convert list of places to data.frame of place;
        a = lapply(loc.1, rbind);
        a1 = ldply(a, rbind);

        b = lapply(loc.2, rbind);
        b1 = ldply(b, rbind);
 
        c = lapply(loc.3, rbind);
        c1 = ldply(c, rbind);

        d = lapply(loc.4, rbind);
        d1 = ldply(d, rbind);

        #to obtain END, ISO, GENE by parallel processing (do.call);
        END = do.call(substr, list(data.work$INFO, a1[, 1]+1, b1[, 1]-1));
        ISO = do.call(substr, list(data.work$INFO, a1[, 2]+1, c1[, 1]-1));

        GENE1 = do.call(substr, list(data.work$INFO, c1[, 1]+1, d1[, 1]));
        GENE = do.call(gsub, list(";.*", "", GENE1)); #to remove ";CANONICAL" in GENE for some observations;

        NOTE = do.call(substr, list(data.work$INFO, b1[, 2]+1, d1[, 1]));
      }

      Gene.info = cbind(Gene.info, END, ISO, GENE, NOTE);

      #keep GENE for ISOFORM and GENE tables;
      data.work = cbind(data.work, GENE);

      end = Sys.time();
      diff = difftime(end, start, units="mins");
      diff;

      write.table(Gene.info, file=paste(dir.GENE, patientID, "_Gene_info_VCF.txt", sep=""), row.names=F, sep="\t");
 

      #---------ISOFORM and gene---------;
 
      start = Sys.time();

      data.exp = NULL

      #in case there are different FORMAT for gene expression;
      freq.names = table(as.character(data.work$FORMAT));

      
      i = 1;
      while (i<=length(freq.names)) {

            data.chrom = NULL; tmp = NULL; data.exp.1 = NULL;

            data.tmp = data.work[data.work$FORMAT %in% names(freq.names)[i], names(data.work) %in% c("CHROM", "GENE", "FORMAT", "TUMOR_RNA")]

            data.chrom = data.tmp[, names(data.tmp) %in% c("CHROM", "GENE")];

            var1 = names(freq.names)[i];
            var2 = data.tmp$TUMOR_RNA;

            names.var = lapply(var1, function(x) unlist(strsplit(toString(x), ":")));
            val.var = lapply(var2, function(x) unlist(strsplit(toString(x), ":")));

            tmp = as.data.frame(do.call(rbind, val.var))
            colnames(tmp) = unlist(names.var);

            data.exp.1 = cbind(data.chrom, tmp);

            data.exp = rbind.fill(data.exp, data.exp.1);

            i = i + 1;

      }

      end = Sys.time();
      diff = difftime(end, start, units="mins");
      diff;

      #ISOFORM;
      ISOFORM = data.exp[, names(data.exp) %in% c("CHROM", "GENE", "GT") | names(data.exp) %like% "^EXP_I_"];

      write.table(ISOFORM, file=paste(dir.ISOFORM, patientID, "_ISOFORM_VCF.txt", sep=""), row.names=F, sep="\t");


      #gene;
      GENE = data.exp[, names(data.exp) %in% c("CHROM", "GENE", "GT") | names(data.exp) %like% "^EXP_G_"];

 
      write.table(GENE, file=paste(dir.GENE, patientID, "_GENE_VCF.txt", sep=""), row.names=F, sep="\t");
      }
    }
    stopCluster(cl)
  }
