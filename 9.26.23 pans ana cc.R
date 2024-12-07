library(ggplot2)
library(ggthemr)
library(tidyverse)
library(dplyr)
library(viridis)


#### QC STUFF ####

#import file and create a dataframe from csv file
r1_mfi <- read.csv("9.25.23 PANS ana cc r1_mfi.csv", stringsAsFactors = F, encoding="UTF-8")
r1_ct <- read.csv("9.25.23 PANS ana cc r1_ct.csv", stringsAsFactors = F, encoding="UTF-8")
r2_mfi <- read.csv("9.26.23 PANS ana cc r2_mfi.csv", stringsAsFactors = F, encoding="UTF-8")
r2_ct <- read.csv("9.26.23 PANS ana cc r2_ct.csv", stringsAsFactors = F, encoding="UTF-8")

sample.list <- c(
  "A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14","A15","A16","A17","A18","A19","A20","A21","A22","A23","A24","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","B13","B14","B15","B16","B17","B18","B19","B20","B21","B22","B23","B24","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","D3","D4","D5","D6","D7","D8","D9","D10","D11","D12","D13","D14","D15","D16","D17","D18","D19","D20","D21","D22","D23","D24","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","E13","E14","E15","E16","E17","E18","E19","E20","E21","E22","E23","E24","F3","F4","F5","F6","F7","F8","F9","F10","F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","G1","G2","G3","G4","G5","G6","G7","G8","G9","G10","G11","G12","G13","G14","G15","G16","G17","G18","G19","G20","G21","H1","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12","H13","H14","H15","H16","H17","H18","H19","H20","H21","H22","H23","H24","I3","I4","I5","I6","I7","I8","I9","I10","I11","I12","I13","I14","I15","I16","I17","I18","I19","I20","I21","I22","I23","I24","J3","J4","J5","J6","J7","J8","J9","J10","J11","J12","J13","J14","J15","J16","J17","J18","J19","J20","J21","J22","J23","J24","K3","K4","K5","K6","K7","K8","K9","K10","K11","K12","K13","K14","K15","K16","K17","K18","K19","K20","K21","K22","K23","K24","L3","L4","L5","L6","L7","L8","L9","L10","L11","L12","L13","L14","L15","L16","L17","L18","L19","L20","L21","L22","L23","L24","M3","M4","M5","M6","M7","M8","M9","M10","M11","M12","M13","M14","M15","M16","M17","M18","M19","M20","M21","M22","M23","M24","N1","N2","N3","N4","N5","N6","N7","N8","N9","N10","N11","N12","N13","N14","N15","N16","N17","N18","N19","N20","N21","N22","O1","O2","O3","O4","O5","O6","O7","O9","O11","O13","O15","O17","O19","O21","O23"
  )
sample.list <- as.factor(sample.list) #converts character vector to factor so that levels can be rearranged
sample.list <- factor(sample.list, levels = c(sample.list <- c(
  "A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14","A15","A16","A17","A18","A19","A20","A21","A22","A23","A24","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","B13","B14","B15","B16","B17","B18","B19","B20","B21","B22","B23","B24","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","D3","D4","D5","D6","D7","D8","D9","D10","D11","D12","D13","D14","D15","D16","D17","D18","D19","D20","D21","D22","D23","D24","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","E13","E14","E15","E16","E17","E18","E19","E20","E21","E22","E23","E24","F3","F4","F5","F6","F7","F8","F9","F10","F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","G1","G2","G3","G4","G5","G6","G7","G8","G9","G10","G11","G12","G13","G14","G15","G16","G17","G18","G19","G20","G21","H1","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12","H13","H14","H15","H16","H17","H18","H19","H20","H21","H22","H23","H24","I3","I4","I5","I6","I7","I8","I9","I10","I11","I12","I13","I14","I15","I16","I17","I18","I19","I20","I21","I22","I23","I24","J3","J4","J5","J6","J7","J8","J9","J10","J11","J12","J13","J14","J15","J16","J17","J18","J19","J20","J21","J22","J23","J24","K3","K4","K5","K6","K7","K8","K9","K10","K11","K12","K13","K14","K15","K16","K17","K18","K19","K20","K21","K22","K23","K24","L3","L4","L5","L6","L7","L8","L9","L10","L11","L12","L13","L14","L15","L16","L17","L18","L19","L20","L21","L22","L23","L24","M3","M4","M5","M6","M7","M8","M9","M10","M11","M12","M13","M14","M15","M16","M17","M18","M19","M20","M21","M22","M23","M24","N1","N2","N3","N4","N5","N6","N7","N8","N9","N10","N11","N12","N13","N14","N15","N16","N17","N18","N19","N20","N21","N22","O1","O2","O3","O4","O5","O6","O7","O9","O11","O13","O15","O17","O19","O21","O23"
  )))

#input ag list as a vector
ag.list <- c(
  "Bare Bead","Anti-Human IgG Fc fragment specific","Anti-Human IgG F(ab') fragment specific","Anti-Human IgG (H+L)","Human IgG from serum","IL-1A","IL-2","IL-4","IL-6","IL-7","IL-11","IL-15","IL-17A","IL-17F","IL-21","IL-22","IL-23","Intrinsic Factor","MPO","U11/U12 RNP","Eotaxin","Eotaxin 2","CX3CL1","IL-1B","CCL8","OSM","CXCL12","VEGFA","CXCL1","CXCL7","LRP11","Beta-tubulin","Ganglioside-GM1","U1-snRNP A","PDC-E2","TPO","TG","CXCL10","CXCL9","CCL3","CXCL13","CXCL16","CXCL5","CCL21","CXCL8","CCL22","CCL19","CCL25","CXCL17","IFN-alpha1","IFN-alpha2","IFN-alpha6","IFN-alpha7","IFN-alpha8","IFN-alpha10","IFN-beta","IFN-epsilon","IFN-omega","IFN-gamma","IFN-lambda1","IFN-lambda2","C3","C1q","FGF7","GM-CSF","LIF","PDGFBB","VEGFB","TNF-alpha","IL1RA","ACE2","Proteinase 3","BPI","Ro52","Ro60","La/SSB","U1-snRNP C","Ribo P0","Ribo P1","Ribo P2","IFN-lambda3","IL-33","CENP A","Scl-70","Fibrillarin","PM/Scl-75","MDA5","EJ","Jo-1","SRP54","OJ","PL-7","PL-12","CCL26"
)
ag.list <- as.factor(ag.list) #converts character vector to factor so that levels can be rearranged
ag.list <- factor(ag.list, levels = c(
  "Bare Bead","Anti-Human IgG Fc fragment specific","Anti-Human IgG F(ab') fragment specific","Anti-Human IgG (H+L)","Human IgG from serum","IL-1A","IL-2","IL-4","IL-6","IL-7","IL-11","IL-15","IL-17A","IL-17F","IL-21","IL-22","IL-23","Intrinsic Factor","MPO","U11/U12 RNP","Eotaxin","Eotaxin 2","CX3CL1","IL-1B","CCL8","OSM","CXCL12","VEGFA","CXCL1","CXCL7","LRP11","Beta-tubulin","Ganglioside-GM1","U1-snRNP A","PDC-E2","TPO","TG","CXCL10","CXCL9","CCL3","CXCL13","CXCL16","CXCL5","CCL21","CXCL8","CCL22","CCL19","CCL25","CXCL17","IFN-alpha1","IFN-alpha2","IFN-alpha6","IFN-alpha7","IFN-alpha8","IFN-alpha10","IFN-beta","IFN-epsilon","IFN-omega","IFN-gamma","IFN-lambda1","IFN-lambda2","C3","C1q","FGF7","GM-CSF","LIF","PDGFBB","VEGFB","TNF-alpha","IL1RA","ACE2","Proteinase 3","BPI","Ro52","Ro60","La/SSB","U1-snRNP C","Ribo P0","Ribo P1","Ribo P2","IFN-lambda3","IL-33","CENP A","Scl-70","Fibrillarin","PM/Scl-75","MDA5","EJ","Jo-1","SRP54","OJ","PL-7","PL-12","CCL26"
))


###BEFORE PROCEEDING, CHECK TO MAKE SURE THAT THE DIMS OF ALL OBJECTS (R1_..., R2_...) ARE IDENTICAL

#pre-processing - set sample names in col 1 to rownames
preproc <- function(raw) {
  rownames(raw) <- sample.list
  raw <- raw[,-1]
  return(raw)
}

r1_mfi <- preproc(r1_mfi)
r1_ct <- preproc(r1_ct)
r2_mfi <- preproc(r2_mfi)
r2_ct <- preproc(r2_ct)

#index IDs and wells w/ <25 bt ct then store in list
bd.ct.qc <- function(ct) {
  ct.1 <- which(ct < 25, arr.ind = T) #indexing
  ct.list <- split(ct.1, row(ct.1)) #store in list
  return(ct.list)
}

r1_ct_list <- bd.ct.qc(r1_ct)
r2_ct_list <- bd.ct.qc(r2_ct)

# to calculate the %CV within IDs for duplicate sera with low bd counts, then index the rep with %CV>20
ct.combined <- c(r1_ct_list, r2_ct_list) #to visually inspect and remember the row position that begins from r2 (row 59)
r1_lowct <- r1_mfi[do.call(rbind, ct.combined)] #pull out the mfis in rep 1 that correspond to the low ct indexes in ct.combined
r2_lowct <- r2_mfi[do.call(rbind, ct.combined)] #same as above
combined_lowct <- cbind(r1_lowct,r2_lowct) #combine MFIs corresponding to low bead count from rep 1&2 into a single list
combined_lowct <- as.data.frame(combined_lowct)
combined_lowct$sd <- apply(combined_lowct, MARGIN = 1, FUN = sd) #calculate sd per row, ie. for sample duplicates
combined_lowct$mean <- apply(combined_lowct, MARGIN = 1, FUN = mean) #calculate mean per row, ie sample dups
combined_lowct$cv <- combined_lowct$sd / combined_lowct$mean * 100 #calc %CV within the dups
combined_lowct$cvdec <- ifelse(combined_lowct$cv <= 20, "KEEP BOTH", "REMOVE LOW CT MFI") #annotate wells w low bd ct and %CV>20
combined_lowct$index <- ct.combined #annotate the index locations for all the wells/IDs to easily process them downstream
combined_lowct <- combined_lowct %>% #to annotate which row belongs to which rep
  mutate(Rep = factor(ifelse(row_number() <= 58, "r1", "r2"))) #input number corresponding to the last row position belonging to rep 1

#write another script to identify samples with low bd ct in both reps
matching_index <- intersect(r1_ct_list, r2_ct_list) #identify indexes present in both lists, ie low bd cts for both dups

for (i in 1:nrow(combined_lowct)) {
  if (combined_lowct$index[i] %in% matching_index) {
    combined_lowct$cvdec[i] <- "REMOVE BOTH" #annotate the indexes with those with low cts in both dups
  } 
  else {
     next
  }
}

r1_mfi.1 <- r1_mfi
r2_mfi.1 <- r2_mfi
r1_mfi.1 <- as.matrix(r1_mfi.1)
r2_mfi.1 <- as.matrix(r2_mfi.1)

for (i in 1:nrow(combined_lowct)) {
  rep_value <- combined_lowct$Rep[i] #need to store in a new variable since this is a vectorized condition
  index_value <- combined_lowct$index[i][[1]] #same logic as above
  row_idx <- index_value[1] #retrieve row and col index
  col_idx <- index_value[2]
  value.1 <- r1_mfi.1[row_idx, col_idx] #store value for current iteration's index 
  value.2 <- r2_mfi.1[row_idx, col_idx]
  cat("index_value data type:", class(index_value), "\n") #check if code is iterating through everything
  cat("index_value value:", index_value, "\n")
  
  if (combined_lowct$cvdec[i] == "REMOVE LOW CT MFI") {
    if (rep_value == "r1") {
      r1_mfi.1[row_idx, col_idx] <- value.2 #if low ct comes from rep 1, replace w rep 2 mfi
    } 
    else if (rep_value == "r2") {
      r2_mfi.1[row_idx, col_idx] <- value.1 #if low ct comes from rep 2, replace w rep 1 mfi
    }
  } 
  
  else if (combined_lowct$cvdec[i] == "REMOVE BOTH") {
    r1_mfi.1[row_idx, col_idx] <- NA 
    r2_mfi.1[row_idx, col_idx] <- NA
    }
}

# Find the indices of NA values
na_indices.1 <- which(is.na(r1_mfi.1), arr.ind = TRUE)
print(na_indices.1)
na_indices.2 <- which(is.na(r2_mfi.1), arr.ind = TRUE)
print(na_indices.2)

colnames(r1_mfi.1)[77]

sample.list <- read.csv("UNORDERED.csv", stringsAsFactors = F, encoding="UTF-8") #input desired sample order (ordered by sample grouping & alphabetically)
sample.list <- as.character(sample.list[, 2])
cat.list <- read.csv("UNORDERED.csv", stringsAsFactors = F, encoding="UTF-8") #input desired sample order (ordered by sample grouping & alphabetically)
cat.list <- as.character(cat.list[, 1])

#sample.list <- c(
#  "126-0004-01","126-0022-01","126-0006-02","126-0057-01","126-0023-02","126-0099-01","126-0032-02","126-0105-01","126-0035-02","126-0115-01","126-0060-01","126-0120-01","126-0063-01","126-0130-01","126-0067-01","126-0154-02","126-0071-02","126-0156-01","126-0080-01","126-0161-02","126-0093-01","77-0001-02","126-0093-04","77-0013-06","126-0097-01","77-0014-01","126-0110-04","77-0014-08","126-0117-01","77-0048-16","126-0147-01","77-0090-02","126-0147-03","77-0091-07","126-0149-01","77-0107-04","126-0165-01","77-0120-02","77-0006-13","77-0120-28","77-0028-09","77-0153-06","77-0039-20","77-0153-11","77-0047-38","77-0157-01","77-0069-03","77-0183-01","77-0071-10","77-0183-81","77-0096-03","77-0185-01","77-0140-02","77-0205-05","77-0146-01","77-0371-02","77-0147-12","77-0206-42","77-0150-02","77-0214-01","77-0160-05","77-0219-45","77-0166-05","77-0226-01","77-0186-01","77-0233-03","77-0196-01","77-0244-01","77-0198-06","77-0268-05","77-0198-36","77-0269-91","77-0227-05","77-0277-01","77-0231-01","77-0280-25","77-0251-03","77-0301-40","77-0251-46","77-0328-58","77-0252-81","77-0343-03","77-0270-01","77-0346-01","77-0275-35","77-0355-02","77-0293-19","77-0363-02","77-0300-01","77-0363-23","77-0353-06","77-0364-02","77-0373-01","77-0397-01","77-0393-01","77-0206-03","77-0393-02","77-0401-01","77-0398-01","77-0403-01","77-0402-01","77-0403-17","77-0419-07","77-0412-01","77-0464-01","77-0445-01","77-0491-01","77-0489-01","77-0499-01","77-1013-01","77-1000-02","111-0012-01","77-1027-01","111-0017-01","111-0013-01","111-0019-01","111-0028-01","111-0039-01","111-0029-01","111-0041-01","111-0047-01","111-0042-02","111-0060-01","111-0069-03","111-0502-01","111-0080-03","111-0506-01","111-0524-01","111-0507-01","111-0547-01","111-0508-01","111-0553-01","111-0532-01","111-0562-01","111-0545-01",
 # "111-0582-01","111-0552-01","111-0632-01","111-0554-01","111-0641-01","111-0594-01","111-0675-01","111-0653-01","111-0742-01","111-0654-01","111-0751-01","111-0702-01","111-0769-01","111-0710-04","126-0043-01","111-0752-01","126-0077-01","111-0770-01","126-0083-01","126-0001-01","126-0002-01","126-0017-03","126-0005-01","126-0027-02","126-0020-01","126-0029-01","126-0024-01","126-0047-01","126-0030-02","126-0049-01","126-0033-01","126-0053-01","126-0073-01","126-0089-01","126-0085-01","126-0106-01","126-0088-02","126-0113-02","126-0092-01","126-0136-02","126-0108-04","126-0148-01","126-0111-01","126-0152-01","126-0119-01","126-0155-01","126-0172-01","126-0170-01","77-0008-05","77-0007-03","77-0009-01","77-0007-29","77-0032-01","77-0010-01","77-0068-01","77-0010-02","77-0068-11","77-0016-01","77-0092-04","77-0016-06","77-0102-04","77-0019-01","77-0103-04","77-0040-02","77-0127-02","77-0040-09","77-0143-01","77-0041-01","77-0148-03","77-0042-03","77-0148-10","77-0067-37","77-0158-02","77-0076-01","77-0162-02","77-0079-06","77-0171-02","77-0085-04","77-0172-03","77-0095-01","77-0178-02","77-0097-01","77-0178-13","77-0152-06","77-0181-02","77-0155-01","77-0189-01","77-0170-02","77-0193-01","77-0170-18","77-0211-02","77-0207-07","77-0218-01","77-0207-37","77-0218-33","77-0208-01","77-0228-02","77-0208-13","77-0229-03","77-0235-01","77-0237-03","77-0241-07","77-0239-01","77-0241-21","77-0257-78","77-0311-34","77-0261-56","77-0312-24","77-0282-16","77-0317-13","77-0282-31","77-0338-01","77-0286-14","77-0354-01","77-0304-52","77-0362-07","77-0324-32","77-0362-15","77-0324-37","77-0370-05","77-0347-05","77-0390-01","77-0365-01","77-0409-07","77-0365-06","77-0421-01","77-0380-03","77-0422-02","77-0387-01","77-0456-02","77-0391-01","77-0482-01","77-0411-01","77-0483-01","77-1006-01","77-1020-01",
 # "77-1014-01","111-0002-02","111-0020-01","111-0022-01","111-0023-01","111-0046-02","111-0024-01","111-0504-01","111-0035-01","111-0568-01","111-0038-01","111-0634-02","111-0510-01","111-0636-01","111-0514-01","111-0640-02","111-0515-01","111-0648-02","111-0549-01","111-0652-01","111-0551-01","111-0666-02","111-0580-01","111-0669-01","111-0589-01","111-0684-01","111-0604-01","111-0685-01","111-0608-01","111-0708-01","111-0651-01","111-0728-01","111-0658-01","111-0734-01","111-0703-01","111-0738-01","111-0718-01","111-0740-01","111-0729-01","111-0768-01","111-0745-01","126-0026-01","111-0753-01","HCT","HIS","HTG","HMS","HRN","HAS","HSC","MM","HC123"
#)

#remove u1bb'
r1_mfi.2 <- r1_mfi.1[,-77]
r2_mfi.2  <- r2_mfi.1[,-77]

#pull out samples with < or > 2.5sd for igg beads
#pull out samples < or > 2.5sd for igg beads
sd.process <- function(input) {
  colnames(input) <- ag.list
  log.mat <- log2(input[ , 2:5]) #change col corresponding to the control beads
  sd.ag <- apply(log.mat, 2, sd, na.rm=T)
  ag.mean <- colMeans(log.mat, na.rm=T)
  num_sd <- 2.5 #can change sd cutoff for stringency
  above_2.5 <- ag.mean + (sd.ag * num_sd)
  below_2.5 <- ag.mean - (sd.ag * num_sd)
  
  return(list(log.mat = log.mat, above_2.5 = above_2.5, below_2.5 = below_2.5))
}

r1.sd <- sd.process(r1_mfi.2)
r2.sd <- sd.process(r2_mfi.2)

sd.igg <- function(matrix, sdvals.abv, sdvals.bel) {
  all.names.abv <- list()
  all.names.bel <- list()
  for (i in 1:length(sdvals.abv)) {
    names.sd.abv <- rownames(matrix[matrix[, i] > sdvals.abv[i], ,drop = FALSE])
    all.names.abv[[i]] <- names.sd.abv
  }
  
  for (j in 1:length(sdvals.bel)) {
    names.sd.bel <- rownames(matrix[matrix[, j] < sdvals.bel[j], ,drop = FALSE])
    all.names.bel[[j]] <- names.sd.bel
  }
  
  common.abv <- Reduce(intersect, all.names.abv)
  common.bel <- Reduce(intersect, all.names.bel)
  
  return(list(common.abv = common.abv, common.bel = common.bel))
}

r1.igg.sd <- sd.igg(r1_mfi.log, above_2.5.r1, below_2.5.r1)
r2.igg.sd <- sd.igg(r2_mfi.log, above_2.5.r2, below_2.5.r2)


matching.r1 <- list(
  abv = which(rownames(r1_mfi.1) %in% r1.igg.sd$common.abv),
  bel = which(rownames(r1_mfi.1) %in% r1.igg.sd$common.bel)
)
print(matching.r1)
#replace 189 (i15), 202 (j6)

matching.r2 <- list(
  abv = which(rownames(r1_mfi.2) %in% r2.igg.sd$common.abv),
  bel = which(rownames(r2_mfi.2) %in% r2.igg.sd$common.bel)
)
print(matching.r2)
#replace 159 (h7), 214 (j18), 236 (k18), 258 (l18)

#replacing low igg r

r1_mfi.2[189, ] <- r2_mfi.2[189, ]
r1_mfi.2[202, ] <- r2_mfi.2[202, ]
#r1_mfi.3[209, ] <- r2_mfi.3[209, ]
r2_mfi.2[159, ] <- r1_mfi.2[159, ]
r2_mfi.2[214, ] <- r1_mfi.2[214, ]
r2_mfi.2[236, ] <- r1_mfi.2[236, ]
#r2_mfi.3[253, ] <- r1_mfi.3[253, ]
r2_mfi.2[258, ] <- r1_mfi.2[258, ]


ag.list.1 <- c(
  "IL-1A","IL-2","IL-4","IL-6","IL-7","IL-11","IL-15","IL-17A","IL-17F","IL-21","IL-22","IL-23","Intrinsic Factor","MPO","U11/U12 RNP","Eotaxin","Eotaxin 2","CX3CL1","IL-1B","CCL8","OSM","CXCL12","VEGFA","CXCL1","CXCL7","LRP11","Beta-tubulin","Ganglioside-GM1","U1-snRNP A","PDC-E2","TPO","TG","CXCL10","CXCL9","CCL3","CXCL13","CXCL16","CXCL5","CCL21","CXCL8","CCL22","CCL19","CCL25","CXCL17","IFN-alpha1","IFN-alpha2","IFN-alpha6","IFN-alpha7","IFN-alpha8","IFN-alpha10","IFN-beta","IFN-epsilon","IFN-omega","IFN-gamma","IFN-lambda1","IFN-lambda2","C3","C1q","FGF7","GM-CSF","LIF","PDGFBB","VEGFB","TNF-alpha","IL1RA","ACE2","Proteinase 3","BPI","Ro52","Ro60","La/SSB","U1-snRNP C","Ribo P0","Ribo P1","Ribo P2","IFN-lambda3","IL-33","CENP A","Scl-70","Fibrillarin","PM/Scl-75","MDA5","EJ","Jo-1","SRP54","OJ","PL-7","PL-12","CCL26"
)

#subtract bb

normbb <- function(matrix) {
  bb.pos <- which(colnames(matrix) == "Bare Bead") #fetches column index that has the title "Bare.bead"
  matrix.sub <- sweep(matrix, 1, matrix[,bb.pos], "-")
  matrix.rem <- matrix.sub[,-1]
  matrix.rem[matrix.rem < 0] <- 0
  return(matrix.rem)
}

r1_mfi.3 <- normbb(r1_mfi.2)
r2_mfi.3 <- normbb(r2_mfi.2)

r1_mfi.4 <- r1_mfi.3[, -c(1:4)]
r2_mfi.4 <- r2_mfi.3[, -c(1:4)]

#average data for final matrix
avg.mfi <- (r1_mfi.4 + r2_mfi.4) / 2

#avg.mfi <- avg.mfi[,-72]
colnames(avg.mfi) <- ag.list.1
rownames(avg.mfi) <- sample.list
na_indexes <- which(rownames(avg.mfi) %in% c("P_NA", "P_NA2", "HC_NA"))
avg.mfi <- avg.mfi[-na_indexes,]

sample.updated <- rownames(avg.mfi)
pans <- as.data.frame(avg.mfi)

#append all columns to a single column
pans <- stack(pans)

#rename the factor titles in dataframe
names(pans)[names(pans) == "ind"] <- "Ag"
names(pans)[names(pans) == "values"] <- "MFI"

pans$Sample <- sample.updated

sort <- read.csv("REORDERED.csv", stringsAsFactors = F, encoding="UTF-8") #input desired sample order (ordered by sample grouping & alphabetically)
ag.sort <- read.csv("ag.sort.csv", stringsAsFactors = F, encoding="UTF-8")

pans$Ag <- factor(pans$Ag, levels = ag.sort$Ag)
pans$Sample <- factor(pans$Sample, levels = sort$Sample)

pans.1 <- pans %>% arrange(Ag, Sample)

pans.1$Cat <- sort$Cat

#write.csv(pans.1, "pans test1.csv")

#MFI heatmap
p <- ggplot(pans, aes(x = Sample, y=Ag)) + 
  geom_tile(aes(fill=MFI)) +
  theme(plot.title=element_text(vjust=1,
                                hjust=0.5,
                                face="bold",
                                size=50),
        axis.text.x=element_text(angle=90, 
                                 hjust=1,
                                 margin=margin(b=20, t=10),
                                 size=30),
        axis.text.y=element_text(margin=margin(l=20, r=10),
                                 size=30),
        axis.title=element_text(size=40,
                                face="bold"),
        legend.title = element_text(color = "black",
                                    face="bold",
                                    size = 30),
        legend.text = element_text(color = "black",
                                   size = 15),
        legend.key.size = unit(1.2, 'cm'),
        legend.position = "right") +
  ggtitle("9.26.23 PANS ANA and CC Screen (Flare vs HC)") +     
  scale_fill_gradientn(colours = c("black","blue","green","yellow","red"),
                       values = c(0,0.25,0.5,0.75,1), 
                       limits=c(0, 12000), 
                       oob = scales::squish) 
p #40*100

panslist <- c("PANS","HC")

#only include pans and hc
pans.only <- filter(pans.1, Cat %in% panslist)
#ifnl.list <- c("IFN-lambda1","IFN-lambda2","IFN-lambda3")
#pans.ifnl <- filter(pans, Ag %in% ifnl.list)

#library(openxlsx)
#write.xlsx(pans.ifnl, file = "ifnl_test.xlsx")

#calculate sd in a new dataframe pans.sd
pans.sd <- pans.only %>% 
  group_by(Ag, Cat) %>% 
  summarize(mean = mean(MFI, na.rm = TRUE),
            sd = sd(MFI, na.rm = TRUE),
            count = n()) 

#multiply sd x 3 + mean
pans.sd$threesd <- pans.sd$sd*5 + pans.sd$mean

#to only keep the 3sd+mean value for HC
tsd <- c("HC")
pans.sd <- pans.sd %>% 
  select(Ag, Cat, threesd) %>% 
  filter(Cat == tsd) 

#MAKE SURE pans.sd and pans.only are in same order
pans.sd$Ag <- factor(pans.sd$Ag, levels = ag.sort$Ag)
pans.only$Ag <- factor(pans.only$Ag, levels = ag.sort$Ag)

#multiply each row x (n of samples) so that it can be the same length as pans.only for appending
pans.sd <- pans.sd[rep(seq_len(nrow(pans.sd)), each = 311), ]

#append the 5sd+mean in dfp to df so we can compare raw MFI with 5sd+mean values
pans.only$tsd <- pans.sd$threesd

#compare the fsd col with MFI col and append if > or < 3SD to new column
pans.only$sdcompare <- ifelse(pans.only$tsd < pans.only$MFI & pans.only$MFI >= 3000,
                              "> 5 SD",
                       ifelse(pans.only$tsd > pans.only$MFI, "< 5 SD", "< 5 SD"))
pal <- c("< 5 SD" = "black", "> 5 SD" = "red")

sd <- ggplot(pans.only, aes(x=Sample, y=Ag)) +
  geom_tile(aes(fill=sdcompare)) +
  theme(plot.title=element_text(vjust=1,
                                hjust=0.5,
                                face="bold",
                                size=50),
        axis.text.x=element_text(angle=90, 
                                 hjust=1,
                                 margin=margin(b=20, t=10),
                                 size=30),
        axis.text.y=element_text(margin=margin(l=20, r=10),
                                 size=30),
        axis.title=element_text(size=40,
                                face="bold"),
        legend.title = element_text(color = "black",
                                    face="bold",
                                    size = 30),
        legend.text = element_text(color = "black",
                                   size = 30),
        legend.key.size = unit(1.2, 'cm')) +
  ggtitle("9.26.23 PANS ANA and CC Screen (Flare vs HC) - SD heatmap") +
  scale_fill_discrete(type = pal) 
sd

#add in demo data
demo <- read.csv("9.26.23 PANS demo.csv", stringsAsFactors = F, encoding="UTF-8")
demo <- as.data.frame(demo)
pans.demo <- cbind(pans.only, demo[rep_len(seq_len(nrow(demo)), length.out = nrow(pans.only)), ])

pans.demo.1 <- pans.demo %>%
  mutate(Demographics_White = as.numeric(Demographics == "White"),
         Demographics_Asian = as.numeric(Demographics == "Asian"),
         Demographics_Multiracial = as.numeric(Demographics == "Multiracial_or_Other"),
         Demographics_Hispanic = as.numeric(Demographics == "Hispanic_or_Latino"),
         Sex_Female = as.numeric(Sex == "Female"),
         Sex_Male = as.numeric(Sex == "Male")
  )

# List of antigen names
antigens <- unique(pans.demo.1$Ag)

# Initialize a list to store regression models
models <- list()

# Loop through each antigen
for (antigen in antigens) {
  subset_data <- subset(pans.demo.1, Ag == antigen)
  formula <- formula(paste("MFI ~ Age + Demographics_White + Demographics_Asian + Demographics_Multiracial + Demographics_Hispanic + Sex_Female"))
  model <- lm(formula, data = subset_data)
  
  # Store the model in the list
  models[[antigen]] <- model
}

# Open a PDF file for output
pdf("diagnostic_plots.1.pdf")

# Loop through each antigen
for (antigen in antigens) {
  # Subset the data for the current antigen
  subset_data <- subset(pans.demo.1, Ag == antigen)
  
  # Retrieve the corresponding linear regression model
  model <- models[[antigen]]
  
  # Predicted values
  predicted_values <- predict(model)
  
  # Combine data into a data frame, handling missing values
  plot_data <- data.frame(
    Observed = subset_data$MFI[!is.na(predicted_values)],
    Predicted = predicted_values[!is.na(predicted_values)],
    Residuals = residuals(model)[!is.na(predicted_values)]
  )
  
  # Check if there is enough data for the plot
  if (nrow(plot_data) < 3) {
    cat("Not enough data for plot for antigen:", antigen, "\n")
    next  # Skip to the next iteration if there's not enough data
  }
  
  # Scatterplot of observed vs. predicted values
  scatterplot <- ggplot(plot_data, aes(x = Observed, y = Predicted)) +
    geom_point() +
    ggtitle(paste("Observed vs. Predicted for", antigen))
  
  # Residuals vs. fitted values plot
  residuals_plot <- ggplot(plot_data, aes(x = Predicted, y = Residuals)) +
    geom_point() +
    ggtitle(paste("Residuals vs. Fitted for", antigen))
  
  # Histogram of residuals
  residuals_histogram <- ggplot(plot_data, aes(x = Residuals)) +
    geom_histogram(binwidth = 1) +
    ggtitle(paste("Histogram of Residuals for", antigen))
  
  # Save plots in the PDF file
  print(scatterplot)
  print(residuals_plot)
  print(residuals_histogram)
}

# Close the PDF file
dev.off()

#multiple regression might not be a good idea - most samples are low (positive skew), and in the context of pans, 
#data is more susceptible to outliers

#can compare tukey or huber, with and without log transform
library(MASS)
library(robustbase)

#regression, no log
h.models <- list()

# Loop through each antigen
for (antigen in antigens) {
  subset_data <- subset(pans.demo.1, Ag == antigen)

  if (any(is.na(subset_data$MFI))) {
    cat("Skipping antigen", antigen, "due to missing MFI values.\n")
    next
  }
  formula <- formula(paste("MFI ~ Age + Demographics_White + Demographics_Asian + Demographics_Multiracial + Demographics_Hispanic + Sex_Female"))
  h.model <- lmrob(formula, data = subset_data, control = lmrob.control(maxit.scale = 500, init = "random"))
  
  # Store the model in the list
  h.models[[antigen]] <- h.model

}

# Filter the data for TSD == ">5 SD"
filtered_data <- pans.only[pans.only$sdcompare == "> 5 SD", ]
exclude_values <- "NA"
filtered_data <- subset(filtered_data, !sdcompare %in% exclude_values & !is.na(sdcompare))
# Initialize a list to store results
chi_squared_results <- list()
unique_ags <- unique(filtered_data$Ag)

# Loop through unique Ag values
for (ag in unique_ags) {
  # Subset the data for the current Ag
  ag_data <- filtered_data[filtered_data$Ag == ag, ]
  
  # Create a contingency table for PANS and HC
  contingency_table <- table(ag_data$Cat)
  
  # Perform a chi-squared test
  chi_squared_test <- chisq.test(contingency_table)
  
  # Determine which category has a higher count (PANS or HC)
  if (contingency_table[1] > contingency_table[2]) {
    chi_squared_test$source_of_significance <- "PANS > HC"
  } else {
    chi_squared_test$source_of_significance <- "HC > PANS"
  }
  
  # Store the chi-squared test result in the list
  chi_squared_results[[ag]] <- chi_squared_test
}

# Create a data frame to store chi-squared results
chi_squared_df <- do.call(rbind, lapply(names(chi_squared_results), function(ag) {
  data.frame(
    Ag = ag,
    Source_of_Significance = chi_squared_results[[ag]]$source_of_significance,
    p_value = chi_squared_results[[ag]]$p.value,
    stringsAsFactors = FALSE
  )
}))

# Filter for significant results (p-value < 0.05, for example)
significant_results <- chi_squared_df[chi_squared_df$p_value < 0.05, ]


# Sort the data frame by p-value (optional)
chi_squared_df <- chi_squared_df[order(chi_squared_df$p_value), ]

# Extract the p-values and convert to a matrix
p_values_matrix <- as.matrix(chi_squared_df$p_value)
rownames(p_values_matrix) <- chi_squared_df$Ag  # Use the Ag column as row names

# Create a bar plot of significant p-values (p < 0.05)
significant_chi_squared_df <- chi_squared_df[chi_squared_df$p_value < 0.05, ]
ggplot(significant_chi_squared_df, aes(x = Ag, y = p_value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Ag", y = "p-value") +
  theme_minimal()


#summarize for each ag, proportion and # of samples that are 3sd above for PANS and HC
pans.sd.summary <- data.frame()

# Get unique values of Ag
unique_ag <- unique(pans.only$Ag)

for (i in seq_along(unique_ag)) {
  ag_val <- unique_ag[i]
  
  subset_ag_pans <- pans.only[pans.only$Ag == ag_val & pans.only$Cat == "PANS", ] #subset samples that are PANS for current Ag iteration
  subset_ag_hc <- pans.only[pans.only$Ag == ag_val & pans.only$Cat == "HC", ] #same as above but for HC
  
  num_3sd_pans <- sum(subset_ag_pans$sdcompare == "> 5 SD", na.rm=T) #count # of pans samples that are >3sd
  num_3sd_hc <- sum(subset_ag_hc$sdcompare == "> 5 SD", na.rm=T) #count # of hc samples that are >3sd
  
  total_samples_pans <- sum(!is.na(subset_ag_pans$MFI)) #count total num of pans samples for current ag iteration
  total_samples_hc <- sum(!is.na(subset_ag_hc$MFI)) #count total num of pans samples for current ag iteration
  
  prop.pans.3sd <- num_3sd_pans / total_samples_pans * 100 #prop of pans >3sd
  prop.hc.3sd <- num_3sd_hc / total_samples_hc * 100 #prop of hc >3sd
  
  print.pans.ct <- paste0(num_3sd_pans, "/", total_samples_pans) #display number of pans >3sd for that ag out of total pans
  print.hc.ct <- paste0(num_3sd_hc, "/", total_samples_hc) #same as above but for hc
  
  df_pans <- data.frame(Ag = ag_val, Cat = "PANS", prop.3sd = prop.pans.3sd, num = total_samples_pans, ct = print.pans.ct)
  df_hc <- data.frame(Ag = ag_val, Cat = "HC", prop.3sd = prop.hc.3sd, num = total_samples_hc, ct = print.hc.ct)
  
  pans.sd.summary <- rbind(pans.sd.summary, df_pans, df_hc)
  
}

library(pheatmap)

######visualize MFI distribution for each Ag before and after scaling#######
# Create an empty list to store the plots (UNSCALED)
#write a generalized function for plotting density curves for each ag, using a matrix as input
plot.density.ag <- function(matrix.plot) {
  df.plot <- as.data.frame(matrix.plot) #converts your matrix into dataframe since ggplot doesn't handle matrices well
  plot.density <- list()
  
  for (Ag in names(df.plot)) { #Loop through each col/ag in the data frame and create a density plot of mfi distributions
    plot.l <- ggplot(df.plot, aes(x = .data[[Ag]])) +
      geom_density(fill = "blue", alpha = 0.5) +
      labs(x = Ag, y = "Density") +
      theme_minimal()
    
    plot.density[[Ag]] <- plot.l #add plots for each ag to a list
  }
  
  for (i in seq_along(plot.density)) { #print the plots, use arrow keys to navigate
    print(plot.density[[i]])
  }
  
  return(plot.density)
}

#set sample names as row names for avg.mfi
rownames(avg.mfi) <- sample.list

#list of control sera/rows
exclude_values <- c("HCT","HIS","HTG","HMS","HRN","HAS","HSC","MM","HC123")

#exclude control sera
pans.hc.matrix <- avg.mfi[!(rownames(avg.mfi) %in% exclude_values), ]

plot.density.ag(pans.hc.matrix) #unscaled, untransformed data

# Create an empty list to store the plots (LOG2 scaling)
log2.m <- log2(pans.hc.matrix) #inf values present due to 0...

center.data <- function(uncentered.matrix) {
  col_means <- colMeans(uncentered.matrix, na.rm=T) #get mean for each column (Ag). to change if row = Ag instead
  centered.matrix <- uncentered.matrix - col_means #center data
  
  return(centered.matrix)
}

log2.m.c <- center.data(log2.m) #this generates a TON of inf values...

plot.density.ag(log2.m) #uncentered
plot.density.ag(log2.m.c) #centered

#to add pseudocounts to avoid inf for log transformation
pseudoct <- 1 #making "0" values = 1... need to tweak and see which is better
p.avg.mfi <- pans.hc.matrix + pseudoct

log2.p <- log2(p.avg.mfi)


log2.c <- center.data(log2.p)
col_sd.1 <- apply(log2.p, 2, sd, na.rm=T) #get sd of each ag
log2.z <- log2.c / col_sd.1

plot.density.ag(log2.p) #uncentered
plot.density.ag(log2.c) #centered
plot.density.ag(log2.z) #centered/sd = Z-scored (standardized) -- right-skewed

###increase pseudocount to 100
pseudoct.100 <- 100 #making "0" values = 1... need to tweak and see which is better
p.avg.mfi.100 <- pans.hc.matrix + pseudoct.100

log2.p.100 <- log2(p.avg.mfi.100)

log2.c.100 <- center.data(log2.p.100)
col_sd.100 <- apply(log2.p.100, 2, sd, na.rm=T) #get sd of each ag
log2.z.100 <- log2.c.100 / col_sd.100

plot.density.ag(log2.p.100) #uncentered -- left skewed
plot.density.ag(log2.c.100) #centered  -- looks amazing
plot.density.ag(log2.z.100) #centered/sd = Z-scored (standardized) -- slightly worse than log2.c.100

###notes:
###centering the data generates a lot of inf values due to inf being calculated into antigen means. less inf values present when
###not centering.
###some antigens tend to follow a more normal distribution when centered, others don't...
###squaring is another method, but the distribution range tends to be a little too off

###evan's rec: add pseudovalues, and plot centered and uncentered log data to see how the heatmaps differ, can try z-scoring

#candidates: log2.c.100 seems to have the best normal distributions amongst antigens

print(nrow(pans.hc.matrix))
custom.sample.order <- c("126-0001-01","126-0002-01","126-0004-01","126-0005-01","126-0006-02","126-0017-03","126-0020-01","126-0022-01","126-0023-02","126-0024-01","126-0027-02","126-0029-01","126-0030-02","126-0032-02","126-0033-01","126-0035-02","126-0047-01","126-0049-01","126-0053-01","126-0057-01","126-0060-01","126-0063-01","126-0067-01","126-0071-02","126-0073-01","126-0080-01","126-0085-01","126-0088-02","126-0089-01","126-0092-01","126-0093-01","126-0093-04","126-0097-01","126-0099-01","126-0105-01","126-0106-01","126-0108-04","126-0110-04","126-0111-01","126-0113-02","126-0115-01","126-0117-01","126-0119-01","126-0120-01","126-0130-01","126-0136-02","126-0147-01","126-0147-03","126-0148-01","126-0149-01","126-0152-01","126-0154-02","126-0155-01","126-0156-01","126-0161-02","126-0165-01","126-0170-01","126-0172-01","77-0001-02","77-0006-13","77-0007-03","77-0007-29","77-0008-05","77-0009-01","77-0010-01","77-0010-02","77-0013-06","77-0014-01","77-0014-08","77-0016-01","77-0016-06","77-0019-01","77-0028-09","77-0032-01","77-0039-20","77-0040-02","77-0040-09","77-0041-01","77-0042-03","77-0047-38","77-0048-16","77-0067-37","77-0068-01","77-0068-11","77-0069-03","77-0071-10","77-0076-01","77-0079-06","77-0085-04","77-0090-02","77-0091-07","77-0092-04","77-0095-01","77-0096-03","77-0097-01","77-0102-04","77-0103-04","77-0107-04","77-0120-02","77-0120-28","77-0127-02","77-0140-02","77-0143-01","77-0146-01","77-0147-12","77-0148-03","77-0148-10",
                         "77-0150-02","77-0152-06","77-0153-06","77-0153-11","77-0155-01","77-0157-01","77-0158-02","77-0160-05","77-0162-02","77-0166-05","77-0170-02","77-0170-18","77-0171-02","77-0172-03","77-0178-02","77-0178-13","77-0181-02","77-0183-01","77-0183-81","77-0185-01","77-0186-01","77-0189-01","77-0193-01","77-0196-01","77-0198-06","77-0198-36","77-0205-05","77-0206-03","77-0206-42","77-0207-07","77-0207-37","77-0208-01","77-0208-13","77-0211-02","77-0214-01","77-0218-01","77-0218-33","77-0219-45","77-0226-01","77-0227-05","77-0228-02","77-0229-03","77-0231-01","77-0233-03","77-0235-01","77-0237-03","77-0239-01","77-0241-07","77-0241-21","77-0244-01","77-0251-03","77-0251-46","77-0252-81","77-0257-78","77-0261-56","77-0268-05","77-0269-91","77-0270-01","77-0275-35","77-0277-01","77-0280-25","77-0282-16","77-0282-31","77-0286-14","77-0293-19","77-0300-01","77-0301-40","77-0304-52","77-0311-34","77-0312-24","77-0317-13","77-0324-32","77-0324-37","77-0328-58","77-0338-01","77-0343-03","77-0346-01","77-0347-05","77-0353-06","77-0354-01","77-0355-02","77-0362-07","77-0362-15","77-0363-02","77-0363-23","77-0364-02","77-0365-01","77-0365-06","77-0370-05","77-0371-02","77-0373-01","77-0380-03","77-0387-01","77-0390-01","77-0391-01","77-0393-01","77-0393-02","77-0397-01","77-0398-01","77-0401-01","77-0402-01","77-0403-01","77-0403-17","77-0409-07","77-0411-01","77-0412-01","77-0419-07","77-0421-01","77-0422-02","77-0445-01","77-0456-02","77-0464-01",
                         "77-0482-01","77-0483-01","77-0489-01","77-0491-01","77-0499-01","77-1000-02","77-1006-01","77-1013-01","77-1014-01","77-1020-01","77-1027-01","111-0002-02","111-0012-01","111-0013-01","111-0017-01","111-0019-01","111-0020-01","111-0022-01","111-0023-01","111-0024-01","111-0028-01","111-0029-01","111-0035-01","111-0038-01","111-0039-01","111-0041-01","111-0042-02","111-0046-02","111-0047-01","111-0060-01","111-0069-03","111-0080-03","111-0502-01","111-0504-01","111-0506-01","111-0507-01","111-0508-01","111-0510-01","111-0514-01","111-0515-01","111-0524-01","111-0532-01","111-0545-01","111-0547-01","111-0549-01","111-0551-01","111-0552-01","111-0553-01","111-0554-01","111-0562-01","111-0568-01","111-0580-01","111-0582-01","111-0589-01","111-0594-01","111-0604-01","111-0608-01","111-0632-01","111-0634-02","111-0636-01","111-0640-02","111-0641-01","111-0648-02","111-0651-01","111-0652-01","111-0653-01","111-0654-01","111-0658-01","111-0666-02","111-0669-01","111-0675-01","111-0684-01","111-0685-01","111-0702-01","111-0703-01","111-0708-01","111-0710-04","111-0718-01","111-0728-01","111-0729-01","111-0734-01","111-0738-01","111-0740-01","111-0742-01","111-0745-01","111-0751-01","111-0752-01","111-0753-01","111-0768-01","111-0769-01","111-0770-01","126-0026-01","126-0043-01","126-0077-01","126-0083-01"
)


pans.hc.matrix <- pans.hc.matrix[custom.sample.order, ]
cat.anno = data.frame("Cat" = pans.only$Cat[1:314])
rownames(cat.anno) = rownames(pans.hc.matrix) #make sure sample order in pans.only matches pans.hc.matrix
annotation_row = cat.anno
ann_colors = list(Cat = c(PANS = "#ffb6a3", HC = "#b4e084"))

###dendro time!!
library(pheatmap)
cols_with_na <- apply(log2.c.100, 2, function(col) any(is.na(col))) #identify ag with NAs. unfort we have to remove them entirely
columns_with_na <- which(cols_with_na)
log2.c.100.1 <- log2.c.100[, !cols_with_na] #get a matrix without NA value to perform correlation clustering


pheatmap(log2.c.100.1,
         clustering_distance_rows = "correlation", #can't get any pts to cluster by themselves
         clustering_distance_cols = "correlation",
         annotation_row = annotation_row,
         annotation_colors = ann_colors
         )

library(reshape2)

sd.matrix <- dcast(pans.only, Sample ~ Ag, value.var = "sdcompare") #convert 3SD values into matrix
rownames(sd.matrix) <- sd.matrix[,1] #set sample names as rownames
sd.matrix <- sd.matrix[,-1] #remove extra col

cols_with_na <- apply(sd.matrix, 2, function(col) any(is.na(col))) #identify ag with NAs. unfort we have to remove them entirely
columns_with_na <- which(cols_with_na)
sd.matrix.1 <- sd.matrix[, !cols_with_na] #get a matrix without NA value to perform correlation clustering

sd.matrix.2 <- ifelse(sd.matrix.1 == "< 3 SD", 0, 1) #convert matrix to binary

# Define custom colors and labels for the legend
legend_colors <- c("0" = "black", "1" = "red")
legend_labels <- c("< 3 SD", "> 3 SD")

pheatmap(sd.matrix.2,
         clustering_distance_rows = "binary",
         clustering_distance_cols = "binary",
         annotation_row = annotation_row,
         annotation_colors = ann_colors,
         color = c("black", "red"),
         legend_breaks = c(0, 1),  # Define breaks for the legend
         legend_labels = legend_labels,  # Specify custom legend labels
         legend_color = legend_colors # Specify custom legend colors
         #cutree_rows = 5,
         #cutree_cols = 5
)

# Calculate distances for rows and columns
row_distances <- dist(sd.matrix.2, method = "binary")
col_distances <- dist(t(sd.matrix.2), method = "binary")

# Reorder rows and columns based on distance
row_order <- order.dendrogram(as.dendrogram(hclust(row_distances)))
col_order <- order.dendrogram(as.dendrogram(hclust(col_distances)))

# Apply the row and column orders to the heatmap
pheatmap(
  sd.matrix.2[row_order, col_order],
  annotation_row = annotation_row,
  annotation_colors = ann_colors,
  color = c("black", "red"),
  legend_breaks = c(0, 1),  # Define breaks for the legend
  legend_labels = legend_labels,  # Specify custom legend labels
  legend_color = legend_colors # Specify custom legend colors
)


###pca
library(ggfortify)

cat.pans.hc.list <- c(
  "PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS",
  "PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS",
  "PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS",
  "PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS",
  "PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS",
  "PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS",
  "PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS",
  "PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS",
  "PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS",
  "PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS",
  "PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS",
  "PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS",
  "PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","PANS","HC","HC","HC","HC","HC","HC",
  "HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC",
  "HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC",
  "HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC","HC"
)

matrix.w.cat <- cbind(log2.c.100.1, cat.pans.hc.list)
df.w.cat <- as.data.frame(matrix.w.cat)

str(df.w.cat) #check that the ag columns are numeric vals
df.w.cat[, 1:89] <- sapply(df.w.cat[, 1:89], as.numeric) #convert ag cols to numeric

pans.pca.stats <- prcomp(df.w.cat[,c(1:89)]) 

pans.pca <- autoplot(pans.pca.stats, 
                     data = df.w.cat, 
                     colour = 'cat.pans.hc.list'
                     ) 

pans.pca

#retrieve antigen list
ag_list <- pans.only$Ag
ag_list <- lapply(ag_list, function(x) sort(x))

#remove duplicates in ag_list
ag_list <- ag_list[!duplicated(ag_list)]

# combine the list elements into a vector
ag_list <- unlist(ag_list)
ag_list <- as.factor(ag_list)
ag_list <- sort(ag_list)

#generate a vector (1 to n) of the n # of antigens in ag_list
num_ag <- seq_along(ag_list)

#split df into list of dataframes for each antigen
df_list <- split(pans.only, pans.only$Ag)
df_list <- as.array(df_list)

#pulls out list of categories and samples (which is the same across antigens)
test <- df_list[[1]]
cat_list <- test[,4]
sample_list <- test[,3]

#remove duplicates in the lists
cat_list <- cat_list[!duplicated(cat_list)]
sample_list <- sample_list[!duplicated(sample_list)]

#convert character vector into factor
cat_list <- as.factor(cat_list) 
sample_list <- as.factor(sample_list)
test$Cat <- as.factor(test$Cat)

#check if these objects are factors, if not, convert to factors. They need to be factors so we can pull the levels from them
is.factor(cat_list) 
is.factor(sample_list)
is.factor(test$Cat)
is.list(df_list)

# define an empty list to store the adapted arrays for graphpad
final.ag.list <- vector("list", length = length(df_list))

names(final.ag.list) <- names(df_list)


for (k in seq_along(df_list)) { #goes thru each dataframe for antigen in dm_list @iteration k
  final.ag.list[[k]] <- matrix(nrow=length(sample_list), ncol=length(cat_list)) #creates matrix for each antigen @ iteration k
  ag.holder <- df_list[[k]] #temporarily stores values for iteration k
  for (i in seq_len(nrow(ag.holder))) { # loop through each row (sample) for specific antigen
    cat_match <- match(ag.holder$Cat[[i]], cat_list)  # find the index of cat_list that matches the sample category
    if (!is.na(cat_match)) {  # if the category was found in cat_list
      final.ag.list[[k]][i, cat_match] <- ag.holder$MFI[i] # insert the MFI value of sample into corresponding column
    }
  }
  
  mat <- final.ag.list[[k]]
  rownames(mat) <- sample_list
  colnames(mat) <- cat_list
  final.ag.list[[k]] <- mat
  
  #replacing illegal characters (i.e. > if present in raw data)
  rownames(final.ag.list[[k]]) <- sub(">", "&gt;", rownames(final.ag.list[[k]])) # Replace ">" with "&gt;"
  colnames(final.ag.list[[k]]) <- sub(">", "&gt;", colnames(final.ag.list[[k]]))
  
}


###time to make my txt file to be inserted into a .pzf file!##
##First, create a file called pzf port.txt##

#edit number of tables
table_list <- c('<TableSequence>')
print(table_list)
for (i in seq_along(ag_list)) {
  table_list <- c(table_list, paste0('<Ref ID = "Table',i,'"/>'))
}

print(table_list)

table_list <- c(table_list, '</TableSequence>') 

#now for the table itself
table_content <- character()

#for each antigen, print the list of samples within each antigen block
for (i in seq_along(ag_list)) {
  
  sample_str <- paste0("<d>", sample_list, "</d>\n", collapse = "") #pastes a list of the samples in <d> tags
  
  mfi_table_content <- character() 
  
  for (k in seq_len(ncol(final.ag.list[[i]]))) { #goes through mfi values under each category for current iteration (antigen)
    mfi_name <- paste0('<YColumn Width="125" Decimals="1" Subcolumns="1">\n', #this chunk pastes the category name
                       "<Title>",colnames(final.ag.list[[i]])[k],"</Title>\n",
                       "<Subcolumn>\n")
    mfi_values <- final.ag.list[[i]][, k]
    mfi_str <- ""
    for (j in seq_along(mfi_values)) {
      if (is.na(mfi_values[j])) {
        mfi_str <- paste0(mfi_str, "<d/>\n") #if there is no value present (patient belongs to other cats), paste <d/>
      } else {
        mfi_str <- paste0(mfi_str, "<d>", mfi_values[j], "</d>\n") #if value is present, add <d> tags
      }
    }
    mfi_str <- paste0(mfi_str, '</Subcolumn>\n','</YColumn>\n') #add closing tags for the category
    mfi_table_content[[k]] <- paste0(mfi_name, mfi_str)
  }
  
  
  mfi_table_str <- paste(mfi_table_content, collapse = "")
  ag_table_content <- paste0('<Table ID="Table',i,'" XFormat="none" TableType="OneWay" EVFormat="AsteriskAfterNumber">\n',
                             '<Title>', ag_list[i], '</Title>\n',
                             '<RowTitlesColumn Width="151">\n',
                             '<Subcolumn>\n', sample_str, '</Subcolumn>\n',
                             '</RowTitlesColumn>\n',
                             mfi_table_str,'</Table>')
  
  table_content <- c(table_content, ag_table_content)
}

final.output <- c(table_list,table_content)
write.table(final.output, "9.26.23 pans ana cc.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)
