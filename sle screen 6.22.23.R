library(ggplot2)
library(ggthemr)
library(tidyverse)
library(dplyr)
library(viridis)

#import file and create a dataframe df from csv file
df <- read.csv("sle comp 2.csv", stringsAsFactors = F, encoding="UTF-8")

#https://delim.co/ to get a string of characters
#input sample list as a vector - NOTE: if list is not in alphabetical order, check if the final list order aligns with the matrix row titles

sample.list <- c(
  "A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14","A15","A16","A17","A18","A19","A20","A21","A22","A23","A24","B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","B13","B14","B15","B16","B17","B18","B19","B20","B21","B22","B23","B24","C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","D1","D2","D3","D4","D5","D6","D7","D8","D9","D10","D11","D12","D13","D14","D15","D16","D17","D18","D19","D20","D21","D22","D23","D24","E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","E13","E14","E15","E16","E17","E18","E19","E20","E21","E22","E23","E24","F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","G1","G2","G3","G4","G5","G6","G7","G8","G9","G10","G11","G12","G13","G14","G15","G16","G17","G18","G19","G20","G21","G22","G23","G24","H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12","H13","H14","H15","H16","H17","H18","H19","H20","H21","H22","H23","H24","I1","I2"
)
is.factor(sample.list) #check if sample.list is a factor
sample.list <- as.factor(sample.list) #converts character vector to factor so that levels can be rearranged
sample.list <- factor(sample.list, levels = c(sample.list <- c(
  "A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14","A15","A16","A17","A18","A19","A20","A21","A22","A23","A24","B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","B13","B14","B15","B16","B17","B18","B19","B20","B21","B22","B23","B24","C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","D1","D2","D3","D4","D5","D6","D7","D8","D9","D10","D11","D12","D13","D14","D15","D16","D17","D18","D19","D20","D21","D22","D23","D24","E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","E13","E14","E15","E16","E17","E18","E19","E20","E21","E22","E23","E24","F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","G1","G2","G3","G4","G5","G6","G7","G8","G9","G10","G11","G12","G13","G14","G15","G16","G17","G18","G19","G20","G21","G22","G23","G24","H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12","H13","H14","H15","H16","H17","H18","H19","H20","H21","H22","H23","H24","I1","I2"
)))

#input ag list as a vector (remove bare beads)
ag.list <- c(
  "C1-direct","C1q-direct","C2-cleaved","C2-pure","C4-cleaved","C4-pure","Factor P","Factor H","Factor I","C4BP","C1-inhibitor","C3b","C4b","C3bBb","C3bBb3b","C4b2b","C4b2b3b","Factor B","Factor D"
)
ag.list <- as.factor(ag.list) #converts character vector to factor so that levels can be rearranged
ag.list <- factor(ag.list, levels = c("C1-direct","C1q-direct","C2-cleaved","C2-pure","C4-cleaved","C4-pure","Factor P","Factor H","Factor I","C4BP","C1-inhibitor","C3b","C4b","C3bBb","C3bBb3b","C4b2b","C4b2b3b","Factor B","Factor D"
))

#set the rownames as the list of samples
rownames(df) <- sample.list

#remove the "sample" column
df <- df[,-1]

#replace NaN with 0
df.0 <- df
df.0[is.na(df.0)] <- 0

#correct the antigen column titles
colnames(df.0) <- ag.list

#subset df.0 into odd and even rows (cells are repeated in duplicates with different bead IDs/rep) - A1 = even, A2 = odd, so on...
odd_rows <- df.0[c(TRUE, FALSE), ]
even_rows <- df.0[c(FALSE, TRUE), ]

#define columns to replace (we will replace bead ID values of odd wells with that of the beads used in col 2)
cols.replace <- c("C4-cleaved","C4-pure","Factor P","C1-inhibitor","C4b","C3bBb","C3bBb3b","C4b2b","C4b2b3b","Factor B")

# Get column indices based on column names
column_indices <- match(cols.replace, colnames(odd_rows))

# Replace data from beads in even rows to the same beads in odd rows
odd_rows[, column_indices] <- even_rows[, column_indices]

df.2 <- odd_rows
df.2 <- as.data.frame(df.2)

#append all columns to a single column
df.3 <- stack(df.2)

#update sample list, make sure it is in the same well order
sample.list <- c(
  "HC4","SLE 9","HC11","SLE 16","SLE 17","HC24","SLE 26","SLE 28","SLE 37","SLE 54","SLE 57","SLE 60","SLE 76","SLE 79","SLE 90","SLE 96","SLE 110","SLE 127","SLE 128","SLE 141","SLE 146","SLE 153","SLE 164","SLE 170","SLE 174","SLE 182","SLE 193","SLE 194","SLE 200","SLE 219","SLE 223","SLE 225","SLE 226","SLE 227","SLE 228","SLE 232","SLE 241","SLE 246","SLE 249","SLE 269","SLE 304","SLE 310","SLE 321","SLE 326","SLE 327","SLE 328","SLE 341","SLE 396","SLE 407","SLE 414","SLE 416","SLE 419","SLE 421","SLE 441","SLE 445","SLE 521","SLE 543","SLE 552","SLE 572","SLE 619","SLE 627","SLE 663","SLE 667","SLE 669","SLE 673","SLE 684","SLE 723","SLE 755","SLE 769","SLE 785","SLE 797","SLE 804","SLE 809","SLE 811","SLE 818","SLE 829","SLE 847","SLE 859","SLE 871","SLE 889","SLE 896","SLE 931","SLE 932","SLE 936","SLE 950","SLE 967","SLE 971","SLE 986","SLE 1018","SLE 1043","SLE 1044","SLE 1102","SLE 1104","SLE 1105","SLE 1115","SLE 1129","Control 1 NS")
is.factor(sample.list) #check if sample.list is a factor
sample.list <- as.factor(sample.list) #converts character vector to factor so that levels can be rearranged
sample.list <- factor(sample.list, levels = c(sample.list <- c(
  "HC4","SLE 9","HC11","SLE 16","SLE 17","HC24","SLE 26","SLE 28","SLE 37","SLE 54","SLE 57","SLE 60","SLE 76","SLE 79","SLE 90","SLE 96","SLE 110","SLE 127","SLE 128","SLE 141","SLE 146","SLE 153","SLE 164","SLE 170","SLE 174","SLE 182","SLE 193","SLE 194","SLE 200","SLE 219","SLE 223","SLE 225","SLE 226","SLE 227","SLE 228","SLE 232","SLE 241","SLE 246","SLE 249","SLE 269","SLE 304","SLE 310","SLE 321","SLE 326","SLE 327","SLE 328","SLE 341","SLE 396","SLE 407","SLE 414","SLE 416","SLE 419","SLE 421","SLE 441","SLE 445","SLE 521","SLE 543","SLE 552","SLE 572","SLE 619","SLE 627","SLE 663","SLE 667","SLE 669","SLE 673","SLE 684","SLE 723","SLE 755","SLE 769","SLE 785","SLE 797","SLE 804","SLE 809","SLE 811","SLE 818","SLE 829","SLE 847","SLE 859","SLE 871","SLE 889","SLE 896","SLE 931","SLE 932","SLE 936","SLE 950","SLE 967","SLE 971","SLE 986","SLE 1018","SLE 1043","SLE 1044","SLE 1102","SLE 1104","SLE 1105","SLE 1115","SLE 1129","Control 1 NS")))

df.3$Sample <- sample.list

#rename the factor titles in dataframe
names(df.3)[names(df.3) == "ind"] <- "Ag"
names(df.3)[names(df.3) == "values"] <- "MFI"

df.3$Cat <- ""
df.3$Cat <- ifelse(grepl("HC", levels(df.3$Sample)), "HC",
                   ifelse(grepl("SLE", levels(df.3$Sample)), "SLE",
                          ifelse(grepl("Control", levels(df.3$Sample)), "No Serum", "")))


#rearrange samples so HC and controls are on the left
df.3$Sample <- factor(df.3$Sample, levels = c(df.3$Sample <- c(
  "Control 1 NS","HC4","HC11","HC24","SLE 9","SLE 16","SLE 17","SLE 26","SLE 28","SLE 37","SLE 54","SLE 57","SLE 60","SLE 76","SLE 79","SLE 90","SLE 96","SLE 110","SLE 127","SLE 128","SLE 141","SLE 146","SLE 153","SLE 164","SLE 170","SLE 174","SLE 182","SLE 193","SLE 194","SLE 200","SLE 219","SLE 223","SLE 225","SLE 226","SLE 227","SLE 228","SLE 232","SLE 241","SLE 246","SLE 249","SLE 269","SLE 304","SLE 310","SLE 321","SLE 326","SLE 327","SLE 328","SLE 341","SLE 396","SLE 407","SLE 414","SLE 416","SLE 419","SLE 421","SLE 441","SLE 445","SLE 521","SLE 543","SLE 552","SLE 572","SLE 619","SLE 627","SLE 663","SLE 667","SLE 669","SLE 673","SLE 684","SLE 723","SLE 755","SLE 769","SLE 785","SLE 797","SLE 804","SLE 809","SLE 811","SLE 818","SLE 829","SLE 847","SLE 859","SLE 871","SLE 889","SLE 896","SLE 931","SLE 932","SLE 936","SLE 950","SLE 967","SLE 971","SLE 986","SLE 1018","SLE 1043","SLE 1044","SLE 1102","SLE 1104","SLE 1105","SLE 1115","SLE 1129"
  )))

p <- ggplot(df.3, aes(x=Sample, y=Ag)) + 
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
  ggtitle("Anti-Complement Autoantibodies SLE screening") +     
  scale_fill_gradientn(colours = c("black","blue","green","yellow","red"),
  values = c(0,0.25,0.5,0.75,1), 
  limits=c(0, 12000), 
  oob = scales::squish) 
  #geom_text(aes(label=MFI), color = "white", size = 7) 
  #scale_fill_viridis(direction=-1, option = "cividis")
p
xp <-  p + #100x22 
  geom_text(aes(label = sprintf("%.2f", MFI)), color = "white", size = 5)

xp

##GRAPHPAD PORT##
library(dplyr)
library(stringr)

#import file and create a dataframe df from csv file
df <- subset(df.3, Cat %in% c(
  "HC","SLE" #type in categories you want to include in this line
))

#retrieve antigen list
ag_list <- df$Ag
ag_list <- lapply(ag_list, function(x) sort(x))

#remove duplicates in ag_list
ag_list <- ag_list[!duplicated(ag_list)]

# combine the list elements into a vector
ag_list <- unlist(ag_list)
is.list(ag_list)
is.factor(ag_list)
ag_list <- as.factor(ag_list)
ag_list <- sort(ag_list)

#generate a vector (1 to n) of the n # of antigens in ag_list
num_ag <- seq_along(ag_list)

#split df into list of dataframes for each antigen
df_list <- split(df, df$Ag)
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
write.table(final.output, "table_comp_sle_screen.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

