# title: "File Compare"
# author: "Kristy Wedel"
# date: "12/3/2019"
# output: html_document


# Intro -------------------------------------------------------------------

# This is a script to compare dataframes. Input data can be from xlsx or csv files. 
# It is intended to be run piece by piece to identify any issues that may need to be resolved along the way.

if(!require('readxl')){ install.packages('readxl', repos='http://cran.us.r-project.org' ,lib=.libPaths()) }
if(!require('tidyverse')){ install.packages('tidyverse', repos='http://cran.us.r-project.org' ,lib=.libPaths()) }
if(!require('rlist')){ install.packages('rlist', repos='http://cran.us.r-project.org' ,lib=.libPaths()) }
if(!require('compareDF')){ install.packages('compareDF', repos='http://cran.us.r-project.org' ,lib=.libPaths()) }
if(!require('htmlTable')){ install.packages('htmlTable', repos='http://cran.us.r-project.org' ,lib=.libPaths()) }
if(!require('lubridate')){ install.packages('lubridate', repos='http://cran.us.r-project.org' ,lib=.libPaths()) }
library(readxl)
library(tidyverse)
library(rlist)
library(compareDF)
library(htmlTable)
library(lubridate)


# Pick Files --------------------------------------------------------------

# Pick the files to compare to. These files can be csv or xlsx.

a <- choose.files(caption = "Choose a csv or xlsx file.")
b <- choose.files(caption = "Choose a csv or xlsx file to compare to.")

test_a <- str_sub(a, start= -4)
test_b <- str_sub(a, start= -4)
if (test_a == '.csv'){
  a_df <- read.csv(a)
}else if (test_a == 'xlsx'){
  a_df <- read_excel(a)
}else{
  a_df <- NULL
}
if (test_b == '.csv'){
  b_df <- read.csv(a)
}else if (test_b == 'xlsx'){
  b_df <- read_excel(b)
}else{
  b_df <- NULL
}

rm(a,b,test_a,test_b)

# Compare Fields ----------------------------------------------------------

# This section compares the fields in each dataset to see if they match. 
# If they do not, it gives some examples on how this could be resolved.

a_names <- names(a_df)
b_names <- names(b_df)

bnotanames <- b_names[!(b_names %in% a_names)]
anotbnames <- a_names[!(a_names %in% b_names)]
if(length(anotbnames)>0){
  output <- paste0("Fields in a not b '", anotbnames, "'.")
  print(output)
}
if(length(bnotanames)>0){
  output <- paste0("Fields in b not a '", bnotanames, "'.")
  print(output)
}

#rename fields if needed
#myvars <- c("asin","Title1","Manufacturer1","Brand1","Category1")
#names(a) <- myvars

#remove fields in one not other.
nameslist <- intersect(a_names, b_names)
a_df <- a_df %>% select(nameslist)
b_df <- b_df %>% select(nameslist)

rm(anotbnames,bnotanames,a_names,b_names)

# Compare Rows ------------------------------------------------------------

# This section compares the # rows in each dataset to see if they match.

anrow <- nrow(a_df)
bnrow <- nrow(b_df)

if(anrow != bnrow){
  print("Number of rows between a and b are different.")
  print(paste0("Number of rows in a ", anrow, "."))
  print(paste0("Number of rows in b ", bnrow, "."))
}

rm(anrow,bnrow)

# Compare Data Types ------------------------------------------------------

# This section compares the data types of fields. If they do not match, the data may not be able to be compared unless the data types are modified. 
# Conversion examples for numeric, character and dates are given.

a_types <- sapply(a_df, class)
b_types <- sapply(b_df, class)

bnotatypes <- b_types[!(b_types %in% a_types)]
anotbtypes <- a_types[!(a_types %in% b_types)]

if(length(anotbtypes) > 0){
  print(paste0("Type mismatch ", anotbtypes, "."))
}
if(length(bnotatypes) > 0){
  print(paste0("Type mismatch ", bnotatypes, "."))
}

#type conversion examples
#as.numeric()
#as.character()
#library(lubridate)
#mdy()
#ymd()

rm(anotbtypes,bnotatypes,a_types,b_types)

# Identify Discrepancies HTML --------------------------------------------------

# Compare the data records to identify discrepancies with color coded HTML output.

# #enter the dataset primary key
print("Enter the dataset primary key.")
print("If multiple fields are required, add a comma and space in between fields.")
thekey <- readline(prompt="Enter Key:  ")
thekeylist <- unlist(strsplit(thekey, ", "))
excl_vals <- readline(prompt="Enter fields to exclude or press enter:  ")

#check primary key
primkeya <- a_df %>% select(thekeylist) %>% distinct(.)
primkeyb <- b_df %>% select(thekeylist) %>% distinct(.)

#is the primary key unique?
primkeyau <- a_df %>% select(thekeylist)
dupvalsa <- primkeyau[duplicated(primkeyau),]
primkeybu <- b_df %>% select(thekeylist)
dupvalsb <- primkeybu[duplicated(primkeybu),]
if(nrow(primkeya) != nrow(primkeyau)){
  print("Key is not unique in dataset a.")
}

if(nrow(primkeyb) != nrow(primkeybu)){
  print("Key is not unique in dataset b.")
}

#do both datasets have the same primary key records?
diffs1a <- anti_join(primkeya, primkeyb) %>% mutate(from = "a")
diffs1b <- anti_join(primkeyb, primkeya) %>% mutate(from = "b")
primkeydiff <- diffs1a
primkeydiff <- bind_rows(primkeydiff, diffs1b)

#open differences
if(nrow(primkeydiff)>0){
  View(primkeydiff)
}

rm(diffs1a,diffs1b,primkeyau,primkeybu,dupvalsa, dupvalsb,primkeya,primkeyb)

ctable_output <- compare_df(a_df, b_df, thekeylist, limit_html = 1000, exclude = excl_vals)
ctable_output$html_output
#gives overall summary
#ctable_output$change_summary
#dataframe with changes
#ctable_output$comparison_df


# Identify Discrepancies Data Table ---------------------------------------

# Compare the data records with side by side output.

# #enter the dataset primary key
print("Enter the dataset primary key.")
print("If multiple fields are required, add a comma and space in between fields.")
thekey <- readline(prompt="Enter Key:  ")
thekeylist <- unlist(strsplit(thekey, ", "))
excl_vals <- readline(prompt="Enter fields to exclude or press enter:  ")

#check primary key
primkeya <- a_df %>% select(thekeylist) %>% select(-excl_vals) %>% distinct(.)
primkeyb <- b_df %>% select(thekeylist) %>% select(-excl_vals) %>% distinct(.)

#is the primary key unique?
primkeyau <- a_df %>% select(thekeylist)
dupvalsa <- primkeyau[duplicated(primkeyau),]
primkeybu <- b_df %>% select(thekeylist)
dupvalsb <- primkeybu[duplicated(primkeybu),]
if(nrow(primkeya) != nrow(primkeyau)){
  print("Key is not unique in dataset a.")
}

if(nrow(primkeyb) != nrow(primkeybu)){
  print("Key is not unique in dataset b.")
}

#do both datasets have the same primary key records?
diffs1a <- anti_join(primkeya, primkeyb) %>% mutate(from = "a")
diffs1b <- anti_join(primkeyb, primkeya) %>% mutate(from = "b")
primkeydiff <- diffs1a
primkeydiff <- bind_rows(primkeydiff, diffs1b)

#open differences
if(nrow(primkeydiff)>0){
  View(primkeydiff)
}

rm(diffs1a,diffs1b,primkeyau,primkeybu,dupvalsa, dupvalsb,primkeya,primkeyb)

outputdf <- NULL
namesdiff <- nameslist[nameslist != thekeylist]
for (i in namesdiff)
{
  thekeylistcp <- thekeylist
  vars <- list.append(thekeylistcp, i)
  df_subseta <- a_df %>% select(vars)
  df_subsetb <- b_df %>% select(vars)
  #converting to char avoid type conversion issues
  df_subseta[i] <- as.character(df_subseta[i])
  df_subsetb[i] <- as.character(df_subsetb[i])
  diffs1 <- anti_join(df_subseta, df_subsetb, by = vars)
  diffs2 <- anti_join(df_subsetb, df_subseta, by = vars)
  outputdftemp <- diffs1
  outputdftemp <- bind_rows(outputdftemp, diffs2)
  #key, field, new, old
  names(df_subseta) <- list.append(thekeylist, "a_val")
  names(df_subsetb) <- list.append(thekeylist, "b_val")
  outputdftemp <- outputdftemp %>% select(thekeylist) %>% mutate(fieldchanged = i)
  outputdftemp <- left_join(outputdftemp, df_subseta, by = thekeylist)
  outputdftemp <- left_join(outputdftemp, df_subsetb, by = thekeylist)
  if(!is.null(outputdf)){
    outputdf <- bind_rows(outputdf, outputdftemp)
  }else{
    outputdf <- outputdftemp
  }
}
#open differences
if(nrow(outputdf)>0){
  View(outputdf)
}
