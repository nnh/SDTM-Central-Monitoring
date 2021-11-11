#' Aggregate the number of cases by grade.
#'
#' @file 3-summarize-by-grade.R
#' @author Mariko Ohtsuka
#' @date 2021.11.11
# ------ settings ------
kInputDirPath <- '/Users/mariko/Documents/GitHub/SDTM-Central-Monitoring/TEST/temp/'
kInputFileName <- 'extract-grade-observation.csv'
kOutputDirpath <- ''  # If it is blank, it is treated as the same as the path set in the "kInputDirPath" variable.
kOutputFileName <- 'summarize-by-grade'
kMinVisitnum <- NA
kMaxVisitnum <- NA
kExcludeVisitnum <- NA
kTargetGrade <- c(3, 4, 5)
kPercentDigit <- 1
kArmColname <- NA  # unused
# ------ constants ------
# ------ functions ------
#' @title readCsvSetEncoding
#' @description Reads a file with the specified encoding.
#' @param target_file Full path of the target file.
#' @param target_encoding Encoding to specify.
#' @return Returns a data frame. If the file fails to read, NA is returned.
readCsvSetEncoding <- function(target_file, target_encoding){
  temp <- tryCatch(
    read.csv(target_file, as.is=T, fileEncoding=target_encoding, stringsAsFactors=F, na.strings=""),
    warning = function(e){ return(NA) }
  )
  return(temp)
}
#' @title RemoveLastBackslash
#' @description If the string of the specified file path ends with '\', remove the '\'.
#' @param input_path The path string.
#' @return String of the path with trailing backslashes removed.
RemoveLastBackslash <- function(input_path){
  temp_path <- ifelse(substr(input_path, nchar(input_path), nchar(input_path)) == '/',
                      substr(input_path, 1, nchar(input_path) - 1),
                      input_path)
  return(temp_path)
}
#' @title ReadTargetCsv
#' @description Read csv.
#' @param input_path The path where the CSV file is located.
#' @param target_path The folder path where the target file resides.
#' @param filename Target file name.
#' @return Returns a data frame. If the file fails to read, NA is returned.
ReadTargetCsv <- function(input_path, filename){
  temp_path <- RemoveLastBackslash(input_path)
  target <- file.path(temp_path, filename)
  temp <- readCsvSetEncoding(target, 'utf-8')
  if (!is.data.frame(temp)){
    temp <- readCsvSetEncoding(target, 'cp932')
  }
  if (!is.data.frame(temp)){
    temp <- readCsvSetEncoding(target, 'UTF-8-BOM')
  }
  return(temp)
}
#' @title WriteOutputCsv
#' @description Write csv.
#' @param df The data frame for output.
#' @param input_path The path to output CSV.
#' @param filename Name of the CSV.
#' @return none
WriteOutputCsv <- function(df, input_path, filename){
  write.table(df, paste0(input_path, '/', filename), , row.names=F, append=F, sep=',', na='""')
}
#' @title GetToxicityList
#' @description Aggregate the values of FAOBJ.Sort FAORRES in descending order of frequency of occurrence.
#' @param input_df A data source.
#' @return data frame.
GetToxicityList <- function(input_df){
  toxicity <- unique(input_df$FAOBJ)
  toxicity_count <- sapply(toxicity, function(x){
    targetToxicity <- subset(input_df, FAOBJ == x & FAORRES >= 3)
    return(nrow(targetToxicity))
  })
  df_toxicity <- data.frame(toxicity, toxicity_count)
  df_toxicity <- df_toxicity[order(df_toxicity$toxicity_count, decreasing=T, df_toxicity$toxicity), ]
  df_toxicity <- df_toxicity[ , 1, drop=F]
  rownames(df_toxicity) <- 1:nrow(df_toxicity)
  return(df_toxicity)
}
#' @title SetNameToList
#' @description Create a named list.
#' @param ... Variables to list.
#' @return a list.
SetNameToList <- function(...){
  return(setNames(list(...), eval(substitute(alist(...)))))
}
#' @title DivideDataFrameToList
#' @description The data frames extracted according to the conditions are stored in a list and returned.
#' @param input_df A data source.
#' @param target_colname Column name of the extraction condition.
#' @return List of extraction results and extraction condition strings.
DivideDataFrameToList <- function(input_df, target_colname){
  target_str <- unique(input_df[ , target_colname])
  df_to_list <- lapply(target_str, function(x){
    return(subset(input_df, input_df[ , target_colname] == x))
  })
  names(df_to_list) <- target_str
  return(list(df_to_list, target_str))
}
# ------ Main ------
# If not specified, it will use the same path as 'kInputDirPath'.
kOutputDirpath <- ifelse(kOutputDirpath != '', kOutputDirpath, kInputDirPath)
# Read csv.
input_fa <- ReadTargetCsv(kInputDirPath, kInputFileName)
if (!is.data.frame(input_fa)){
  stop(print('The input file was not found. Please check the path specification of the input file.'))
}
df_toxicity <- GetToxicityList(input_fa)
visitnumInfo <- SetNameToList(kMinVisitnum, kMaxVisitnum, kExcludeVisitnum)
summarizeConditions <- c(visitnumInfo, SetNameToList(kTargetGrade), SetNameToList(kArmColname), SetNameToList(kPercentDigit))

SummarizeByVisit <- function(input_df, summarizeConditions){
  target_df <- input_df
  list_by_visit <- DivideDataFrameToList(target_df, 'VISITNUM')
  list_df_by_visit <- list_by_visit[[1]]
  a <- lapply(list_df_by_visit, SummarizeByToxicity, summarizeConditions)
  return(a)
}
SummarizeByToxicity <- function(input_df, summarizeConditions){
  target_df <- input_df
  list_by_toxycity <- DivideDataFrameToList(target_df, 'FAOBJ')
  list_df_by_toxycity <- list_by_toxycity[[1]]
  df_by_toxicity <- lapply(list_df_by_toxycity, CountToxicityByGrade, summarizeConditions)
  print(df_by_toxicity)
  return(df_by_toxicity)
}
CountToxicityByGrade <- function(input_df, summarizeConditions){
  target_df <- input_df
  n <- nrow(target_df)
  count_per_by_grade <- lapply(summarizeConditions[['kTargetGrade']], CountByGrade, target_df, summarizeConditions, n)
  return(c(unique(target_df$FAOBJ), n, count_per_by_grade))
}
CountByGrade <- function(grade_value, input_df, summarizeConditions, n){
  digit <- summarizeConditions[['kPercentDigit']]
  count <- nrow(subset(input_df, input_df$FAORRES == grade_value))
  percent <- round((count / n) * 100, digits=digit)
  return(c(count, percent))
}
a <- SummarizeByVisit(input_fa, summarizeConditions)
