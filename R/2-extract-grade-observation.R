#' Extract the observations of Grade.
#'
#' @file 2-extract-grade-observation.R
#' @author Mariko Ohtsuka
#' @date 2021.11.24
# ------ settings ------
kInputDirPath <- '~/Documents/GitHub/SDTM-Central-Monitoring/TEST/temp/'
kInputFileName <- 'getVisit.csv'
kOutputDirpath <- ''  # If it is blank, it is treated as the same as the path set in the "kInputDirPath" variable.
kOutputFileName <- 'extract-grade-observation.csv'
kCriteriaVariable <- 'FATESTCD'
kGradeCriteria <- 'GRADE'
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
# ------ Main ------
# For test
if (exists('exec_test')){
  SetSettingsForTest(exec_test)
}
# If not specified, it will use the same path as 'kInputDirPath'.
kOutputDirpath <- ifelse(kOutputDirpath != '', kOutputDirpath, kInputDirPath)
# Read csv.
input_fa <- ReadTargetCsv(kInputDirPath, kInputFileName)
if (!is.data.frame(input_fa)){
  stop(print('The input file was not found. Please check the path specification of the input file.'))
}
# Extraction of target column.
colnames(input_fa) <- toupper(colnames(input_fa))
outputColnames <- c('USUBJID', 'FAOBJ', 'FAORRES', 'VISIT', 'VISITNUM', toupper(kCriteriaVariable))
target_df <- input_fa[ , colnames(input_fa) %in% outputColnames]
output_df <- subset(target_df, target_df[ , toupper(kCriteriaVariable)] == kGradeCriteria)
output_df <- output_df[ , colnames(output_df) != toupper(kCriteriaVariable)]
WriteOutputCsv(output_df, kOutputDirpath, kOutputFileName)
