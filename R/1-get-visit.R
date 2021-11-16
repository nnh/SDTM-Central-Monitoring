#' Derive VISIT from VISITNUM.
#'
#' @file get-visit.R
#' @author Mariko Ohtsuka
#' @date 2021.11.16
# ------ settings ------
kInputDirPath <- '~/Documents/GitHub/SDTM-Central-Monitoring/TEST/temp/'
kInputFileName <- 'rawFA.csv'
kExternalDirPath <- ''  # If it is blank, it is treated as the same as the path set in the "kInputDirPath" variable.
kExternalFileName <- 'rawFA.csv'
kOutputDirpath <- ''  # If it is blank, it is treated as the same as the path set in the "kInputDirPath" variable.
kOutputFileName <- 'getVisit.csv'
# ------ constants ------
kColnameVisitnum <- 'VISITNUM'
kColnameVisit <- 'VISIT'
kVisitListVisitnumCol <- 1
# ------ functions ------
#' @title readCsvSetEncoding
#' @description Reads a file with the specified encoding.
#' @param target_file Full path of the target file.
#' @param target_encoding Encoding to specify.
#' @return Returns a data frame. If the file fails to read, NA is returned.
readCsvSetEncoding <- function(target_file, target_encoding){
  temp <- tryCatch(
    read.csv(target_file, as.is=T, fileEncoding=target_encoding, stringsAsFactors=F, na.strings=c("", NA)),
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
# ------ Init ------
# For test
if (exists('exec_test')){
  SetSettingsForTest(exec_test)
}
# If not specified, it will use the same path as 'kInputDirPath'.
kExternalDirPath <- ifelse(kExternalDirPath != '', kExternalDirPath, kInputDirPath)
kOutputDirpath <- ifelse(kOutputDirpath != '', kOutputDirpath, kInputDirPath)
# Read csv.
input_fa <- ReadTargetCsv(kInputDirPath, kInputFileName)
input_external_file <- ReadTargetCsv(kExternalDirPath, kExternalFileName)
if (!is.data.frame(input_fa) | !is.data.frame(input_external_file)){
  stop(print('The input file was not found. Please check the path specification of the input file.'))
}
# ------ Edit FA ------
# Delete the column 'visit' if it exists.
temp_col_idx <- grep(x=colnames(input_fa), pattern=paste0('^', kColnameVisit, '$'), ignore.case=T)
if (length(temp_col_idx) > 0){
  input_fa[temp_col_idx] <- NULL
}
# ------ Get visit info ------
# Extract only the columns named 'visitnum' and 'visit'.
target_col_num_idx <- grep(x=colnames(input_external_file), pattern=paste0('^', kColnameVisitnum, '$'), ignore.case=T)
target_col_idx <- grep(x=colnames(input_external_file), pattern=paste0('^', kColnameVisit, '$'), ignore.case=T)
input_visitlist <- input_external_file[ , c(target_col_num_idx, target_col_idx)]
colnames(input_visitlist) <- c(kColnameVisitnum, kColnameVisit)
# Remove duplicates and sort in ascending order by 'visitnum'.
input_visitlist <- unique(input_visitlist)
input_visitlist <- input_visitlist[order(input_visitlist[ , kVisitListVisitnumCol]), ]
# ------ Merge dataframes ------
raw_colnames <- colnames(input_fa)
by_x_col_idx <- grep(x=raw_colnames, pattern=paste0('^', kColnameVisitnum, '$'), ignore.case=T)
input_fa$save_order <- 1:nrow(input_fa)
temp <- merge(input_fa, input_visitlist, by.x=by_x_col_idx, by.y=kVisitListVisitnumCol, all.x=T, sort=F)
# Restore the rows to their original order.
temp <- temp[order(temp$save_order), ]
temp <- temp[ , colnames(temp) != 'save_order']
row.names(temp) <- 1:nrow(temp)
# Restore the columns to their original order.
output_fa <- NULL
output_colnames <- c(raw_colnames, kColnameVisit)
temp <- rbind(data.frame(), lapply(temp[1:ncol(temp)], as.character))
for (i in 1:length(output_colnames)){
  from_idx <- grep(x=colnames(temp), pattern=paste0('^', output_colnames[i], '$'), ignore.case=T)
  if (i != 1){
    output_fa[i] <- temp[from_idx]

  } else {
    output_fa <- temp[from_idx]
  }
}
# ------ Write csv ------
WriteOutputCsv(output_fa, kOutputDirpath, kOutputFileName)
