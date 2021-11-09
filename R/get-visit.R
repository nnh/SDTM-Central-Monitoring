#' Derive VISIT from VISITNUM.
#'
#' @file get-visit.R
#' @author Mariko Ohtsuka
#' @date 2021.10.20
# ------ settings ------
kInputDirPath <- '/Users/mariko/Documents/GitHub/SDTM-Central-Monitoring/TEST/temp/'
kInputFileName <- 'FA.csv'
kExternalDirPath <- ''  # If it is blank, it is treated as the same as the path set in the "kInputDirPath" variable.
kExternalFileName <- 'visit.csv'
kOutputDirpath <- ''  # If it is blank, it is treated as the same as the path set in the "kInputDirPath" variable.
kOutputFileName <- 'output.csv'
# ------ constants ------
kColnameVisitnum <- 'visitnum'
kColnameVisit <- 'visit'
kVisitListVisitnumCol <- 1
kVisitListVisitCol <- 2
# ------ functions ------
#' @title ReadTargetCsv
#' @param inputPath The path where the CSV file is located.
#' @param targetFilename Name of the CSV file.
#' @return a data frame.
ReadTargetCsv <- function(inputPath, targetFilename){
  # If the string of the specified file path ends with '\', remove the '\'.
  temp_path <- ifelse(substr(inputPath, nchar(inputPath), nchar(inputPath)) == '/',
                      substr(inputPath, 1, nchar(inputPath) - 1),
                      inputPath)
  # Returns NULL if the file fails to read.
  res <- tryCatch(read.csv(paste0(temp_path, '/', targetFilename), header=T, as.is=T),
                  error = function(e){
                    return(NULL)
                  },
                  warning = function(e){
                    return(NULL)
                  },
                  silent=F)
  return(res)
}
# ------ Init ------
# If not specified, it will use the same path as 'kInputDirPath'.
kExternalDirPath <- ifelse(kExternalDirPath != '', kExternalDirPath, kInputDirPath)
kOutputDirpath <- ifelse(kOutputDirpath != '', kOutputDirpath, kInputDirPath)
# Read csv.
input_fa <- ReadTargetCsv(kInputDirPath, kInputFileName)
input_external_file <- ReadTargetCsv(kExternalDirPath, kExternalFileName)
if (is.null(input_fa) | is.null(input_external_file)){
  stop('The input file was not found. Please check the path specification of the input file.')
}
# ------ Edit FA ------
# Delete the column 'visit' if it exists.
temp_col_idx <- grep(x=colnames(input_fa), pattern=paste0('^', kColnameVisit, '$'), ignore.case=T)
if (length(temp_col_idx) > 0){
  # Get the name of the 'visit' column.
  raw_colnames_visit <- colnames(input_fa)[c(temp_col_idx)]
  # Delete the 'visit' column.
  input_fa[temp_col_idx] <- NULL
}
# ------ Get visit info ------
# Extract only the columns named 'visitnum' and 'visit'.
target_col_num_idx <- grep(x=colnames(input_external_file), pattern=paste0('^', kColnameVisitnum, '$'), ignore.case=T)
target_col_idx <- grep(x=colnames(input_external_file), pattern=paste0('^', kColnameVisit, '$'), ignore.case=T)
input_visitlist <- input_external_file[ , c(target_col_num_idx, target_col_idx)]
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
for (i in 1:length(output_colnames)){
  from_idx <- grep(x=colnames(temp), pattern=paste0('^', output_colnames[i], '$'), ignore.case=T)
  if (i != 1){
    output_fa[i] <- temp[from_idx]

  } else {
    output_fa <- temp[from_idx]
  }
}
write.table(output_fa, paste0(kOutputDirpath, kOutputFileName), , row.names=F, append=F, sep=',')
