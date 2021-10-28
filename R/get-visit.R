#' Derive VISIT from VISITNUM.
#'
#' @file get-visit.R
#' @author Mariko Ohtsuka
#' @date 2021.10.20
# ------ constants ------
kInputDirPath <- '/Users/mariko/Library/CloudStorage/Box-Box/Datacenter/Users/ohtsuka/2021/20211021/DS-ALL-2016_cdisc_210812_1835/'
kInputFileName <- 'FA.csv'
kExternalDirPath <- ''
kExternalFileName <- 'FA.csv'
kOutputDirpath <- ''
kColnameVisitnum <- 'visitnum'
kColnameVisit <- 'visit'
kVisitListVisitnumCol <- 1
kVisitListVisitCol <- 2
# ------ functions ------
ReadTargetCsv <- function(inputPath, targetFilename){
  # If the string of the specified file path ends with '\', remove the '\'.
  temp_path <- ifelse(substr(inputPath, nchar(inputPath), nchar(inputPath)) == '/',
                      substr(inputPath, 1, nchar(inputPath) - 1),
                      inputPath)
  # Returns NULL if the file fails to read.
  res <- tryCatch(read.csv(paste0(temp_path, '/', targetFilename)),
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
target_col_idx <- grep(x=colnames(input_external_file), pattern=paste0('^', kColnameVisitnum, '$|^', kColnameVisit, '$'), ignore.case=T)
input_visitlist <- input_external_file[ , target_col_idx]
# Match the column name to the FA.
target_colname <- colnames(input_visitlist)
target_colname[kVisitListVisitCol] <- raw_colnames_visit
colnames(input_visitlist) <- target_colname
# Remove duplicates and sort in ascending order by 'visitnum'.
input_visitlist <- unique(input_visitlist)
input_visitlist <- input_visitlist[order(input_visitlist[ , kVisitListVisitnumCol]), ]
# ------ Merge dataframes ------
raw_colnames <- colnames(input_fa)
by_x_col_idx <- grep(x=raw_colnames, pattern=paste0('^', kColnameVisitnum, '$'), ignore.case=T)
temp <- merge(input_fa, input_visitlist, by.x=by_x_col_idx, by.y=kVisitListVisitnumCol)
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
