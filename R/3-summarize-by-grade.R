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
kMinVisitnum <- 300
kMaxVisitnum <- 1000
kExcludeVisitnum <- c(400, 1000)
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
#' @title CreateCountTableByGrade
#' @description Aggregate by condition.
#' @param target_conditions Data frame of condition.
#' @param df Data frame of the aggregation source.
#' @param summarize_conditions List of extraction conditions.
#' @return List of aggregate results.
CreateCountTableByGrade <- function(target_conditions, df, summarize_conditions){
  target_df <- subset(df, VISITNUM == target_conditions[1] & FAOBJ == target_conditions[2])
  digit <- summarize_conditions[['kPercentDigit']]
  n <- nrow(target_df)
  count <- nrow(subset(target_df, FAORRES == target_conditions[3]))
  percent <- round((count / n) * 100, digits=digit)
  return(list(target_conditions[1], target_conditions[2], target_conditions[3], n, count, percent))
}
#' @title SetEditVisitnumCondition
#' @description If the input value is NA, it returns -1.
#' @param target_condition Variable to be checked.
#' @return If the input value is NA, -1 is returned. Otherwise, it returns the input value.
SetEditVisitnumCondition <- function(target_condition){
  condition <- unlist(target_condition)
  temp <- ifelse(!is.na(condition), condition, -1)
  return(temp)
}
#' @title GetTargetVisitnumList
#' @description
#' @param input_df a data frame.
#' @param target_conditions List of extraction conditions.
#' @return a list of visitnum as a vector.
GetTargetVisitnumList <- function(input_df, target_conditions){
  conditions_name <- c('minVisitnum', 'maxVisitnum', 'excludeVisitnum')
  for (i in 1:length(conditions_name)){
    assign(conditions_name[i], SetEditVisitnumCondition(target_conditions[i]))
  }
  visit_table <- input_df
  if (is.numeric(minVisitnum) & minVisitnum >= 0){
    visit_table <- subset(visit_table, VISITNUM >= minVisitnum)
  }
  if (is.numeric(maxVisitnum) & maxVisitnum >= 0){
    visit_table <- subset(visit_table, VISITNUM <= maxVisitnum)
  }
  for (i in 1:length(excludeVisitnum)){
    if (is.numeric(excludeVisitnum[i]) & excludeVisitnum[i] >= 0){
      visit_table <- subset(visit_table, VISITNUM != excludeVisitnum[i])
    }
  }
  visit_table <- unique(visit_table$VISITNUM)
  return(visit_table)
}
# ------ Main ------
# If not specified, it will use the same path as 'kInputDirPath'.
kOutputDirpath <- ifelse(kOutputDirpath != '', kOutputDirpath, kInputDirPath)
# Read csv.
input_fa <- ReadTargetCsv(kInputDirPath, kInputFileName)
if (!is.data.frame(input_fa)){
  stop(print('The input file was not found. Please check the path specification of the input file.'))
}
summarize_conditions <- c(SetNameToList(kTargetGrade, kArmColname, kPercentDigit))
toxicity_table <- GetToxicityList(input_fa)
visit_table <- GetTargetVisitnumList(input_fa, list(kMinVisitnum, kMaxVisitnum, kExcludeVisitnum))
visit_toxicity_grade_table <- expand.grid(visit_table, toxicity_table[ , 1, drop=T], kTargetGrade)
count_table <- apply(visit_toxicity_grade_table, 1, CreateCountTableByGrade, input_fa, summarize_conditions)
