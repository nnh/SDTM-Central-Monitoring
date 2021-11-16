#' Aggregate the number of cases by grade.
#'
#' @file 3-summarize-by-grade.R
#' @author Mariko Ohtsuka
#' @date 2021.11.12
# ------ settings ------
kInputDirPath <- '/Users/mariko/Documents/GitHub/SDTM-Central-Monitoring/TEST/temp/'
kInputFileName <- 'extract-grade-observation.csv'
kOutputDirpath <- ''  # If it is blank, it is treated as the same as the path set in the "kInputDirPath" variable.
kOutputFileName <- 'summarize-by-grade.csv'
kMinVisitnum <- NA
kMaxVisitnum <- NA
kExcludeVisitnum <- c(NA)
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
#' @title ConvertColumnNameIntoLowerCase
#' @description Convert data frame column names to lowercase.
#' @param input_df Data frame to be converted.
#' @return List of converted data frames and column names before conversion.
ConvertColumnNameIntoLowerCase <- function(input_df){
  save_colnames <- colnames(input_df)
  colnames(input_df) <- tolower(save_colnames)
  return(list(input_df, save_colnames))
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
#' @param input_df Data source.
#' @return Data frame.
GetToxicityList <- function(input_df){
  toxicity <- unique(input_df$faobj)
  toxicity_count <- sapply(toxicity, function(x){
    targetToxicity <- subset(input_df, faobj == x & faorres >= 3)
    return(nrow(targetToxicity))
  })
  df_toxicity <- data.frame(toxicity, toxicity_count)
  df_toxicity <- df_toxicity[order(df_toxicity$toxicity_count, decreasing=T, df_toxicity$toxicity), ]
  df_toxicity <- df_toxicity[ , 1, drop=F]
  df_toxicity$toxicity_sortorder <- 1:nrow(df_toxicity)
  rownames(df_toxicity) <- df_toxicity$toxicity_sortorder
  return(df_toxicity)
}
#' @title SetNameToList
#' @description Create a named list.
#' @param ... Variables to list.
#' @return List.
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
  output_colnames <- c('visitnum', 'visit', 'toxicity', 'grade', 'n', 'count', 'percent')
  target_df <- subset(df, visitnum == as.numeric(target_conditions[1]) & faobj == target_conditions[2])
  digit <- summarize_conditions[['kPercentDigit']]
  n <- nrow(target_df)
  count <- nrow(subset(target_df, faorres == target_conditions[3]))
  percent <- ifelse(n > 0, round((count / n) * 100, digits=digit), 0)
  res <- data.frame(as.numeric(target_conditions[1]), target_conditions[4], target_conditions[2], as.numeric(target_conditions[3]),
                    n, count, percent)
  colnames(res) <- output_colnames
  return(res)
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
  for (i in 1:length(target_conditions)){
    assign(names(target_conditions[i]), SetEditVisitnumCondition(target_conditions[i]))
  }
  visit_table <- input_df
  if (is.numeric(kMinVisitnum) & kMinVisitnum >= 0){
    visit_table <- subset(visit_table, visitnum >= kMinVisitnum)
  }
  if (is.numeric(kMaxVisitnum) & kMaxVisitnum >= 0){
    visit_table <- subset(visit_table, visitnum <= kMaxVisitnum)
  }
  for (i in 1:length(kExcludeVisitnum)){
    if (is.numeric(kExcludeVisitnum[i]) & kExcludeVisitnum[i] >= 0){
      visit_table <- subset(visit_table, visitnum != kExcludeVisitnum[i])
    }
  }
  visit_table <- visit_table[ , c('visitnum', 'visit')]
  visit_table <- unique(visit_table)
  visit_table <- visit_table[order(visit_table$visitnum), ]
  return(visit_table)
}
# ------ Main ------
# If not specified, it will use the same path as 'kInputDirPath'.
kOutputDirpath <- ifelse(kOutputDirpath != '', kOutputDirpath, kInputDirPath)
# Read csv.
raw_input_fa <- ReadTargetCsv(kInputDirPath, kInputFileName)
input_fa <- ConvertColumnNameIntoLowerCase(raw_input_fa)[[1]]
if (!is.data.frame(input_fa)){
  stop(print('The input file was not found. Please check the path specification of the input file.'))
}
summarize_conditions <- c(SetNameToList(kTargetGrade, kArmColname, kPercentDigit))
toxicity_table <- GetToxicityList(input_fa)
visit_table <- GetTargetVisitnumList(input_fa, SetNameToList(kMinVisitnum, kMaxVisitnum, kExcludeVisitnum))
visit_toxicity_grade_table <- expand.grid(visitnum=visit_table[ , 1, drop=T], toxicity=toxicity_table[ , 1, drop=T], grade=kTargetGrade)
# Merge visit
visit_toxicity_grade_table <- merge(visit_toxicity_grade_table, visit_table, by='visitnum', all.x=T)
visit_toxicity_grade_table <- visit_toxicity_grade_table[ , c('visitnum', 'toxicity', 'grade', 'visit')]
count_table <- apply(visit_toxicity_grade_table, 1, CreateCountTableByGrade, input_fa, summarize_conditions)
# Convert a list to a data frame.
df_summarize <- count_table[[1]]
for (i in 2:length(count_table)){
  df_summarize <- rbind(df_summarize, count_table[[i]])
}
# Edit table template.
table_template <- expand.grid(visit_table$visitnum, toxicity_table$toxicity, kTargetGrade)
colnames(table_template) <- c('visitnum', 'toxicity', 'grade')
table_template <- merge(table_template, toxicity_table, by='toxicity', all.x=T)
# Edit output data frame.
df_output <- merge(table_template, df_summarize, by=c('visitnum', 'toxicity', 'grade'), all.x=T)
# Sort by visitnum, toxicity, grade.
df_output <- df_output[order(df_output$visitnum, df_output$toxicity_sortorder, df_output$grade), c('visit', 'toxicity', 'grade', 'n', 'count', 'percent')]
WriteOutputCsv(df_output, kOutputDirpath, kOutputFileName)
