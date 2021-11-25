#' Aggregate the number of cases by grade.
#'
#' @file 3-summarize-by-grade.R
#' @author Mariko Ohtsuka
#' @date 2021.11.25
# ------ settings ------
kInputDirPath <- '~/Documents/GitHub/SDTM-Central-Monitoring/TEST/temp/'
kInputFileName <- 'extract-grade-observation.csv'
kOutputDirpath <- ''  # If it is blank, it is treated as the same as the path set in the "kInputDirPath" variable.
kOutputFileName <- 'summarize-by-grade.csv'
kMinVisitnum <- NA
kMaxVisitnum <- NA
kExcludeVisitnum <- c(NA)
kTargetGrade <- c(3, 4, 5)
kPercentDigit <- 1
kArmColname <- NA  # unused
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
#' @param input_df Data source.
#' @return Data frame.
GetToxicityList <- function(input_df){
  aeterm <- unique(input_df$FAOBJ)
  aeterm <- na.omit(aeterm)
  toxicity_count <- sapply(aeterm, function(x){
    targetToxicity <- subset(input_df, FAOBJ == x & FAORRES >= 3)
    return(nrow(targetToxicity))
  })
  df_toxicity <- data.frame(AETERM=aeterm, TOXICITY_COUNT=toxicity_count)
  df_toxicity <- df_toxicity[order(df_toxicity$TOXICITY_COUNT, decreasing=T, df_toxicity$AETERM), ]
  df_toxicity <- df_toxicity[ , 1, drop=F]
  df_toxicity$TOXICITY_SORTORDER <- 1:nrow(df_toxicity)
  rownames(df_toxicity) <- df_toxicity$TOXICITY_SORTORDER
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
  idx_visitnum <- 1
  idx_aeterm <- 2
  idx_grade <- 3
  idx_visit <- 4
  target_df <- subset(df, VISITNUM == as.numeric(target_conditions[idx_visitnum]) & FAOBJ == target_conditions[idx_aeterm])
  digit <- IsNumeric(summarize_conditions[['kPercentDigit']])
  n <- nrow(target_df)
  if (length(grep('-', target_conditions[idx_grade])) > 0){
    # total
    split_grade <- strsplit(target_conditions[idx_grade], '-')
    min_grade <- as.numeric(split_grade[[1]][1])
    # Only the worst grade counts.
    temp <- subset(target_df, FAORRES >= min_grade, c(USUBJID, VISITNUM, FAOBJ))
    temp <- unique(temp)
    count <- nrow(temp)
  } else {
    # by grade
    count <- nrow(subset(target_df, FAORRES == target_conditions[idx_grade]))
  }
  percent <- ifelse(n > 0, round((count / n) * 100, digits=digit), 0)
  res <- data.frame(as.numeric(target_conditions[idx_visitnum]), target_conditions[idx_visit], target_conditions[idx_aeterm],
                    target_conditions[idx_grade], n, count, percent)
  output_colnames <- c('VISITNUM', 'VISIT', 'AETERM', 'GRADE', 'N', 'COUNT', 'PERCENT')
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
  temp <- IsNumeric(kMinVisitnum)
  if (is.numeric(temp) & temp >= 0){
    visit_table <- subset(visit_table, VISITNUM >= temp)
  }
  temp <- IsNumeric(kMaxVisitnum)
  if (is.numeric(temp) & temp >= 0){
    visit_table <- subset(visit_table, VISITNUM <= temp)
  }
  for (i in 1:length(kExcludeVisitnum)){
    temp <- IsNumeric(kExcludeVisitnum[i])
    if (is.numeric(temp) & temp >= 0){
      visit_table <- subset(visit_table, VISITNUM != temp)
    }
  }
  visit_table <- visit_table[ , c('VISITNUM', 'VISIT')]
  visit_table <- unique(visit_table)
  visit_table <- visit_table[order(visit_table$VISITNUM), ]
  return(visit_table)
}
#' @title GetGradeList
#'
#' @param targetGrade Vector of target grades.
#' @return A vector.
GetGradeList <- function(targetGrade){
  sortorder_max <- length(targetGrade) + 1
  res <- data.frame(GRADE=c(targetGrade, paste0(min(targetGrade), '-', max(targetGrade))), GRADE_SORTORDER=1:sortorder_max)
  return(res)
}
#' @title IsNumeric
#' @description If input_value can be converted to a number, convert it to a number. Otherwise, it returns the value as is.
#' @param input_value Vector of target value.
#' @return A vector.
IsNumeric <- function(input_value){
  temp <- !is.na(suppressWarnings(as.numeric(input_value)))
  if (temp){
    return(as.numeric(input_value))
  } else {
    return(input_value)
  }
}
# ------ Main ------
# For test
if (exists('exec_test')){
  SetSettingsForTest(exec_test)
  if (exists('exec_test2')){
    kExcludeVisitnum <- exec_test2
  }
  if (exists('exec_test3')){
    kTargetGrade <- exec_test3
  }
}
# If not specified, it will use the same path as 'kInputDirPath'.
kOutputDirpath <- ifelse(kOutputDirpath != '', kOutputDirpath, kInputDirPath)
# Read csv.
raw_input_fa <- ReadTargetCsv(kInputDirPath, kInputFileName)
if (!is.data.frame(raw_input_fa)){
  stop(print('The input file was not found. Please check the path specification of the input file.'))
}
input_fa <- raw_input_fa
colnames(input_fa) <- toupper(colnames(input_fa))
# If multiple GRADE values exist for the same USUBJID, AETERM, VISITNUM, the worst value is used.
input_fa$delete_flg <- F
input_fa <- input_fa[order(input_fa$VISITNUM, input_fa$USUBJID, input_fa$FAOBJ, input_fa$FAORRES , decreasing=T), ]
for (i in 1:(nrow(input_fa) - 1)){
  if (input_fa[i, 'USUBJID'] == input_fa[i + 1, 'USUBJID'] &
      input_fa[i, 'FAOBJ'] == input_fa[i + 1, 'FAOBJ'] &
      input_fa[i, 'VISITNUM'] == input_fa[i + 1, 'VISITNUM']){
    input_fa[i + 1, 'delete_flg'] <- T
  }
}
input_fa <- input_fa[!input_fa$delete_flg, ]
summarize_conditions <- c(SetNameToList(kArmColname, kPercentDigit))
toxicity_table <- GetToxicityList(input_fa)
grade_table <- GetGradeList(kTargetGrade)
visit_table <- GetTargetVisitnumList(input_fa, SetNameToList(kMinVisitnum, kMaxVisitnum, kExcludeVisitnum))
visit_toxicity_grade_table <- expand.grid(VISITNUM=visit_table[ , 1, drop=T], AETERM=toxicity_table[ , 1, drop=T],
                                          GRADE=grade_table[ , 1, drop=T])
# Merge visit
visit_toxicity_grade_table <- merge(visit_toxicity_grade_table, visit_table, by='VISITNUM', all.x=T)
count_table <- apply(visit_toxicity_grade_table, 1, CreateCountTableByGrade, input_fa, summarize_conditions)
# Convert a list to a data frame.
df_summarize <- count_table[[1]]
for (i in 2:length(count_table)){
  df_summarize <- rbind(df_summarize, count_table[[i]])
}
# Edit table template.
table_template <- expand.grid(VISITNUM=visit_table$VISITNUM, AETERM=toxicity_table$AETERM, GRADE=grade_table$GRADE)
table_template <- merge(table_template, toxicity_table, by='AETERM', all.x=T)
table_template <- merge(table_template, grade_table, by='GRADE', all.x=T)
# Edit output data frame.
df_output <- merge(table_template, df_summarize, by=c('VISITNUM', 'AETERM', 'GRADE'), all.x=T)
# Sort by visitnum, toxicity, grade.
df_output <- df_output[order(df_output$VISITNUM, df_output$TOXICITY_SORTORDER, df_output$GRADE_SORTORDER), c('VISITNUM', 'VISIT', 'AETERM', 'GRADE', 'N', 'COUNT', 'PERCENT')]
# If N is zero, replace count with a hyphen.
for (i in 1:nrow(df_output)){
  if (df_output[i, 'N'] == 0){
    df_output[i, 'N'] <- '-'
    df_output[i, 'COUNT'] <- '-'
    df_output[i, 'PERCENT'] <- '-'
  }
}
WriteOutputCsv(df_output, kOutputDirpath, kOutputFileName)
