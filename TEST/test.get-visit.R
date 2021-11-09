#' test script
#'
#' @file test.get-visit.R
#' @author Mariko Ohtsuka
#' @date 2021.11.9
rm(list=ls())
# ------ libraries ------
library(RUnit)
library(tidyverse)
library(here)
# ------ functions ------
#' @title SetDataframe
#' @description Creates a data frame in the global environment from the information in the given arguments.
#' @param objectname_header String to use for the name of the data frame.
#' @param index Number to use for the name of the data frame.
#' @param data Data to be stored in data frame.
#' @param colname_list Column name to be set in the data frame.
#' @return Index whose value has been increased by 1.
SetDataframe <- function(objectname_header, index, data, colname_list){
  objectname <- str_c(objectname_header, '_', index)
  temp <- data
  colnames(temp) <- colname_list
  assign(objectname, temp, envir=.GlobalEnv)
  return(index + 1)
}
# ------ init ------
test_dir <- file.path(here('test', 'temp'))
if (!file.exists(test_dir)){
  dir.create(test_dir)
}
# 比較対象データフレーム
id <- c(1, 2, 2, 4, 6, 7, 8, 9) %>% as.integer()
visitnum <- c(100, 100, 110, 300, 400, NA, '', 1000) %>% as.integer()
test_rawdata <- data.frame(id, visitnum)
test_rawdata_filename <- 'FA.csv'
test_rawdata %>% write.table(str_c(test_dir, '/', test_rawdata_filename), append=F, row.names=F, sep=',')
# visit table
visitnum_table_visitnum_num <- c(100, 110, 200, 300, 1000, 1300)
visitnum_table_visit <- c('v100', 'v110', 'v200', 'v300', 'v1000', 'v1300')
for (i in 1:9){
  assign(str_c('visitnum_table_filler_num_', i), runif(length(visitnum_table_visitnum_num), min=1, max=999))
  assign(str_c('visitnum_table_filler_str_', i), stringi::stri_rand_strings(length(visitnum_table_visitnum_num), length=6))
}
# visitnumが数字
visit_table_num_base <- data.frame(visitnum_table_visitnum_num, visitnum_table_visit)
# visitnumが文字列
visit_table_str_base <- data.frame(as.character(visitnum_table_visitnum_num), visitnum_table_visit)
# 結果確認用データフレームのベース
checkdf_base <- test_rawdata[ , c('id', 'visitnum')]
checkdf_base$visit <- c('v100', 'v100', 'v110', 'v300', NA, NA, NA, 'v1000')
# 'visitnum', 'visit'の２変数のみのファイル
dataframe_count <- 1
# v_1
assign(str_c('check_', dataframe_count), checkdf_base)
dataframe_count <- SetDataframe('v', dataframe_count, visit_table_num_base, c('visitnum', 'visit'))  # visitnumが数値
# v_2
assign(str_c('check_', dataframe_count), checkdf_base)
dataframe_count <- SetDataframe('v', dataframe_count, visit_table_str_base, c('visitnum', 'visit'))  # visitnumが文字列
# 'Visitnum', 'Visit'の２変数のみのファイル
assign(str_c('check_', dataframe_count), checkdf_base)
assign(str_c('check_', dataframe_count), get(str_c('check_', dataframe_count)) %>% rename(Visit=visit))
dataframe_count <- SetDataframe('v', dataframe_count, visit_table_num_base, c('Visitnum', 'Visit'))
# 'VISITNUM', 'VISIT'の２変数のみのファイル
assign(str_c('check_', dataframe_count), checkdf_base)
assign(str_c('check_', dataframe_count), get(str_c('check_', dataframe_count)) %>% rename(VISIT=visit))
dataframe_count <- SetDataframe('v', dataframe_count, visit_table_num_base, c('VISITNUM', 'VISIT'))
# 'test1', 'test2', 'visitnum', 'visit'ファイル
assign(str_c('check_', dataframe_count), checkdf_base)
dataframe_count <- cbind(data.frame(visitnum_table_filler_num_1, visitnum_table_filler_num_2), visit_table_num_base) %>%
  SetDataframe('v', dataframe_count, ., c('test1', 'test2', 'visitnum', 'visit'))
# 'Visitnum', 'test1', 'test2', 'Visit'のファイル
assign(str_c('check_', dataframe_count), checkdf_base)
assign(str_c('check_', dataframe_count), get(str_c('check_', dataframe_count)) %>% rename(Visit=visit))
dataframe_count <- data.frame(visitnum_table_visitnum_num, visitnum_table_filler_str_1, visitnum_table_filler_str_2, visitnum_table_visit) %>%
  SetDataframe('v', dataframe_count, ., c('Visitnum', 'test1', 'test2', 'Visit'))
# 'VISITNUM', 'VISIT', 'test1', 'test2'のファイル
assign(str_c('check_', dataframe_count), checkdf_base)
assign(str_c('check_', dataframe_count), get(str_c('check_', dataframe_count)) %>% rename(VISIT=visit))
dataframe_count <- data.frame(visitnum_table_visitnum_num, visitnum_table_visit, visitnum_table_filler_str_3, visitnum_table_filler_num_3) %>%
  SetDataframe('v', dataframe_count, ., c('VISITNUM', 'VISIT', 'test1', 'test2'))
# 'visit', 'test1', 'visitnum'のファイル
assign(str_c('check_', dataframe_count), checkdf_base)
dataframe_count <- data.frame(visitnum_table_visit, visitnum_table_filler_str_4, visitnum_table_visitnum_num) %>%
  SetDataframe('v', dataframe_count, ., c('visit', 'test1', 'visitnum'))
# run test
filename <- 'visit.csv'
error_f <- T
# ### Data Frame Equivalence ###
for (i in 1:(dataframe_count - 1)){
  get(str_c('v', '_', i)) %>% write.table(str_c(test_dir, '/', filename), row.names=F, append=F, sep=',')
  save_i <- i
  source(here('R', 'get-visit.R'))
  i <- save_i
  output_fa <- read.csv(str_c(test_dir, '/', 'output.csv'), na.strings=c('', NA))
  print(str_c('test ', i))
  res <- checkEquals(output_fa, get(str_c('check_', i))) %>% print()
  error_f <- ifelse(!res, res, error_f)
}
# ### file encoding ###
test_fileencoding <- data.frame(c(1, 2, 3), c('あいう', 'abc', 'テスト'))
colnames(test_fileencoding) <- c('test1', 'test2')
fileencoding_list <- c('utf-8', 'cp932')
for (i in 1:length(fileencoding_list)){
  filename <- str_c('encode_', i, '.csv')
  filepath <- str_c(test_dir, '/', filename)
  test_fileencoding %>% write.table(filepath, row.names=F, append=F, sep=',', fileEncoding=fileencoding_list[i])
  assign(str_c('encode_', i), ReadTargetCsv(test_dir, filename))
}
# utf-8-bom
fileencoding_list <- c(fileencoding_list, 'utf-8-bom')
i <- i + 1
assign(str_c('encode_', i), ReadTargetCsv(test_dir, 'encode_bom.csv'))
for (i in 1:length(fileencoding_list)){
  print(fileencoding_list[i])
  res <- checkEquals(test_fileencoding, get(str_c('encode_', i))) %>% print()
  error_f <- ifelse(!res, res, error_f)
}
if (error_f){
  print('test_ok')
} else {
  print('test_ng')
}
