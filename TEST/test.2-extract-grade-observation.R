#' test script
#'
#' @file test.extract-grade-observation.R
#' @author Mariko Ohtsuka
#' @date 2021.11.10
rm(list=ls())
# テスト用入力ファイル作成
source(here('R', '1-get-visit.R'))
rm(list=ls())
# ------ libraries ------
library(RUnit)
library(tidyverse)
library(here)
# ------ constants ------
kTestInputFileName <- 'getVisit.csv'
kTestTargetFileName <- 'extract-grade-observation.csv'
# ------ functions ------
ReadCsvForCompare <- function(kTestInputFileName){
  temp <- read.csv(here('TEST', 'temp', kTestInputFileName)) %>% filter(FATESTCD == 'GRADE') %>%
    select(c('USUBJID', 'FAOBJ', 'FAORRES', 'VISITNUM', 'VISIT'))
  return(temp)
}
# ------ init ------
print('*** test.extract-grade-observation start ***')
test_dir <- file.path(here('test', 'temp'))
if (!file.exists(test_dir)){
  dir.create(test_dir)
}
# ------ main ------
error_f <- T
# ### normal process ###
source(here('TEST', 'test.inputfile-edit.R'))
source(here('R', '1-get-visit.R'))
source(here('R', '2-extract-grade-observation.R'))
test_comp_input <- ReadCsvForCompare(kTestInputFileName)
test_input <- read.csv(here('TEST', 'temp', kTestTargetFileName))
print('check output dataframe(normal process)')
res <- checkEquals(test_input, test_comp_input) %>% print()
error_f <- ifelse(!res, res, error_f)
# ### abnormal process ###
# 出力列の一部が存在しない場合
source(here('TEST', 'test.inputfile-edit.R'))
source(here('R', '1-get-visit.R'))
remove_colname <- 'FAOBJ'
test_comp_input <- ReadCsvForCompare(kTestInputFileName) %>% select(-c(remove_colname))
temp <- read_csv(here('TEST', 'temp', kTestInputFileName)) %>% select(-c(remove_colname)) %>%
  write.csv(here('TEST', 'temp', kTestInputFileName), quote=T, row.names=F)
source(here('R', '2-extract-grade-observation.R'))
test_input <- read.csv(here('TEST', 'temp', kTestTargetFileName))
print('check output dataframe(Case where part of the output column does not exist)')
res <- checkEquals(test_input, test_comp_input) %>% print()
error_f <- ifelse(!res, res, error_f)
# ### Read table by file encoding ###
source(here('TEST', 'test.common.R'))
error_f <- TestReadTable(error_f, here('R', '2-extract-grade-observation.R'), test_dir)
PrintOverallResults(error_f)
print('*** test.extract-grade-observation end ***')
