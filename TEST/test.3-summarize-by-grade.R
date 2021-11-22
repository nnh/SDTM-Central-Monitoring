#' test script
#'
#' @file test.3-summarize-by-grade.R
#' @author Mariko Ohtsuka
#' @date 2021.11.22
rm(list=ls())
# ------ libraries ------
library(RUnit)
library(tidyverse)
library(here)
# ------ constants ------
kTestRawdataFileName <- 'dummyFA.csv'
kTestVisitFileName <- 'dummyVisit.csv'
kTestInputFileName <- 'extract-grade-observation.csv'
kTestTargetFileName <- 'summarize-by-grade.csv'
# ------ functions ------
GetWorstGrade <- function(input_df){
  # NAのレコードを無視
  input_df <- input_df %>% filter(!is.na(FAOBJ)) %>% filter(!is.na(FAORRES)) %>% filter(!is.na(VISITNUM))
  # 同じUSUBJID、AETERM、VISITNUMで複数GRADEの値が存在した場合は悪い方を取る
  res <- input_df %>% group_by(USUBJID, FAOBJ, VISITNUM) %>% filter(FAORRES == max(FAORRES))
  return(res)
}
# ------ init ------
print('*** test.summarize-by-grade start ***')
source(here('TEST', 'test.common.R'), encoding="utf-8")
test_dir = CreateTestFolder(here('test', 'temp'))
# ------ main ------
# run test
error_f <- T
# ### Test using anonymized rawdata ###
print('### Create a dummy file for testing. ###')
# source(here('TEST', 'test.inputfile-edit.R'), encoding="utf-8")
exec_test <- c(kInputFileName='dummy_extract-grade-observation.csv', dummy='')
source(here('R', '3-summarize-by-grade.R'), encoding="utf-8")
rm(exec_test)
test_input <- ReadTargetCsv(test_dir, kTestTargetFileName)
compare_input <- ReadTargetCsv(test_dir, 'compare_summarize-by-grade.csv')
res <- checkEquals(test_input, compare_input) %>% print()
error_f <- ifelse(!res, res, error_f)
print('### Worst grade. ###')
USUBJID <-  c('1', '1', '1', '2', '1', '1')
FAOBJ <- c('ae1', 'ae1', 'ae1', 'ae1', 'ae1', 'ae0')
FAORRES <- c(1, 3, 4, 5, 0, 2)
VISITNUM <- c(100, 100, 100, 100, 101, 100)
VISIT <- c('test1', 'test1', 'test1', 'test1', 'test0', 'test1')
df <- data.frame(USUBJID, FAOBJ, FAORRES, VISITNUM, VISIT)
compare_worstgrade <- df %>% GetWorstGrade()
df %>% write.table(here(test_dir, 'test2_extract-grade-observation.csv'), row.names=F, append=F, sep=',')
exec_test <- c(kInputFileName='test2_extract-grade-observation.csv', dummy='')
source(here('R', '3-summarize-by-grade.R'), encoding="utf-8")
rm(exec_test)
test_input <- ReadTargetCsv(test_dir, kTestTargetFileName)
VISITNUM <- c(rep(100, 8), rep(101, 8))
VISIT <- c(rep('test1', 8), rep('test0', 8))
AETERM <- rep(c(rep('ae1', 4), rep('ae0', 4)), 2)
GRADE <- rep(c('3', '4', '5', '3-5'), 4)
N <- c(rep(2, 4), rep(1, 8), rep('-', 4))
COUNT <- c(0, 1, 1, 2, rep(0, 8), rep('-', 4))
PERCENT <- c(0, 50, 50, 100, rep(0, 8), rep('-', 4))
compare_input <- data.frame(VISITNUM, VISIT, AETERM, GRADE, N, COUNT, PERCENT)
res <- checkEquals(test_input, compare_input) %>% print()
error_f <- ifelse(!res, res, error_f)
PrintOverallResults(error_f)
print('*** test.summarize-by-grade end ***')
