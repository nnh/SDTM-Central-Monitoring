#' test script
#'
#' @file test.extract-grade-observation.R
#' @author Mariko Ohtsuka
#' @date 2021.11.24
rm(list=ls())
# ------ libraries ------
library(RUnit)
library(tidyverse)
library(here)
# ------ constants ------
kTestRawdataFileName <- 'dummyFA.csv'
kTestVisitFileName <- 'dummyVisit.csv'
kTestInputFileName <- 'getVisit.csv'
kTestTargetFileName <- 'extract-grade-observation.csv'
# ------ init ------
print('*** test.extract-grade-observation start ***')
source(here('TEST', 'test.common.R'), encoding="utf-8")
test_dir = CreateTestFolder(here('test', 'temp'))
# ------ main ------
# edit input data
# run test
error_f <- T
# ### normal process ###
usubjid <- c('U-1', 'U-1', 'U-2', 'U-3', 'U-4', 'U-4')
faobj <- c('TEST1', 'TEST2', 'TEST3', NA, '', 'TEST4')
fatestcd <- c('GRADE', 'GRADE', 'AAA', 'GRADE', 'GRADE', NA)
faorres <- c(3, NA, NA, 5, 4, 1)
visitnum <- c(100, 200, 300, 400, 500, 600)
visit <- c('v100', 'v200', 'v300', 'v400', 'v500', 'v600')
test_rawdata <- data.frame(usubjid, faobj, faorres, visitnum, fatestcd) %>% write.table(str_c(test_dir, '/', kTestRawdataFileName), append=F, row.names=F, sep=',')
test_visit <- data.frame(visitnum, visit) %>% write.table(str_c(test_dir, '/', kTestVisitFileName), append=F, row.names=F, sep=',')
exec_test <- c(kInputFileName=kTestRawdataFileName, kExternalFileName=kTestVisitFileName)
source(here('R', '1-get-visit.R'), encoding="utf-8")
rm(exec_test)
source(here('R', '2-extract-grade-observation.R'), encoding="utf-8")
test_comp <- data.frame(c('U-1', 'U-1', 'U-3', 'U-4'), c('TEST1', 'TEST2', NA, NA), c(3, NA, 5, 4), c(100, 200, 400, 500), c('v100', 'v200', 'v400', 'v500'))
colnames(test_comp) <- c('USUBJID', 'FAOBJ', 'FAORRES', 'VISITNUM', 'VISIT')
test_input <- ReadTargetCsv(test_dir, kTestTargetFileName)
print('check output dataframe(normal process)')
res <- checkEquals(test_input, test_comp) %>% print()
error_f <- ifelse(!res, res, error_f)
# ### Change variable name ### #
usubjid <- c('U-1', 'U-1', 'U-2', 'U-3', 'U-4', 'U-4')
faobj <- c('TEST1', 'TEST2', 'TEST3', NA, '', 'TEST4')
test_var <- c('GRD', 'GRD', 'AAA', 'GRD', 'GRD', NA)
faorres <- c(3, NA, NA, 5, 4, 1)
visitnum <- c(100, 200, 300, 400, 500, 600)
visit <- c('v100', 'v200', 'v300', 'v400', 'v500', 'v600')
test_rawdata <- data.frame(usubjid, faobj, faorres, visitnum, test_var) %>% write.table(str_c(test_dir, '/', kTestRawdataFileName), append=F, row.names=F, sep=',')
test_visit <- data.frame(visitnum, visit) %>% write.table(str_c(test_dir, '/', kTestVisitFileName), append=F, row.names=F, sep=',')
exec_test <- c(kInputFileName=kTestRawdataFileName, kExternalFileName=kTestVisitFileName)
source(here('R', '1-get-visit.R'), encoding="utf-8")
rm(exec_test)
exec_test <- c(kCriteriaVariable='test_var', kGradeCriteria='GRD')
source(here('R', '2-extract-grade-observation.R'), encoding="utf-8")
test_comp <- data.frame(c('U-1', 'U-1', 'U-3', 'U-4'), c('TEST1', 'TEST2', NA, NA), c(3, NA, 5, 4), c(100, 200, 400, 500), c('v100', 'v200', 'v400', 'v500'))
colnames(test_comp) <- c('USUBJID', 'FAOBJ', 'FAORRES', 'VISITNUM', 'VISIT')
test_input <- ReadTargetCsv(test_dir, kTestTargetFileName)
print('check output dataframe(Change variable name)')
res <- checkEquals(test_input, test_comp) %>% print()
error_f <- ifelse(!res, res, error_f)
# ### Read table by file encoding ###
error_f <- TestReadTable(error_f, here('R', '2-extract-grade-observation.R'), test_dir)
PrintOverallResults(error_f)
print('*** test.extract-grade-observation end ***')
