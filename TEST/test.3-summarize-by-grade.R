#' test script
#'
#' @file test.3-summarize-by-grade.R
#' @author Mariko Ohtsuka
#' @date 2021.11.16
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
  # 同じUSUBJID、AETERM、VISITNUMで複数GRADEの値が存在した場合は悪い方を取る
  res <- input_df %>% group_by(USUBJID, FAOBJ, VISITNUM) %>% filter(FAORRES == max(FAORRES))
  return(res)
}
# ------ init ------
print('*** test.summarize-by-grade start ***')
source(here('TEST', 'test.common.R'))
test_dir = CreateTestFolder(here('test', 'temp'))
# ------ main ------
# テスト用ダミーファイル作成
#source(here('TEST', 'test.inputfile-edit.R'))
# run test
error_f <- T
# ### normal process ###
exec_test <- c(kInputFileName=kTestRawdataFileName, kExternalFileName=kTestVisitFileName)
source(here('R', '1-get-visit.R'))
rm(exec_test)
source(here('R', '2-extract-grade-observation.R'))
source(here('R', '3-summarize-by-grade.R'))
test_input <- ReadTargetCsv(test_dir, kTestRawdataFileName) %>% filter(FATESTCD == 'GRADE') %>% GetWorstGrade()
target_visitnum_list <- test_input$VISITNUM %>% unique() %>% sort()
temp <- test_input %>% filter(FAORRES >= 3)
target_aeterm <- unique(temp$FAOBJ) %>% map(~{
  target <- .
  count <- temp %>% filter(FAOBJ == target) %>% nrow()
  return(c(., count))
})
target_aeterm <- data.table::transpose((rbind(data.frame(), target_aeterm))) %>% arrange(desc(as.numeric(V2)))
temp <- test_input$FAOBJ %>% unique() %>% .[-which(. %in% target_aeterm$V1)] %>% sort()
target_aeterm_list <- c(target_aeterm, temp)
target_grade_list <- c(3, 4, 5)
by_visitnum <- target_visitnum_list %>% map(~{
  target <- .
  target_visitnum <- test_input %>% filter(VISITNUM == target)
})
