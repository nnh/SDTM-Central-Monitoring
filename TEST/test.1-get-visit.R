#' test script
#'
#' @file test.get-visit.R
#' @author Mariko Ohtsuka
#' @date 2021.11.22
rm(list=ls())
# ------ libraries ------
library(RUnit)
library(tidyverse)
library(here)
# ------ constants ------
kTestInputFileName <- 'dummyFA.csv'
kTestVisitFileName <- 'dummyVISIT.csv'
kTestTargetFileName <- 'getVisit.csv'
# ------ functions ------
CreateVisitTableForTest <- function(index, res_string, df_visit){
  assign(str_c('test_', index), res_string, envir=.GlobalEnv)
  assign(str_c('visit_', index), df_visit, envir=.GlobalEnv)
}
# ------ init ------
print('*** test.get-visit start ***')
source(here('TEST', 'test.common.R'), encoding="utf-8")
test_dir = CreateTestFolder(here('test', 'temp'))
# ------ Create a data frame for comparison. ------
id <- c(1, 2, 2, 4, 6, 7, 8, 9) %>% as.integer()
visitnum <- c(100, 100, 110, 300, 400, NA, '', 1000) %>% as.integer()
test_rawdata <- data.frame(id, visitnum)
test_rawdata %>% write.table(str_c(test_dir, '/', kTestInputFileName), append=F, row.names=F, sep=',')
# visit table
visit_base <- data.frame(c(100, 110, 200, 300, 1000, 1300),
                         c('v100', 'v110', 'v200', 'v300', 'v1000', 'v1300'))
colnames(visit_base) <- c('visitnum', 'visit')
# 出力結果確認用データフレーム
checkdf <- data.frame(test_rawdata$id,
                      test_rawdata$visitnum,
                      c('v100', 'v100', 'v110', 'v300', NA, NA, NA, 'v1000'))
colnames(checkdf) <- c('id', 'visitnum', 'VISIT')
test_table <- list(
  list("visit tableが'visitnum', 'visit'の２変数、visitnumが数値", visit_base),
  list("visit tableが'visitnum', 'visit'の２変数、visitnumが文字列",
       data.frame(visitnum=as.character(visit_base$visitnum), visit=visit_base$visit)),
  list("visit tableが'Visitnum', 'Visit'の２変数",
       data.frame(Visitnum=visit_base$visitnum, Visit=visit_base$visit)),
  list("visit tableが'VISITNUM', 'VISIT'の２変数",
       data.frame(VISITNUM=visit_base$visitnum, VISIT=visit_base$visit)),
  list("visit tableが'test1', 'test2', 'visitnum', 'visit'の４変数",
       data.frame(test1=runif(nrow(visit_base), min=1, max=999),
                  test2=runif(nrow(visit_base), min=1, max=999),
                  visitnum=visit_base$visitnum,
                  visit=visit_base$visit)),
  list("visit tableが'Visitnum', 'test1', 'test2', 'Visit'の４変数",
       data.frame(Visitnum=visit_base$visitnum,
                  test1=stringi::stri_rand_strings(nrow(visit_base), length=6),
                  test2=runif(nrow(visit_base), min=1, max=999),
                  Visit=visit_base$visit)),
  list("visit tableが'VISITNUM', 'VISIT', 'test1', 'test2'の４変数",
       data.frame(VISITNUM=visit_base$visitnum,
                  VISIT=visit_base$visit,
                  test1=stringi::stri_rand_strings(nrow(visit_base), length=6),
                  test2=runif(nrow(visit_base), min=1, max=999))),
  list("visit tableが'visit', 'test1', 'visitnum'の３変数",
       data.frame(visit=visit_base$visit,
                  test1=stringi::stri_rand_strings(nrow(visit_base), length=6),
                  visitnum=visit_base$visitnum))
)
for (i in 1:length(test_table)){
  CreateVisitTableForTest(i, test_table[[i]][[1]], test_table[[i]][[2]])
}
# run test
error_f <- T
# ### Data Frame Equivalence ###
for (i in 1:length(test_table)){
  get(str_c('visit', '_', i)) %>% write.table(str_c(test_dir, '/', kTestVisitFileName), row.names=F, append=F, sep=',')
  save_i <- i
  exec_test <- c(kInputFileName=kTestInputFileName, kExternalFileName=kTestVisitFileName)
  source(here('R', '1-get-visit.R'), encoding="utf-8")
  output_fa <- ReadTargetCsv(kOutputDirpath, kOutputFileName)
  i <- save_i
  print(str_c('test ', i))
  compare_df <- checkdf
  res <- checkEquals(output_fa, compare_df) %>% print()
  error_f <- ifelse(!res, res, error_f)
}
# ### Read table by file encoding ###
error_f <- TestReadTable(error_f, here('R', '1-get-visit.R'), test_dir)
# ### Overall Results ###
PrintOverallResults(error_f)
print('*** test.get-visit end ***')
