#' test script
#'
#' @file test.common.R
#' @author Mariko Ohtsuka
#' @date 2021.11.15
# ------ libraries ------
library(RUnit)
library(tidyverse)
library(here)
# ------ functions ------
#' @title TestReadTable
#' @description  Testing the function "ReadTargetCsv".
#' @param input_error_f The error flag
#' @param script_path Path of the script file where the function to be tested resides.
#' @param test_dir Path of the test directory.
#' @return Boolean value, TRUE or FALSE.
TestReadTable <- function(input_error_f, script_path, test_dir){
  source(script_path, encoding="utf-8")
  error_f <- input_error_f
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
  return(error_f)
}
#' @title PrintOverallResults
#'
#' @param error_f Boolean value, TRUE or FALSE.
#' @return none
PrintOverallResults <- function(error_f){
  if (error_f){
    print('test_ok')
  } else {
    print('test_ng')
  }
}
#' @title SetSettingsForTest
#' @description Overwrite variables.
#' @param ... Variable name and variable contents to be overwritten.
#' @return none
#' @example SetSettingsForTest(c(kInputFileName='dummyFA.csv', kExternalFileName='dummyVISIT.csv'))
SetSettingsForTest <- function(...){
  list(...) %>% map(~{
    for (i in 1:length(.)){
      assign(names(.[i]), .[i], envir=.GlobalEnv)
    }
  })
}
#' @title CreateTestFolder
#'
#' @param target_path String of full path of the target folder.
#' @return String of full path of the target folder.
CreateTestFolder <- function(target_path){
  test_dir <- file.path(target_path)
  if (!file.exists(test_dir)){
    dir.create(test_dir)
  }
  return(test_dir)
}
