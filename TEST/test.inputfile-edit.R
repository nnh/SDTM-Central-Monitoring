#' test script
#' Create anonymized test files.
#' @file test.inputfile-edit.R
#' @author Mariko Ohtsuka
#' @date 2021.11.25
# ------ libraries ------
library(RUnit)
library(tidyverse)
library(here)
input_fa <- read_csv(here('TEST', 'temp', 'rawFA.csv'))
# visit table
visit <- input_fa %>% select(c('VISITNUM', 'VISIT')) %>% unique() %>% arrange(VISITNUM)
visit$VISIT <- str_c('visit_', visit$VISITNUM)
write.csv(visit, here('TEST', 'temp', 'dummyVISIT.csv'), quote=T, row.names=F)
# FA
input_fa$STUDYID <- 'test-studyid'
input_fa$FASEQ <- NA
temp_usubjid <- input_fa$USUBJID %>% unique()
temp <- runif(length(temp_usubjid), min=1000, max=9999) %>% unique()
usubjid_t <- data.frame(temp_usubjid, str_c('testusubjid-', as.integer(temp)))
for (i in 1:nrow(input_fa)){
  for (j in 1:nrow(usubjid_t)){
    if (input_fa[i, 'USUBJID'] == usubjid_t[j, 1]){
      input_fa[i, 'USUBJID'] <- usubjid_t[j, 2]
      break
    }
  }
}
input_fa$FASPID <- stringi::stri_rand_strings(nrow(input_fa), length=6)
temp_faobj <- input_fa$FAOBJ %>% unique()
temp <- 1:length(temp_faobj) %>% str_c('faobj-', .)
faobj_t <- data.frame(temp_faobj, temp)
for (i in 1:nrow(input_fa)){
  for (j in 1:nrow(faobj_t)){
    if (input_fa[i, 'FAOBJ'] == faobj_t[j, 1]){
      input_fa[i, 'FAOBJ'] <- faobj_t[j, 2]
      break
    }
  }
}
input_fa$VISIT <- NULL
output_df <- map(input_fa, ~ { as.character(.)}) %>% rbind(data.frame(), .)
write.csv(output_df, here('TEST', 'temp', 'dummyFA.csv'), quote=T, row.names=F)
