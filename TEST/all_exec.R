#' Run a series of scripts.
#'
#' @file all_exec.R
#' @author Mariko Ohtsuka
#' @date 2021.11.22
rm(list=ls())
library(here)
source(here("R", "1-get-visit.R"))
rm(list=ls())
source(here("R", "2-extract-grade-observation.R"))
rm(list=ls())
source(here("R", "3-summarize-by-grade.R"))
rm(list=ls())
library(rmarkdown)
library(knitr)
opts_chunk$set(echo=F, comment=NA)
render(here("R", "4-output-docx.Rmd"), output_format=word_document(), output_dir=here("TEST", "temp"), output_file="test.docx")
