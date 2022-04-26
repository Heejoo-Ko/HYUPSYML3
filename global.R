library(magrittr);library(dplyr);library(data.table);library(labelled);library(jstable)

setwd("/home/heejooko/ShinyApps/HYUPSYML3")

a <- data.table(readRDS("REAP-3-20220426.rds"))

varlist <- list(
  "Outcome" = names(a)[1:6],
  "Variables" = names(a)[c(7:28)]
)

out <- a[, .SD, .SDcols = unlist(varlist)] %>% na.omit

vars.factor <- c(varlist$Outcome, setdiff(varlist$Variables, c("Age")))
out[, (vars.factor) := lapply(.SD, factor), .SDcols = vars.factor]

out.label <- jstable::mk.lev(out)