################################################################################
### prepare tables and plots for report
################################################################################

library("icesTAF")
library("glue")
### create report directory
mkdir("report")

### save tables for report
source("report_stf_table.R")

### save tables for report
source("report_tables.R")

### save figures for report
source("report_figures.R")
