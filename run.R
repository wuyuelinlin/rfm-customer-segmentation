# 入口脚本：自动切到项目根目录，再运行分析
# 用法：Rscript run.R
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_dir <- if (length(file_arg)) dirname(sub("^--file=", "", file_arg[1])) else getwd()
setwd(normalizePath(file.path(script_dir, ".")))
source("src/analysis.R", encoding = "UTF-8")
