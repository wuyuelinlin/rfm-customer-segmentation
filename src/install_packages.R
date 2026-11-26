# 安装项目所需的 R 包
# 使用清华 CRAN 镜像加速
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))

pkgs <- c(
  "readxl", "openxlsx", "dplyr", "tidyr", "ggplot2",
  "corrplot", "gridExtra", "scales", "fmsb", "showtext", "RColorBrewer"
)

for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    cat("Installing", p, "...\n")
    install.packages(p, quiet = TRUE)
  } else {
    cat("Already installed:", p, "\n")
  }
}

cat("\n=== 安装结果检查 ===\n")
for (p in pkgs) {
  cat(sprintf("%-14s %s\n", p,
    if (requireNamespace(p, quietly = TRUE)) "OK" else "FAILED"))
}
