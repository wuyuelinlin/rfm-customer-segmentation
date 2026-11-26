@echo off
chcp 65001 >nul
cd /d "%~dp0"
set LC_ALL=
set LANG=
where Rscript >nul 2>nul
if errorlevel 1 (
  echo [提示] 未找到 Rscript，请把 R 加入 PATH，或修改本文件中的 Rscript 路径。
  echo 参考路径：C:/Program Files\R\R-4.6.2\bin\Rscript.exe
  pause
  exit /b
)
Rscript run.R
echo.
echo ========================================
echo 运行结束，结果已保存到 results 目录
echo ========================================
pause
