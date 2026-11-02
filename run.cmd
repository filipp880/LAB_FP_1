@echo off
chcp 65001 >nul
cd /d "%~dp0"
scala-cli run . --server=false --main-class backup.backupApp --java-opt -Dfile.encoding=UTF-8
