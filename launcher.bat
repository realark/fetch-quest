@echo off
set SCRIPT_DIR=%~dp0

cd %SCRIPT_DIR%
set PATH=%SCRIPT_DIR%

fetch-quest-windows.exe
