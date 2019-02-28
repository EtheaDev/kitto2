@echo off

@set FrameworkDir=C:\WINDOWS\Microsoft.NET\Framework\
@set FrameworkVersion=v4.0.30319
@set FrameworkSDKDir=
@set PATH=%FrameworkDir%%FrameworkVersion%;%FrameworkSDKDir%;%PATH%

rem This works on Windows 7 x64 ENU.
@set PF=D:\Program Files (x86)\
@set DOCS=%PUBLIC%\Documents

@set dver=10_3

if "%dver%"=="10_1" set BDS=%PF%\Embarcadero\Studio\18.0
if "%dver%"=="10_1" set BDSCOMMONDIR=%DOCS%\Embarcadero\Studio\18.0
if "%dver%"=="10_1" set BDSINCLUDE=%PF%\Embarcadero\Studio\18.0\Include
if "%dver%"=="10_2" set BDS=%PF%\Embarcadero\Studio\19.0
if "%dver%"=="10_2" set BDSCOMMONDIR=%DOCS%\Embarcadero\Studio\19.0
if "%dver%"=="10_2" set BDSINCLUDE=%PF%\Embarcadero\Studio\19.0\Include
if "%dver%"=="10_3" set BDS=%PF%\Embarcadero\Studio\20.0
if "%dver%"=="10_3" set BDSCOMMONDIR=%DOCS%\Embarcadero\Studio\20.0
if "%dver%"=="10_3" set BDSINCLUDE=%PF%\Embarcadero\Studio\20.0\Include

del /Q ..\bin\KIDE2.exe > nul

@echo =======================
@echo Bulding KIDE2 editor...
@echo =======================
cd ..\Projects\D%dver%
msbuild /t:Build /p:config=Release /nologo KIDE2.dproj /fl /flp:logfile=KIDE2.log;verbosity=diagnostic

cd ..\..\Install

echo.
@echo =================================
CHOICE /C YN /M "Build Kitto2 Examples?"
IF ERRORLEVEL == 2 goto BuildInstaller
IF ERRORLEVEL == 1 goto BuildExamples
goto exit

:BuildExamples
@echo 
@echo =============================
@echo Bulding HelloKitto example...
@echo =============================
cd ..\..\Examples\HelloKitto\Projects\D%dver%
msbuild /t:Build /p:config=Release /nologo HelloKitto.dproj /fl /flp:logfile=HelloKitto.log;verbosity=diagnostic

@echo 
@echo ===========================
@echo Bulding TasKitto example...
@echo ===========================
cd ..\..\..\TasKitto\Projects\D%dver%
msbuild /t:Build /p:config=Release /nologo TasKitto.dproj /fl /flp:logfile=TasKitto.log;verbosity=diagnostic

@echo 
@echo ============================
@echo Bulding KEmployee example...
@echo ============================
cd ..\..\..\KEmployee\Projects\D%dver%
msbuild /t:Build /p:config=Release /nologo KEmployee.dproj /fl /flp:logfile=KEmployee.log;verbosity=diagnostic

@echo 
@echo ============================
@echo Bulding DbDemos example...
@echo ============================
cd ..\..\..\DbDemos\Projects\D%dver%
msbuild /t:Build /p:config=Release /nologo DbDemos.dproj /fl /flp:logfile=DbDemos.log;verbosity=diagnostic

cd ..\..\..\..\Kide\Install

:BuildInstaller
echo.
@echo ================================
CHOICE /C YN /M "Build Setup?"
IF ERRORLEVEL == 2 goto exit
IF ERRORLEVEL == 1 goto BuildStdSetup
goto exit

:BuildStdSetup
"%PF%\Inno Setup 5\iscc.exe" KittoSetup.iss

pause
goto exit

:exit
pause