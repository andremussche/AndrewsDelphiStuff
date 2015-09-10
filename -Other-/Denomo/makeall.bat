cd Inspector
call mk.bat
if errorlevel 1 goto end
cd ..\

cd LeakGenerator
call mk.bat
if errorlevel 1 goto end
cd ..\

:end
