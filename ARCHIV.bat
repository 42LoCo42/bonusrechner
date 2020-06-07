@echo off
cd %~dp0
mkdir Archiv 2>nul
:loop
set archiv=
set /p "archiv=Archivname: "
if exist "Archiv\%archiv%" (
	echo %archiv% existiert schon!
	goto loop
)
move Datenbanken "Archiv\%archiv%"
