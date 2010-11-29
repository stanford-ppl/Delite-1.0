@echo off

SET XLP=%DELITE_HOME%\scala\ext_libs
SET XLS=%XLP%\jsr166y.jar;%XLP%\ScalaCheck-1.5.jar;%XLP%\log4j-1.2.15.jar

scala -cp %XLS%;%DELITE_HOME%\scala\classes %1 %2 %3 %4 %5 %6 %7 %8 %9
