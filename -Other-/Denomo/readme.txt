Denomo is a memory and resource leak detection tool for Delphi.
It supports many useful features to help you to detect
and locate the leak quickly.

1, Author and contact

This work's original author is Wang Qi.
Website: http://www.kbasm.com/
Mail: kbasm.com@gmail.com


2, License
The license is MPL, please read license.txt for more information.


3, Quick start
A, Modify you project's code.
   Place the unit "Denomo.pas" as the very first unit under the "uses"
   section in your project's .dpr file.
   If you use another memory manager explicitly, e.g, FastMM, put the
   unit "Denomo.pas" just under the memory manager.
B, Change project options in Delphi project.
   In Delphi, in your project to detect memory leak, go to menu
   Project->Options->Compiler, be sure "Optimization" is unchecked,
   and "Stack frames", "Debug information", "Reference info",
   "Definitions only", "Assertions", and "Use Debug DCUs" are checked.
C, Compile and run your project.
D, Run bin\LeakInspector to start monitoring.
E, Click button "Inc Session Leak Begin".
F, Do something you suspect has memory leak.
G, Click button "Inc Session Leak End", then check the output text.


4, Compiling
There is a batch file makeall.bat for compiling.
Before run it, be sure to check setpath.bat to make the path correct.
