Denomo for C++ a memory and resource leak detection tool for
Microsoft VC and Borland C++ Builder (BCB).
It supports many useful features to help you to detect
and locate the leak quickly.

1, Author and contact

This work's original author is Wang Qi.
Website: http://www.kbasm.com/
Mail: kbasm.com@gmail.com


2, License
The license is MPL, please read license.txt for more information.


3, Quick start
A, Modify your project code to include the proper header file.
   Include "denomo.h" in any one of your C++ source code.

B, Link to DenomoCPP.dll
   To link the DLL statically, include the library file "DenomoCPP_BC.lib"
   (for BCB) or "DenomoCPP_VC.lib" (for VC) in your project. You can also
   include that library file by define "DENOMO_LIB". Read the comment in
   "denomo.h".
   To linke the DLL dynamically, define a macro "RUNTIME_DLL" before
   include the header file.
   
C, Put DenomoCPP.dll to where the exe is outputed.
   

4, C++ compilers that have been tested on.
Following compilers have be tested that can compile Denomo for C++.
BCB 5.5, BCB 6, VC 6, VC 2008 Express Edition.


5, For more help document, go to http://www.kbasm.com/ and download
   the source code of Denomo. You can read a comprehensive help document
   in it and you can also read all of the source code (written in Delphi,
   not C++).
