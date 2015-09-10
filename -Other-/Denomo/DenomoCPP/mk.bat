call ..\setpath.bat

set OUTPUT=output

mkdir %OUTPUT%

%DDCC32% -B -$A8 -$B- -$C- -$D- -$E- -$F- -$G+ -$H+ -$I+ -$J- -$K- -$L- -$M- -$N+ -$O -$P+ -$Q- -$R- -$S- -$T- -$U- -$V+ -$W- -$X+ -$Y- -$Z1 -cg -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE; -H+ -W+ -M -$M16384,1048576 -K$00400000 -E"%OUTPUT%" -N"%OUTPUT%" -LE"c:\program files\borland\delphi7\Projects\Bpl" -LN"c:\program files\borland\delphi7\Projects\Bpl" -U"D:\DelphiComponent\Disasm32\Source;D:\DelphiComponent\jcl\source;D:\DelphiComponent\jcl\source\common;D:\DelphiComponent\jcl\source\windows;..\" -O"D:\DelphiComponent\Disasm32\Source;D:\DelphiComponent\jcl\source;D:\DelphiComponent\jcl\source\common;D:\DelphiComponent\jcl\source\windows;..\" -I"D:\DelphiComponent\Disasm32\Source;D:\DelphiComponent\jcl\source;D:\DelphiComponent\jcl\source\common;D:\DelphiComponent\jcl\source\windows;..\" -R"D:\DelphiComponent\Disasm32\Source;D:\DelphiComponent\jcl\source;D:\DelphiComponent\jcl\source\common;D:\DelphiComponent\jcl\source\windows;..\" -DDEBUG -w-UNSAFE_TYPE -w-UNSAFE_CODE -w-UNSAFE_CAST DenomoCPP.dpr
