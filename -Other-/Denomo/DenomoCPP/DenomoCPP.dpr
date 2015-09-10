library DenomoCPP;

uses
  SysUtils, Classes, Windows,
  DenomoCPPMain in 'DenomoCPPMain.pas';

{$R *.res}

exports
  GetCPPService,
  GetCPPService name '_GetCPPService@4',
  GetCPPService name 'GetCPPService@4';

procedure DllMain(Reason: Integer);
begin
  if Reason = DLL_PROCESS_ATTACH then
    InitStartDll
  else
  begin
    if Reason = DLL_PROCESS_DETACH then
      DeInitDll;
  end;
end;

begin
  DllProc := @DllMain;
  DllProc(DLL_PROCESS_ATTACH) ;
end.


