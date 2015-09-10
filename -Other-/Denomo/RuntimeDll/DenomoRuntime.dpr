library DenomoRuntime;

uses
  SysUtils, Windows,
  RuntimeDllMain in 'RuntimeDllMain.pas';

exports
  GetDllService;

{$R *.res}

procedure DllMain(Reason: Integer);
begin
  if Reason = DLL_PROCESS_ATTACH then
  begin
  end
  else
  begin
    if Reason = DLL_PROCESS_DETACH then
    begin
    end;
  end;
end;

begin
  DllProc := @DllMain;
  DllProc(DLL_PROCESS_ATTACH) ;
end.

