program RemoteControlTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

  XMLTestRunner = c:\Program Files (x86)\Embarcadero\RAD Studio\7.0\source\dUnit\Contrib\XMLReporting\XMLTestRunner.pas
  TestFramework = c:\Program Files (x86)\Embarcadero\RAD Studio\7.0\source\dUnit\src\TestFramework.pas

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  SysUtils,
  TestFramework,
  GUITestRunner,
  //XMLTestRunner,
  TextTestRunner,
  TestRemoteControl in 'TestRemoteControl.pas',
  CommunicationTypes in '..\Shared\CommunicationTypes.pas',
  BaseObject in '..\Shared\BaseObject.pas',
  mcRemoteApp in '..\Shared\mcRemoteApp.pas',
  mcRemoteControlTypes in '..\Shared\mcRemoteControlTypes.pas',
  mcRemoteControlExecuter in '..\Shared\mcRemoteControlExecuter.pas',
  RemoteConnector in '..\Shared\RemoteConnector.pas' {dmRemoteConnector: TDataModule},
  JvCreateProcess in '..\Shared\JvCreateProcess.pas',
  XMLTestRunnerTime in 'XMLTestRunnerTime.pas';

{$R *.RES}

function DoXmlOutput : boolean;
var i: integer;
begin
  Result := false;
  if ParamCount > 0 then
    for i := 1 to ParamCount do
      if LowerCase(ParamStr(i)) = '-xml' then
      begin
        Result := true;
        Break;
      end;
end;

begin
  Application.Initialize;

  if DoXmlOutput then
  begin
    with XMLTestRunnerTime.RunRegisteredTests('DUnitTesting.xml') do
      Free;
  end
  else if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

