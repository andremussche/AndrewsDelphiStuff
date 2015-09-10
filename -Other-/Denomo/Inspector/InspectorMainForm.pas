{***********************************************************************
Denomo 2.1.0
http://www.kbasm.com/

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in
compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
for the specific language governing rights and limitations under the
License.

The Original Code is InspectorMainForm.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit InspectorMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Menus, ShellAPI,
  AboutForm, InspectorMainFrame;

type
  TFormMain = class(TForm)
    MainMenu1: TMainMenu;
    About1: TMenuItem;
    MMAbout: TMenuItem;
    File1: TMenuItem;
    MMExit: TMenuItem;
    N1: TMenuItem;
    MMHelpContent: TMenuItem;
    MMHelpFAQs: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MMAboutClick(Sender: TObject);
    procedure MMExitClick(Sender: TObject);
    procedure MMHelpContentClick(Sender: TObject);
    procedure MMHelpFAQsClick(Sender: TObject);
  private
    { Private declarations }
    FFrameMain: TFrameMain;

    procedure ShowHelpFile(const AFileName: string);
  protected
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FFrameMain := TFrameMain.Create(Self);
  FFrameMain.Parent := Self;
  FFrameMain.Align := alClient;

  FFrameMain.InitFrame;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FFrameMain.DeInitFrame;
end;

procedure TFormMain.MMAboutClick(Sender: TObject);
var
  lForm: TFormAbout;
begin
  lForm := TFormAbout.Create(nil);
  try
    lForm.ShowModal;
  finally
    lForm.Free;
  end;
end;

procedure TFormMain.MMExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.ShowHelpFile(const AFileName: string);
var
  lFN: string;
  S: string;
begin
  lFN := LowerCase(ExtractFileDir(ParamStr(0)));
  lFN := ExtractFilePath(lFN);

  lFN := lFN + 'help\' + AFileName;

  if not FileExists(lFN) then
  begin
    S := 'Can''t find file ' + lFN;
    Application.MessageBox(PChar(S), 'Error');
    Exit;
  end;

  lFN := 'file://' + lFN;
  ShellExecute(0, 'open', PChar(lFN), nil, nil, SW_SHOW);
end;

procedure TFormMain.MMHelpContentClick(Sender: TObject);
begin
  ShowHelpFile('index.html');
end;

procedure TFormMain.MMHelpFAQsClick(Sender: TObject);
begin
  ShowHelpFile('faq.html');
end;

end.

