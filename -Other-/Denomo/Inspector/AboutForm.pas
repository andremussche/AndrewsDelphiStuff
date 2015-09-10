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

The Original Code is AboutForm.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit AboutForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ShellAPI;

type
  TFormAbout = class(TForm)
    RichEdit: TRichEdit;
    BtnOK: TButton;
    LabelURL: TLabel;
    LabelMail: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LabelURLClick(Sender: TObject);
    procedure LabelURLMouseEnter(Sender: TObject);
    procedure LabelURLMouseLeave(Sender: TObject);
  private
    { Private declarations }
    procedure InitAboutInfo;

    procedure URLMouseHover(ALabel: TLabel);
    procedure URLMouseLeave(ALabel: TLabel);
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

function GetProductVersion: Int64;
type
  PVS_FIXEDFILEINFO = ^VS_FIXEDFILEINFO;
var
  lSize: Integer;
  lAppName: string;
  C: Cardinal;
  lBuf: PChar;
  lFFI: PVS_FIXEDFILEINFO;
  P: Pointer;
begin
  Result := 0;
  lAppName := ParamStr(0);
  C := 0;
  lSize := GetFileVersionInfoSize(PChar(lAppName), C);
  GetMem(lBuf, lSize);
  try
    if GetFileVersionInfo(PChar(lAppName), C, lSize, lBuf) then
    begin
      C := SizeOf(lFFI);
      VerQueryValue(lBuf, '\', P, C);
      lFFI := PVS_FIXEDFILEINFO(P);
      Result := lFFI^.dwFileVersionMS;
      Result := (Result shl 32) + lFFI^.dwFileVersionLS;
    end;
  finally
    FreeMem(lBuf);
  end;
end;

function GetProductVersionString(AVersion: Int64): string;
var
  vMajor, vMinor, vRevision{, vBuild}: Integer;
begin
  vMajor := (AVersion shr 48) and $ffff;
  vMinor := (AVersion shr 32) and $ffff;
  vRevision := (AVersion shr 16) and $ffff;
//  vBuild := (AVersion shr 0) and $ffff;

//  Result := Format('%d.%d.%d.%d', [ vMajor, vMinor, vRevision, vBuild ]);
  Result := Format('%d.%d.%d', [ vMajor, vMinor, vRevision ]);
end;

{ TFormAbout }

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  InitAboutInfo;
end;

procedure TFormAbout.InitAboutInfo;
var
  lRS: TResourceStream;
  lMS: TMemoryStream;
  S: string;
  lVersion: string;
begin
  RichEdit.Align := alNone;
  RichEdit.Top := 4;
  RichEdit.Left := 4;
  RichEdit.Width := RichEdit.Parent.ClientWidth - RichEdit.Left * 2;

  LabelURL.Caption := 'http://www.kbasm.com/';
  URLMouseLeave(LabelURL);
  LabelURL.Left := RichEdit.Left;
  LabelURL.Top := RichEdit.Top + RichEdit.Height + 8;

  LabelMail.Caption := 'mailto:kbasm.com@gmail.com';
  URLMouseLeave(LabelMail);
  LabelMail.Left := LabelURL.Left;
  LabelMail.Top := LabelURL.Top + LabelURL.Height + 4;

  try
    S := '';
    lRS := TResourceStream.Create(HInstance, 'AboutText', RT_RCDATA);
    try
      lRS.Position := 0;
      SetLength(S, lRS.Size);
      lRS.Read(S[1], lRS.Size);
    finally
      lRS.Free;
    end;
    lVersion := GetProductVersionString(GetProductVersion);
    S := StringReplace(S, '##ver', lVersion, [ rfReplaceAll ]);
    lMS := TMemoryStream.Create;
    try
      lMS.Write(S[1], Length(S));
      lMS.Position := 0;
      RichEdit.Lines.LoadFromStream(lMS);
    finally
      lMS.Free;
    end;
  except
  end;
end;

procedure TFormAbout.URLMouseHover(ALabel: TLabel);
begin
//  ALabel.Font.Color := clRed;
  ALabel.Font.Style := ALabel.Font.Style + [ fsItalic ];
end;

procedure TFormAbout.URLMouseLeave(ALabel: TLabel);
begin
  ALabel.Font.Color := clBlue;
  ALabel.Font.Style := ALabel.Font.Style - [ fsItalic ];
end;

procedure TFormAbout.LabelURLClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(TLabel(Sender).Caption), nil, nil, 0);
end;

procedure TFormAbout.LabelURLMouseEnter(Sender: TObject);
begin
  URLMouseHover(TLabel(Sender));
end;

procedure TFormAbout.LabelURLMouseLeave(Sender: TObject);
begin
  URLMouseLeave(TLabel(Sender));
end;

end.

