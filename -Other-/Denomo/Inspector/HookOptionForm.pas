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

The Original Code is HookOptionForm.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit HookOptionForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  DenomoUtils;

type
  TFormHookOption = class(TForm)
    GroupBoxMainFlags: TGroupBox;
    BtnOK: TButton;
    BtnCancel: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FMainFlagsCheckBoxes: array[0..HookOptionFlagCount - 1] of TCheckBox;
  protected
    procedure InitMainFlagsCheckBoxes;
    procedure OnHookOptionFlagClick(Sender: TObject);
  public
    { Public declarations }
    procedure LoadFromOption(AOption: PHookOption);
    procedure SaveToOption(AOption: PHookOption);
  end;

implementation

{$R *.dfm}

{ TFormHookOption }

procedure TFormHookOption.FormCreate(Sender: TObject);
begin
  InitMainFlagsCheckBoxes;
end;

procedure TFormHookOption.InitMainFlagsCheckBoxes;
var
  I: Integer;
  lChkBox: TCheckBox;
  Y: Integer;
begin
  Y := 16;
  for I := 0 to HookOptionFlagCount - 1 do
  begin
    lChkBox := TCheckBox.Create(Self);
    FMainFlagsCheckBoxes[I] := lChkBox;
    lChkBox.Caption := HookOptionFlagCaptions[I];
    lChkBox.Tag := 1 shl I;
    lChkBox.Parent := GroupBoxMainFlags;
    lChkBox.Checked := True;
    lChkBox.Left := 8;
    lChkBox.Top := Y;
    lChkBox.Width := lChkBox.Parent.ClientWidth - lChkBox.Left * 2;
    lChkBox.OnClick := OnHookOptionFlagClick;
    Inc(Y, lChkBox.Height + 4);
  end;

  GroupBoxMainFlags.ClientHeight := Y + 2;

  BtnOK.Top := GroupBoxMainFlags.Top + GroupBoxMainFlags.Height + 8;
  BtnCancel.Top := BtnOK.Top;
  ClientHeight := BtnOK.Top + BtnOK.Height + 8;
end;

procedure TFormHookOption.LoadFromOption(AOption: PHookOption);
var
  I: Integer;
begin
  for I := 0 to HookOptionFlagCount - 1 do
  begin
    FMainFlagsCheckBoxes[I].Checked := (AOption^.Flags
      and (FMainFlagsCheckBoxes[I].Tag)) <> 0;
  end;
end;

procedure TFormHookOption.OnHookOptionFlagClick(Sender: TObject);
var
  I: Integer;
begin
  if TCheckBox(Sender).Tag = HOF_OutputStackTrace then
  begin
    for I := 0 to HookOptionFlagCount - 1 do
    begin
      if FMainFlagsCheckBoxes[I].Tag = HOF_OutputSourceInfo then
      begin
        FMainFlagsCheckBoxes[I].Enabled := TCheckBox(Sender).Checked;
        Break;
      end;
    end;
  end;
end;

procedure TFormHookOption.SaveToOption(AOption: PHookOption);
var
  I: Integer;
begin
  AOption^.Flags := 0;

  for I := 0 to HookOptionFlagCount - 1 do
  begin
    if FMainFlagsCheckBoxes[I].Checked then
      AOption^.Flags := AOption^.Flags or Cardinal(FMainFlagsCheckBoxes[I].Tag);
  end;
end;

end.

