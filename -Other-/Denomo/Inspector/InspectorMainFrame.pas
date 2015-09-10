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

The Original Code is InspectorMainFrame.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit InspectorMainFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, ToolWin, ActnList, Menus,
  DenomoConfig, DenomoMemHook, DenomoUtils, LeakInspect, HookOptionForm, AboutForm,
  InspectorBaseFrame;

type
  TFrameMain = class(TFrameBase, IInterObjectInspectorService)
    ActionListInspect: TActionList;
    ActionMemCount: TAction;
    ActionIncMemSessionLeakBegin: TAction;
    ActionIncMemSessionLeakEnd: TAction;
    PanelLeft: TPanel;
    PanelRight: TPanel;
    RichEditOutput: TRichEdit;
    ActionListMem: TAction;
    GroupBoxParam: TGroupBox;
    ButtonMemCount: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    PanelMemTypes: TPanel;
    LabelMemTypes: TLabel;
    PanelGroupID: TPanel;
    LabelGroupID: TLabel;
    CBoxGroupID: TComboBox;
    Button4: TButton;
    ActionHostOption: TAction;
    Panel1: TPanel;
    BtnClear: TButton;
    BtnSaveToFile: TButton;
    ActionListUI: TActionList;
    PopupMenuRichEdit: TPopupMenu;
    ActionCopy: TAction;
    ActionSelectAll: TAction;
    ActionClear: TAction;
    ActionFind: TAction;
    ActionFindAgain: TAction;
    FindDlg: TFindDialog;
    Copy1: TMenuItem;
    Clear1: TMenuItem;
    SelectAll1: TMenuItem;
    N1: TMenuItem;
    Find1: TMenuItem;
    FindAgain1: TMenuItem;
    procedure ActionListInspectUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure ActionListInspectExecute(Action: TBasicAction;
      var Handled: Boolean);
    procedure BtnSaveToFileClick(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure ActionClearExecute(Sender: TObject);
    procedure ActionFindExecute(Sender: TObject);
    procedure ActionFindAgainUpdate(Sender: TObject);
    procedure ActionFindAgainExecute(Sender: TObject);
    procedure FindDlgFind(Sender: TObject);
    procedure ActionListUIUpdate(Action: TBasicAction;
      var Handled: Boolean);
  private
    { Private declarations }
    FMemTypeCheckBoxes: array[0..MEMTYPE_COUNT - 1] of TCheckBox;
    FEnabledMemTypes: Cardinal;

    FSearchTypes: TSearchTypes;
    FSearchText: string;

    procedure SetEnabledMemTypes(const Value: Cardinal);
  protected
    procedure InitMemTypeCheckBoxes;
    procedure OnMemTypeCheckBoxClick(Sender: TObject);

    procedure InitGroupIDs;
    procedure SelectGroupIDCombo(AGroupID: Integer);

    procedure EnableMemTypeCheckBoxes;

    procedure FindNextText;

    { IInterObjectInspectorService }
    function GetSessionID: Integer;
    function GetMemoryTypes: Cardinal;
    function OutputStrings(AType: Integer): Integer;
    function EditHookOption(AOption: PHookOption): Boolean;
    procedure BeforeExecuteCommand(ACommand: Integer);
    procedure AfterExecuteCommand(ACommand: Integer);
    procedure OnConnected(Sender: TObject);
  public
    { Public declarations }
    procedure InitFrame; override;
    procedure DeInitFrame; override;

    property EnabledMemTypes: Cardinal read FEnabledMemTypes write SetEnabledMemTypes;
  end;

implementation

{$R *.dfm}

procedure TFrameMain.ActionListInspectUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  TAction(Action).Enabled := (InterObject <> nil) and InterObject.Connected;
  Handled := True;
end;

procedure TFrameMain.InitMemTypeCheckBoxes;
var
  I: Integer;
  lChkBox: TCheckBox;
  Y: Integer;
begin
  PanelMemTypes.BevelInner := bvNone;
  PanelMemTypes.BevelOuter := bvNone;
  PanelMemTypes.Caption := '';

  Y := LabelMemTypes.Top + LabelMemTypes.Height + 2;
  for I := 0 to MEMTYPE_COUNT - 1 do
  begin
    lChkBox := TCheckBox.Create(Self);
    FMemTypeCheckBoxes[I] := lChkBox;
    lChkBox.Caption := MemTypeCaptions[I];
    lChkBox.Tag := I;
    lChkBox.Parent := PanelMemTypes;
    lChkBox.Checked := True;
    lChkBox.OnClick := OnMemTypeCheckBoxClick;
    lChkBox.Left := LabelMemTypes.Left;
    lChkBox.Top := Y;
    lChkBox.Width := lChkBox.Parent.ClientWidth - lChkBox.Left * 2;
    Inc(Y, lChkBox.Height + 2);
  end;

  PanelMemTypes.ClientHeight := Y + 2;

  GroupBoxParam.ClientHeight := PanelMemTypes.Height + PanelGroupID.Height + 16;
end;

procedure TFrameMain.OnConnected(Sender: TObject);
begin
  InterObject.TalkToGuest(SM_GetEnabledMemTypes, 0);
  EnabledMemTypes := InterObject.GetShareMemory^.EnumData.ResultValue;
end;

procedure TFrameMain.OnMemTypeCheckBoxClick(Sender: TObject);
var
  I: Integer;
  lCheckedCount: Integer;
  lCheckBox: TCheckBox;
begin
  EnableMemTypeCheckBoxes;
  lCheckBox := nil;
  lCheckedCount := 0;
  for I := 0 to MEMTYPE_COUNT - 1 do
  begin
    if FMemTypeCheckBoxes[I].Checked then
    begin
      lCheckBox := FMemTypeCheckBoxes[I];
      Inc(lCheckedCount);
    end;
  end;
  if lCheckedCount = 1 then
    lCheckBox.Enabled := False;
  if lCheckedCount = 0 then
  begin
    for I := 0 to MEMTYPE_COUNT - 1 do
    begin
      if FMemTypeCheckBoxes[I].Enabled then
      begin
        FMemTypeCheckBoxes[I].Checked := True;
        Break;
      end;
    end;
  end;
end;

procedure TFrameMain.InitGroupIDs;
var
  I: Integer;
begin
  PanelGroupID.BevelInner := bvNone;
  PanelGroupID.BevelOuter := bvNone;
  PanelGroupID.Caption := '';

  for I := 0 to MaxGroupID do
    CBoxGroupID.AddItem(IntToStr(I), Pointer(I));

  CBoxGroupID.ItemIndex := 0;
end;

procedure TFrameMain.SelectGroupIDCombo(AGroupID: Integer);
var
  I: Integer;
begin
  for I := CBoxGroupID.Items.Count - 1 downto 0 do
  begin
    if Integer(CBoxGroupID.Items.Objects[I]) = AGroupID then
    begin
      CBoxGroupID.ItemIndex := I;
      Break;
    end;
  end;
end;

procedure TFrameMain.SetEnabledMemTypes(const Value: Cardinal);
begin
  FEnabledMemTypes := Value;
  EnableMemTypeCheckBoxes;
end;

procedure TFrameMain.ActionListInspectExecute(Action: TBasicAction;
  var Handled: Boolean);
begin
  Handled := TAction(Action).Tag > 0;
  if Handled then
  begin
    Screen.Cursor := crHourGlass;
    try
      InterObject.ExecuteCommand(TAction(Action).Tag);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

function TFrameMain.GetSessionID: Integer;
begin
  Result := Integer(CBoxGroupID.Items.Objects[CBoxGroupID.ItemIndex]);
end;

function TFrameMain.GetMemoryTypes: Cardinal;
var
  I: Integer;
begin
  Result := 0;

  for I := 0 to MEMTYPE_COUNT - 1 do
  begin
    if FMemTypeCheckBoxes[I].Checked then
      Result := Result or (1 shl FMemTypeCheckBoxes[I].Tag);
  end;
end;

function TFrameMain.OutputStrings(AType: Integer): Integer;
var
  S: string;
  I: Integer;
begin
  InterObject.StringBufferReader.ReadAll(S);

  Result := 0;
  if S = '' then
    Exit;

  case AType of
    SOT_YesNo:
    begin
      if Application.MessageBox(PChar(S), 'Denomo', MB_YESNO) = IDYES then
        Result := 1
      else
        Result := 0;
    end;

    SOT_Print, SOT_Error:
    begin
      RichEditOutput.HideSelection := True;
      try
        I := RichEditOutput.Perform(WM_GETTEXTLENGTH, 0, 0);
        RichEditOutput.Perform(EM_SETSEL, I, I);
        RichEditOutput.SelText := S;
        RichEditOutput.Perform(EM_SETSEL, I, I + Length(S));
        if AType = SOT_Error then
          RichEditOutput.SelAttributes.Color := clRed
        else
          RichEditOutput.SelAttributes.Color := clBlack;

        RichEditOutput.SelStart := -1;
        RichEditOutput.SelLength := 0;
        RichEditOutput.Perform(EM_SCROLLCARET, 0, 0);

        if AType = SOT_Error then
        begin
          if not Application.Active then
            FlashWindow(Application.Handle, True);
        end;

        Application.ProcessMessages;
      finally
        RichEditOutput.HideSelection := False;
      end;
    end;
  end;
end;

function TFrameMain.EditHookOption(AOption: PHookOption): Boolean;
var
  lForm: TFormHookOption;
begin
  Result := False;
  lForm := TFormHookOption.Create(nil);
  try
    lForm.LoadFromOption(AOption);
    if lForm.ShowModal = mrOk then
    begin
      lForm.SaveToOption(AOption);
      Result := True;
    end;
  finally
    lForm.Free;
  end;
end;

procedure TFrameMain.EnableMemTypeCheckBoxes;
var
  I: Integer;
begin
  for I := 0 to MEMTYPE_COUNT - 1 do
  begin
    FMemTypeCheckBoxes[I].Enabled := (EnabledMemTypes
      and (1 shl FMemTypeCheckBoxes[I].Tag)) <> 0;
    if not FMemTypeCheckBoxes[I].Enabled then
      FMemTypeCheckBoxes[I].Checked := False;
  end;
end;

procedure TFrameMain.BeforeExecuteCommand(ACommand: Integer);
begin

end;

procedure TFrameMain.AfterExecuteCommand(ACommand: Integer);
var
  lGroupID: Integer;
begin
  case ACommand of
    IC_IncrementalSessionLeakBegin,
      IC_IncrementalSessionLeakEnd:
    begin
      lGroupID := InterObject.GetShareMemory^.EnumData.ResultValue;
      SelectGroupIDCombo(lGroupID);
    end;
  end;
end;

procedure TFrameMain.BtnSaveToFileClick(Sender: TObject);
var
  lSaveDlg: TSaveDialog;
begin
  lSaveDlg := TSaveDialog.Create(nil);
  try
    lSaveDlg.Options := lSaveDlg.Options + [ ofOverwritePrompt ];
    if lSaveDlg.Execute then
    begin
      Screen.Cursor := crHourGlass;
      try
        RichEditOutput.Lines.SaveToFile(lSaveDlg.FileName);
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  finally
    lSaveDlg.Free;
  end;
end;

procedure TFrameMain.InitFrame;
begin
  inherited;

  InitMemTypeCheckBoxes;
  InitGroupIDs;

  InitInspector(Self);

  ActionMemCount.Tag := IC_MemCount;
  ActionIncMemSessionLeakBegin.Tag := IC_IncrementalSessionLeakBegin;
  ActionIncMemSessionLeakEnd.Tag := IC_IncrementalSessionLeakEnd;
  ActionListMem.Tag := IC_ListMem;
  ActionHostOption.Tag := IC_EditHookOption;

  RichEditOutput.MaxLength := MaxInt;
end;

procedure TFrameMain.DeInitFrame;
begin
  inherited;

  InterObject.InspectorService := nil;

  DeInitInspector;
end;

procedure TFrameMain.ActionCopyExecute(Sender: TObject);
begin
  inherited;

  RichEditOutput.CopyToClipboard;
end;

procedure TFrameMain.ActionSelectAllExecute(Sender: TObject);
begin
  inherited;

  RichEditOutput.SelectAll;
end;

procedure TFrameMain.ActionClearExecute(Sender: TObject);
begin
  inherited;

  RichEditOutput.Clear;
end;

procedure TFrameMain.ActionFindExecute(Sender: TObject);
begin
  inherited;

  if RichEditOutput.SelLength > 0 then
    FindDlg.FindText := RichEditOutput.SelText;
  FindDlg.Execute;
end;

procedure TFrameMain.ActionFindAgainUpdate(Sender: TObject);
begin
  inherited;

  TAction(Sender).Enabled := True;
end;

procedure TFrameMain.ActionFindAgainExecute(Sender: TObject);
begin
  inherited;

  FindNextText;
end;

procedure TFrameMain.FindDlgFind(Sender: TObject);
begin
  FSearchTypes := [];
  if frWholeWord in FindDlg.Options then
    Include(FSearchTypes, stWholeWord);
  if frMatchCase in FindDlg.Options then
    Include(FSearchTypes, stMatchCase);

  FSearchText := FindDlg.FindText;

  FindNextText;
end;

procedure TFrameMain.FindNextText;
var
  lPos: Integer;
  lSelStart: Integer;
begin
  lSelStart := RichEditOutput.SelStart + 1;
  lPos := RichEditOutput.FindText(FSearchText,
    lSelStart, RichEditOutput.GetTextLen - lSelStart, FSearchTypes);
  if lPos >= 0 then
  begin
    RichEditOutput.SelStart := lPos;
    RichEditOutput.SelLength := Length(FSearchText);
    RichEditOutput.Perform(EM_SCROLLCARET, 0, 0);
  end;
end;

procedure TFrameMain.ActionListUIUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  inherited;

  Handled := True;
end;

end.

