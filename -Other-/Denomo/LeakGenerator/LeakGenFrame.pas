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

The Original Code is LeakGenFrame.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit LeakGenFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls,
  LeakGen, ToolWin, ActnList;

type
  TFrameLeakGen = class(TFrame)
    ListViewStrategy: TListView;
    PanelRight: TPanel;
    RichEditDesc: TRichEdit;
    Panel1: TPanel;
    RichEditReport: TRichEdit;
    ToolBar1: TToolBar;
    ActionList1: TActionList;
    ActionLeak: TAction;
    ToolButton1: TToolButton;
    BtnClear: TButton;
    procedure ActionLeakUpdate(Sender: TObject);
    procedure ActionLeakExecute(Sender: TObject);
    procedure ListViewStrategyChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure BtnClearClick(Sender: TObject);
  private
    { Private declarations }
    FInited: Boolean;
  protected
    procedure ExecuteLeak;
    procedure UpdateDescription;

    procedure OnLeakGeneratorOutputString(const S: string);

    property Inited: Boolean read FInited write FInited;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    procedure Init;
  end;

implementation

{$R *.dfm}

{ TFrameLeakGen }

procedure TFrameLeakGen.Init;
var
  I: Integer;
begin
  if Inited then
    Exit;

  ListViewStrategy.Clear;
  for I := 0 to LeakGenerator.StrategyManager.StrategyList.Count - 1 do
  begin
    ListViewStrategy.AddItem(
      LeakGenerator.StrategyManager.StrategyList[I].Name, TObject(I));
  end;

  if ListViewStrategy.Items.Count > 0 then
    ListViewStrategy.Selected := ListViewStrategy.Items[0];

  UpdateDescription;

  Inited := True;
end;

procedure TFrameLeakGen.ActionLeakUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := ListViewStrategy.Selected <> nil;
end;

procedure TFrameLeakGen.ActionLeakExecute(Sender: TObject);
begin
  ExecuteLeak;
end;

procedure TFrameLeakGen.ExecuteLeak;
begin
  if ListViewStrategy.Selected <> nil then
    LeakGenerator.PerformLeak(Integer(ListViewStrategy.Selected.Data));
end;

procedure TFrameLeakGen.ListViewStrategyChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if Change = ctState then
    UpdateDescription;
end;

procedure TFrameLeakGen.UpdateDescription;
begin
  if ListViewStrategy.Selected <> nil then
    RichEditDesc.Text := ''
      + LeakGenerator.StrategyManager.StrategyList[Integer(ListViewStrategy.Selected.Data)].Desc;
end;

procedure TFrameLeakGen.OnLeakGeneratorOutputString(const S: string);
begin
  RichEditReport.Lines.Add(S);

  RichEditReport.SelStart := -1;
  RichEditReport.SelLength := 0;
  RichEditReport.Perform(EM_SCROLLCARET, 0, 0);
end;

constructor TFrameLeakGen.Create(AOwner: TComponent);
begin
  inherited;

  LeakGenerator.OnOutputString := OnLeakGeneratorOutputString;
end;

procedure TFrameLeakGen.BtnClearClick(Sender: TObject);
begin
  RichEditReport.Clear;
end;

end.
