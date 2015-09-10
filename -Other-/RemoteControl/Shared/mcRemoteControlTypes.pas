unit mcRemoteControlTypes;

interface

uses
  Classes, SysUtils;
  //RttiClasses;

type
  {$TYPEINFO ON}
  TBaseRemoteObject = class //(TRttiEnabled)
  private
    FName: string;
    FOwner: TBaseRemoteObject;
  protected
    procedure AutoCreateRTTIStuff;
  published
    constructor Create(aOwner: TBaseRemoteObject; const aName: string);overload;
    procedure   AfterConstruction;override;

    function  Enabled : Boolean;
    function  Visible : Boolean;
    function  CanFocus: Boolean;
    function  Exists: Boolean;

    procedure CheckEnabled(aTimeOut: Integer = 5 * 1000);
    procedure CheckVisible(aTimeOut: Integer = 5 * 1000);
    procedure CheckCanFocus(aTimeOut: Integer = 5 * 1000);
    procedure CheckExists(aTimeOut: Integer = 5 * 1000);

    //Parent: TBaseRemoteObject;
    property Owner: TBaseRemoteObject read FOwner;
    property Name : string            read FName write FName;
    //    
    function FullPath: string;
  end;
  TBaseRemoteObjectClass = class of TBaseRemoteObject;

  TForm = class(TBaseRemoteObject)
  public
    procedure Close;
  end;

  TAction = class(TBaseRemoteObject)
  published
    function Execute: Boolean;
  end;
  TRBKAction = class(TAction)
  end;

  TButton = class(TBaseRemoteObject)
  private
    function GetCaption: string;
    procedure SetCaption(const Value: string);
  public
    procedure Click;
    property  Caption: string read GetCaption write SetCaption;
  end;
  TEdit = class(TBaseRemoteObject)
  private
    function GetText: string;
    procedure SetText(const Value: string);
  public
    property  Text : string  read GetText  write SetText;
  end;
  TGridPanel = class(TBaseRemoteObject)
  end;

  TAdvGlowButton = class(TBaseRemoteObject)
  public
    function Click: Boolean;
  end;
  TAdvSpeedButton = class(TBaseRemoteObject)
  public
    function Click: Boolean;
  end;
  TAdvEdit = class(TBaseRemoteObject)
  end;
  TAdvSmoothTabPager = class(TBaseRemoteObject)
  private
    function GetActivePageIndex: Integer;
    procedure SetActivePageIndex(const Value: Integer);
  public
    property ActivePageIndex: Integer read GetActivePageIndex write SetActivePageIndex;
  end;
  TAdvSmoothListBox = class(TBaseRemoteObject)
  private
    function GetSelectedItemIndex: integer;
    procedure SetSelectedItemIndex(const Value: integer);
  public
    procedure Click;
    property  SelectedItemIndex: integer read GetSelectedItemIndex write SetSelectedItemIndex;
  end;
  TEditButton = class(TBaseRemoteObject)
  published
    AdvSpeedButton : TAdvSpeedButton;
  end;
  TRBKTouchEditBtn = class(TBaseRemoteObject)
  private
    function GetText: string;
    function GetValue: Variant;
    procedure SetText(const Value: string);
    procedure SetValue(const Value: Variant);
  public
    function  ClickBtn: Boolean;
    property  Value: Variant read GetValue write SetValue;
    property  Text : string  read GetText  write SetText;
  published
    EditButton     : TEditButton;
  end;
  TAdvSmoothCalendar = class(TBaseRemoteObject)
  public
    procedure SetDate(aNewDate: TDate);
  end;
  TAdvSmoothTouchKeyBoard = class(TBaseRemoteObject)
  end;
  TAdvColumnGrid = class(TBaseRemoteObject)
  private
    function GetRowCount: Integer;
  public
    property RowCount: Integer read GetRowCount;
  end;

  TBaseRemoteFrame = class(TBaseRemoteObject)
  end;
  TFrame = class(TBaseRemoteFrame)
  end;

  TBaseRemoteTouchMain = class(TBaseRemoteObject)
  private
    FactMelding: TAction;
    FactBack: TAction;
    FactHome: TAction;
    FactExit: TAction;
    FactNext: TAction;
  published
    property actHome    : TAction read FactHome write FactHome;
    property actBack    : TAction read FactBack write FactBack;
    property actNext    : TAction read FactNext write FactNext;
    property actMelding : TAction read FactMelding write FactMelding;
    property actExit    : TAction read FactExit write FactExit;
  end;

implementation

uses
  mcRemoteControlExecuter, Rtti, TypInfo, Messages;

var
  RttiCache: TRttiContext;

{ TAction }

function TAction.Execute: Boolean;
begin
  Result := TRemoteControlExecutor.Execute(Self, 'Enabled', []).AsBoolean;
  TRemoteControlExecutor.Execute_Async(Self, 'Execute', []);  //async, because can do showmodal
end;

{ TRBKTouchEditBtn }

function TRBKTouchEditBtn.ClickBtn: Boolean;
begin
  Result := TRemoteControlExecutor.Execute(Self, 'Enabled', []).AsBoolean;
  TRemoteControlExecutor.Execute_Async(Self, 'ClickBtn', []);  //async, because can do showmodal
  //TRemoteControlExecutor.PostMessage(Self, WM_LBUTTONDOWN, 0, 0);  //async, because can do showmodal
  //Result := Result and
  //          Self.EditButton.AdvSpeedButton.Click;
end;

{ TBaseRemoteTouchFrame }

//function TBaseRemoteTouchFrame.GoNext: Boolean;
//begin
//  Result := TRemoteControlExecutor.Execute(Self, 'GoNext',[]).AsBoolean;
//end;

function TRBKTouchEditBtn.GetText: string;
begin
  Result := TRemoteControlExecutor.Execute(Self, 'Text', []).AsString;
end;

function TRBKTouchEditBtn.GetValue: Variant;
begin
  Result := TRemoteControlExecutor.Execute(Self, 'Value', []).AsVariant;
end;

procedure TRBKTouchEditBtn.SetText(const Value: string);
begin
  TRemoteControlExecutor.Execute(Self, 'Text', [Value]);
end;

procedure TRBKTouchEditBtn.SetValue(const Value: Variant);
begin
  TRemoteControlExecutor.Execute(Self, 'Value', [TValue.FromVariant(Value)]);
end;

{ TAdvSmoothCalendar }

procedure TAdvSmoothCalendar.SetDate(aNewDate: TDate);
begin
  TRemoteControlExecutor.Execute(Self, 'SelectedDate',[aNewDate]);
end;

{ TBaseRemoteObject }

procedure TBaseRemoteObject.AfterConstruction;
begin
  inherited;
  AutoCreateRTTIStuff;
end;

procedure TBaseRemoteObject.AutoCreateRTTIStuff;
var
  rttitype: TRttiType;
  rttiprop: TRttiProperty;
  rttifield: TRttiField;
begin
  rttitype := RttiCache.GetType(Self.ClassType);

  for rttiprop in rttitype.GetProperties do
  begin
    rttiprop.Name;

    if rttiprop.PropertyType.IsInstance and
       rttiprop.IsWritable and
       (rttiprop.Visibility in [mvPublished]) and
       (rttiprop.GetValue(Self).AsObject = nil) and
       rttiprop.PropertyType.AsInstance.MetaclassType.InheritsFrom(TBaseRemoteObject) then
    begin
      rttiprop.SetValue( Self,
                         TBaseRemoteObjectClass(rttiprop.PropertyType.AsInstance.MetaclassType)
                           .Create( Self, rttiprop.Name )
                       );
    end;
  end;

  for rttifield in rttitype.GetFields do
  begin
    rttifield.Name;

    if rttifield.FieldType.IsInstance and
       (rttifield.Visibility in [mvPublished]) and
       (rttifield.GetValue(Self).AsObject = nil) and
       rttifield.FieldType.AsInstance.MetaclassType.InheritsFrom(TBaseRemoteObject) then
    begin
      rttifield.SetValue( Self,
                          TBaseRemoteObjectClass(rttifield.FieldType.AsInstance.MetaclassType)
                            .Create( Self, rttifield.Name )
                       );
    end;
  end;

end;

function TBaseRemoteObject.CanFocus: Boolean;
begin
  Result := TRemoteControlExecutor.Execute(Self, 'CanFocus', []).AsBoolean;
end;

procedure TBaseRemoteObject.CheckCanFocus(aTimeOut: Integer);
begin
  if not TRemoteControlExecutor.Execute(Self, 'CanFocus', [], aTimeOut).AsBoolean then
    raise Exception.CreateFmt('Cannot focus "%s"', [Self.FullPath]);
end;

procedure TBaseRemoteObject.CheckEnabled(aTimeOut: Integer);
begin
  if not TRemoteControlExecutor.Execute(Self, 'Enabled', [], aTimeOut).AsBoolean then
    raise Exception.CreateFmt('Object not enabled: "%s"', [Self.FullPath]);
end;

procedure TBaseRemoteObject.CheckExists(aTimeOut: Integer);
begin
  if not TRemoteControlExecutor.Exists(Self, aTimeOut) then
    raise Exception.CreateFmt('Object does not exist: "%s"', [Self.FullPath]);
end;

procedure TBaseRemoteObject.CheckVisible(aTimeOut: Integer);
begin
  if not TRemoteControlExecutor.Execute(Self, 'Visible', [], aTimeOut).AsBoolean then
    raise Exception.CreateFmt('Object not visible: "%s"', [Self.FullPath]);
end;

constructor TBaseRemoteObject.Create(aOwner: TBaseRemoteObject; const aName: string);
begin
  Create;
  FName  := aName;
  FOwner := aOwner;
end;

function TBaseRemoteObject.Enabled: Boolean;
begin
  Result := TRemoteControlExecutor.Execute(Self, 'Enabled', []).AsBoolean;
end;

function TBaseRemoteObject.Exists: Boolean;
begin
  Result := TRemoteControlExecutor.Exists(Self);
end;

function TBaseRemoteObject.FullPath: string;
var
  o: TBaseRemoteObject;
begin
  Result := Self.Name;
  o      := Self.Owner;
  while o <> nil do
  begin
    Result := o.Name + '.' + Result; 
    o := o.Owner;
  end;
end;

function TBaseRemoteObject.Visible: Boolean;
begin
  Result := TRemoteControlExecutor.Execute(Self, 'Visible', []).AsBoolean;
end;

{ TAdvGlowButton }

function TAdvGlowButton.Click: Boolean;
begin
  Result := //TRemoteControlExecutor.Execute(Self, 'Enabled', []).AsBoolean and
            //TRemoteControlExecutor.Execute(Self, 'Visible', []).AsBoolean and
            TRemoteControlExecutor.Execute(Self, 'CanFocus', []).AsBoolean;

  if Result then
    TRemoteControlExecutor.Execute_Async(Self, 'Click', []);
    //TRemoteControlExecutor.PostMessage(Self, WM_LBUTTONDOWN, 0, 0);  //async, because can do showmodal
end;

{ TAdvSpeedButton }

function TAdvSpeedButton.Click: Boolean;
begin
  Result := TRemoteControlExecutor.Execute(Self, 'Enabled', []).AsBoolean;
  TRemoteControlExecutor.PostMessage(Self, WM_LBUTTONDOWN, 0, 0);  //async, because can do showmodal
end;

{ TAdvSmoothTabPager }

function TAdvSmoothTabPager.GetActivePageIndex: Integer;
begin
  Result := TRemoteControlExecutor.Execute(Self, 'ActivePageIndex', []).AsInteger;
end;

procedure TAdvSmoothTabPager.SetActivePageIndex(const Value: Integer);
begin
  TRemoteControlExecutor.Execute(Self, 'ActivePageIndex', [Value]);
end;

{ TAdvColumnGrid }

function TAdvColumnGrid.GetRowCount: Integer;
begin
  Result := TRemoteControlExecutor.Execute(Self, 'RowCount', []).AsInteger;
end;

{ TAdvSmoothListBox }

procedure TAdvSmoothListBox.Click;
begin
  TRemoteControlExecutor.PostMessage(Self, WM_LBUTTONDOWN, 0, 0);
end;

function TAdvSmoothListBox.GetSelectedItemIndex: integer;
begin
  Result := TRemoteControlExecutor.Execute(Self, 'SelectedItemIndex', []).AsInteger;
end;

procedure TAdvSmoothListBox.SetSelectedItemIndex(const Value: integer);
begin
  TRemoteControlExecutor.Execute(Self, 'SelectedItemIndex', [Value]);
end;

{ TEdit }

function TEdit.GetText: string;
begin
  Result := TRemoteControlExecutor.Execute(Self, 'Text', []).AsString;
end;

procedure TEdit.SetText(const Value: string);
begin
  TRemoteControlExecutor.Execute(Self, 'Text', [Value]);
end;

{ TButton }

procedure TButton.Click;
begin
  TRemoteControlExecutor.PostMessage(Self, WM_LBUTTONDOWN, 0, 0);
  TRemoteControlExecutor.PostMessage(Self, WM_LBUTTONUP,   0, 0);
end;

function TButton.GetCaption: string;
begin
  Result := TRemoteControlExecutor.Execute(Self, 'Caption', []).AsString;
end;

procedure TButton.SetCaption(const Value: string);
begin
  TRemoteControlExecutor.Execute(Self, 'Caption', [Value]);
end;

{ TForm }

procedure TForm.Close;
begin
  TRemoteControlExecutor.Execute(Self, 'Close', []);
end;

initialization
  RttiCache := TRttiContext.Create;

finalization
  RttiCache.Free;

end.
