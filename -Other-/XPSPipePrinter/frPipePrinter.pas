unit frPipePrinter;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TframPipePrinter = class(TFrame)
    edtPipe: TEdit;
    Label1: TLabel;
    cmbxPrinter: TComboBox;
    Label2: TLabel;
    btnDelete: TButton;
    lblPrinted: TLabel;
    tmrPrinted: TTimer;
    procedure tmrPrintedTimer(Sender: TObject);
  private
    FSectionName: string;
    FPrintCount: Integer;
    function GetOutputPrinter: string;
    function GetPipeName: string;
    procedure SetOutputPrinter(const Value: string);
    procedure SetPipeName(const Value: string);
    procedure SetPrintCount(const Value: Integer);
  public
    procedure  AfterConstruction;override;
    destructor Destroy; override;

    property SectionName  : string read FSectionName     write FSectionName;
    property PipeName     : string read GetPipeName      write SetPipeName;
    property OutputPrinter: string read GetOutputPrinter write SetOutputPrinter;

    property PrintCount: Integer read FPrintCount write SetPrintCount;
  end;

implementation

uses
  Printers;

{$R *.dfm}

{ TframPipePrinter }

procedure TframPipePrinter.AfterConstruction;
begin
  inherited;
  edtPipe.Text := 'printpipe1';

  //load available printers
  cmbxPrinter.Items.Text := Printer.Printers.Text;
end;

destructor TframPipePrinter.Destroy;
begin
  inherited;
end;

function TframPipePrinter.GetOutputPrinter: string;
begin
  Result := cmbxPrinter.Text;
end;

function TframPipePrinter.GetPipeName: string;
begin
  Result := edtPipe.Text;
end;

procedure TframPipePrinter.SetOutputPrinter(const Value: string);
begin
  cmbxPrinter.ItemIndex := cmbxPrinter.Items.IndexOf(Value);
end;

procedure TframPipePrinter.SetPipeName(const Value: string);
begin
  edtPipe.Text := Value;
end;

procedure TframPipePrinter.SetPrintCount(const Value: Integer);
begin
  FPrintCount := Value;
  lblPrinted.Caption := Format('Printed: %d',[Value]);

  if Value > 0 then
  begin
    Self.Color := clLime;
    tmrPrinted.Enabled := True;
  end;
end;

procedure TframPipePrinter.tmrPrintedTimer(Sender: TObject);
begin
  tmrPrinted.Enabled := False;
  Self.Color := clBtnFace;
end;

end.
