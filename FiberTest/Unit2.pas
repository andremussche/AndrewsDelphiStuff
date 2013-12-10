unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  DSharp.Core.Fibers,
  AsyncCalls;

{$R *.dfm}

type
  TAsyncCallAnonymFunc = class(TAsyncCall)
  private
    FFiber: TFiber;
    FProc: TProc;
  protected
    function ExecuteAsyncCall: Integer; override;
  public
    constructor Create(AProc: TProc);
    procedure ExecuteAsyncAndYield;
  end;

procedure Await(aAnonymousProc: TProc);
var
  async: IAsyncCall;
begin
  async := TAsyncCallAnonymFunc.Create(aAnonymousProc);
  TAsyncCallAnonymFunc(async).ExecuteAsyncAndYield;
  async.Sync; //will re-raise internal exception if present
end;

procedure Awaitable(aAnonymousProc: TProc);
begin
  TActionFiber.Create(aAnonymousProc, False{no threads but only mainthread?})
              .Resume;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  Awaitable(             //run in anonymous procedure in a fiber
    procedure
    begin
      Button1.Caption := 'busy';
      Button1.Enabled := False;
      Self.Caption    := 'GUI is fully resposive!';

      try
        //await: it will pause current fiber till "sleep" is finished
        //note: GUI will be responsive in the mean time!
        Await(
          procedure
          begin
            Sleep(5 * 1000);
          end);
      finally
        Button1.Enabled := True;
        Button1.Caption := 'done';
      end;

      //exception handling works too
      try
        Await(
          procedure
          begin
            Sleep(2 * 1000);
            raise Exception.Create('Error!');
          end);
      except
        on E:Exception do
          Button1.Caption := e.Message;
      end;

    end);
end;

{ TAsyncCallAnonymFunc }

constructor TAsyncCallAnonymFunc.Create(AProc: TProc);
begin
  inherited Create;
  FProc  := AProc;
  FFiber := TFiber.CurrentFiber;
end;

procedure TAsyncCallAnonymFunc.ExecuteAsyncAndYield;
begin
  ExecuteAsync;
  FFiber.Yield;    //pause current fiber
end;

function TAsyncCallAnonymFunc.ExecuteAsyncCall: Integer;
begin
  Result := 0;
  try
    FProc();
  finally
    TThread.Queue(nil,
      procedure
      begin
        FFiber.Resume;     //resume fiber again
      end);
  end;
end;

end.
