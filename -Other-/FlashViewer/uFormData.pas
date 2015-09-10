unit uFormData;

interface

uses
  Windows, Classes, SysUtils,
  RttiClasses;

type
  TBaseData = class(TRttiEnabled);

  TFlashData = class;
  TFlashPage = class;
  TPageData  = class;
  TQuestionData  = class;

  TFlashPageArray = array of TFlashPage;
  TPageDataArray  = array of TPageData;
  TQuestionDataArray = array of TQuestionData;

  TFlashData = class(TBaseData)
  private
    FPages: TFlashPageArray;
    FXMLFileName: TFilename;
    function GetPageCount: Integer;
    procedure SetPageCount(const Value: Integer);
  public
    procedure LoadFromFile(const aXMLFile: TFilename);
    procedure SaveToFile(const aXMLFile: TFilename);
    property  XMLFileName: TFilename read FXMLFileName;

    property PageCount: Integer read GetPageCount write SetPageCount;
  published
    property Pages: TFlashPageArray read FPages write FPages;
  end;

  TFlashPage = class(TBaseData)
  private
    FData: TPageDataArray;
    FQuestions: TQuestionDataArray;
  public
    function DataCount: Integer;
    function AddData  : TPageData;

    function QuestionCount: Integer;
    function AddQuestion  : TQuestionData;
  published
    property Data: TPageDataArray read FData write FData;
    property Questions: TQuestionDataArray read FQuestions write FQuestions;
  end;

  TSize = class(TBaseData)
  private
    FWidth: Integer;
    FTop: Integer;
    FHeight: Integer;
    FLeft: Integer;
  public
//    procedure  AfterConstruction;override;
//    destructor Destroy;override;
  published
    property Top   : Integer read FTop    write FTop;
    property Left  : Integer read FLeft   write FLeft;
    property Width : Integer read FWidth  write FWidth;
    property Height: Integer read FHeight write FHeight;
  end;

  TPageData = class(TBaseData)
  private
    FAnswerText: string;
//    FButtonText: string;
//    FButtonPos: TSize;
    FAnswerPos: TSize;
    FFontSize: Integer;
  published
    //property ButtonText: string read FButtonText write FButtonText;
   // property ButtonPos : TSize  read FButtonPos  write FButtonPos;

    property AnswerText: string read FAnswerText write FAnswerText;
    property AnswerPos : TSize  read FAnswerPos  write FAnswerPos;
    property FontSize  : Integer read FFontSize  write FFontSize;
  end;

  TQuestionData = class(TBaseData)
  private
    FQuestionPos: TSize;
    FImageFile: TFileName;
  published
    property QuestionPos: TSize   read FQuestionPos write FQuestionPos;
    property ImageFile: TFileName read FImageFile   write FImageFile;
  end;

implementation

uses
  XMLFile;

{ TFlashData }

procedure TFlashData.LoadFromFile(const aXMLFile: TFilename);
var
  temp: TFlashData;
begin
  //create empty file
  if not FileExists(aXMLFile) then
  begin
    temp := TFlashData.Create;
    try
      temp.SaveToFile(aXMLFile);
    finally
      temp.Free
    end;
  end;

  FXMLFileName := aXMLFile;
  Self.PageCount := 0;  //clear
  XMLFile.LoadFromFile(aXMLFile, Self);
end;

function TFlashData.GetPageCount: Integer;
begin
  Result := Length(FPages);
end;

procedure TFlashData.SaveToFile(const aXMLFile: TFilename);
begin
  XMLFile.SaveToFile(aXMLFile, Self);
end;

procedure TFlashData.SetPageCount(const Value: Integer);
var
  i: Integer;
begin
  for i := Value to High(FPages) do
  begin
    if FPages[i] <> nil then
    begin
      FPages[i].Free;
      FPages[i] := nil;
    end;
  end;

  SetLength(FPages, Value);

  for i := 0 to High(FPages) do
  begin
    if FPages[i] = nil then
      FPages[i] := TFlashPage.Create;
  end;
end;

{ TFlashPage }

function TFlashPage.AddData: TPageData;
begin
  Result := TPageData.Create;
//  Result.ButtonPos.Height := 25;
//  Result.ButtonPos.Width  := 50;
  Result.AnswerPos.Height := 100;
  Result.AnswerPos.Width  := 200;

  SetLength(FData, Length(FData)+1);
  FData[High(FData)] := Result;
end;

function TFlashPage.AddQuestion: TQuestionData;
begin
  Result := TQuestionData.Create;
  Result.QuestionPos.Height := 30;
  Result.QuestionPos.Width  := 30;

  SetLength(FQuestions, Length(FQuestions)+1);
  FQuestions[High(FQuestions)] := Result;
end;

function TFlashPage.DataCount: Integer;
begin
  Result := Length(FData);
end;

function TFlashPage.QuestionCount: Integer;
begin
  Result := Length(FQuestions);
end;

end.
