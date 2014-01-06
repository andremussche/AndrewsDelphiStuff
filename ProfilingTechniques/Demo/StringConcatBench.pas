// See http://www.delphitools.info
unit StringConcatBench;

interface

uses Windows, Classes, SysUtils,
     dwsUtils,
     SynCommons,
     HVStringData;

function UseStringBuilder : String;
function UseStringBuilderPreallocated : String;
function UseStringStream : String;
function UseStringStream2 : String;
function UseConcatTrivial : String;
function UseConcatPreAllocated : String;
function UseConcatFormat : String;
function UseWOBS : String;
function UseTextWriter : String;
function UseStringData : String;

type
   TFunc = function : String;

function Measure(f : TFunc) : String;
function MeasureThreaded(f : TFunc) : String;

var
   NB: Integer = 10 * 1000;
   NB_THREADS: Integer = 4;
   MEASURE_ITER: Integer = 100 * 1000;
   MEASURE_THREAD_ITER: Integer = 1000 * 1000;

implementation

function UseStringBuilder : String;
var
   i : Integer;
   sb : TStringBuilder;
begin
   sb:=TStringBuilder.Create;
   try
      for i:=1 to NB do
         sb.Append(#13#10'Eating apple #').Append(i);
      Result:=sb.ToString;
   finally
      sb.Free;
   end;
end;

function UseStringBuilderPreallocated : String;
var
   i : Integer;
   sb : TStringBuilder;
begin
   sb:=TStringBuilder.Create;
   try
      sb.Capacity:=NB*21;
      for i:=1 to NB do
         sb.Append(#13#10'Eating apple #').Append(i);
      Result:=sb.ToString;
   finally
      sb.Free;
   end;
end;

function UseStringStream : String;
var
   i : Integer;
   ss : TStringStream;
begin
   ss:=TStringStream.Create;
   try
      for i:=1 to NB do begin
         ss.WriteString(#13#10'Eating apple #');
         ss.WriteString(IntToStr(i));
      end;
      Result:=ss.DataString;
   finally
      ss.Free;
   end;
end;

function UseStringStream2 : String;
var
   i : Integer;
   ss : TStringStream;
begin
   ss:=TStringStream.Create;
   try
      for i:=1 to NB do begin
         ss.WriteString(#13#10'Eating apple #'+IntToStr(i));
      end;
      Result:=ss.DataString;
   finally
      ss.Free;
   end;
end;

function UseConcatTrivial : String;
var
   i : Integer;
begin
   Result:='';
   for i:=1 to NB do
      Result:=Result+#13#10'Eating apple #'+IntToStr(i);
end;

function UseConcatPreAllocated : String;
var
   i : Integer;
begin
   Result:='a';
   SetLength(Result, NB*21);
   SetLength(Result, 1);
   for i:=1 to NB do
      Result:=Result+#13#10'Eating apple #'+IntToStr(i);
end;

function UseConcatFormat : String;
var
   i : Integer;
begin
   Result:='';
   for i:=1 to NB do
      Result:=Result+Format(#13#10'Eating apple #%d', [i]);
//      Result:=Format('%s'#13#10'Eating apple #%d', [Result, i]);
end;

function UseWOBS : String;
var
   i : Integer;
   wobs : TWriteOnlyBlockStream;
begin
   wobs:=TWriteOnlyBlockStream.Create;
   try
      for i:=1 to NB do begin
         wobs.WriteString(#13#10'Eating apple #');
         wobs.WriteString(i);
      end;
      Result:=wobs.ToString;
   finally
      wobs.Free;
   end;
end;

function UseTextWriter : String;
var
   i : Integer;
   tw : TTextWriter;
begin
   tw:=TTextWriter.CreateOwnedStream;
   try
      for i:=1 to NB do begin
         tw.AddString(#13#10'Eating apple #');
         tw.Add(Int64(i));
      end;
      Result:=UTF8ToSynUnicode(tw.Text);
   finally
      tw.Free;
   end;
end;

function UseStringData : String;
var
   i : Integer;
   sd : TStringData;
begin
   sd:=TStringData.Create;
   try
      for i:=1 to NB do begin
         sd.Append(#13#10'Eating apple #');
         sd.Append(IntToStr(i));
      end;
      Result:=sd.Data;
   finally
      sd.Free;
   end;
end;

function Measure(f : TFunc) : String;
var
   i, k : Integer;
   t1, t2, mt : Int64;
begin
   mt:=MaxInt;
   for k:=1 to 15 do begin
      QueryPerformanceCounter(t1);
      for i:=1 to MEASURE_ITER div NB do
         f();
      QueryPerformanceCounter(t2);
      Dec(t2, t1);
      if t2<mt then
         mt:=t2;
   end;
   QueryPerformanceFrequency(t1);
   Result:=Format('%.3fms', [1000*mt/t1]);
end;

type
   TTestThread = class(TThread)
      f : TFunc;
      procedure Execute; override;
   end;

var
   vLock : TRTLCriticalSection;

// Execute
//
procedure TTestThread.Execute;
var
   i : Integer;
begin
   EnterCriticalSection(vLock);
   LeaveCriticalSection(vLock);
   for i:=1 to MEASURE_THREAD_ITER div NB do
      f();
end;

function MeasureThreaded(f : TFunc) : String;
var
   i, k : Integer;
   t1, t2, mt, tf : Int64;
   threads : array of TTestThread;
begin
  SetLength(threads, NB_THREADS);

   InitializeCriticalSection(vLock);
   mt:=MaxInt;
   for i:=1 to 10 do begin
      EnterCriticalSection(vLock);
      for k:=0 to High(threads) do begin
         threads[k]:=TTestThread.Create(True);
         threads[k].f:=f;
         threads[k].FreeOnTerminate:=False;
         threads[k].Start;
      end;
      LeaveCriticalSection(vLock);
      QueryPerformanceCounter(t1);
      for k:=0 to High(threads) do
         threads[k].WaitFor;
      QueryPerformanceCounter(t2);
      for k:=0 to High(threads) do
         threads[k].Free;
      t2:=t2-t1;
      if t2<mt then  //get fastest
         mt:=t2;
   end;
   QueryPerformanceFrequency(tf);
   Result:=Format('%.3fms', [1000*mt/tf]);
   DeleteCriticalSection(vLock);
end;

end.
