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

The Original Code is LeakGeneratorMain.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit LeakGeneratorMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ShellAPI,
  LeakGen, LeakGenFrame;

type
  TFormMain = class(TForm)
    FrameLeakGen: TFrameLeakGen;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

var
  StrategyItemList0: array[0..0] of TRecordLeakStrategyItem = (
    (
      StrategyType: ST_Memory;
      MinCount: 1;
      MaxCount: 1;
      Flags: 0;

      MinSize: 10;
      MaxSize: 1024;
    )
  );

  StrategyItemList1: array[0..0] of TRecordLeakStrategyItem = (
    (
      StrategyType: ST_Object;
      MinCount: 2;
      MaxCount: 2;
      Flags: 0;

      MinDepth: 0;
      MaxDepth: 0;
    )
  );

  StrategyItemList2: array[0..0] of TRecordLeakStrategyItem = (
    (
      StrategyType: ST_Object;
      MinCount: 1;
      MaxCount: 1;
      Flags: 0;

      MinDepth: 1;
      MaxDepth: 5;
    )
  );

  StrategyItemList3: array[0..0] of TRecordLeakStrategyItem = (
    (
      StrategyType: ST_Object;
      MinCount: 1;
      MaxCount: 10;
      Flags: 0;

      MinDepth: 0;
      MaxDepth: 1;
    )
  );

  StrategyItemList4: array[0..0] of TRecordLeakStrategyItem = (
    (
      StrategyType: ST_MulThread;
      MinCount: 1;
      MaxCount: 1;
      Flags: 0;

      DestStrategyName: 'Sub strategyA';
      MinExecute: 1;
      MaxExecute: 5;
      MinThreads: 10;
      MaxThreads: 100;
    )
  );

  StrategyItemList5: array[0..0] of TRecordLeakStrategyItem = (
    (
      StrategyType: ST_FreeOnNil;
      MinCount: 1;
      MaxCount: 1;
      Flags: 0;
    )
  );

  StrategyItemList6: array[0..0] of TRecordLeakStrategyItem = (
    (
      StrategyType: ST_StringAndDynArray;
      MinCount: 1;
      MaxCount: 1;
      Flags: 0;
    )
  );

  StrategyItemList7: array[0..0] of TRecordLeakStrategyItem = (
    (
      StrategyType: ST_GDI;
      MinCount: 1;
      MaxCount: 1;
      Flags: 0;
    )
  );

  StrategyList: array[0..7] of TRecordLeakStrategy = (
    (
      Name: 'Simplest memory leak';
      Desc: 'Leak only one block between $minsize to $maxsize';
      Items: @StrategyItemList0[0];
      ItemCount: Length(StrategyItemList0)
    ),

    (
      Name: 'Simplest object leak';
      Desc: 'Leak only one object between depth of $mindepth to $maxdepth';
      Items: @StrategyItemList1[0];
      ItemCount: Length(StrategyItemList1)
    ),

    (
      Name: 'Object leak';
      Desc: 'Leak only one object between depth of $mindepth to $maxdepth';
      Items: @StrategyItemList2[0];
      ItemCount: Length(StrategyItemList2)
    ),

    (
      Name: 'Sub strategyA';
      Desc: 'For internal using';
      Items: @StrategyItemList3[0];
      ItemCount: Length(StrategyItemList3)
    ),

    (
      Name: 'Multiple thread';
      Desc: 'Multiple thread';
      Items: @StrategyItemList4[0];
      ItemCount: Length(StrategyItemList4)
    ),

    (
      Name: 'Free on freed block';
      Desc: 'Free on freed block';
      Items: @StrategyItemList5[0];
      ItemCount: Length(StrategyItemList5)
    ),

    (
      Name: 'String leaks';
      Desc: 'String leaks';
      Items: @StrategyItemList6[0];
      ItemCount: Length(StrategyItemList6)
    ),

    (
      Name: 'GDI leaks';
      Desc: 'GDI leaks';
      Items: @StrategyItemList7[0];
      ItemCount: Length(StrategyItemList7)
    )
  );

{$R *.dfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
//var
//  SFI: SHFILEINFO;
begin
//  SHGetFileInfo('c:\', FILE_ATTRIBUTE_NORMAL, SFI, SizeOf(SFI), SHGFI_SYSICONINDEX);

  LeakGenerator.StrategyManager.LoadFromItemArray(StrategyList, Length(StrategyList));

  FrameLeakGen.Align := alClient;
  FrameLeakGen.Init;
end;

end.

