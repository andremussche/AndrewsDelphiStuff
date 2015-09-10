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

The Original Code is InspectorBaseFrame.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit InspectorBaseFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs;

type
  TFrameBase = class(TFrame)
  private
    { Private declarations }
  public
    { Public declarations }
    procedure InitFrame; virtual;
    procedure DeInitFrame; virtual;
  end;

implementation

{$R *.dfm}

{ TFrameBase }

procedure TFrameBase.DeInitFrame;
begin

end;

procedure TFrameBase.InitFrame;
begin

end;

end.
