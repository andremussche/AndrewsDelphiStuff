unit XMLDemo;


  {*************************************************************************}
  {                                                                         }
  {  Hitsoft Xml Object Library                                             }
  {                                                                         }
  {  Copyright (C) 2009    Hitsoft LLC. (http://opensource.hitsoft-it.com)  }
  {                                                                         }
  {  This program is free software: you can redistribute it and/or modify   }
  {  it under the terms of the GNU General Public License as published by   }
  {  the Free Software Foundation, either version 3 of the License, or      }
  {  (at your option) any later version.                                    }
  {                                                                         }
  {  This program is distributed in the hope that it will be useful,        }
  {  but WITHOUT ANY WARRANTY; without even the implied warranty of         }
  {  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          }
  {  GNU General Public License for more details.                           }
  {                                                                         }
  {  You should have received a copy of the GNU General Public License      }
  {  along with this program.  If not, see <http://www.gnu.org/licenses/>.  }
  {                                                                         }
  {*************************************************************************}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, RttiClasses;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    mmo1: TMemo;
    lbl1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  XMLFile, XMLModel;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Model: TDemoModel;
begin
  Model := TDemoModel.Create;
  Model.Number := 1;
  Model.Caption := 'Hello';
  Model.Birthday := Now;
  Model.Price := 235.04;
  Model.SubNode.Hello := '2008 Year';
  Model.InsertInterval(12.5);
  Model.InsertInterval(12.5 * 2);
  Model.InsertInterval(12.5 * 3);
  Model.InsertInterval(12.5 * 4);
  Model.InsertInterval(12.5 * 5);
  SaveToFile('demo.xml', Model);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Model: TDemoModel;
begin
  Model := TDemoModel.Create;
  if FileExists('demo.xml') then
  begin
    LoadFromFile('demo.xml', Model);
    mmo1.Lines.Text := SaveToString(Model);
  end;
end;

end.
