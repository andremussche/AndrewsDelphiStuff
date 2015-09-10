unit XMLModel;


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
  RttiClasses;

type
  TRealEnabled = class(TRttiEnabled)
  private
    FValue: Real;
  published
    property Value : Real read FValue write FValue;
  end;
  TRealEnabledArray = array of TRealEnabled;

  TSubNode = class(TRttiEnabled)
  private
    FHello: string;
  published
    property Hello : string read FHello write FHello;
  end;

  TDemoModel = class(TRttiEnabled)
  private
    FNumber: Integer;
    FCaption: string;
    FBirthday: TDateTime;
    FPrice: Currency;
    FIntervals: TRealEnabledArray;//array of real
    FSubNode: TSubNode;
  public                     
    procedure InsertInterval(Value: Real);
  published
    property Number : Integer read FNumber write FNumber;
    property Caption : string read FCaption write FCaption;
    property Birthday : TDateTime read FBirthday write FBirthday;
    property Price : Currency read FPrice write FPrice;
    property Intervals : TRealEnabledArray read FIntervals write FIntervals;
    property SubNode : TSubNode read FSubNode write FSubNode;
  end;

implementation

{ TDemoModel }

procedure TDemoModel.InsertInterval(Value: Real);
begin
  SetLength(FIntervals, Length(FIntervals) + 1);
  FIntervals[High(FIntervals)] := TRealEnabled.Create;
  FIntervals[High(FIntervals)].Value := Value;
end;

end.









