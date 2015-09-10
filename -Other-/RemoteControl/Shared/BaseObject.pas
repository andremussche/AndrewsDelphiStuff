unit BaseObject;

interface

uses
  RttiClasses;

type
  TBaseObject = class(TRttiEnabled)
  public
    { public declarations }
//    class function ObjectToJSON<T : class>(myObject: T): TJSONValue;
//    class function JSONToObject<T : class>(json: TJSONValue): T;
  end;

implementation

{ TBaseObject }

end.
