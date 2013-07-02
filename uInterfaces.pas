unit uInterfaces;

interface

type
  IPage = interface
  ['{42D39AF5-7BEE-421C-A7FF-BD9151E905C3}']
    procedure AddParam(const Name, Value : string);
    procedure ParamsLoaded;
    procedure Refresh;
    function NeedRefresh : Boolean;
    procedure AfterShow;
  end;

implementation

end.
