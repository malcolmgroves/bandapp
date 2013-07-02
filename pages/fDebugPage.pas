unit fDebugPage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, uInterfaces, FMX.Layouts, FMX.ListBox;

type
  TDebugPage = class(TFrame, IPage)
    Panel1: TPanel;
    ListBox1: TListBox;
  private
    { Private declarations }
    procedure AddParam(const Name: string; const Value: string);
    procedure ParamsLoaded;
    procedure Refresh;
    function NeedRefresh: Boolean;
    procedure AfterShow;
  end;

implementation

{$R *.fmx}

{ TFrameSample1 }

procedure TDebugPage.AddParam(const Name, Value: string);
begin
  Listbox1.Items.Add(Format('%s = %s', [Name, Value]));
end;

procedure TDebugPage.AfterShow;
begin
  Listbox1.Items.Add('AfterShow');
end;

function TDebugPage.NeedRefresh: Boolean;
begin
  Result := True;
end;

procedure TDebugPage.ParamsLoaded;
begin
  Listbox1.Items.Add('ParamsLoaded');
end;

procedure TDebugPage.Refresh;
begin
  Listbox1.Items.Add('Refresh');
end;

initialization
  RegisterClass(TDebugPage);

finalization


end.
