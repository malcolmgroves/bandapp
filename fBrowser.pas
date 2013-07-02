unit fBrowser;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.WebBrowser,
  System.Actions, FMX.ActnList, FMX.StdCtrls;

type
  TfrmBrowser = class(TForm)
    ToolBar1: TToolBar;
    Button1: TButton;
    WebBrowser1: TWebBrowser;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Browse(const url : string);
  end;

implementation

{$R *.fmx}

{ TfrmBrowser }

procedure TfrmBrowser.Browse(const url: string);
begin
  WebBrowser1.URL := url;
  WebBrowser1.Navigate;
end;

end.
