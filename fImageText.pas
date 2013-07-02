unit fImageText;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Objects, FMX.Layouts;

type
  TImageTextFrame = class(TFrame)
    Rectangle1: TRectangle;
    Image1: TImage;
    Rectangle2: TRectangle;
    VertScrollBox1: TVertScrollBox;
    Text1: TText;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
