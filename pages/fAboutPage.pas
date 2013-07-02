unit fAboutPage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, uInterfaces, FMX.Layouts, FMX.ListBox, FMX.Objects,
  FMX.TabControl, FMX.Gestures, System.Actions, FMX.ActnList, fImageText,
  System.Generics.Collections;

type
  TAboutPage = class(TFrame, IPage)
    TabControl1: TTabControl;
    ActionList1: TActionList;
    actNextTab: TChangeTabAction;
    actPrevTab: TChangeTabAction;
    GestureManager1: TGestureManager;
    procedure TabControl1Change(Sender: TObject);
    procedure FrameGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
  private
    { Private declarations }
    FTabs : TStrings;
    FParams : TDictionary<String,String>;
    procedure AddParam(const Name: string; const Value: string);
    procedure ParamsLoaded;
    procedure Refresh;
    function NeedRefresh: Boolean;
    procedure AfterShow;
    procedure CreateTabs;
    procedure AssignNextAndPrevTabs;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation
uses
  IOUtils;

{$R *.fmx}

{ TFrameSample1 }

procedure TAboutPage.AddParam(const Name, Value: string);
begin
  if Name = 'Tabs' then
  begin
    FTabs.CommaText := Value;
  end
  else
    FParams.Add(Name, Value);
end;

procedure TAboutPage.AfterShow;
begin
  //
end;

procedure TAboutPage.AssignNextAndPrevTabs;
begin
  if TabControl1.TabCount < 2 then
  begin
    actNextTab.Tab := nil;
    actPrevTab.Tab := nil;
  end
  else
  begin
    if TabControl1.TabIndex = TabControl1.TabCount - 1 then
      actNextTab.Tab := nil
    else
      actNextTab.Tab := TabControl1.Tabs[TabControl1.TabIndex + 1];

    if TabControl1.TabIndex = 0 then
      actPrevTab.Tab := nil
    else
      actPrevTab.Tab := TabControl1.Tabs[TabControl1.TabIndex - 1];
  end;
end;

constructor TAboutPage.Create(AOwner: TComponent);
begin
  inherited;
  FTabs := TStringList.Create;
  FParams := TDictionary<String, String>.Create;
end;

procedure TAboutPage.CreateTabs;
var
  Tab: String;
  LFrame : TImageTextFrame;
  LTabItem : TTabItem;
  ImagePath : string;
const
  TextSuffix = 'Text';
  ImageSuffix = 'Image';
begin
  for Tab in FTabs do
  begin
    if FParams.ContainsKey(Tab + TextSuffix) and
       FParams.ContainsKey(Tab + ImageSuffix) then
    begin
      LFrame := TImageTextFrame.Create(nil);
      LFrame.Text1.Text := FParams.Items[Tab + TextSuffix];
      ImagePath := GetHomePath + PathDelim + 'Library' + PathDelim + FParams.Items[Tab + ImageSuffix];
      if TFile.Exists(ImagePath) then
        LFrame.Image1.Bitmap.LoadFromFile(ImagePath);
      LTabItem := TTabItem.Create(nil);
      TabControl1.AddObject(LTabItem);
      LFrame.Parent := LTabItem;
      LFrame.Align := TAlignLayout.alClient;
      LFrame.Visible := True;
    end;
  end;
end;

procedure TAboutPage.FrameGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    sgiLeft : begin
                actNextTab.ExecuteTarget(self);
                Handled := True;
              end;
    sgiRight: begin
                actPrevTab.ExecuteTarget(self);
                Handled := True;
              end;
  end;
end;

function TAboutPage.NeedRefresh: Boolean;
begin
  Result := False;
end;

procedure TAboutPage.ParamsLoaded;
begin
  CreateTabs;
  AssignNextAndPrevTabs;
end;

procedure TAboutPage.Refresh;
begin
  //
end;

procedure TAboutPage.TabControl1Change(Sender: TObject);
begin
  AssignNextAndPrevTabs;
end;

initialization
  RegisterClass(TAboutPage);

finalization


end.
