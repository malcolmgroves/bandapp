unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts,
  System.Actions, FMX.ActnList, FMX.ListBox, System.Generics.Collections,
  Data.Bind.GenData, Fmx.Bind.GenData, Data.Bind.Components,
  Data.Bind.ObjectScope, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, FMX.Objects;

type
  TPageMenuItem = class
  private
    fTitle: string;
    fIcon: TBitmap;
    fPage: TFrame;
  public
    property Title : string read fTitle write fTitle;
    property Icon : TBitmap read fIcon write fIcon;
    property Page : TFrame read fPage write fPage;
  end;

  TfrmMain = class(TForm)
    lytDrawer: TLayout;
    lytMain: TLayout;
    ToolBar1: TToolBar;
    ActionList1: TActionList;
    actOpenDrawer: TAction;
    btnDrawer: TButton;
    ListBox1: TListBox;
    Panel1: TPanel;
    lblTitle: TLabel;
    Panel2: TPanel;
    lytPageHost: TLayout;
    StyleBook2: TStyleBook;
    lblPageTitle: TLabel;
    btnRefresh: TButton;
    actRefreshPage: TAction;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    LinkPropertyToFieldText: TLinkPropertyToField;
    procedure FormResize(Sender: TObject);
    procedure actOpenDrawerExecute(Sender: TObject);
    procedure actOpenDrawerUpdate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure actRefreshPageExecute(Sender: TObject);
    procedure PrototypeBindSource1CreateAdapter(Sender: TObject;
      var ABindSourceAdapter: TBindSourceAdapter);
    procedure FormShow(Sender: TObject);
  private
    FDrawerVisible: boolean;
    FPageMenuItems : TObjectList<TPageMenuItem>;
    procedure BeforeAdapterScroll(Adapter: TBindSourceAdapter);
    procedure AfterAdapterScroll(Adapter: TBindSourceAdapter);
    function IsPad : boolean;
    function IsLandscape : boolean;
    procedure LayoutForm;
    procedure SetDrawerVisible(const Value: boolean);
    procedure LoadConfig;
    function CurrentPageMenuItem : TPageMenuItem;
  public
    { Public declarations }
    property DrawerVisible : boolean read FDrawerVisible write SetDrawerVisible;
  end;

var
  frmMain: TfrmMain;

implementation
uses
  FMX.Platform, System.Inifiles, IOUtils, uInterfaces, iOSapi.UIKit;

{$R *.fmx}

{$REGION 'Drawer-related code'}
procedure TfrmMain.actOpenDrawerExecute(Sender: TObject);
begin
  DrawerVisible := not DrawerVisible;
end;

procedure TfrmMain.actOpenDrawerUpdate(Sender: TObject);
begin
  actOpenDrawer.Visible := not (IsPad and IsLandscape);
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  LayoutForm;
end;

function TfrmMain.IsLandscape: boolean;
begin
  Result := self.Width > self.Height;
end;

function TfrmMain.IsPad: boolean;
begin
  Result := TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).userInterfaceIdiom = UIUserInterfaceIdiomPad;
end;

procedure TfrmMain.LayoutForm;
begin
  lytMain.Height := self.Height;
  lytDrawer.Height := self.Height;

  if IsPad and IsLandscape then
  begin
    lytDrawer.Align := TAlignLayout.alLeft;
    lytMain.Align := TAlignLayout.alClient;
  end
  else
  begin
    lytDrawer.Align := TAlignLayout.alNone;
    lytMain.Align := TAlignLayout.alNone;
    lytMain.Width := self.Width;

    if DrawerVisible then
      lytMain.Position.X := lytDrawer.Position.X + lytDrawer.Width
    else
      lytMain.Position.X := 0;
  end;
end;

procedure TfrmMain.SetDrawerVisible(const Value: boolean);
begin
  if FDrawerVisible <> Value then
  begin
    FDrawerVisible := Value;
    if DrawerVisible then
      lytMain.AnimateFloat('Position.X', lytDrawer.Position.X + lytDrawer.Width)
    else
      lytMain.AnimateFloat('Position.X', 0);
  end;
end;

{$ENDREGION}

procedure TfrmMain.FormShow(Sender: TObject);
begin
  PrototypeBindSource1.InternalAdapter.First;
end;

procedure TfrmMain.actRefreshPageExecute(Sender: TObject);
begin
  if CurrentPageMenuItem <> nil then
    (CurrentPageMenuItem.Page as IPage).Refresh;
end;

procedure TfrmMain.AfterAdapterScroll(Adapter: TBindSourceAdapter);
begin
//  Current PageMenuItem has just changed, so show the correct Page
  if CurrentPageMenuItem <> nil then
  begin
    CurrentPageMenuItem.Page.Parent := lytPageHost;
    CurrentPageMenuItem.Page.Align := TAlignLayout.alClient;
    btnRefresh.Visible := (CurrentPageMenuItem.Page as IPage).NeedRefresh;
    CurrentPageMenuItem.Page.Visible := True;
    (CurrentPageMenuItem.Page as IPage).AfterShow;
  end;
end;

procedure TfrmMain.BeforeAdapterScroll(Adapter: TBindSourceAdapter);
begin
//  Current PageMenuItem is about to change, so pull down the displayed Page
  if CurrentPageMenuItem <> nil then
  begin
    CurrentPageMenuItem.Page.Visible := False;
    CurrentPageMenuItem.Page.Parent := nil;
  end;
end;

function TfrmMain.CurrentPageMenuItem: TPageMenuItem;
begin
  if Assigned(PrototypeBindSource1.InternalAdapter) and
     Assigned(PrototypeBindSource1.InternalAdapter.Current) and
     (PrototypeBindSource1.InternalAdapter.Current is TPageMenuItem) then
    Result := PrototypeBindSource1.InternalAdapter.Current as TPageMenuItem
  else
    Result := nil;
end;

procedure TfrmMain.ListBox1Click(Sender: TObject);
begin
  DrawerVisible := False;
end;

procedure TfrmMain.LoadConfig;
var
  ConfigFile: TIniFile;
  Keys, Params : TStrings;
  Key, PageClassName, LibraryPath, ConfigPath, ParamKey : String;
  LPersistentClass : TPersistentClass;
  PageMenuItem : TPageMenuItem;
begin
  LibraryPath := GetHomePath + PathDelim + 'Library' + PathDelim;
  ConfigPath := LibraryPath + 'config.ini';
  if not TFile.Exists(ConfigPath) then
    raise Exception.Create(Format('Config file not found : %s', [ConfigPath]));

  ConfigFile := TIniFile.Create(ConfigPath);
  try
    Keys := TStringList.Create;
    try
      lblTitle.Text := ConfigFile.ReadString('General', 'BandName', 'Missing Band Name');
      ConfigFile.ReadSection('Pages', Keys);
      for Key in Keys do
      begin
        PageMenuItem := TPageMenuItem.Create;
        PageMenuItem.Title := ConfigFile.ReadString(Key, 'Title', Key);
        PageMenuItem.Icon := TBitmap.CreateFromFile(LibraryPath + ConfigFile.ReadString(Key, 'Icon', ''));

        PageClassName := ConfigFile.ReadString(Key, 'PageClass', '');
        if PageClassName = '' then
          raise Exception.Create(Format('No PageClass in config.ini for %s', [Key]));
        LPersistentClass := GetClass(PageClassName);
        PageMenuItem.Page := TComponentClass(LPersistentClass).Create(nil) as TFrame;

        Params := TStringList.Create;
        try
          ConfigFile.ReadSection(Key, Params);
          for ParamKey in Params do
            (PageMenuItem.Page as IPage).AddParam(ParamKey, ConfigFile.ReadString(Key, ParamKey, ''));
          (PageMenuItem.Page as IPage).ParamsLoaded;
        finally
          Params.Free;
        end;

        FPageMenuItems.Add(PageMenuItem);

      end;
    finally
      Keys.Free;
    end;
  finally
    ConfigFile.Free;
  end;
end;

procedure TfrmMain.PrototypeBindSource1CreateAdapter(Sender: TObject;
  var ABindSourceAdapter: TBindSourceAdapter);
begin
  FPageMenuItems := TObjectList<TPageMenuItem>.Create(True);
  LoadConfig;
  ABindSourceAdapter := TListBindSourceAdapter<TPageMenuItem>.Create(self, FPageMenuItems, True);
  ABindSourceAdapter.BeforeScroll := BeforeAdapterScroll;
  ABindSourceAdapter.AfterScroll := AfterAdapterScroll;
end;


end.
