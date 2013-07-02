unit fRSSFeedPage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, uInterfaces, FMX.Layouts, FMX.ListBox, Xml.xmldom, Xml.XMLIntf,
  Xml.adomxmldom, Xml.XMLDoc, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdHTTP, System.Actions, FMX.ActnList, FMX.StdActns,
  FMX.MediaLibrary.Actions, System.Generics.Collections, Data.Bind.GenData,
  Data.Bind.Components, Data.Bind.ObjectScope, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt;

type
  TFeedItem = class
  private
    FTitle: string;
    FDescription: string;
    FURL: string;
  public
    property Title : string read FTitle write FTitle;
    property Description : string read FDescription write FDescription;
    property URL : string read FURL write FURL;
  end;

  TRSSFeedPage = class(TFrame, IPage)
    Panel1: TPanel;
    ListBox1: TListBox;
    IdHTTP1: TIdHTTP;
    XMLDocument1: TXMLDocument;
    ActionList1: TActionList;
    actShare: TShowShareSheetAction;
    actBrowse: TAction;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    procedure actShareBeforeExecute(Sender: TObject);
    procedure actBrowseExecute(Sender: TObject);
    procedure PrototypeBindSource1CreateAdapter(Sender: TObject;
      var ABindSourceAdapter: TBindSourceAdapter);
    procedure LinkListControlToField1FilledListItem(Sender: TObject;
      const AEditor: IBindListEditorItem);
  private
    fFeedItems : TObjectList<TFeedItem>;
    fFeed : string;
    fFirstTime : Boolean;
    procedure AddParam(const Name: string; const Value: string);
    procedure ParamsLoaded;
    procedure Refresh;
    function NeedRefresh: Boolean;
    procedure AfterShow;
  public
  end;

implementation
uses
  uUtils, fBrowser, FMX.Objects;

{$R *.fmx}

{ TFrameSample1 }

procedure TRSSFeedPage.AddParam(const Name, Value: string);
begin
  if Name = 'Feed' then
    FFeed := Value;
end;

procedure TRSSFeedPage.AfterShow;
begin
  if fFirstTime then
  begin
    fFirstTime := False;
    Refresh;
  end;
end;


procedure TRSSFeedPage.LinkListControlToField1FilledListItem(Sender: TObject;
  const AEditor: IBindListEditorItem);
var
  DescriptionHeight : Single;
  DescriptionObject : TFMXObject;
begin
  if AEditor.CurrentObject is TListboxItem then
  begin
    // hook up the OnClick events of the two buttons on the listboxitem style
    TListboxItem(AEditor.CurrentObject).StylesData['sharebutton.Action'] := TValue.From<TShowShareSheetAction>(actShare);
    TListboxItem(AEditor.CurrentObject).StylesData['browsebutton.Action'] := TValue.From<TAction>(actBrowse);
    // adjust the item height to cater for the length of the description
    TListboxItem(AEditor.CurrentObject).ApplyStyleLookup;
    DescriptionObject := TListboxItem(AEditor.CurrentObject).FindStyleResource('description');
    if DescriptionObject is TText then
    begin
      DescriptionHeight := TText(DescriptionObject).Height;
      TListboxItem(AEditor.CurrentObject).Height := DescriptionHeight + 100;
    end;
  end;
end;

function TRSSFeedPage.NeedRefresh: Boolean;
begin
  Result := True;
end;

procedure TRSSFeedPage.ParamsLoaded;
begin
  fFirstTime := True;
end;

procedure TRSSFeedPage.PrototypeBindSource1CreateAdapter(Sender: TObject;
  var ABindSourceAdapter: TBindSourceAdapter);
begin
  fFeedItems := TObjectList<TFeedItem>.Create(True);
  ABindSourceAdapter := TListBindSourceAdapter<TFeedItem>.Create(self,
                                                                 fFeedItems,
                                                                 True);
end;

procedure TRSSFeedPage.Refresh;
var
  strXml: string;
  I: Integer;
  ChannelNode, ItemNode: IXMLNode;
  FeedItem : TFeedItem;
begin
  strXml := IdHTTP1.Get (fFeed);
  XMLDocument1.LoadFromXML(strXml);
  XMLDocument1.Active := True;


  try
    fFeedItems.Clear;
    ChannelNode := XMLDocument1.DocumentElement.ChildNodes.FindNode ('channel');
    for I := 0 to ChannelNode.ChildNodes.Count - 1 do
    begin
      ItemNode := ChannelNode.ChildNodes[I];
      if ItemNode.NodeName = 'item' then
      begin
        FeedItem := TFeedItem.Create;
        FeedItem.Title := ItemNode.ChildValues ['title'];
        FeedItem.URL := ItemNode.ChildValues ['link'];
        FeedItem.Description := StripHtmlMarkup(ItemNode.ChildValues ['description']);
        FFeedItems.Add(FeedItem);
      end;
    end;
  finally
    PrototypeBindSource1.InternalAdapter.Refresh;
  end;

end;

procedure TRSSFeedPage.actBrowseExecute(Sender: TObject);
var
  frmBrowser: TfrmBrowser;
begin
  frmBrowser := TfrmBrowser.Create(nil);
  try
    frmBrowser.Browse(ListBox1.Selected.StylesData['browsebutton.TagString'].ToString);
    frmBrowser.ShowModal;
  finally
    frmBrowser.Free;
  end;
end;

procedure TRSSFeedPage.actShareBeforeExecute(Sender: TObject);
begin
  actShare.TextMessage := ListBox1.Selected.StylesData['sharebutton.TagString'].ToString;
end;

initialization
  RegisterClass(TRSSFeedPage);

finalization


end.
