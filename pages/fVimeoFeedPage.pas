unit fVimeoFeedPage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, uInterfaces, FMX.Layouts, FMX.ListBox, Xml.xmldom, Xml.XMLIntf,
  Xml.adomxmldom, Xml.XMLDoc, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdHTTP, System.Actions, FMX.ActnList, FMX.StdActns,
  FMX.MediaLibrary.Actions, System.Generics.Collections, Data.Bind.GenData,
  Data.Bind.Components, Data.Bind.ObjectScope, Fmx.Bind.GenData,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  AnonThread;

type
  TVimeoFeedItem = class
  private
    FDownloadThread : TAnonymousThread<TBitmap>;
    FTitle: string;
    FDescription: string;
    FURL: string;
    FImageURL: string;
    FThumbnailBitmap: TBitmap;
    FDownloadCompleteProc : TProc;
    procedure SetImageURL(const Value: string);
    procedure DownloadThumbnail;
    procedure DoDownloadComplete;
  public
    constructor Create(ADownloadCompletedProc : TProc); virtual;
    property Title : string read FTitle write FTitle;
    property Description : string read FDescription write FDescription;
    property URL : string read FURL write FURL;
    property ImageURL : string read FImageURL write SetImageURL;
    property ThumbnailBitmap : TBitmap read FThumbnailBitmap write FThumbnailBitmap;
  end;

  TVimeoFeedPage = class(TFrame, IPage)
    Panel1: TPanel;
    ListBox1: TListBox;
    IdHTTP1: TIdHTTP;
    XMLDocument1: TXMLDocument;
    ActionList1: TActionList;
    actShare: TShowShareSheetAction;
    actPlayVideo: TAction;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    procedure actShareBeforeExecute(Sender: TObject);
    procedure actPlayVideoExecute(Sender: TObject);
    procedure PrototypeBindSource1CreateAdapter(Sender: TObject;
      var ABindSourceAdapter: TBindSourceAdapter);
    procedure LinkListControlToField1FilledListItem(Sender: TObject;
      const AEditor: IBindListEditorItem);
  private
    fFeedItems : TObjectList<TVimeoFeedItem>;
    fFeed : string;
    fFirstTime : Boolean;
    procedure AddParam(const Name: string; const Value: string);
    procedure ParamsLoaded;
    procedure Refresh;
    function NeedRefresh: Boolean;
    procedure AfterShow;
  end;

implementation
uses
  uUtils, fBrowser, FMX.Objects;

{$R *.fmx}

{ TFrameSample1 }

procedure TVimeoFeedPage.AddParam(const Name, Value: string);
begin
  if Name = 'Feed' then
    FFeed := Value;
end;

procedure TVimeoFeedPage.AfterShow;
begin
  if fFirstTime then
  begin
    fFirstTime := False;
    Refresh;
  end;
end;

procedure TVimeoFeedPage.LinkListControlToField1FilledListItem(Sender: TObject;
  const AEditor: IBindListEditorItem);
begin
  if AEditor.CurrentObject is TListboxItem then
  begin
    // hook up the OnClick events of the two buttons on the listboxitem style
    TListboxItem(AEditor.CurrentObject).StylesData['sharebutton.Action'] := TValue.From<TShowShareSheetAction>(actShare);
    TListboxItem(AEditor.CurrentObject).StylesData['playbutton.Action'] := TValue.From<TAction>(actPlayVideo);
  end;
end;

function TVimeoFeedPage.NeedRefresh: Boolean;
begin
  Result := True;
end;

procedure TVimeoFeedPage.ParamsLoaded;
begin
  fFirstTime := True;
end;

procedure TVimeoFeedPage.PrototypeBindSource1CreateAdapter(Sender: TObject;
  var ABindSourceAdapter: TBindSourceAdapter);
begin
  fFeedItems := TObjectList<TVimeoFeedItem>.Create(True);
  ABindSourceAdapter := TListBindSourceAdapter<TVimeoFeedItem>.Create(self,
                                                                      fFeedItems,
                                                                      True);
end;

procedure TVimeoFeedPage.Refresh;
var
  strXml: string;
  I: Integer;
  ChannelNode, ItemNode, MediaNode, ThumbnailNode: IXMLNode;
  FeedItem : TVimeoFeedItem;
  DownloadCompleteProc : TProc;
begin
  strXml := IdHTTP1.Get (fFeed);
  XMLDocument1.LoadFromXML(strXml);
  XMLDocument1.Active := True;

  // anonymous method that will be run when the image has completed downloading
  DownloadCompleteProc := procedure
                          begin
                            PrototypeBindSource1.InternalAdapter.Refresh;
                          end;


  try
    fFeedItems.Clear;

    ChannelNode := XMLDocument1.DocumentElement.ChildNodes.FindNode ('channel');
    for I := 0 to ChannelNode.ChildNodes.Count - 1 do
    begin
      ItemNode := ChannelNode.ChildNodes[I];
      if ItemNode.NodeName = 'item' then
      begin
        FeedItem := TVimeoFeedItem.Create(DownloadCompleteProc);
        FeedItem.Title := ItemNode.ChildValues ['title'];
        FeedItem.URL := ItemNode.ChildValues ['link'];
        FeedItem.Description := StripHtmlMarkup(ItemNode.ChildValues ['description']);

        if ItemNode.ChildNodes.Count > 0 then
        begin
          MediaNode := ItemNode.ChildNodes.FindNode('media:content');
          if Assigned(MediaNode) then
            if MediaNode.ChildNodes.Count > 0 then
            begin
              ThumbnailNode := MediaNode.ChildNodes.FindNode('media:thumbnail');
              if Assigned(ThumbnailNode) then
                FeedItem.ImageURL := ThumbnailNode.Attributes['url'];
            end;
        end;
        FFeedItems.Add(FeedItem);
      end;
    end;
  finally
    PrototypeBindSource1.InternalAdapter.Refresh;
  end;
end;

procedure TVimeoFeedPage.actPlayVideoExecute(Sender: TObject);
var
  frmBrowser: TfrmBrowser;
begin
  frmBrowser := TfrmBrowser.Create(nil);
  try
    frmBrowser.Browse(ListBox1.Selected.StylesData['playbutton.TagString'].ToString);
    frmBrowser.ShowModal;
  finally
    frmBrowser.Free;
  end;
end;

procedure TVimeoFeedPage.actShareBeforeExecute(Sender: TObject);
begin
  actShare.TextMessage := ListBox1.Selected.StylesData['sharebutton.TagString'].ToString;
end;

{ TVimeoFeedItem }

constructor TVimeoFeedItem.Create(ADownloadCompletedProc : TProc);
begin
  FThumbnailBitmap := nil;
  FDownloadCompleteProc := ADownloadCompletedProc;
end;

procedure TVimeoFeedItem.DoDownloadComplete;
begin
  if Assigned(FDownloadCompleteProc) then
    FDownloadCompleteProc;
end;

procedure TVimeoFeedItem.DownloadThumbnail;
begin
  FDownloadThread := TAnonymousThread<TBitmap>.Create(
                       function : TBitmap
                       var
                         memStream: TMemoryStream;
                         idHTTP : TIdHTTP;
                       begin
                         memStream := TMemoryStream.Create;
                         try
                           idHTTP := TIdHTTP.Create(nil);
                           idHTTP.Get(ImageURL, memStream);
                         except
                           memStream.Free;
                           exit;
                         end;
                         try
                           memStream.Position := 0;
                           Result := TBitmap.CreateFromStream(memStream);
                         finally
                           memStream.Free;
                         end;
                      end,
                      procedure (AResult : TBitmap)
                      begin
                        ThumbnailBitmap := AResult;
                        DoDownloadComplete;
                      end,
                      nil);
end;

procedure TVimeoFeedItem.SetImageURL(const Value: string);
begin
  if FImageURL <> Value then
  begin
    FImageURL := Value;
    DownloadThumbnail;
  end;
end;

initialization
  RegisterClass(TVimeoFeedPage);

finalization


end.
