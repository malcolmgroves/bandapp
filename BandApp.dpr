program BandApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {frmMain},
  uInterfaces in 'uInterfaces.pas',
  fVimeoFeedPage in 'pages\fVimeoFeedPage.pas' {VimeoFeedPage: TFrame},
  fAboutPage in 'pages\fAboutPage.pas' {AboutPage: TFrame},
  uUtils in 'uUtils.pas',
  fBrowser in 'fBrowser.pas' {frmBrowser},
  fDebugPage in 'pages\fDebugPage.pas' {DebugPage: TFrame},
  fRSSFeedPage in 'pages\fRSSFeedPage.pas' {RSSFeedPage: TFrame},
  fImageText in 'fImageText.pas' {ImageTextFrame: TFrame},
  AnonThread in 'AnonThread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
