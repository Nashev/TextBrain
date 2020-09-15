unit MainForm2Unit;

{$IFDEF  FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  {$IFDEF FPC}
  FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ActnList, Menus, ExtCtrls, LCLType, LCLIntf,
  {$ENDIF}
  {$IFNDEF FPC}
  System.Contnrs, System.Actions,
  FMX.Forms, FMX.Memo.Types, FMX.TabControl,
  FMX.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.ActnList,
  FMX.Dialogs, FMX.Menus, FMX.StdCtrls, FMX.Controls
  {$ENDIF}
  , Engine1Unit
  , Engine11Unit
  ;

type
{$IFNDEF FPC}
  SizeInt = Integer;
  SizeUint = Word;
  UTF8String = string;
{$ELSE}
  TFileName = UTF8String;
{$ENDIF}

  { TMainForm }

  { TMemo }
  TMemo = class({$IFDEF FPC}stdctrls.{$ELSE}FMX.Memo.{$ENDIF}TMemo)
  private
    FObjects: TList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddObject(const AText: string; AObject: TObject);
    procedure Clear;
    property Objects: TList read FObjects;
  end;

  /// <summary>Main form owns Brain and allow users to manipulate them</summary>
  TMainForm = class(TForm)
    actLoadNewUTF8Source: TAction;
    actLoadNewAnsiSource: TAction;
    ActionList: TActionList;
    btnClear: TButton;
    btnTreeColumnsGoHome: TButton;
    btnShowBrainContent: TButton;
    btnShowSubsets: TButton;
    MainMenu: TMainMenu;
    CenterMemo: TMemo;
    RightMemo: TMemo;
    LeftMemo: TMemo;
    OutMemo: TMemo;
    mmiExitSeparator: TMenuItem;
    mmiExit: TMenuItem;
    mmiAbout: TMenuItem;
    mmiGithub: TMenuItem;
    mmiHelp: TMenuItem;
    pnlOutMemoToolbar: TPanel;
    pcBrowser: TTabControl;
    pnlOutMemoToolbar1: TPanel;
    splLeftMemo: TSplitter;
    splRighMemo: TSplitter;
    StatusMemo: TMemo;
    mmiLoadUTF8: TMenuItem;
    mmiSources: TMenuItem;
    mmiLoadAnsi: TMenuItem;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    SourceMemo: TMemo;
    splSourceMemo: TSplitter;
    Splitter2: TSplitter;
    tsOutMemo: TTabItem;
    tsThreeColumns: TTabItem;
    ToggleBoxPause: TCheckBox;
    ToggleBoxSilent: TCheckBox;
    procedure actLoadNewAnsiSourceExecute(Sender: TObject);
    procedure actLoadNewUTF8SourceExecute(Sender: TObject);
    procedure btnTreeColumnsGoHomeClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonShowSubsetsClick(Sender: TObject);
    procedure btnShowBrainContentClick(Sender: TObject);
    procedure ClearOutMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LeftOrRightMemoDblClick(Sender: TObject);
    procedure BrowserMemoClick(Sender: TObject);
    procedure OutMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mmiAboutClick(Sender: TObject);
    procedure mmiExitClick(Sender: TObject);
    procedure mmiGithubClick(Sender: TObject);
  private
    /// <summary>Main brain instance in this app</summary>
    FBrain: TBrain1;
    Source: TSimpleTextFileSource;
    procedure FillTreeColumnBrowserBy(AItem: TKnowledgeItem);
    procedure FillTreeColumnsBy(AMemo: TMemo; AItems: TTabledKnowledgeBaseSubset);
    procedure ProcessSource;
  protected
    procedure ShowBrainContent;
    procedure UpdateStatus;
    procedure HighlightSource(FocusedItem: TKnowledgeItem);
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

type

  { TNewItemDetector }
  /// <summary>
  /// It's special detector that only shows new brain's knowledge items at the MainForm if this activity are enabled
  /// </summary>
  TNewItemDetector = class(TDetector)
  private
    LastMessagesTime: TDateTime;
  public
    procedure Evalute(AKnowledgeItem: TKnowledgeItem); override;
  end;

implementation

uses
{$IF defined(IOS)} //  and NOT defined(CPUARM)
  FMX.Helpers.iOS, iOSapi.Foundation;
{$ELSEIF defined(ANDROID)}
  Androidapi.JNI.GraphicsContentViewText,
  FMX.Helpers.Android;
{$ELSE}
  Winapi.Windows, Winapi.ShellAPI;
{$ENDIF}

{ TNewItemDetector }

resourcestring
  rsBrainSubsetHeader = 'Brain subset %d:';
  rsNoSourceFile = 'No source file';

{ TMemo }

constructor TMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FObjects := TList.Create;
end;

destructor TMemo.Destroy;
begin
  FreeAndNil(FObjects);
  inherited Destroy;
end;

procedure TMemo.AddObject(const AText: string; AObject: TObject);
var
  i, FirstIndex: Integer;
begin
  FirstIndex := Lines.Count;
  Lines.Add(AText); // possible CRs
  Objects.Count := Lines.Count;
  for i := FirstIndex to Objects.Count - 1 do
    Objects[i] := AObject;
end;

procedure TMemo.Clear;
begin
  Lines.Clear;
  Objects.Clear;
end;

procedure TNewItemDetector.Evalute(AKnowledgeItem: TKnowledgeItem);
var
  i: Integer;
begin
  if not MainForm.ToggleBoxSilent.{$IFDEF FPC}Checked{$ELSE}IsChecked{$ENDIF} then
    begin
      MainForm.OutMemo.AddObject(string(AKnowledgeItem.InfoText), AKnowledgeItem);
      for i := 0 to AKnowledgeItem.Owner.SubsetsCount - 1 do
        begin
          MainForm.OutMemo.AddObject(Format(rsBrainSubsetHeader, [i]), nil);
          MainForm.OutMemo.AddObject(string(AKnowledgeItem.Owner.Subset[i].ContentText), AKnowledgeItem.Owner.Subset[i]);
        end;
    end;
  if (Now - LastMessagesTime) > (333 / (24*60*60*1000)) then
    begin
      MainForm.UpdateStatus;
      Application.ProcessMessages;
      LastMessagesTime := Now;
    end;
  while MainForm.ToggleBoxPause.{$IFDEF FPC}Checked{$ELSE}IsChecked{$ENDIF} and not Application.Terminated do
    begin
      Application.ProcessMessages;
      Sleep(10);
    end;
  if Application.Terminated then
    Abort;
end;

{$IFDEF  FPC}
{$R *.lfm}
{$ELSE}
{$R *.fmx}
{$ENDIF}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FBrain := TBrain1.Create;
  FBrain.Detectors.Insert(0, TNewItemDetector.Create);
  UpdateStatus;
end;

procedure TMainForm.UpdateStatus;
begin
  if Assigned(Source) then
    StatusMemo.Text := string(Source.InfoText)
  else
    StatusMemo.Text := rsNoSourceFile;
end;

procedure TMainForm.actLoadNewAnsiSourceExecute(Sender: TObject);
begin
  with OpenDialog do
    if Execute then
      begin
        Source := TSimpleAnsiTextFileSource.Create(nil, FileName);
        ProcessSource;
      end;
end;

procedure TMainForm.actLoadNewUTF8SourceExecute(Sender: TObject);
begin
  with OpenDialog do
    if Execute then
      begin
        Source := TSimpleUTF8TextFileSource.Create(nil, FileName);
        ProcessSource;
      end;
end;

procedure TMainForm.ProcessSource;
begin
  SourceMemo.Text := string(Source.ToString);
  Source.IntegrateToBrain(FBrain);
    Application.ProcessMessages;
  UpdateStatus;
    Application.ProcessMessages;
  ShowBrainContent;
    Application.ProcessMessages;
  btnTreeColumnsGoHomeClick(nil);
end;

procedure TMainForm.btnTreeColumnsGoHomeClick(Sender: TObject);
begin
  FillTreeColumnsBy(LeftMemo, FBrain);
  CenterMemo.Clear;
  RightMemo.Clear;
end;

procedure TMainForm.FillTreeColumnsBy(AMemo: TMemo; AItems: TTabledKnowledgeBaseSubset);
var
  i: Integer;
  LItem: TKnowledgeItem;
  s: string;
begin
  AMemo.Lines.BeginUpdate;
  try
    AMemo.Clear;
    for i := 0 to AItems.Count - 1 do begin
      LItem := AItems[i];
      s := string(LItem.InfoText);
      AMemo.AddObject(s, LItem);
    end;
  finally
    AMemo.Lines.EndUpdate;
  end;
end;

procedure TMainForm.ButtonClearClick(Sender: TObject);
begin
  ClearOutMemo;
end;

resourcestring
  rsSubsetIndex = 'Subset %d ';
  rsSubsetContent = 'Subset content: ';
  rsSubsetContentEmpty = 'Subset content empty.';
  rsSubsetSubsets = 'Subsets (%d): ';
  rsSubsetSubsetsEmpty = 'No subsets.';

procedure TMainForm.ButtonShowSubsetsClick(Sender: TObject);

  procedure ShowKnowledgeSet(APrefix: string; AKnowledgeBaseSubset: TKnowledgeBaseSubset);
  var
    i: Integer;
  begin
    if AKnowledgeBaseSubset is TWordIndex then
      OutMemo.AddObject(APrefix + ' ' + string(AKnowledgeBaseSubset.ContentText), AKnowledgeBaseSubset)
    else
      begin
        OutMemo.AddObject(APrefix + ' ' + string(AKnowledgeBaseSubset.InfoText), AKnowledgeBaseSubset);
        with AKnowledgeBaseSubset.GetIterator do
          try
            if not EOF then
              OutMemo.AddObject(APrefix + rsSubsetContent, AKnowledgeBaseSubset)
            else
              OutMemo.AddObject(APrefix + rsSubsetContentEmpty, AKnowledgeBaseSubset);

            while not EOF do
              begin
                OutMemo.AddObject(APrefix + ' - ' + string(CurrentItem.InfoText), CurrentItem);
                Next;
              end;
          finally
            Free;
          end;
      end;

    if AKnowledgeBaseSubset.SubsetsCount > 0 then
      OutMemo.AddObject(APrefix + Format(rsSubsetSubsets, [AKnowledgeBaseSubset.SubsetsCount]), AKnowledgeBaseSubset)
    else
      OutMemo.AddObject(APrefix + rsSubsetSubsetsEmpty, AKnowledgeBaseSubset);

    for i := 0 to AKnowledgeBaseSubset.SubsetsCount - 1 do
      ShowKnowledgeSet(APrefix + Format(rsSubsetIndex, [i]), AKnowledgeBaseSubset.Subset[i]);
  end;

begin
  OutMemo.Lines.BeginUpdate;
  try
    ClearOutMemo;
    ShowKnowledgeSet('', FBrain);
  finally
    OutMemo.Lines.EndUpdate;
  end;
end;

procedure TMainForm.btnShowBrainContentClick(Sender: TObject);
begin
  ShowBrainContent;
end;

procedure TMainForm.ClearOutMemo;
begin
  OutMemo.Clear;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBrain);
end;

{$IFNDEF FPC}
procedure Beep;
begin
  MessageBeep(0);
end;
{$ENDIF}

procedure TMainForm.LeftOrRightMemoDblClick(Sender: TObject);
begin
  if TMemo(Sender).{$IFDEF FPC}CaretPos.Y{$ELSE}CaretPosition.Line{$ENDIF} < TMemo(Sender).Lines.Count then
    FillTreeColumnBrowserBy(TKnowledgeItem(TMemo(Sender).Objects[TMemo(Sender).{$IFDEF FPC}CaretPos.Y{$ELSE}CaretPosition.Line{$ENDIF}]))
  else
    Beep;
end;

procedure TMainForm.FillTreeColumnBrowserBy(AItem: TKnowledgeItem);
begin
  if AItem = nil then
    begin
      Beep;
      Exit;
    end;
  try
    FillTreeColumnsBy(LeftMemo, AItem.Basis);
  except
    on e: Exception do
      LeftMemo.Text := e.ToString;
  end;
  CenterMemo.Text := string(AItem.InfoText);
  try
    FillTreeColumnsBy(RightMemo, AItem.Consequences);
  except
    on e: Exception do
      RightMemo.Text := e.ToString;
  end;
end;

procedure TMainForm.BrowserMemoClick(Sender: TObject);
var
  FocusedItem: TObject;
begin
  if (Sender as TMemo).{$IFDEF FPC}CaretPos.Y{$ELSE}CaretPosition.Line{$ENDIF} >= (sender as TMemo).Lines.Count then
      Exit;
  FocusedItem := TObject(TMemo(Sender).Objects[TMemo(Sender).{$IFDEF FPC}CaretPos.Y{$ELSE}CaretPosition.Line{$ENDIF}]);
  if (FocusedItem <> nil) and (FocusedItem is TKnowledgeItem) then
    HighlightSource(TKnowledgeItem(FocusedItem));
end;

procedure TMainForm.OutMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  FocusedItem: TObject;
begin
  if OutMemo.{$IFDEF FPC}CaretPos.Y{$ELSE}CaretPosition.Line{$ENDIF} >= OutMemo.Lines.Count then
      Exit;
  FocusedItem := TObject(OutMemo.Objects[OutMemo.{$IFDEF FPC}CaretPos.Y{$ELSE}CaretPosition.Line{$ENDIF}]);
  if (FocusedItem <> nil) and (FocusedItem is TKnowledgeItem) then
    HighlightSource(TKnowledgeItem(FocusedItem));
end;

resourcestring
  rsAboutText =
    'TextBrain now is a text analysis first prototype'#13+
    'made by Nenashev Ilya (aka Nashev), Russia, Moscow.'#13+
    'Project are hosted on GitHub at http://github.com/Nashev/TextBrain'#13+
    'See additional information there.';

procedure TMainForm.mmiAboutClick(Sender: TObject);
begin
  ShowMessage(
    rsAboutText
  )
end;

procedure TMainForm.mmiExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

{$IF defined(IOS)} //  and NOT defined(CPUARM)
procedure openURL(url:String);
begin
  SharedApplication.openURL(TNSURL.Wrap(TNSURL.OCClass.URLWithString(NSSTR(PChar(String(url))))));
end;

{$ELSEIF defined(ANDROID)}

procedure openURL(url:String);
var
  Intent: JIntent;
begin
  Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
  Intent.setData(StrToJURI(url));
  SharedActivity.startActivity(Intent);
end;

{$ELSE}

procedure openURL(url:String);
begin
  ShellExecute(0, 'open', PWideChar(url), nil, nil, SW_SHOW);
end;
{$ENDIF}

procedure TMainForm.mmiGithubClick(Sender: TObject);
begin
  OpenURL('http://github.com/Nashev/TextBrain');
end;

procedure TMainForm.HighlightSource(FocusedItem: TKnowledgeItem);
begin
  if FocusedItem is TSourceItem then
    begin
      SourceMemo.SelStart := TSourceItem(FocusedItem).GetItemStart;
      SourceMemo.SelLength := TSourceItem(FocusedItem).GetItemLength;
    end;
end;

procedure TMainForm.ShowBrainContent;
var
  i, j: Integer;
  Item: TKnowledgeItem;
begin
  OutMemo.Lines.BeginUpdate;
  try
    ClearOutMemo;
    for i := 0 to FBrain.Count - 1 do
      begin
        Item := FBrain[i];
        if (Item is TSource) or (Item is TSourceItem) then
          Continue;
        OutMemo.AddObject(string(Item.InfoText), Item);
        for j := 0 to Item.Basis.Count - 1 do
          OutMemo.AddObject('           '#9 + string(Item.Basis[j].InfoText), Item.Basis[j]);
      end;
  finally
    OutMemo.Lines.EndUpdate;
  end;
end;

end.

