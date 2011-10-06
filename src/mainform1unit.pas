unit MainForm1Unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ActnList, Menus, ExtCtrls, Engine1Unit, Engine11Unit;

type

  { TMainForm1 }

  TMainForm1 = class(TForm)
    actLoadNewUTF8Source: TAction;
    actLoadNewAnsiSource: TAction;
    ActionList: TActionList;
    ButtonClear: TButton;
    ButtonShowSubsets: TButton;
    MainMenu: TMainMenu;
    KnowledgeMemo: TMemo;
    MenuItem1: TMenuItem;
    mmiSources: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    SourceMemo: TMemo;
    Splitter1: TSplitter;
    ToggleBoxPause: TToggleBox;
    ToggleBoxSilent: TToggleBox;
    procedure actLoadNewAnsiSourceExecute(Sender: TObject);
    procedure actLoadNewUTF8SourceExecute(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonShowSubsetsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FBrain: TBrain1;
    Source: TSimpleTextFileSource;
  protected
    procedure ShowBrainContent;
  public
    { public declarations }
  end; 

var
  MainForm1: TMainForm1;

type

  { TNewItemDetector }

  TNewItemDetector = class(TDetector)
  private
    LastMessagesTime: TDateTime;
  public
    procedure Evalute(AKnowledgeItem: TKnowledgeItem); override;
  end;

implementation

{ TNewItemDetector }

procedure TNewItemDetector.Evalute(AKnowledgeItem: TKnowledgeItem);
begin
  if Assigned(MainForm1.Source) then
    MainForm1.SourceMemo.Text := MainForm1.Source.InfoText;
  if not MainForm1.ToggleBoxSilent.Checked then
    begin
      MainForm1.KnowledgeMemo.Lines.Add(AKnowledgeItem.InfoText);
      MainForm1.KnowledgeMemo.Lines.Add(AKnowledgeItem.Owner.Subset[0].ContentText);
    end;
  if (Now - LastMessagesTime) > (333 / (24*60*60*1000)) then
    begin
      Application.ProcessMessages;
      LastMessagesTime := Now;
    end;
  while MainForm1.ToggleBoxPause.Checked and not Application.Terminated do
    begin
      Application.ProcessMessages;
      Sleep(10);
    end;
  if Application.Terminated then
    Abort;
end;

{$R *.lfm}

{ TMainForm1 }

procedure TMainForm1.FormCreate(Sender: TObject);
begin
  FBrain := TBrain1.Create;
  FBrain.Detectors.Add(TNewItemDetector.Create);
end;

procedure TMainForm1.actLoadNewAnsiSourceExecute(Sender: TObject);
begin
  with OpenDialog do
    if Execute then
      begin
        Source := TSimpleAnsiTextFileSource.Create(nil, FileName);
        Source.IntegrateToBrain(FBrain);
        ShowBrainContent;
      end;
end;

procedure TMainForm1.actLoadNewUTF8SourceExecute(Sender: TObject);
begin
  with OpenDialog do
    if Execute then
      begin
        Source := TSimpleUTF8TextFileSource.Create(nil, FileName);
        Source.IntegrateToBrain(FBrain);
        ShowBrainContent;
      end;
end;

procedure TMainForm1.ButtonClearClick(Sender: TObject);
begin
  KnowledgeMemo.Lines.Clear;
end;

procedure TMainForm1.ButtonShowSubsetsClick(Sender: TObject);
  procedure ShowKnowledgeSet(APrefix: string; AKnowledgeBaseSubset: TKnowledgeBaseSubset);
  var
    i: Integer;
  begin
    KnowledgeMemo.Lines.Add(APrefix + ' ' + AKnowledgeBaseSubset.InfoText);
    for i := 0 to AKnowledgeBaseSubset.SubsetsCount - 1 do
      ShowKnowledgeSet('*' + APrefix, AKnowledgeBaseSubset.Subset[i]);
  end;
begin
  ShowKnowledgeSet('', FBrain);
end;

procedure TMainForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBrain);
end;

procedure TMainForm1.ShowBrainContent;
var
  i, j: Integer;
  Item: TKnowledgeItem;
begin
  KnowledgeMemo.Lines.BeginUpdate;
  try
    KnowledgeMemo.Clear;
    for i := 0 to FBrain.Count - 1 do
      begin
        Item := FBrain[i];
        if (Item is TSource) or (Item is TSourceItem) then
          Continue;
        KnowledgeMemo.Lines.Add(Item.InfoText);
        for j := 0 to Item.Basis.Count - 1 do
          KnowledgeMemo.Lines.Add('           '#9 + Item.Basis[j].InfoText);
      end;
  finally
    KnowledgeMemo.Lines.EndUpdate;
  end;
end;

end.

