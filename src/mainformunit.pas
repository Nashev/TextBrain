unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ActnList, Menus, ExtCtrls, Engine1Unit, Engine11Unit;

type

  { TMainForm }

  TMainForm = class(TForm)
    actLoadNewUTF8Source: TAction;
    actLoadNewAnsiSource: TAction;
    ActionList: TActionList;
    ButtonClear: TButton;
    ButtonShowSubsets: TButton;
    ButtonShowBrainContent: TButton;
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
    procedure ButtonShowBrainContentClick(Sender: TObject);
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
  MainForm: TMainForm;

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
  if Assigned(MainForm.Source) then
    MainForm.SourceMemo.Text := MainForm.Source.InfoText;
  if not MainForm.ToggleBoxSilent.Checked then
    begin
      MainForm.KnowledgeMemo.Lines.Add(AKnowledgeItem.InfoText);
      MainForm.KnowledgeMemo.Lines.Add(AKnowledgeItem.Owner.Subset[0].ContentText);
    end;
  if (Now - LastMessagesTime) > (333 / (24*60*60*1000)) then
    begin
      Application.ProcessMessages;
      LastMessagesTime := Now;
    end;
  while MainForm.ToggleBoxPause.Checked and not Application.Terminated do
    begin
      Application.ProcessMessages;
      Sleep(10);
    end;
  if Application.Terminated then
    Abort;
end;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FBrain := TBrain1.Create;
  FBrain.Detectors.Add(TNewItemDetector.Create);
end;

procedure TMainForm.actLoadNewAnsiSourceExecute(Sender: TObject);
begin
  with OpenDialog do
    if Execute then
      begin
        Source := TSimpleAnsiTextFileSource.Create(nil, FileName);
        Source.IntegrateToBrain(FBrain);
        ShowBrainContent;
      end;
end;

procedure TMainForm.actLoadNewUTF8SourceExecute(Sender: TObject);
begin
  with OpenDialog do
    if Execute then
      begin
        Source := TSimpleUTF8TextFileSource.Create(nil, FileName);
        Source.IntegrateToBrain(FBrain);
        ShowBrainContent;
      end;
end;

procedure TMainForm.ButtonClearClick(Sender: TObject);
begin
  KnowledgeMemo.Lines.Clear;
end;

procedure TMainForm.ButtonShowSubsetsClick(Sender: TObject);

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

procedure TMainForm.ButtonShowBrainContentClick(Sender: TObject);
begin
  ShowBrainContent;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBrain);
end;

procedure TMainForm.ShowBrainContent;
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

