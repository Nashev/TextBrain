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
    Button1: TButton;
    MainMenu: TMainMenu;
    KnowledgeMemo: TMemo;
    MenuItem1: TMenuItem;
    mmiSources: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    SourceMemo: TMemo;
    Splitter1: TSplitter;
    ToggleBox1: TToggleBox;
    procedure actLoadNewAnsiSourceExecute(Sender: TObject);
    procedure actLoadNewUTF8SourceExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ToggleBox1Change(Sender: TObject);
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
    procedure Evalute(AKnowledgeItem: TKnowledgeItem); override;
  end;

implementation

{ TNewItemDetector }

procedure TNewItemDetector.Evalute(AKnowledgeItem: TKnowledgeItem);
begin
  if Assigned(MainForm1.Source) then
    MainForm1.SourceMemo.Text := MainForm1.Source.InfoText;
  MainForm1.KnowledgeMemo.Lines.Add(AKnowledgeItem.InfoText);
  Application.ProcessMessages;
  while MainForm1.ToggleBox1.Checked do
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

procedure TMainForm1.Button1Click(Sender: TObject);
begin
  KnowledgeMemo.Lines.Clear;
end;

procedure TMainForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBrain);
end;

procedure TMainForm1.ToggleBox1Change(Sender: TObject);
begin

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

