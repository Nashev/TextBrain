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
    MainMenu: TMainMenu;
    KnowledgeMemo: TMemo;
    MenuItem1: TMenuItem;
    mmiSources: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog: TOpenDialog;
    SourceMemo: TMemo;
    Splitter1: TSplitter;
    procedure actLoadNewAnsiSourceExecute(Sender: TObject);
    procedure actLoadNewUTF8SourceExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FBrain: TBrain1;
  protected
    procedure ShowBrainContent;
  public
    { public declarations }
  end; 

var
  MainForm1: TMainForm1;

implementation

{$R *.lfm}

{ TMainForm1 }

procedure TMainForm1.FormCreate(Sender: TObject);
begin
  FBrain:= TBrain1.Create;
end;

procedure TMainForm1.actLoadNewAnsiSourceExecute(Sender: TObject);
var
  Source : TSimpleTextFileSource;
begin
  with OpenDialog do
    if Execute then
      begin
        Source := TSimpleAnsiTextFileSource.Create(nil, FileName);
        SourceMemo.Text := Source.InfoText;
        Application.ProcessMessages;
        Source.IntegrateToBrain(FBrain);
        ShowBrainContent;
      end;
end;

procedure TMainForm1.actLoadNewUTF8SourceExecute(Sender: TObject);
var
  Source : TSimpleTextFileSource;
begin
  with OpenDialog do
    if Execute then
      begin
        Source := TSimpleUTF8TextFileSource.Create(nil, FileName);
        SourceMemo.Text := Source.InfoText;
        Application.ProcessMessages;
        Source.IntegrateToBrain(FBrain);
        ShowBrainContent;
      end;
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

