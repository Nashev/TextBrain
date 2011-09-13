unit MainForm1Unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ActnList, Menus, ExtCtrls, Engine11Unit;

type
  TMainForm1 = class(TForm)
    actLoadNewSource: TAction;
    ActionList: TActionList;
    MainMenu: TMainMenu;
    KnowledgeMemo: TMemo;
    mmiSources: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog: TOpenDialog;
    SourceMemo: TMemo;
    Splitter1: TSplitter;
    procedure actLoadNewSourceExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
  private
    FBrain: TBrain1;
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

procedure TMainForm1.actLoadNewSourceExecute(Sender: TObject);
var
  Source : TSimpleTextFileSource;
  i: Integer;
begin
  with OpenDialog do
    if Execute then
      begin
        Source := TSimpleTextFileSource.Create(FileName);
        FBrain.AddSource(Source);
        SourceMemo.Text := Source.ToString;
        for i := 0 to FBrain.KnowledgeBase.Count - 1 do
          KnowledgeMemo.Lines.Add(FBrain.KnowledgeBase[i].ToString);
      end;
end;

procedure TMainForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBrain);
end;

procedure TMainForm1.MenuItem2Click(Sender: TObject);
begin

end;

end.

