unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ActnList, Menus, ExtCtrls, Engine1Unit, Engine11Unit, LCLType,
  LMessages, Messages, Windows;

type
  { TMainForm }

  //trick

  { TMemo }

  TMemo = class(StdCtrls.TCustomMemo)
  private
    FCanvas: TControlCanvas;
    procedure DestroyWnd; override;
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure PaintWindow(DC: HDC); override;
    procedure DrawHighlights;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Alignment;
    property Anchors;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property Lines;
    property MaxLength;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property ParentShowHint;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordWrap;
  end;

  TMainForm = class(TForm)
    actLoadNewUTF8Source: TAction;
    actLoadNewAnsiSource: TAction;
    ActionList: TActionList;
    ButtonClear: TButton;
    ButtonShowSubsets: TButton;
    ButtonShowBrainContent: TButton;
    MainMenu: TMainMenu;
    KnowledgeMemo: TMemo;
    StatusMemo: TMemo;
    MenuItem1: TMenuItem;
    mmiSources: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    SourceMemo: TMemo;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    ToggleBoxPause: TToggleBox;
    ToggleBoxSilent: TToggleBox;
    procedure actLoadNewAnsiSourceExecute(Sender: TObject);
    procedure actLoadNewUTF8SourceExecute(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonShowSubsetsClick(Sender: TObject);
    procedure ButtonShowBrainContentClick(Sender: TObject);
    procedure ClearKnowledgeMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure KnowledgeMemoClick(Sender: TObject);
    procedure KnowledgeMemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FBrain: TBrain1;
    Source: TSimpleTextFileSource;
    KnowlledgeLineItems: TList;
  protected
    procedure ShowBrainContent;
    procedure UpdateStatus;
    procedure AppendToKnowledgeMemo(AText: string; AItem: TObject);
    procedure HighlightSource(FocusedItem: TKnowledgeItem);
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

resourcestring
  rsBrainSubsetHeader = 'Brain subset %d:';
  rsNoSourceFile = 'No source file';

{ TMemo }

constructor TMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TMemo.Destroy;
begin
  FreeAndNil(FCanvas);
  inherited Destroy;
end;

procedure TMemo.DestroyWnd;
begin
  if FCanvas <> nil then
    TControlCanvas(FCanvas).FreeHandle;
  inherited DestroyWnd;
end;

procedure TMemo.WMPaint(var Message: TLMPaint);
begin
  if (csDestroying in ComponentState) or (not HandleAllocated) then exit;
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Message);
  Exclude(FControlState, csCustomPaint);
end;

procedure TMemo.PaintWindow(DC: HDC);
var
  DCChanged: boolean;
begin
  DCChanged := (not FCanvas.HandleAllocated) or (FCanvas.Handle <> DC);
  if DCChanged then
    FCanvas.Handle := DC;
  try
    inherited PaintWindow(DC);
    DrawHighlights;
  finally
    if DCChanged then FCanvas.Handle := 0;
  end;
end;

procedure TMemo.DrawHighlights;
var
  r: LResult;
begin
  FCanvas.Pen.Color := clRed;
  FCanvas.Pen.Width := 5;
  FCanvas.Pen.Style := psSolid;
  r := Windows.SendMessage(Handle, EM_POSFROMCHAR, SelStart, 0);
  if r <> -1 then
    FCanvas.MoveTo(LoWord(r), HiWord(r));

  r := Windows.SendMessage(Handle, EM_POSFROMCHAR, SelStart + SelLength, 0);
  if r <> -1 then
    FCanvas.LineTo(LoWord(r), HiWord(r));
end;

{ TNewItemDetector }

procedure TNewItemDetector.Evalute(AKnowledgeItem: TKnowledgeItem);
var
  i: Integer;
begin
  if not MainForm.ToggleBoxSilent.Checked then
    begin
      MainForm.AppendToKnowledgeMemo(AKnowledgeItem.InfoText, AKnowledgeItem);
      for i := 0 to AKnowledgeItem.Owner.SubsetsCount - 1 do
        begin
          MainForm.AppendToKnowledgeMemo(Format(rsBrainSubsetHeader, [i]), nil);
          MainForm.AppendToKnowledgeMemo(AKnowledgeItem.Owner.Subset[i].ContentText, AKnowledgeItem.Owner.Subset[i]);
        end;
    end;
  if (Now - LastMessagesTime) > (333 / (24*60*60*1000)) then
    begin
      MainForm.UpdateStatus;
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
  FBrain.Detectors.Insert(0, TNewItemDetector.Create);
  KnowlledgeLineItems := TList.Create;
  UpdateStatus;
end;

procedure TMainForm.UpdateStatus;
begin
  if Assigned(Source) then
    StatusMemo.Text := Source.InfoText
  else
    StatusMemo.Text := rsNoSourceFile;
end;

procedure TMainForm.actLoadNewAnsiSourceExecute(Sender: TObject);
begin
  with OpenDialog do
    if Execute then
      begin
        Source := TSimpleAnsiTextFileSource.Create(nil, FileName);
        SourceMemo.Text := Source.ToString;
        Source.IntegrateToBrain(FBrain);
        UpdateStatus;
        ShowBrainContent;
      end;
end;

procedure TMainForm.actLoadNewUTF8SourceExecute(Sender: TObject);
begin
  with OpenDialog do
    if Execute then
      begin
        Source := TSimpleUTF8TextFileSource.Create(nil, FileName);
        SourceMemo.Text := Source.ToString;
        Source.IntegrateToBrain(FBrain);
        UpdateStatus;
        ShowBrainContent;
      end;
end;

procedure TMainForm.ButtonClearClick(Sender: TObject);
begin
  ClearKnowledgeMemo;
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
      AppendToKnowledgeMemo(APrefix + ' ' + AKnowledgeBaseSubset.ContentText, AKnowledgeBaseSubset)
    else
      begin
        AppendToKnowledgeMemo(APrefix + ' ' + AKnowledgeBaseSubset.InfoText, AKnowledgeBaseSubset);
        with AKnowledgeBaseSubset.GetIterator do
          try
            if not EOF then
              AppendToKnowledgeMemo(APrefix + rsSubsetContent, AKnowledgeBaseSubset)
            else
              AppendToKnowledgeMemo(APrefix + rsSubsetContentEmpty, AKnowledgeBaseSubset);

            while not EOF do
              begin
                AppendToKnowledgeMemo(APrefix + ' - ' + CurrentItem.InfoText, CurrentItem);
                Next;
              end;
          finally
            Free;
          end;
      end;

    if AKnowledgeBaseSubset.SubsetsCount > 0 then
      AppendToKnowledgeMemo(APrefix + Format(rsSubsetSubsets, [AKnowledgeBaseSubset.SubsetsCount]), AKnowledgeBaseSubset)
    else
      AppendToKnowledgeMemo(APrefix + rsSubsetSubsetsEmpty, AKnowledgeBaseSubset);

    for i := 0 to AKnowledgeBaseSubset.SubsetsCount - 1 do
      ShowKnowledgeSet(APrefix + Format(rsSubsetIndex, [i]), AKnowledgeBaseSubset.Subset[i]);
  end;

begin
  KnowledgeMemo.Lines.BeginUpdate;
  try
    ClearKnowledgeMemo;
    ShowKnowledgeSet('', FBrain);
  finally
    KnowledgeMemo.Lines.EndUpdate;
  end;
end;

procedure TMainForm.ButtonShowBrainContentClick(Sender: TObject);
begin
  ShowBrainContent;
end;

procedure TMainForm.ClearKnowledgeMemo;
begin
  KnowledgeMemo.Lines.Clear;
  KnowlledgeLineItems.Clear;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBrain);
  FreeAndNil(KnowlledgeLineItems);
end;

procedure TMainForm.KnowledgeMemoClick(Sender: TObject);
var
  FocusedItem: TObject;
begin
  if KnowledgeMemo.CaretPos.y >= KnowlledgeLineItems.Count then
      Exit;
  FocusedItem := TObject(KnowlledgeLineItems[KnowledgeMemo.CaretPos.y]);
  if (FocusedItem <> nil) and (FocusedItem is TKnowledgeItem) then
    HighlightSource(TKnowledgeItem(FocusedItem));
end;

procedure TMainForm.KnowledgeMemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  FocusedItem: TObject;
begin
  if KnowledgeMemo.CaretPos.y >= KnowlledgeLineItems.Count then
      Exit;
  FocusedItem := TObject(KnowlledgeLineItems[KnowledgeMemo.CaretPos.y]);
  if (FocusedItem <> nil) and (FocusedItem is TKnowledgeItem) then
    HighlightSource(TKnowledgeItem(FocusedItem));
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
  KnowledgeMemo.Lines.BeginUpdate;
  try
    ClearKnowledgeMemo;
    for i := 0 to FBrain.Count - 1 do
      begin
        Item := FBrain[i];
        if (Item is TSource) or (Item is TSourceItem) then
          Continue;
        AppendToKnowledgeMemo(Item.InfoText, Item);
        for j := 0 to Item.Basis.Count - 1 do
          AppendToKnowledgeMemo('           '#9 + Item.Basis[j].InfoText, Item.Basis[j]);
      end;
  finally
    KnowledgeMemo.Lines.EndUpdate;
  end;
end;

procedure TMainForm.AppendToKnowledgeMemo(AText: string; AItem: TObject);
var
  i, FirstIndex: Integer;
begin
  FirstIndex := KnowledgeMemo.Lines.Count;
  KnowledgeMemo.Lines.Add(AText); // possible CRs
  KnowlledgeLineItems.Count := KnowledgeMemo.Lines.Count;
  for i := FirstIndex to KnowlledgeLineItems.Count - 1 do
    KnowlledgeLineItems[i] := AItem;
end;

end.

