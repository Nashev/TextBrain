unit Engine11Unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, Engine1Unit;

type
  TKnowledgeBase1 = class(TKnowledgeBase)
  private
    FItems: TObjectList;
  protected
    function GetItem(Index: integer): TKnowledgeItem; override;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer; override;
    procedure Add(AItem: TKnowledgeItem); override;
  end;

  TBrain1 = class(TBrain)
  private
    FSources, FDetectors: TObjectList;
    FKnowledgeBase: TKnowledgeBase1;
  protected
    function GetDetector(Index: integer): TDetector; override;
    function GetSource(Index: integer): TSource; override;
    function GetKnowledgeBase: TKnowledgeBase; override;
  public
    constructor Create;
    destructor Destroy; override;
    function DetectorCount: integer; override;
    function SourceCount: integer; override;
    procedure AddSource(ASource: TSource); override;
    procedure DoProcessSource(ASource: TSource);
  end;

  TSimpleTextFileSource = class(TSource)
  private
    FFileName: string;
    FFileContent: string;
    FPosition: PChar;
  public
    function ToString: string; override;
    constructor Create(AFileName: string);
    function EOF: boolean; override;
    function GetNextItem: TSourceItem; override;
  end;

  TSimpleTextFileSourceInfo = class(TSourceInfo)
  private
    FSource: TSimpleTextFileSource;
    FPosition: integer;
    FLength: integer;
  public
    constructor Create(ASource: TSimpleTextFileSource; APosition: integer;
      ALength: integer);
    function ToString: string; override;
    function GetSource: TSource; override;
  end;

  TSimpleTextFileSourceItem = class(TSourceItem)
  private
    FWord: string;
    FSourceInfo: TSimpleTextFileSourceInfo;
  public
    constructor Create(ASource: TSimpleTextFileSource; AWord: string;
      APosition: integer);
    function ToString: string; override;
    function GetSourceInfo: TSourceInfo; override;
  end;

  TKnowledgeItem1 = class(TKnowledgeItem)
  private
    FProofs: TObjectList;
  protected
    function GetProof(Index: integer): TSourceInfo; override;
  public
    constructor Create(ASourceInfo: TSourceInfo);
    function ProofCount: integer; override;
    procedure AddProof(AProof: TSourceInfo); override;
    function Merge(AOtherItem: TKnowledgeItem): boolean; override;
    function IsSameKnowledge(AOtherItem: TKnowledgeItem): Boolean; virtual; abstract;
  end;

  TWord = class(TKnowledgeItem1)
  private
    FWord: string;
  public
    function ToString: string; override;
    constructor Create(ASourceInfo: TSourceInfo; AWord: string);
    function IsSameKnowledge(AOtherItem: TKnowledgeItem): Boolean; override;
  end;

  TWordDetector = class(TDetector)
    procedure Evalute(AKnowledgeBaseSubset: TKnowledgeBaseSubset; ASourceItem: TSourceItem); override;
    procedure Evalute(AKnowledgeBaseSubset: TKnowledgeBaseSubset); override;
  end;

implementation

resourcestring
  rsDProofs = ' (%d proofs)';
  rsProofFromFileSDDS = 'From file %s (%d-%d): "%s".';



{ TWord }

function TWord.ToString: string;
begin
  Result := FWord + Format(rsDProofs, [ProofCount]);
end;

constructor TWord.Create(ASourceInfo: TSourceInfo; AWord: string);
begin
  inherited Create(ASourceInfo);
  FWord := AWord;
  if (Length(FWord) = 1) and (ord(FWord[1]) <= 32) then
     FWord := '#' + IntToStr(ord(FWord[1]));
end;

function TWord.IsSameKnowledge(AOtherItem: TKnowledgeItem): Boolean;
begin
  Result := (AOtherItem is TWord) and (TWord(AOtherItem).FWord = FWord);
end;

{ TKnowledgeItem1 }

function TKnowledgeItem1.GetProof(Index: integer): TSourceInfo;
begin
  Result := TSourceInfo(FProofs[Index]);
end;

constructor TKnowledgeItem1.Create(ASourceInfo: TSourceInfo);
begin
  FProofs := TObjectList.Create(True);
  AddProof(ASourceInfo);
end;

function TKnowledgeItem1.ProofCount: integer;
begin
  Result := FProofs.Count;
end;

procedure TKnowledgeItem1.AddProof(AProof: TSourceInfo);
begin
  FProofs.Add(AProof);
end;

function TKnowledgeItem1.Merge(AOtherItem: TKnowledgeItem): boolean;
var
  i: Integer;
begin
  Result := IsSameKnowledge(AOtherItem);
  if Result then
    begin
      for i := 0 to AOtherItem.ProofCount - 1 do
        AddProof(AOtherItem.Proof[i]);
      (AOtherItem as TKnowledgeItem1).FProofs.OwnsObjects := False;
      AOtherItem.Free;
    end;
end;

{ TWordDetector }

procedure TWordDetector.Evalute(AKnowledgeBaseSubset: TKnowledgeBaseSubset;
  ASourceItem: TSourceItem);
begin
  AKnowledgeBaseSubset.Add(TWord.Create(ASourceItem.GetSourceInfo, ASourceItem.ToString));
end;

procedure TWordDetector.Evalute(AKnowledgeBaseSubset: TKnowledgeBaseSubset);
begin
  // do nothing;
end;

{ TSimpleTextFileSourceInfo }

constructor TSimpleTextFileSourceInfo.Create(ASource: TSimpleTextFileSource;
  APosition: integer; ALength: integer);
begin
  FSource := ASource;
  FPosition := APosition;
  FLength := ALength;
end;

function TSimpleTextFileSourceInfo.ToString: string;
begin
  Result := Format(rsProofFromFileSDDS,
    [FSource.FFileName, FPosition, FPosition + FLength,
    copy(FSource.ToString, FPosition, FLength)]);
end;

function TSimpleTextFileSourceInfo.GetSource: TSource;
begin
  Result := FSource;
end;

{ TSimpleTextFileSourceItem }

constructor TSimpleTextFileSourceItem.Create(ASource: TSimpleTextFileSource;
  AWord: string; APosition: integer);
begin
  FWord := AWord;
  FSourceInfo := TSimpleTextFileSourceInfo.Create(ASource, APosition, Length(AWord));
end;

function TSimpleTextFileSourceItem.ToString: string;
begin
  Result := FWord;
end;

function TSimpleTextFileSourceItem.GetSourceInfo: TSourceInfo;
begin
  Result := FSourceInfo;
end;

{ TSimpleTextFileSource }

function TSimpleTextFileSource.ToString: string;
begin
  Result := FFileName + #13#10'-----------------------------------------------------------'#13#10#13#10 + FFileContent;
end;

constructor TSimpleTextFileSource.Create(AFileName: string);
begin
  FFileName := AFileName;
  with TStringList.Create do
    try
      LoadFromFile(FFileName);
      FFileContent := Text + #0;
    finally
      Free;
    end;
  FPosition := @FFileContent[1];
end;

function TSimpleTextFileSource.EOF: boolean;
begin
  Result := FPosition^ = #0;
end;

function TSimpleTextFileSource.GetNextItem: TSourceItem;
var
  EndPosition: PChar;
begin
  Assert(eof);

  // skip spaces
  if (FPosition^<>#0) and (FPosition^ in [#32, '.', ',', ':', ';', '(', ')', '[', ']', '/', '+', '-', '*', '{', '}', '=']) then // TODO: letter / non letter by Unicode routines
    begin
      Result := TSimpleTextFileSourceItem.Create(Self, FPosition^, FPosition - @FFileContent[1] + 1);
      Inc(FPosition);
      Exit;
    end;

  EndPosition := FPosition;

  // collect nonspaces:
  repeat
    Inc(EndPosition);
  until (EndPosition^ in [#0..#32, '.', ',', ':', ';', '(', ')', '[', ']', '/', '+', '-', '*', '{', '}', '=']); // TODO: letter / non letter by Unicode routines

  Result := TSimpleTextFileSourceItem.Create(Self, copy(string(FPosition), 1, EndPosition - FPosition), FPosition - @FFileContent[1] + 1);

  FPosition := EndPosition;
end;

{ TBrain1 }

function TBrain1.GetDetector(Index: integer): TDetector;
begin
  Result := TDetector(FDetectors[Index]);
end;

function TBrain1.GetSource(Index: integer): TSource;
begin
  Result := TSource(FSources[Index]);
end;

function TBrain1.GetKnowledgeBase: TKnowledgeBase;
begin
  Result := FKnowledgeBase;
end;

constructor TBrain1.Create;
begin
  FSources := TObjectList.Create(True);
  FDetectors := TObjectList.Create(True);
  FKnowledgeBase := TKnowledgeBase1.Create;

  FDetectors.Add(TWordDetector.Create);
end;

destructor TBrain1.Destroy;
begin
  FreeAndNil(FSources);
  FreeAndNil(FDetectors);
  FreeAndNil(FKnowledgeBase);
  inherited Destroy;
end;

function TBrain1.DetectorCount: integer;
begin
  Result := FDetectors.Count;
end;

function TBrain1.SourceCount: integer;
begin
  Result := FSources.Count;
end;

procedure TBrain1.AddSource(ASource: TSource);
begin
  FSources.Add(ASource);
  DoProcessSource(ASource);
end;

procedure TBrain1.DoProcessSource(ASource: TSource);
var
  NextSourceItem: TSourceItem;
  i: integer;
begin
  while not ASource.EOF do
  begin
    NextSourceItem := ASource.GetNextItem;
    for i := 0 to DetectorCount - 1 do
      Detectors[i].Evalute(KnowledgeBase, NextSourceItem);
  end;
end;

{ TKnowledgeBase1 }

function TKnowledgeBase1.GetItem(Index: integer): TKnowledgeItem;
begin
  Result := TKnowledgeItem(FItems[Index]);
end;

constructor TKnowledgeBase1.Create;
begin
  FItems := TObjectList.Create(True);
end;

destructor TKnowledgeBase1.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TKnowledgeBase1.Count: integer;
begin
  Result := FItems.Count;
end;

procedure TKnowledgeBase1.Add(AItem: TKnowledgeItem);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].Merge(AItem) then
      Exit;
  FItems.Add(AItem);
end;

end.

