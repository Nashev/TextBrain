unit Engine11Unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, Engine1Unit;

type
  PUTF8Char = type PChar;

  TBrain1 = class(TBrain)
  private
    FSources, FDetectors: TObjectList;
    FItems: TObjectList;
  protected
    function GetItem(Index: integer): TKnowledgeItem; override;
    function GetDetector(Index: integer): TDetector; override;
    function GetSource(Index: integer): TSource; override;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer; override;
    function Add(AItem: TKnowledgeItem): TKnowledgeItem; override;
    function DetectorCount: integer; override;
    function SourceCount: integer; override;
    procedure AddSource(ASource: TSource); override;
  end;

  TSimpleTextFileSource = class(TSource)
  private
    FFileName: UTF8String;
    FFileContent: UTF8String;
    FPosition: PUTF8String;
  protected
    function EncodeFileStringListToContent(AStringList: TStringList): UTF8String; virtual; abstract;
  public
    function ToString: UTF8String; override;
    constructor Create(AFileName: UTF8String); virtual;
    function EOF: boolean; override;
    function GetNextItem: TSourceItem; override;
  end;

  { TSimpleAnsiTextFileSource }

  TSimpleAnsiTextFileSource = class(TSimpleTextFileSource)
  protected
    function EncodeFileStringListToContent(AStringList: TStringList): UTF8String; override;
  end;

  { TSimpleUTF8TextFileSource }

  TSimpleUTF8TextFileSource = class(TSimpleTextFileSource)
    function EncodeFileStringListToContent(AStringList: TStringList): UTF8String; override;
  end;

  TSimpleTextFileSourceItem = class(TSourceItem)
  private
    FSource: TSimpleTextFileSource;
    FPosition: PUTF8Char;
    FLength: Integer;
    FSize: Integer;
  public
    constructor Create(ASource: TSimpleTextFileSource; APosition: PUTF8Char; ALength, ASize: integer);
    function ToString: UTF8String; override;
  end;

  { TKnowledgeItem1 }

  TKnowledgeItem1 = class(TKnowledgeItem)
  private
    FProofs: TObjectList;
  protected
    function GetProof(Index: integer): TKnowledgeItem; override;
    function GetDetectorClass: TDetectorClass; override;
    function GetLinkedKnowledge(Index: Integer): TKnowledgeItem; override;
  public
    function LinkCount: Integer; override;
    constructor Create(ASourceInfo: TKnowledgeItem);
    function ProofCount: integer; override;
    procedure AddProof(AProof: TKnowledgeItem); override;
    function Merge(AOtherItem: TKnowledgeItem): boolean; override;
  end;

  { TWord }

  TWord = class(TKnowledgeItem1)
  public
    class function FindWord(AKnowledgeBaseSubset: TKnowledgeBaseSubset; AWord: UTF8String): TWord;
    class function FindWordOrRegisterModified(
      AKnowledgeBaseSubset: TKnowledgeBaseSubset;
      AModifiedWord: UTF8String;
      AOriginalWord: TWord
    ): TWord;
    function IsSameKnowledge(AOtherItem: TKnowledgeItem): Boolean; override;
    function SameWord(AWord: UTF8String): Boolean;
    function ToString: UTF8String; override;
  end;

  { TModifiedWord }

  TModifiedWord = class(TWord)
  private
    FModifiedWord: UTF8String;
  public
    constructor Create(AOriginalWord: TWord; AModifiedWord: UTF8String); reintroduce;
    function ToString: UTF8String; override;
  end;

  TWordDetector = class(TDetector)
    procedure Evalute(AKnowledgeBaseSubset: TKnowledgeBaseSubset; AKnowledgeItem: TKnowledgeItem); override;
    procedure Evalute(AKnowledgeBaseSubset: TKnowledgeBaseSubset); override;
  end;

  { TCapitalizedWordDetector }

  TCapitalizedWordDetector = class(TDetector)
    procedure Evalute(AKnowledgeBaseSubset: TKnowledgeBaseSubset; AKnowledgeItem: TKnowledgeItem); override;
    procedure Evalute(AKnowledgeBaseSubset: TKnowledgeBaseSubset); override;
  end;

  { TWordWasCapitalizedFact }

  TWordWasCapitalizedFact = class(TKnowledgeItem)
  private
    FNotLowercasedWord, FLowercasedWord: TWord;
  protected
    function GetProof(Index: integer): TKnowledgeItem; override;
    function GetLinkedKnowledge(Index: Integer): TKnowledgeItem; override;
    function GetDetectorClass: TDetectorClass; override;
  public
    constructor Create(ANotLowercasedWord, ALowercasedWord: TWord);
    procedure AddProof(AProof: TKnowledgeItem); override;
    function ToString: UTF8String; override;
    function LinkCount: Integer; override;
    function IsSameKnowledge(AOtherItem: TKnowledgeItem): Boolean; override;
    function ProofCount: integer; override;
    function Merge(AOtherItem: TKnowledgeItem): boolean; override;
  end;

implementation

uses FileUtil;

resourcestring
  rsProofFromFileSDDS = 'From file %s (%d-%d): "%s".';

{ TModifiedWord }

constructor TModifiedWord.Create(AOriginalWord: TWord;
  AModifiedWord: UTF8String);
begin
  Assert(AOriginalWord.ProofCount > 0, '{CD1B333C-C278-4990-84A9-E97A5D6F9E59}');
  inherited Create(AOriginalWord.Proof[0]);
  FModifiedWord := AModifiedWord;
end;

function TModifiedWord.ToString: UTF8String;
begin
  Result := FModifiedWord;
end;

{ TWordWasCapitalizedFact }

function TWordWasCapitalizedFact.GetProof(Index: integer): TKnowledgeItem;
begin
  Result := FNotLowercasedWord.Proof[Index];
end;

function TWordWasCapitalizedFact.GetLinkedKnowledge(Index: Integer): TKnowledgeItem;
begin
  case Index of
    0:  Result := FNotLowercasedWord;
    1:  Result := FLowercasedWord;
  end;
end;

function TWordWasCapitalizedFact.GetDetectorClass: TDetectorClass;
begin
  Result := TCapitalizedWordDetector;
end;

constructor TWordWasCapitalizedFact.Create(ANotLowercasedWord, ALowercasedWord: TWord);
begin
  inherited Create;
  FNotLowercasedWord  := ANotLowercasedWord;
  FLowercasedWord     := ALowercasedWord;
end;

procedure TWordWasCapitalizedFact.AddProof(AProof: TKnowledgeItem);
begin
  Assert(False, 'not acceptable');
end;

function TWordWasCapitalizedFact.ToString: UTF8String;
begin
  Result := 'link: ' + FNotLowercasedWord.ToString + ' <-> ' + FLowercasedWord.ToString;
end;

function TWordWasCapitalizedFact.LinkCount: Integer;
begin
  Result := 2;
end;

function TWordWasCapitalizedFact.IsSameKnowledge(AOtherItem: TKnowledgeItem): Boolean;
begin
  Result := (AOtherItem is TWordWasCapitalizedFact) and (TWordWasCapitalizedFact(AOtherItem).FNotLowercasedWord = FNotLowercasedWord);
end;

function TWordWasCapitalizedFact.ProofCount: integer;
begin
  Result := FNotLowercasedWord.ProofCount;
end;

function TWordWasCapitalizedFact.Merge(AOtherItem: TKnowledgeItem): boolean;
begin
  Result := IsSameKnowledge(AOtherItem);
  if Result then
    AOtherItem.Free;
end;

{ TCapitalizedWordDetector }

class function TWord.FindWord(AKnowledgeBaseSubset: TKnowledgeBaseSubset; AWord: UTF8String): TWord;
var
  i: Integer;
  Item: TKnowledgeItem;
begin
  Result := nil;
  for i := 0 to AKnowledgeBaseSubset.Count - 1 do
    begin
      Item := AKnowledgeBaseSubset[i];
      if not (Item is TWord) then
        Continue;
      if TWord(Item).SameWord(AWord) then
        begin
          Result := TWord(Item);
          Exit;
        end;
    end;
end;

class function TWord.FindWordOrRegisterModified(AKnowledgeBaseSubset: TKnowledgeBaseSubset; AModifiedWord: UTF8String; AOriginalWord: TWord): TWord;
var
  i: Integer;
  Item: TKnowledgeItem;
begin
  Result := FindWord(AKnowledgeBaseSubset, AModifiedWord);
  if not Assigned(Result) then
    Result := TWord(AKnowledgeBaseSubset.Add(TModifiedWord.Create(AOriginalWord, AModifiedWord)))
  else
    Result.AddProof(AOriginalWord); // TODO: indeed?
end;


procedure TCapitalizedWordDetector.Evalute(AKnowledgeBaseSubset: TKnowledgeBaseSubset; AKnowledgeItem: TKnowledgeItem);
var
  NewWord, LowercaseWord: WideString;
  CapitalizedWordItem, LowercaseWordItem: TWord;
begin
  NewWord := AKnowledgeItem.ToString;
  LowercaseWord := WideLowerCase(NewWord);
  if LowercaseWord = NewWord then
    Exit;

  CapitalizedWordItem := TWord.FindWord(AKnowledgeBaseSubset, NewWord);
  LowercaseWordItem   := TWord.FindWordOrRegisterModified(AKnowledgeBaseSubset, LowercaseWord, CapitalizedWordItem);

  AKnowledgeBaseSubset.Add(TWordWasCapitalizedFact.Create(CapitalizedWordItem, LowercaseWordItem));
end;

procedure TCapitalizedWordDetector.Evalute(
  AKnowledgeBaseSubset: TKnowledgeBaseSubset);
begin
  // do nothing;
end;

{ TWord }

function TWord.ToString: UTF8String;
begin
  Assert(ProofCount > 0, '{0190F889-CB9D-48D3-A01A-369E4B823FC1}');
  Result := Proof[0].ToString;
  if (Length(Result) = 1) and (ord(Result[1]) <= 32) then
    Result := '#' + IntToStr(ord(Result[1]));
end;

function TWord.IsSameKnowledge(AOtherItem: TKnowledgeItem): Boolean;
begin
  Result := (AOtherItem is TWord) and SameWord(TWord(AOtherItem).AsString);
end;

function TWord.SameWord(AWord: UTF8String): Boolean;
var
  OwnWord: TUTF8String;
begin
  OwnWord := AsString;
  Result := (CompareStr(OwnWord, AWord) = 0);
end;

{ TKnowledgeItem1 }

function TKnowledgeItem1.GetProof(Index: integer): TSourceInfo;
begin
  Result := TSourceInfo(FProofs[Index]);
end;

function TKnowledgeItem1.GetDetectorClass: TDetectorClass;
begin
  Result := nil;
end;

function TKnowledgeItem1.GetLinkedKnowledge(Index: Integer): TKnowledgeItem;
begin
  Assert(False, 'Not acceptable!')
end;

function TKnowledgeItem1.LinkCount: Integer;
begin
  Result := 0;
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
  AKnowledgeItem: TSourceItem);
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

function TSimpleTextFileSourceInfo.GetSource: TSource;
begin
  Result := FSource;
end;

{ TSimpleTextFileSourceItem }

constructor TSimpleTextFileSourceItem.Create(ASource: TSimpleTextFileSource; APosition: PUTF8Char; ALength, ASize: integer);
begin
  FSource := ASource;
  FPosition := APosition;
  FLength   := ALength;
  FSize     := ASize;
end;

function TSimpleTextFileSourceItem.ToString: UTF8String;
var
  SourceContent: UTF8String;
begin
  SourceContent := FSource.FFileContent;
  Result :=
    Format(
      rsProofFromFileSDDS, [
        FSource.FFileName,
        FPosition,
        FPosition + FLength,
        copy(UTF8String(FPosition), 1, FLength)
      ]
    );
end;

{ TSimpleTextFileSource }

function TSimpleTextFileSource.ToString: UTF8String;
begin
  Result := FFileName + #13#10'-----------------------------------------------------------'#13#10#13#10 + FFileContent;
end;

constructor TSimpleTextFileSource.Create(AFileName: string);
var
  StringList: TStringList;
begin
  FFileName := AFileName;
  StringList := TStringList.Create;
    try
      StringList.LoadFromFile(UTF8ToSys(FFileName));
      FFileContent := EncodeFileStringListToContent(StringList) + #0;
    finally
      StringList.Free;
    end;
  FPosition := @FFileContent[1];
end;

function TSimpleTextFileSource.EOF: boolean;
begin
  Result := FPosition^ = #0;
end;

function TSimpleTextFileSource.GetNextItem: TSourceItem;
var
  EndPosition: PUTF8String;
  NextChar: UnicodeChar;
  CharSize: SizeInt;
const
  // TODO: letter / non letter by Unicode routines
  DelimiterChars: UnicodeString =
    #1#2#3#4#5#6#7#8#9 +
    #10#11#12#13#14#15#16#17#18#19 +
    #20#21#22#23#24#25#26#27#28#29 +
    #30#31#32+
    '.,:;!?()[]{}<>=+-*/\|''"'+
    '„“«»—… ' (* $nbsp; *)
  ;
begin
  Assert(eof);

  CharSize := Utf8ToUnicode(@NextChar, PChar(FPosition), 1);
  // skip spaces
  if Pos(NextChar, DelimiterChars) > 0 then
    begin
      Result := TSimpleTextFileSourceItem.Create(Self, NextChar, FPosition - @FFileContent[1] + 1);
      Inc(FPosition, CharSize);
      Exit;
    end;

  EndPosition := FPosition;

  // collect nonspaces:
  repeat
    Inc(EndPosition);
  until (FPosition^ = #0) or (Pos(FPosition^, DelimiterChars) > 0);

  Result := TSimpleTextFileSourceItem.Create(Self, copy(string(FPosition), 1, EndPosition - FPosition), FPosition - @FFileContent[1] + 1);

  FPosition := EndPosition;
end;

{ TSimpleUTF8TextFileSource }

function TSimpleUTF8TextFileSource.EncodeFileStringListToContent(
  AStringList: TStringList): UTF8String;
begin
  Result := AStringList.Text;
end;

{ TSimpleAnsiTextFileSource }

function TSimpleAnsiTextFileSource.EncodeFileStringListToContent(
  AStringList: TStringList): UTF8String;
begin
  Result := AnsiToUTF8(AStringList.Text);
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
  FItems := TObjectList.Create(True);

  FDetectors.Add(TWordDetector.Create);
  FDetectors.Add(TCapitalizedWordDetector.Create);
end;

destructor TBrain1.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FSources);
  FreeAndNil(FDetectors);
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
  while not ASource.EOF do
    Add(ASource.GetNextItem);
end;

function TBrain1.GetItem(Index: integer): TKnowledgeItem;
begin
  Result := TKnowledgeItem(FItems[Index]);
end;

function TBrain1.Count: integer;
begin
  Result := FItems.Count;
end;

function TBrain1.Add(AItem: TKnowledgeItem): TKnowledgeItem;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].Merge(AItem) then
      begin
        Result := Items[i]; // TODO: detect anything om merge
        Exit;
      end;
  FItems.Add(AItem);
  Result := AItem;
  for i := 0 to DetectorCount - 1 do
    Detectors[i].Evalute(KnowledgeBase, Result);
end;

end.

