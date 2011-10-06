unit Engine11Unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, Engine1Unit;

type
  PUTF8Char = type PChar;

  EWrongCallException = class(Exception)
  end;

  EWrongStateException = class(Exception)
  end;

  { TKnowledgeItem1 }

  TKnowledgeItem1 = class(TKnowledgeItem)
  protected
    function GetBasisClass: TBasisClass; override;
    function GetConsequencesClass: TTabledKnowledgeBaseSubsetClass; override;
  end;

  { TBasis1 }

  TBasis1 = class(TBasis)
  private
    FItems: TObjectList;
  protected
    function GetItem(Index: Integer): TKnowledgeItem; override;
    function InternalAdd(AItem: TKnowledgeItem): TKnowledgeItem; override;
  public
    constructor Create(AOwner: TKnowledgeItem); override;
    destructor Destroy; override;

    function Count: Integer; override;
  end;

  { TTabledKnowledgeBaseSubset1 }

  TTabledKnowledgeBaseSubset1 = class(TTabledKnowledgeBaseSubset)
  private
    FItems: TObjectList;
  protected
    function GetItem(Index: Integer): TKnowledgeItem; override;
    function InternalAdd(AItem: TKnowledgeItem): TKnowledgeItem; override;
  public
    constructor Create(ASuperset: TKnowledgeBaseSubset); override;
    destructor Destroy; override;

    function Count: Integer; override;
  end;

  { TBrain1 }

  TBrain1 = class(TBrain)
  private
    FDetectors: TObjectList;
    FItems: TTabledKnowledgeBaseSubset1;
  protected
    function GetItem(Index: integer): TKnowledgeItem; override;
    function GetDetector(Index: integer): TDetector; override;
    function InternalAdd(AItem: TKnowledgeItem): TKnowledgeItem; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function Count: integer; override;
    property Detectors: TObjectList read FDetectors;
    function DetectorCount: integer; override;
  end;

  { TSimpleTextFileSource }

  TSimpleTextFileSource = class(TSource)
  private
    FFileName: UTF8String;
    FFileContent: UTF8String;
    FPosition, FEndChar: PUTF8Char;
  protected
    function GetBasisClass: TBasisClass; override;
    function GetConsequencesClass: TTabledKnowledgeBaseSubsetClass; override;
    function EncodeFileStringListToContent(AStringList: TStringList): UTF8String; virtual; abstract;
  public
    constructor Create(ADetectorClass: TDetectorClass; AFileName: UTF8String);
    function ToString: UTF8String; override;
    function InfoText: UTF8String; override;
    function IsSameKnowledge(AOtherItem: TKnowledgeItem): Boolean; override;

    function EOF: boolean; override;
    function ReadNextItem(ADetectorClass: TDetectorClass): TSourceItem; override;
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

  { TSimpleTextFileSourceItem }

  TSimpleTextFileSourceItem = class(TSourceItem)
  private
    FPosition: PUTF8Char;
    FSize: Integer;
    function Source: TSimpleTextFileSource;
  protected
    function GetBasisClass: TBasisClass; override;
    function GetConsequencesClass: TTabledKnowledgeBaseSubsetClass; override;
    function TryMergeToBrain(ABrain: TBrain): Boolean; override;
  public
    function IsSameKnowledge(AOtherItem: TKnowledgeItem): Boolean; override;
    constructor Create(ADetectorClass: TDetectorClass; ASource: TSimpleTextFileSource; APosition: PUTF8Char; ASize: integer);
    function InfoText: UTF8String; override;
    function ToString: UTF8String; override;
  end;

  TWordIndex = class;
  TWordIndexInfo = class;
  TWordIndexRealInfo = class;
  TWord = class;

  { TWordIndexInfo }

  TWordIndexInfo = class
  protected
    function GetLowerWordInfo: TWordIndexInfo; virtual; abstract;
    function GetHigherWordInfo: TWordIndexInfo; virtual; abstract;
    procedure SetHigherWordInfo(AValue: TWordIndexInfo); virtual; abstract;
    procedure SetLowerWordInfo(AValue: TWordIndexInfo); virtual; abstract;
  public
    property LowerWordInfo: TWordIndexInfo read GetLowerWordInfo write SetLowerWordInfo;
    property HigherWordInfo: TWordIndexInfo read GetHigherWordInfo write SetHigherWordInfo;
    function Weight: Integer; virtual; abstract;
    function UpdateWeight: TWordIndexInfo; virtual; abstract;
    function RegisterNewWord(AWord: TWord):TWordIndexInfo; virtual; abstract;
    function FindWord(AWord: UTF8String): TWord; virtual; abstract;
  end;

  { TWordIndexRealInfo }

  TWordIndexRealInfo = class(TWordIndexInfo)
  private
    Word: TWord;
    FLowerWordInfo: TWordIndexInfo;
    FHigherWordInfo: TWordIndexInfo;
    FWeight: Integer;
  protected
    function GetLowerWordInfo: TWordIndexInfo; override;
    function GetHigherWordInfo: TWordIndexInfo; override;
    procedure SetHigherWordInfo(AValue: TWordIndexInfo); override;
    procedure SetLowerWordInfo(AValue: TWordIndexInfo); override;
  public
    destructor Destroy; override;
    function Weight: Integer; override;
    function UpdateWeight: TWordIndexInfo; override;
    function RegisterNewWord(AWord: TWord):TWordIndexInfo; override;
    function FindWord(AWord: UTF8String): TWord; override;
  end;

  { TWordIndexStubInfo }

  TWordIndexStubInfo = class(TWordIndexInfo)
  protected
    function GetLowerWordInfo: TWordIndexInfo; override;
    function GetHigherWordInfo: TWordIndexInfo; override;
    procedure SetHigherWordInfo(AValue: TWordIndexInfo); override;
    procedure SetLowerWordInfo(AValue: TWordIndexInfo); override;
  public
    function Weight: Integer; override;
    function UpdateWeight:TWordIndexInfo; override;
    function RegisterNewWord(AWord: TWord):TWordIndexInfo; override;
    function FindWord(AWord: UTF8String): TWord; override;
  end;

  TWordIndex = class(TKnowledgeBaseSubset)
  private
    Stub: TWordIndexStubInfo;
    Root: TWordIndexInfo;
  public
    function FindWord(AWord: UTF8String): TWord;

    constructor Create(ASuperset: TKnowledgeBaseSubset); override;
    destructor Destroy; override;

    // subset content management
    function GetIterator: TKnowledgeIterator; override;
    function ContentText: UTF8String; override;

    // subset hierarchy
    procedure SupersetItemAdded(AItem: TKnowledgeItem); override;
    function InfoText: UTF8String; override;
  end;

  { TWord }

  TWord = class(TKnowledgeItem1)
  private
    FContentCashe: string;
  protected
    function TryMergeToBrain(ABrain: TBrain): Boolean; override;
  public
    class function FindWord(ABrain: TBrain; AWord: UTF8String): TWord;
    function IsSameKnowledge(AOtherItem: TKnowledgeItem): Boolean; override;
    function IsSameWord(AWord: UTF8String): Boolean;
    function ToString: UTF8String; override;
  end;

  TWordDetector = class(TDetector)
    procedure Evalute(AKnowledgeItem: TKnowledgeItem); override;
  end;

  { TModifiedWord }

  TModifiedWord = class(TWord)
  private
    FModifiedWord: UTF8String;
  public
    constructor Create(ADetectorClass: TDetectorClass; AOriginalWord: TWord; AModifiedWord: UTF8String); reintroduce;
    function ToString: UTF8String; override;
  end;

  { TCapitalizedWordDetector }

  TCapitalizedWordDetector = class(TDetector)
    procedure Evalute(AKnowledgeItem: TKnowledgeItem); override;
  end;

  { TWordWasCapitalizedFact }

  TWordWasCapitalizedFact = class(TKnowledgeItem1) // it doubles some TWord.Basis, when word had based on other word.
  protected
    function TryMergeToBrain(ABrain: TBrain): Boolean; override;
  public
    constructor Create(ADetectorClass: TDetectorClass; ACapitalizedWord, ALowercasedWord: TWord);
    function ToString: UTF8String; override;
    function IsSameKnowledge(AOtherItem: TKnowledgeItem): Boolean; override;
  end;

implementation

uses FileUtil, Math;

resourcestring
  rsProofFromFileSDDS = 'From file %s (%d-%d): "%s".';

{ TBasis1 }

function TBasis1.GetItem(Index: Integer): TKnowledgeItem;
begin
  Result := TKnowledgeItem(FItems[Index]);
end;

function TBasis1.InternalAdd(AItem: TKnowledgeItem): TKnowledgeItem;
begin
  Result := AItem;
  FItems.Add(AItem);
end;

constructor TBasis1.Create(AOwner: TKnowledgeItem);
begin
  inherited Create(AOwner);
  FItems := TObjectList.Create(False);
end;

destructor TBasis1.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TBasis1.Count: Integer;
begin
  Result := FItems.Count;
end;

function TWordIndex.FindWord(AWord: UTF8String): TWord;
begin
  Result := Root.FindWord(AWord);
end;

constructor TWordIndex.Create(ASuperset: TKnowledgeBaseSubset);
begin
  inherited Create(ASuperset);
  Stub := TWordIndexStubInfo.Create;
  Root := Stub;
end;

destructor TWordIndex.Destroy;
begin
  FreeAndNil(Root);
  FreeAndNil(Stub);
  inherited Destroy;
end;

function TWordIndex.GetIterator: TKnowledgeIterator;
begin
  // TODO
  Result := nil;
end;

function TWordIndex.ContentText: UTF8String;

  procedure AddContentText(APrefix: UTF8string; AWordIndexInfo: TWordIndexInfo);
  begin
    if AWordIndexInfo is TWordIndexRealInfo then
      with TWordIndexRealInfo(AWordIndexInfo) do
        begin
          AddContentText(APrefix + '*', LowerWordInfo);
          Result := Result + APrefix + ' ' + Word.ToString + ' (' + IntToStr(Weight) + ' weight)'#13#10;
          AddContentText(APrefix + '*', HigherWordInfo);
        end;
  end;

begin
  Result := InfoText + #13#10;
  AddContentText('*', Root);
end;

procedure TWordIndex.SupersetItemAdded(AItem: TKnowledgeItem);
begin
  if not (AItem is TWord) then
    Exit;

  Root := Root.RegisterNewWord(AItem as TWord);
end;

function TWordIndex.InfoText: UTF8String;
begin
  Result := inherited InfoText + ' Weight of root is ' + IntToStr(Root.Weight);
  if Root is TWordIndexRealInfo then
    Result := Result + ', word:' + TWordIndexRealInfo(Root).Word.InfoText;
end;

{ TWordIndexStubInfo }

function TWordIndexStubInfo.GetLowerWordInfo: TWordIndexInfo;
begin
  Result := Self;
end;

function TWordIndexStubInfo.GetHigherWordInfo: TWordIndexInfo;
begin
  Result := Self;
end;

procedure TWordIndexStubInfo.SetHigherWordInfo(AValue: TWordIndexInfo);
begin
  raise EWrongCallException.Create('TWordIndexStubInfo.SetHigherWordInfo');
end;

procedure TWordIndexStubInfo.SetLowerWordInfo(AValue: TWordIndexInfo);
begin
  raise EWrongCallException.Create('TWordIndexStubInfo.SetLowerWordInfo');
end;

function TWordIndexStubInfo.Weight: Integer;
begin
  Result := 0;
end;

function TWordIndexStubInfo.UpdateWeight:TWordIndexInfo;
begin
  Result := Self;// do nothing
end;

function TWordIndexStubInfo.RegisterNewWord(AWord: TWord): TWordIndexInfo;
begin
  Result := TWordIndexRealInfo.Create;
  TWordIndexRealInfo(Result).Word := AWord;
  TWordIndexRealInfo(Result).LowerWordInfo := Self;
  TWordIndexRealInfo(Result).HigherWordInfo := Self;
  Result := TWordIndexRealInfo(Result).UpdateWeight;
end;

function TWordIndexStubInfo.FindWord(AWord: UTF8String): TWord;
begin
  Result := nil;
end;

{TWordIndexRealInfo}

function TWordIndexRealInfo.GetLowerWordInfo: TWordIndexInfo;
begin
  Result := FLowerWordInfo;
end;

function TWordIndexRealInfo.GetHigherWordInfo: TWordIndexInfo;
begin
  Result := FHigherWordInfo;
end;

procedure TWordIndexRealInfo.SetHigherWordInfo(AValue: TWordIndexInfo);
begin
  FHigherWordInfo := AValue;
end;

procedure TWordIndexRealInfo.SetLowerWordInfo(AValue: TWordIndexInfo);
begin
  FLowerWordInfo := AValue;
end;

destructor TWordIndexRealInfo.Destroy;
begin
  if FLowerWordInfo.Weight > 0 then
    FreeAndNil(FLowerWordInfo);
  if FHigherWordInfo.Weight > 0 then
    FreeAndNil(FHigherWordInfo);
  inherited Destroy;
end;

function TWordIndexRealInfo.Weight: Integer;
begin
  Result := FWeight;
end;

function TWordIndexRealInfo.UpdateWeight:TWordIndexInfo;
var
  Middle: TWordIndexInfo;
  LowerWeight, HigherWeight: Integer;
begin
  Result := Self;
  LowerWeight  := LowerWordInfo.Weight;
  HigherWeight := HigherWordInfo.Weight;
  if LowerWeight >= (HigherWeight + 2) then
    begin
      if LowerWordInfo.LowerWordInfo.Weight > HigherWeight then // to prevent simple inverting of balance
        begin
          Result := LowerWordInfo;
          Middle := Result.HigherWordInfo;
          LowerWordInfo := Middle;
          LowerWeight   := Middle.Weight;
          FWeight := max(LowerWeight, HigherWeight) + 1; // calculate our weight
          Result.HigherWordInfo := Self;
          Result := Result.UpdateWeight; // it look to our weight, becouse now we are its child
        end
    end
  else if HigherWeight >= (LowerWeight + 2) then
    begin
      if HigherWordInfo.HigherWordInfo.Weight > LowerWeight then
        begin
          Result := HigherWordInfo;
          Middle := Result.LowerWordInfo;
          HigherWordInfo := Middle;
          HigherWeight   := Middle.Weight;
          FWeight := max(LowerWeight, HigherWeight) + 1;
          Result.LowerWordInfo := Self;
          Result := Result.UpdateWeight;
        end
    end
  else
    FWeight := max(LowerWeight, HigherWeight) + 1;
end;

function TWordIndexRealInfo.RegisterNewWord(AWord: TWord):TWordIndexInfo;
var
  Compare: Integer;
begin
  Compare := CompareStr(AWord.ToString, Word.ToString);
  if Compare = 0 then
    raise EWrongStateException('Double registering' + AWord.ToString)
  else if Compare < 0 then
    LowerWordInfo := LowerWordInfo.RegisterNewWord(AWord)
  else // > 0
    HigherWordInfo := HigherWordInfo.RegisterNewWord(AWord);
  Result := UpdateWeight;
end;

function TWordIndexRealInfo.FindWord(AWord: UTF8String): TWord;
var
  Compare: Integer;
begin
  Result := nil;
  Compare := CompareStr(AWord, Word.ToString);
  if Compare = 0 then
    Result := Word
  else if Compare < 0 then
    Result := LowerWordInfo.FindWord(AWord)
  else
    Result := HigherWordInfo.FindWord(AWord);
end;

{ TCapitalizedWordDetector }

procedure TCapitalizedWordDetector.Evalute(AKnowledgeItem: TKnowledgeItem);
var
  OriginalWord: TWord;
  NewWordUnicode, LowercaseWordUnicode: UnicodeString; // WideString;
  LowercaseWord: UTF8String;
  LowercaseWordItem: TWord;
begin
  if not (AKnowledgeItem is TWord) then
    Exit
  else
    OriginalWord := TWord(AKnowledgeItem);

  NewWordUnicode := UTF8Decode(OriginalWord.ToString);
  LowercaseWordUnicode := WideLowerCase(NewWordUnicode);
  if LowercaseWordUnicode = NewWordUnicode then
    Exit;

  LowercaseWord := UTF8Encode(LowercaseWordUnicode);

  LowercaseWordItem := TWord.FindWord(AKnowledgeItem.Owner, LowercaseWord);
  if not Assigned(LowercaseWordItem) then
    begin
      LowercaseWordItem := TModifiedWord.Create(SelfClass, OriginalWord, LowercaseWord);
      LowercaseWordItem.IntegrateToBrain(OriginalWord.Owner);
    end
  else
    LowercaseWordItem.Basis.Add(OriginalWord); // TODO: indeed?

  TWordWasCapitalizedFact.Create(SelfClass, OriginalWord, LowercaseWordItem).IntegrateToBrain(OriginalWord.Owner);
end;

{ TTabledKnowledgeBaseSubset1 }

function TTabledKnowledgeBaseSubset1.GetItem(Index: Integer): TKnowledgeItem;
begin
  Result := TKnowledgeItem(FItems[Index]);
end;

constructor TTabledKnowledgeBaseSubset1.Create(ASuperset: TKnowledgeBaseSubset);
begin
  inherited Create(ASuperset);
  FItems := TObjectList.Create(False);
end;

destructor TTabledKnowledgeBaseSubset1.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TTabledKnowledgeBaseSubset1.Count: Integer;
begin
  Result := FItems.Count;
end;

function TTabledKnowledgeBaseSubset1.InternalAdd(AItem: TKnowledgeItem): TKnowledgeItem;
begin
  Result := AItem;
  FItems.Add(AItem);
end;

{ TModifiedWord }

constructor TModifiedWord.Create(ADetectorClass: TDetectorClass; AOriginalWord: TWord; AModifiedWord: UTF8String);
begin
  inherited Create(ADetectorClass);
  Basis.Add(AOriginalWord);
  FModifiedWord := AModifiedWord;
end;

function TModifiedWord.ToString: UTF8String;
begin
  Result := FModifiedWord;
end;

{ TWordWasCapitalizedFact }

function TWordWasCapitalizedFact.TryMergeToBrain(ABrain: TBrain): Boolean;
begin
  Result := False; // always unique
end;

constructor TWordWasCapitalizedFact.Create(ADetectorClass: TDetectorClass; ACapitalizedWord, ALowercasedWord: TWord);
begin
  inherited Create(ADetectorClass);
  Basis.Add(ACapitalizedWord); // Basis[0]
  Basis.Add(ALowercasedWord);  // Basis[1]
end;

function TWordWasCapitalizedFact.ToString: UTF8String;
begin
  Result := 'link: ' + Basis[0].ToString + ' -> ' + Basis[1].ToString;
end;

function TWordWasCapitalizedFact.IsSameKnowledge(AOtherItem: TKnowledgeItem): Boolean;
begin
  Result := (AOtherItem is TWordWasCapitalizedFact)
        and (Basis[0] = AOtherItem.Basis[0]);

  // In case of TWord it's not possible to merge because TWord haven't duplicates!
  if Result then
    raise EWrongStateException.Create('TWordWasCapitalizedFact.IsSameKnowledge: ' + InfoText);
end;

// no need to multiply basis. But in case of TWord it's not possible to merge because TWord haven't duplicates!
//function TWordWasCapitalizedFact.Merge(AOtherItem: TKnowledgeItem): boolean;
//begin
//  Result := IsSameKnowledge(AOtherItem);
//  if Result then
//    AOtherItem.Free;
//end;

{ TWord }

function TWord.TryMergeToBrain(ABrain: TBrain): Boolean;
var
  FoundedWord: TWord;
begin
  Result := False;
  FoundedWord := FindWord(ABrain, ToString);
  if Assigned(FoundedWord) then
    begin
      FoundedWord.Merge(Self);
      Result := True;
    end;
end;


var
  //todo: IndexList, BrainList: TList;
  MainBrainWordIndex: TWordIndex = nil;

function GetWordIndexSubset(ABrain: TBrain): TWordIndex;
begin
  // todo: Result := IndexList[BrainList.IndexOf(ABrain)];
  if not Assigned(MainBrainWordIndex) then
    MainBrainWordIndex := TWordIndex.Create(ABrain);
  Result := MainBrainWordIndex;
end;

class function TWord.FindWord(ABrain: TBrain; AWord: UTF8String): TWord;
begin
  Result := GetWordIndexSubset(ABrain).FindWord(AWord);
end;

function TWord.ToString: UTF8String;
begin
  if FContentCashe = '' then
    begin
      if Basis.Count <= 0 then
        raise EWrongStateException.Create(InfoText);
      FContentCashe := (Basis[0] as TSourceItem).ToString;
    end;
  Result := FContentCashe;
end;

function TWord.IsSameKnowledge(AOtherItem: TKnowledgeItem): Boolean;
begin
  Result := (AOtherItem is TWord) and IsSameWord(TWord(AOtherItem).ToString);
end;

function TWord.IsSameWord(AWord: UTF8String): Boolean;
var
  OwnWord: UTF8String;
begin
  OwnWord := ToString;
  Result := (CompareStr(OwnWord, AWord) = 0);
end;

{ TKnowledgeItem1 }

function TKnowledgeItem1.GetBasisClass: TBasisClass;
begin
  Result := TBasis1;
end;

function TKnowledgeItem1.GetConsequencesClass: TTabledKnowledgeBaseSubsetClass;
begin
  Result := TTabledKnowledgeBaseSubset1;
end;

{ TWordDetector }

procedure TWordDetector.Evalute(AKnowledgeItem: TKnowledgeItem);
var
  NewWord: TWord;
begin
  if AKnowledgeItem is TSourceItem then
    begin
      NewWord := TWord.Create(SelfClass);
      NewWord.Basis.Add(AKnowledgeItem);
      NewWord.IntegrateToBrain(AKnowledgeItem.Owner);
    end;
end;

{ TSimpleTextFileSourceItem }

function TSimpleTextFileSourceItem.Source: TSimpleTextFileSource;
begin
  Result := Basis[0] as TSimpleTextFileSource;
end;

function TSimpleTextFileSourceItem.GetBasisClass: TBasisClass;
begin
  Result := TBasis1;
end;

function TSimpleTextFileSourceItem.GetConsequencesClass: TTabledKnowledgeBaseSubsetClass;
begin
  Result := TTabledKnowledgeBaseSubset1;
end;

function TSimpleTextFileSourceItem.TryMergeToBrain(ABrain: TBrain): Boolean;
begin
  Result := False; // always unique
end;

function TSimpleTextFileSourceItem.IsSameKnowledge(AOtherItem: TKnowledgeItem): Boolean;
begin
  Result := (AOtherItem is TSimpleTextFileSourceItem) and (TSimpleTextFileSourceItem(AOtherItem).FPosition = FPosition);
end;

constructor TSimpleTextFileSourceItem.Create(ADetectorClass: TDetectorClass; ASource: TSimpleTextFileSource; APosition: PUTF8Char; ASize: integer);
begin
  inherited Create(ADetectorClass);
  Basis.Add(ASource);
  FPosition := APosition;
  FSize     := ASize;
end;

function TSimpleTextFileSourceItem.InfoText: UTF8String;
var
  CodedS: UTF8String;
begin
  CodedS := ToString;
  Result :=
    Format(
      rsProofFromFileSDDS, [
        Source.FFileName,
        SizeInt(FPosition - @Source.FFileContent[1]),
        length(CodedS),
        CodedS
      ]
    );
end;

function TSimpleTextFileSourceItem.ToString: UTF8String;
var
  s: string;
begin
  s := copy(String(FPosition), 1, FSize);
  Result := UTF8String(@s[1]);
  if (Length(Result) = 1) and (ord(Result[1]) <= 32) then
    Result := '#' + IntToStr(ord(Result[1]));
end;

{ TSimpleTextFileSource }

function TSimpleTextFileSource.ToString: UTF8String;
begin
  Result := FFileContent;
end;

function TSimpleTextFileSource.InfoText: UTF8String;
begin
  Result := FFileName + #13#10
    + ' retain ' + IntToStr(FEndChar - FPosition) + ' bytes'
    + ' of ' + IntToStr(FEndChar - @FFileContent[1]) + ' bytes.'
    + ' ' + IntToStr(100 - Round(100 * (FEndChar - FPosition) / (FEndChar - @FFileContent[1]))) + '% done.'
end;

function TSimpleTextFileSource.IsSameKnowledge(AOtherItem: TKnowledgeItem): Boolean;
begin
  Result := (AOtherItem is TSimpleTextFileSource) and (TSimpleTextFileSource(AOtherItem).FFileName = FFileName);
end;

function TSimpleTextFileSource.GetBasisClass: TBasisClass;
begin
  Result := TBasis1;
end;

function TSimpleTextFileSource.GetConsequencesClass: TTabledKnowledgeBaseSubsetClass;
begin
  Result := TTabledKnowledgeBaseSubset1;
end;

constructor TSimpleTextFileSource.Create(ADetectorClass: TDetectorClass; AFileName: UTF8String);
var
  StringList: TStringList;
begin
  inherited Create(ADetectorClass);
  FFileName := AFileName;
  StringList := TStringList.Create;
    try
      StringList.LoadFromFile(UTF8ToSys(FFileName));
      FFileContent := EncodeFileStringListToContent(StringList) + #0;
    finally
      StringList.Free;
    end;
  FPosition := @FFileContent[1];
  FEndChar := @FFileContent[Length(FFileContent)];
end;

function TSimpleTextFileSource.EOF: boolean;
begin
  Result := (FPosition = FEndChar);
end;

function TSimpleTextFileSource.ReadNextItem(ADetectorClass: TDetectorClass): TSourceItem;
var
  EndPosition: PUTF8Char;
  NextChar: UTF8String;
  CharSize: SizeInt;
  RestSize: SizeInt;

  function UTF8CharSize(AChar: PUTF8Char): SizeUint;
  var
    LookAhead: SizeUInt;
    TempBYTE, IBYTE: byte;
  begin
    IBYTE := byte(AChar^);
    if (IBYTE and $80) = 0 then
      Result := 1
    else
      begin
        Result := 0;
        TempByte := IByte;
        while (TempBYTE and $80) <> 0 do
          begin
            TempBYTE:=(TempBYTE shl 1) and $FE;
            inc(Result);
          end;
        //Test for the "Result" conforms UTF-8 string
        //This means the 10xxxxxx pattern.
        if Result > 5 then
          begin
            //Insuficient chars in string to decode
            //UTF-8 array. Fallback to single char.
            Result := 1;
          end;
        for LookAhead := 1 to Result-1 do
          begin
            if ((byte((AChar+LookAhead)^) and $80)<>$80) or
               ((byte((AChar+LookAhead)^) and $40)<>$00) then
              begin
                //Invalid UTF-8 sequence, fallback.
                Result:= LookAhead;
                break;
              end;
          end;
      end;
  end;

const
  // TODO: letter / non letter by Unicode routines
  DelimiterChars: UTF8string =
    #1#2#3#4#5#6#7#8#9 +
    #10#11#12#13#14#15#16#17#18#19 +
    #20#21#22#23#24#25#26#27#28#29 +
    #30#31#32+
    '.,:;!?()[]{}<>=+-*/\|''"'+
    '„“«»—… ' (* $nbsp; *)
  ;
begin
  Assert(eof);

  RestSize := FEndChar - FPosition;

  CharSize := UTF8CharSize(FPosition);
  NextChar := Copy(UTF8String(FPosition^), 1, 1);
  // check if now there are a delimiter
  if Pos(NextChar, DelimiterChars) > 0 then
    begin
      Result := TSimpleTextFileSourceItem.Create(ADetectorClass, Self, FPosition, CharSize);
      Result.IntegrateToBrain(Owner);
      Inc(FPosition, CharSize);
      Exit;
    end;

  EndPosition := FPosition;
  // collect nondelimiters:
  repeat
    Inc(EndPosition, CharSize);
    RestSize := FEndChar - EndPosition;
    if RestSize <= 0 then
      Break;
    CharSize := UTF8CharSize(EndPosition);
    NextChar := Copy(UTF8String(EndPosition^), 1, 1);
  until (EndPosition >= FEndChar) or (Pos(NextChar, DelimiterChars) > 0);

  Result := TSimpleTextFileSourceItem.Create(ADetectorClass, Self, FPosition, EndPosition - FPosition);
  Result.IntegrateToBrain(Owner);

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

constructor TBrain1.Create;
begin
  inherited Create(nil);

  FDetectors := TObjectList.Create(True);
  FItems := TTabledKnowledgeBaseSubset1.Create(nil);

  FDetectors.Add(TSourceDetector.Create);
  FDetectors.Add(TWordDetector.Create);
  FDetectors.Add(TCapitalizedWordDetector.Create);
end;

destructor TBrain1.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FDetectors);
  inherited Destroy;
end;

function TBrain1.GetDetector(Index: integer): TDetector;
begin
  Result := TDetector(FDetectors[Index]);
end;

function TBrain1.DetectorCount: integer;
begin
  Result := FDetectors.Count;
end;

function TBrain1.GetItem(Index: integer): TKnowledgeItem;
begin
  Result := FItems[Index];
end;

function TBrain1.Count: integer;
begin
  Result := FItems.Count;
end;

function TBrain1.InternalAdd(AItem: TKnowledgeItem): TKnowledgeItem;
begin
  Result := FItems.Add(AItem);
end;

end.

