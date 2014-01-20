unit Engine1Unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  TKnowledgeBaseSubset = class;
  //TKnowledgeBaseSubsetClass = class of TKnowledgeBaseSubset;
  TTabledKnowledgeBaseSubset = class;
  TTabledKnowledgeBaseSubsetClass = class of TTabledKnowledgeBaseSubset;
  TBasis = class;
  TBasisClass = class of TBasis;
  TKnowledgeItem = class;
  TBrain = class;
  TDetector = class;
  TDetectorClass = class of TDetector;

  { TKnowledgeItem }

  TKnowledgeItem = class
  private
    //FDetectorClass: TDetectorClass;
    FBasis: TBasis;
    FConsequences: TTabledKnowledgeBaseSubset;
    FOwner: TBrain;
  protected
//    function GetLinkedKnowledge(Index: Integer): TKnowledgeItem; virtual; abstract;
    function GetBasisClass: TBasisClass; virtual; abstract;
    function GetConsequencesClass: TTabledKnowledgeBaseSubsetClass; virtual; abstract;
    function TryMergeToBrain(ABrain: TBrain): Boolean; virtual;
    function IsSameKnowledge(AOtherItem: TKnowledgeItem): Boolean; virtual; abstract;
    procedure Merge(AOtherItem: TKnowledgeItem); virtual;
  public
    constructor Create(ADetectorClass: TDetectorClass);
    destructor Destroy; override;
    procedure IntegrateToBrain(ABrain: TBrain); // need be called when this item will be ready for merge.

    property Owner: TBrain read FOwner;
    //function LinkCount: Integer; virtual; abstract;
    //property LinkedKnowledge[Index: Integer]: TKnowledgeItem read GetLinkedKnowledge;

    //property DetectorClass: TDetectorClass read FDetectorClass;

    property Basis: TBasis read FBasis;
    property Consequences: TTabledKnowledgeBaseSubset read FConsequences; // opposite side of Basis: filled only when item added to some basis

    function ToString: UTF8String; virtual; abstract; reintroduce;
    function InfoText: UTF8String; virtual;

    procedure Changed; virtual;
  end;

  TSourceItem = class;

  TSource = class(TKnowledgeItem)
  public
    function ToString: UTF8String; virtual; abstract; reintroduce;
    function EOF: Boolean;virtual; abstract;
    function ReadNextItem(ADetectorClass: TDetectorClass): TSourceItem; virtual; abstract;
  end;

  TSourceItem = class(TKnowledgeItem)
  end;

  { TSequenceInfo }

  TSequenceInfo = class(TKnowledgeItem)
  private
    FPreviousItem: TKnowledgeItem;
  public
    property PreviousItem: TKnowledgeItem read FPreviousItem;
    constructor Create(APreviousItem: TKnowledgeItem);
  end;

  { TKnowledgeIterator }

  TKnowledgeIterator = class
  private
    FKnowledgeBaseSubset: TKnowledgeBaseSubset;
  protected
    property KnowledgeBaseSubset: TKnowledgeBaseSubset read FKnowledgeBaseSubset;
  public
    constructor Create(AKnowledgeBaseSubset: TKnowledgeBaseSubset); virtual;
    function EOF: boolean; virtual; abstract;
    procedure First; virtual; abstract;
    procedure Next; virtual; abstract;
    function CurrentItem: TKnowledgeItem; virtual; abstract;
  end;

  { TKnowledgeBaseSubset }

  TKnowledgeBaseSubset = class
  private
    FSubsets: TList;
    FSuperset: TKnowledgeBaseSubset;
    function GetSubset(Index: Integer): TKnowledgeBaseSubset;
    function GetSubsetsCount: Integer;
  public
    constructor Create(ASuperset: TKnowledgeBaseSubset); virtual;
    destructor Destroy; override;

    // subset content management
    function GetIterator: TKnowledgeIterator; virtual; abstract;
    function InfoText: UTF8String; virtual;
    function ContentText: UTF8String; virtual;

    // subset hierarchy
    property Superset: TKnowledgeBaseSubset read FSuperset;
    procedure RegisterSubset(ASubset: TKnowledgeBaseSubset);
    procedure UnRegisterSubset(ASubset: TKnowledgeBaseSubset);
    property SubsetsCount: Integer read GetSubsetsCount;
    property Subset[Index: Integer]: TKnowledgeBaseSubset read GetSubset;
    procedure SupersetItemAdded(AItem: TKnowledgeItem); virtual;
    procedure SupersetItemChanged(AItem: TKnowledgeItem); virtual;
  end;

  { TFunctionalKnowledgeBaseSubset }

  TFunctionalKnowledgeBaseSubset = class(TKnowledgeBaseSubset)
  protected
  public
    function GetIterator: TKnowledgeIterator; override;
    function IsKnowledgeInSubset(AKnowledgeItem: TKnowledgeItem): Boolean; virtual; abstract;
  end;

  TFunctionalKnowledgeIterator = class(TKnowledgeIterator)
  private
    FSupersetIterator: TKnowledgeIterator;
    procedure FindNext;
  public
    constructor Create(AKnowledgeBaseSubset: TFunctionalKnowledgeBaseSubset); reintroduce;
    function EOF: boolean; override;
    procedure First; override;
    procedure Next; override;
    function CurrentItem: TKnowledgeItem; override;
  end;

  { TTabledKnowledgeBaseSubset }

  TTabledKnowledgeBaseSubset = class(TKnowledgeBaseSubset)
  protected
    function GetItem(Index: Integer): TKnowledgeItem; virtual; abstract;
    function InternalAdd(AItem: TKnowledgeItem): TKnowledgeItem; virtual; abstract;
  public
    function GetIterator: TKnowledgeIterator; override;
    function Count: Integer; virtual; abstract;
    property Items[Index: Integer]: TKnowledgeItem read GetItem; default;
    function Add(AItem: TKnowledgeItem): TKnowledgeItem; virtual;
    function InfoText: UTF8String; override;
  end;

  { TTabledKnowledgeIterator }

  TTabledKnowledgeIterator = class(TKnowledgeIterator)
  private
    Index: Integer;
  public
    constructor Create(AKnowledgeBaseSubset: TTabledKnowledgeBaseSubset); reintroduce;
    function EOF: boolean; override;
    procedure First; override;
    procedure Next; override;
    function CurrentItem: TKnowledgeItem; override;
  end;


  { TBasis }

  TBasis = class(TTabledKnowledgeBaseSubset)
    FOwner: TKnowledgeItem;
  public
    constructor Create(AOwner: TKnowledgeItem); virtual; reintroduce;
    property Owner: TKnowledgeItem read FOwner;
    function Add(AItem: TKnowledgeItem): TKnowledgeItem; override;
  end;

  { TDetector }

  TDetector = class
  public
    procedure Evalute(AKnowledgeItem: TKnowledgeItem); virtual; abstract;
    function SelfClass: TDetectorClass;
  end;

  { TSourceDetector }

  TSourceDetector = class(TDetector)
  public
    procedure Evalute(AKnowledgeItem: TKnowledgeItem); override;
  end;

  { TBrain }

  TBrain = class(TTabledKnowledgeBaseSubset)
  private
  protected
    function GetDetector(Index: Integer): TDetector; virtual; abstract;
  public
    function DetectorCount: Integer; virtual; abstract;
    property Detectors[Index: Integer]: TDetector read GetDetector;
    function Add(AItem: TKnowledgeItem): TKnowledgeItem; override;
  end;


implementation

{ TBasis }

constructor TBasis.Create(AOwner: TKnowledgeItem);
begin
  inherited Create(nil);
  FOwner := AOwner;
end;

function TBasis.Add(AItem: TKnowledgeItem): TKnowledgeItem;
begin
  Result := inherited Add(AItem);
  AItem.Consequences.Add(Owner);
end;

{ TDetector }

function TDetector.SelfClass: TDetectorClass;
begin
  Result := TDetectorClass(ClassType);
end;

{ TBrain }

function TBrain.Add(AItem: TKnowledgeItem): TKnowledgeItem;
var
  i: Integer;
begin
  Result := InternalAdd(AItem);
  SupersetItemAdded(Result);

  for i := 0 to DetectorCount - 1 do
    Detectors[i].Evalute(Result);
end;

{ TKnowledgeBaseSubset }

function TKnowledgeBaseSubset.GetSubset(Index: Integer): TKnowledgeBaseSubset;
begin
  Result := TKnowledgeBaseSubset(FSubsets[Index]);
end;

function TKnowledgeBaseSubset.GetSubsetsCount: Integer;
begin
  if Assigned(FSubsets) then
    Result := FSubsets.Count
  else
    Result := 0;
end;

constructor TKnowledgeBaseSubset.Create(ASuperset: TKnowledgeBaseSubset);
begin
  inherited Create;
  FSuperset := ASuperset;
  if Assigned(FSuperset) then
    FSuperset.RegisterSubset(Self);
end;

destructor TKnowledgeBaseSubset.Destroy;
begin
  if Assigned(FSuperset) then
    FSuperset.UnRegisterSubset(Self);
  inherited Destroy;
end;

function TKnowledgeBaseSubset.InfoText: UTF8String;
begin
  Result := ClassName;
end;

function TKnowledgeBaseSubset.ContentText: UTF8String;
begin
  Result := InfoText + #13#10;
  with GetIterator do
    try
      while not EOF do
        begin
          Result := Result + '  ' + CurrentItem.InfoText;
          Next;
        end;
    finally
      Free;
    end;
end;

procedure TKnowledgeBaseSubset.RegisterSubset(ASubset: TKnowledgeBaseSubset);
begin
  if not Assigned(FSubsets) then
    FSubsets := TList.Create;
  FSubsets.Add(ASubset);
end;

procedure TKnowledgeBaseSubset.UnRegisterSubset(ASubset: TKnowledgeBaseSubset);
begin
  FSubsets.Remove(ASubset);
end;

procedure TKnowledgeBaseSubset.SupersetItemAdded(AItem: TKnowledgeItem);
var
  i: Integer;
begin
  // default forwarding
  if Assigned(FSubsets) then
    for i := 0 to FSubsets.Count - 1 do
      TKnowledgeBaseSubset(FSubsets[i]).SupersetItemAdded(AItem);
end;

procedure TKnowledgeBaseSubset.SupersetItemChanged(AItem: TKnowledgeItem);
var
  i: Integer;
begin
  // default forwarding
  if Assigned(FSubsets) then
    for i := 0 to FSubsets.Count - 1 do
      TKnowledgeBaseSubset(FSubsets[i]).SupersetItemChanged(AItem);
end;

{ TSourceDetector }

procedure TSourceDetector.Evalute(AKnowledgeItem: TKnowledgeItem);
begin
  if AKnowledgeItem is TSource then
    while not TSource(AKnowledgeItem).EOF do
      TSource(AKnowledgeItem).ReadNextItem(SelfClass);
end;

{ TTabledKnowledgeIterator }

constructor TTabledKnowledgeIterator.Create(AKnowledgeBaseSubset: TTabledKnowledgeBaseSubset);
begin
  inherited Create(AKnowledgeBaseSubset);
end;

function TTabledKnowledgeIterator.EOF: boolean;
begin
  Result := (Index = TTabledKnowledgeBaseSubset(KnowledgeBaseSubset).Count);
end;

procedure TTabledKnowledgeIterator.First;
begin
  Index := 0;
end;

procedure TTabledKnowledgeIterator.Next;
begin
  inc(Index);
end;

function TTabledKnowledgeIterator.CurrentItem: TKnowledgeItem;
begin
  Result := TTabledKnowledgeBaseSubset(KnowledgeBaseSubset)[Index];
end;

{ TTabledKnowledgeBaseSubset }

function TTabledKnowledgeBaseSubset.GetIterator: TKnowledgeIterator;
begin
  Result := TTabledKnowledgeIterator.Create(Self);
end;

function TTabledKnowledgeBaseSubset.Add(AItem: TKnowledgeItem): TKnowledgeItem;
begin
  Result := InternalAdd(AItem);
  SupersetItemAdded(Result);
end;

resourcestring
  rsTabledKnowledgeBaseSubsetInfoText = ', Count: %d';

function TTabledKnowledgeBaseSubset.InfoText: UTF8String;
begin
  Result := Inherited InfoText + format(rsTabledKnowledgeBaseSubsetInfoText, [Count]);
end;

{ TFunctionalKnowledgeBaseSubset }

function TFunctionalKnowledgeBaseSubset.GetIterator: TKnowledgeIterator;
begin
  Result := TFunctionalKnowledgeIterator.Create(Self);
end;

{ TFunctionalKnowledgeIterator }

constructor TFunctionalKnowledgeIterator.Create(AKnowledgeBaseSubset: TFunctionalKnowledgeBaseSubset);
begin
  inherited Create(AKnowledgeBaseSubset);
  FSupersetIterator := KnowledgeBaseSubset.GetIterator;
end;

procedure TFunctionalKnowledgeIterator.FindNext;
begin
  while not FSupersetIterator.EOF do
    begin
      FSupersetIterator.Next;
      if TFunctionalKnowledgeBaseSubset(KnowledgeBaseSubset).IsKnowledgeInSubset(FSupersetIterator.CurrentItem) then
        Exit;
    end;
end;

function TFunctionalKnowledgeIterator.EOF: boolean;
begin
  Result := FSupersetIterator.EOF;
end;

procedure TFunctionalKnowledgeIterator.First;
begin
  FSupersetIterator.First;
  FindNext;
end;

procedure TFunctionalKnowledgeIterator.Next;
begin
  Assert(EOF, '{ABFF5494-6642-46AE-A6E4-90BEB348CDF4}');
  FindNext;  // TODO: check eof effect (need here double next to detect eof?)
end;

function TFunctionalKnowledgeIterator.CurrentItem: TKnowledgeItem;
begin
  Assert(EOF, '{ABFF5494-6642-46AE-A6E4-90BEB348CDF4}');
  Result := FSupersetIterator.CurrentItem;
end;

{ TKnowledgeIterator }

constructor TKnowledgeIterator.Create(AKnowledgeBaseSubset: TKnowledgeBaseSubset);
begin
  FKnowledgeBaseSubset := AKnowledgeBaseSubset;
end;

{ TSequenceInfo }

constructor TSequenceInfo.Create(APreviousItem: TKnowledgeItem);
begin
  FPreviousItem := APreviousItem;
end;

{ TKnowledgeItem }

constructor TKnowledgeItem.Create(ADetectorClass: TDetectorClass);
begin
  inherited Create;
  //FDetectorClass := ADetectorClass;
  FBasis := GetBasisClass.Create(Self);
  FConsequences := GetConsequencesClass.Create(nil);
end;

destructor TKnowledgeItem.Destroy;
begin
  FreeAndNil(FBasis);
  FreeAndNil(FConsequences);
end;

procedure TKnowledgeItem.IntegrateToBrain(ABrain: TBrain);
begin
  FOwner := ABrain;
  if not TryMergeToBrain(ABrain) then
    begin
      ABrain.Add(Self);
      //FBasis.Superset := ABrain; TODO: Is we need a superset for basis?
    end;
end;

function TKnowledgeItem.TryMergeToBrain(ABrain: TBrain): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to ABrain.Count - 1 do
    if IsSameKnowledge(ABrain.Items[i]) then
      begin
        ABrain.Items[i].Merge(Self);  // TODO: detect anything om merge
        Result := True;
        Exit;
      end;
end;

resourcestring
  rsKnowledgeInfo = ' (%d basis, %d consequences)';

function TKnowledgeItem.InfoText: UTF8String;
begin
  Result := ToString + Format(rsKnowledgeInfo, [Basis.Count, Consequences.Count]);
end;

procedure TKnowledgeItem.Merge(AOtherItem: TKnowledgeItem);
var
  i: Integer;
begin
  for i := 0 to AOtherItem.Basis.Count - 1 do
    Basis.Add(AOtherItem.Basis[i]);
  AOtherItem.Free;
end;

procedure TKnowledgeItem.Changed;
begin
  if Assigned(FOwner) then
    FOwner.SupersetItemAdded(Self);
end;

end.

