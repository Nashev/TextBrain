unit Engine1Unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  TKnowledgeBaseSubset = class;
  TTabledKnowledgeBaseSubset = class;
  TTabledKnowledgeBaseSubsetClass = class of TTabledKnowledgeBaseSubset;
  TKnowledgeItem = class;
  TBrain = class;
  TDetector = class;
  TDetectorClass = class of TDetector;

  { TKnowledgeItem }

  TKnowledgeItem = class
  private
    FDetectorClass: TDetectorClass;
    FBasis: TTabledKnowledgeBaseSubset;
    FOwner: TBrain;
  protected
//    function GetLinkedKnowledge(Index: Integer): TKnowledgeItem; virtual; abstract;
    function GetBasisClass: TTabledKnowledgeBaseSubsetClass; virtual; abstract;
  public
    constructor Create(ADetectorClass: TDetectorClass);
    procedure IntegrateToBrain(ABrain: TBrain); // need be called when this item will be ready for merge.

    property Owner: TBrain read FOwner;
    //function LinkCount: Integer; virtual; abstract;
    //property LinkedKnowledge[Index: Integer]: TKnowledgeItem read GetLinkedKnowledge;

    property DetectorClass: TDetectorClass read FDetectorClass;
    property Basis: TTabledKnowledgeBaseSubset read FBasis;

    function ToString: UTF8String; virtual; abstract; reintroduce;
    function InfoText: UTF8String; virtual;

    function IsSameKnowledge(AOtherItem: TKnowledgeItem): Boolean; virtual; abstract;
    function Merge(AOtherItem: TKnowledgeItem): Boolean;
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
  public
    constructor Create(ASuperset: TKnowledgeBaseSubset); virtual;
    destructor Destroy; override;

    // subset content management
    function GetIterator: TKnowledgeIterator; virtual; abstract;

    // subset hierarchy
    property Superset: TKnowledgeBaseSubset read FSuperset;
    procedure RegisterSubset(ASubset: TKnowledgeBaseSubset);
    procedure UnRegisterSubset(ASubset: TKnowledgeBaseSubset);
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
  public
    function GetIterator: TKnowledgeIterator; override;
    function Count: Integer; virtual; abstract;
    property Items[Index: Integer]: TKnowledgeItem read GetItem; default;
    function Add(AItem: TKnowledgeItem): TKnowledgeItem; virtual; abstract;
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
    function InternalAdd(AItem: TKnowledgeItem): TKnowledgeItem; virtual; abstract;
  end;


implementation


resourcestring
  rsKnowledgeBasisInfo = ' (%d basis)';

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
  for i := 0 to Count - 1 do
    if Items[i].Merge(AItem) then
      begin
        Result := Items[i]; // TODO: detect anything om merge
        Exit;
      end;

  Result := InternalAdd(AItem);

  for i := 0 to DetectorCount - 1 do
    Detectors[i].Evalute(Result);
end;

{ TKnowledgeBaseSubset }

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
  FDetectorClass := ADetectorClass;
  FBasis := GetBasisClass.Create(nil); // TODO: Is we need a superset for basis?
end;

procedure TKnowledgeItem.IntegrateToBrain(ABrain: TBrain);
begin
  FOwner := ABrain;
  ABrain.Add(Self);
  //FBasis.Superset := ABrain; TODO: Is we need a superset for basis?
end;

function TKnowledgeItem.InfoText: UTF8String;
begin
  Result := ToString + Format(rsKnowledgeBasisInfo, [Basis.Count]);
end;

function TKnowledgeItem.Merge(AOtherItem: TKnowledgeItem): Boolean;
var
  i: Integer;
begin
  Result := IsSameKnowledge(AOtherItem);
  if Result then
    begin
      for i := 0 to AOtherItem.Basis.Count - 1 do
        Basis.Add(AOtherItem.Basis[i]);
      AOtherItem.Free;
    end;
end;

end.

