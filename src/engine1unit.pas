unit Engine1Unit;
/// <summary>First try to make some test brain engine</summary>

{$IFDEF  FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Types;

type
{$IFNDEF FPC}
  SizeInt = Integer;
  SizeUint = Word;
  UTF8String = string;
{$ELSE}
  TFileName = UTF8String;
{$ENDIF}

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
  /// <summary>
  /// Is a base class for all knowledge items in a text brain
  /// </summary>
  /// <remarks>
  /// It have link to they Brain, list of consequences and a basis subset of knowledge.
  /// It have a text representation and text with some additional info.
  /// </remarks>
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
    function IsSameKnowledge(AItemToMerge: TKnowledgeItem): Boolean; virtual; abstract;
    procedure Merge(AItemToMerge: TKnowledgeItem); virtual;
  public
    constructor Create(ADetector: TDetector);
    destructor Destroy; override;

    /// <remarks>Note: This method need be called when this item will be initialised and ready for merge.</remarks>
    procedure IntegrateToBrain(ABrain: TBrain);

    property Owner: TBrain read FOwner;

    //function LinkCount: Integer; virtual; abstract;
    //property LinkedKnowledge[Index: Integer]: TKnowledgeItem read GetLinkedKnowledge;
    //property DetectorClass: TDetectorClass read FDetectorClass;

    /// <summary>Basis is a set of knowledge items, which are was a sources to make this new knowledge item</summary>
    property Basis: TBasis read FBasis;
    /// <summary>Consequences is a list of produced knowledge items, which are have this knowledge item as a basis</summary>
    /// <remarks>This is an opposite side of Basis: filled only when this item added to some body basis</remarks>
    property Consequences: TTabledKnowledgeBaseSubset read FConsequences;

    /// <summary>String representation of this item itself</summary>
    function ToString: UTF8String; {$IFNDEF FPC} reintroduce;{$ENDIF} virtual; abstract; {$IFDEF FPC} reintroduce;{$ENDIF}
    /// <summary>Full generated text description of this knowledge item. Usually include string representation and some specific aditional info</summary>
    function InfoText: UTF8String; virtual;

    procedure Changed; virtual;
  end;

  TSourceItem = class;

  /// <summary>
  /// This item of knowledge is a source text, loaded from a file, for example.
  /// It can provide next SourceItems while not EOF (end of file)
  /// </summary>
  TSource = class(TKnowledgeItem)
  public
    function EOF: Boolean; virtual; abstract;
    function ReadNextItem(ADetector: TDetector): TSourceItem; virtual; abstract;
  end;

  /// <summary>
  /// TSourceItem knows, when in source starts and finishs some item...
  /// </summary>
  TSourceItem = class(TKnowledgeItem)
  public
    function GetItemStart: Integer; virtual; abstract;
    function GetItemLength: Integer; virtual; abstract;
  end;

  { TSequenceInfo }

  /// <summary>
  ///   It is knowledge about order relation, when one knowledge item IS AFTER another, previous one.
  /// </summary>
  TSequenceInfo = class(TKnowledgeItem)
  private
    FPreviousItem: TKnowledgeItem;
  public
    property PreviousItem: TKnowledgeItem read FPreviousItem;
    constructor Create(APreviousItem: TKnowledgeItem);
  end;

  /// <summary>
  ///   service class to iterate knowledge items
  /// </summary>
  TKnowledgeIterator = class
  private
    FKnowledgeBaseSubset: TKnowledgeBaseSubset;
  protected
    property KnowledgeBaseSubset: TKnowledgeBaseSubset read FKnowledgeBaseSubset;
  public
    constructor Create(AKnowledgeBaseSubset: TKnowledgeBaseSubset); virtual;
    function EOF: Boolean; virtual; abstract;
    procedure First; virtual; abstract;
    procedure Next; virtual; abstract;
    function CurrentItem: TKnowledgeItem; virtual; abstract;
  end;

  { TEmptyIterator }
  TEmptyIterator = class(TKnowledgeIterator)
    constructor Create; reintroduce;
    function EOF: Boolean; override;
    procedure First; override;
    procedure Next; override;
    function CurrentItem: TKnowledgeItem; override;
  end;

  { TKnowledgeBaseSubset }
  /// <summary>
  ///   Base subset of knowledge items with support to organize tree of subsets:
  ///   it have a link to superset and list of subsets
  /// </summary>
  /// <remarks>
  ///   It provide a possibility to get Iterator to theirs own contents
  ///   and a notifications of content's changing to all their's subsets
  /// </remarks>
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
    /// <remarks>Not used yet</remarks>
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
  /// <summary>
  ///   Allows to iterate all knowledge items and return only that, which pass throught IsKnowledgeInSubset function.
  /// </summary>
  TFunctionalKnowledgeBaseSubset = class(TKnowledgeBaseSubset)
  protected
  public
    function GetIterator: TKnowledgeIterator; override;
    function IsKnowledgeInSubset(AKnowledgeItem: TKnowledgeItem): Boolean; virtual; abstract;
  end;

  /// <summary>
  /// Returns items, which pass throught TFunctionalKnowledgeBaseSubset.IsKnowledgeInSubset function.
  /// </summary>
  TFunctionalKnowledgeIterator = class(TKnowledgeIterator)
  private
    FSupersetIterator: TKnowledgeIterator;
    procedure FindNext;
  public
    constructor Create(AKnowledgeBaseSubset: TFunctionalKnowledgeBaseSubset); reintroduce;
    function EOF: Boolean; override;
    procedure First; override;
    procedure Next; override;
    function CurrentItem: TKnowledgeItem; override;
  end;

  { TTabledKnowledgeBaseSubset }

  /// <summary>
  ///   Is an abstract collection of a knowledge items.
  ///   Has Items property and direct iterator over this Items.
  /// </summary>
  /// <remarks>
  ///   Descendants can implement different way to store all contained TKnowledgeItem instances.
  ///   As an array, or as a memory stream, or a queue, or a TCollection, etc
  /// </remarks>
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
    function EOF: Boolean; override;
    procedure First; override;
    procedure Next; override;
    function CurrentItem: TKnowledgeItem; override;
  end;


  { TBasis }

  /// <summary>TBasis is a TTabledKnowledgeBaseSubset with Owner knowledge item, made by Detector from set of other items. It adds the Owner to basis item's Consequences lists</summary>
  TBasis = class(TTabledKnowledgeBaseSubset)
  private
    FOwner: TKnowledgeItem;
    /// <stereotype>instantiate</stereotype>
    FDetector: TDetector;
  public
    constructor Create(AOwner: TKnowledgeItem; ADetector: TDetector); {$IFNDEF FPC} reintroduce;{$ENDIF}virtual; {$IFDEF FPC} reintroduce;{$ENDIF}
    /// <summary>Owner of the Basis. It is a Knowledge Item, made by the Detector from Basis Items.</summary>
    property Owner: TKnowledgeItem read FOwner;
    /// <summary>Detector, used to make Owner for this Basis from the Items in this subset</summary>
    property Detector: TDetector read FDetector;
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
  /// <summary>Abstract class of TKnowledgeItem's owner, that is a
  /// 1) holder of all knowledges (TTabledKnowledgeBaseSubset) and
  /// 2) holder of all knowledge detectors</summary>
  TBrain = class(TTabledKnowledgeBaseSubset)
  private
  protected
    function GetDetector(Index: Integer): TDetector; virtual; abstract;
  public
    function DetectorCount: Integer; virtual; abstract;
    /// <link>aggregationByValue</link>
    /// <stereotype>include</stereotype>
    property Detectors[Index: Integer]: TDetector read GetDetector;
    function Add(AItem: TKnowledgeItem): TKnowledgeItem; override;
  end;


implementation

{ TEmptyIterator }

constructor TEmptyIterator.Create;
begin
  // do nothing
end;

function TEmptyIterator.EOF: Boolean;
begin
  Result := True;
end;

procedure TEmptyIterator.First;
begin
  // do nothing
end;

procedure TEmptyIterator.Next;
begin
  // do nothing
end;

function TEmptyIterator.CurrentItem: TKnowledgeItem;
begin
  Assert(False, 'TEmptyIterator.CurrentItem called!');
  Result := nil;
end;

{ TBasis }

constructor TBasis.Create(AOwner: TKnowledgeItem; ADetector: TDetector);
begin
  inherited Create(nil);
  FOwner := AOwner;
  FDetector := ADetector;
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
          Result := Result + '  ' + CurrentItem.InfoText + #13#10;
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
      TSource(AKnowledgeItem).ReadNextItem(Self);
end;

{ TTabledKnowledgeIterator }

constructor TTabledKnowledgeIterator.Create(AKnowledgeBaseSubset: TTabledKnowledgeBaseSubset);
begin
  inherited Create(AKnowledgeBaseSubset);
end;

function TTabledKnowledgeIterator.EOF: Boolean;
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
  Result := inherited InfoText + Format(rsTabledKnowledgeBaseSubsetInfoText, [Count]);
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

function TFunctionalKnowledgeIterator.EOF: Boolean;
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

constructor TKnowledgeItem.Create(ADetector: TDetector);
begin
  inherited Create;
  FBasis := GetBasisClass.Create(Self, ADetector);
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
  if not TryMergeToBrain(ABrain) then // if not merged to any existing element, add it as new
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
    if IsSameKnowledge(ABrain.Items[i]) then // merging with same knowleddge.
      begin
        ABrain.Items[i].Merge(Self);  // TODO: detect anything om merge
        Result := True;
        Exit;
      end;
end;

resourcestring
  rsKnowledgeInfo = '%s: "%s" (%d basis, %d consequences)';

function TKnowledgeItem.InfoText: UTF8String;
begin
  Result := Format(rsKnowledgeInfo, [ClassName, ToString, Basis.Count, Consequences.Count]);
end;

procedure TKnowledgeItem.Merge(AItemToMerge: TKnowledgeItem);
var
  i: Integer;
begin
  for i := 0 to AItemToMerge.Basis.Count - 1 do
    Basis.Add(AItemToMerge.Basis[i]); // items of merged item will be added to self basis
  AItemToMerge.Free;
end;

procedure TKnowledgeItem.Changed;
begin
  if Assigned(FOwner) then
    FOwner.SupersetItemAdded(Self);
end;

end.
