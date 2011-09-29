unit Engine1Unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  TKnowledgeBaseSubset = class;
  TKnowledgeBaseSubsetClass = class of TKnowledgeBaseSubset;
  TKnowledgeItem = class;
  TDetector = class;
  TDetectorClass = class of TDetector;

  TKnowledgeItem = class
  private
    FBasis: TKnowledgeBaseSubset;
    FOwner: TKnowledgeBaseSubset;
  protected
    function GetProof(Index: Integer): TKnowledgeItem; virtual; abstract;
    function GetLinkedKnowledge(Index: Integer): TKnowledgeItem; virtual; abstract;
    function GetDetectorClass: TDetectorClass; virtual; abstract;
    function GetBasisClass: TKnowledgeBaseSubsetClass; virtual; abstract;
  public
    property Owner: TKnowledgeBaseSubset read FOwner;
    function LinkCount: Integer; virtual; abstract;
    property LinkedKnowledge[Index: Integer]: TKnowledgeItem read GetLinkedKnowledge;

    property DetectorClass: TDetectorClass read GetDetectorClass;
    property Basis: TKnowledgeBaseSubset read FBasis;

    function ToString: UTF8String; virtual; abstract; reintroduce;
    function InfoText: UTF8String; virtual;

    function IsSameKnowledge(AOtherItem: TKnowledgeItem): Boolean; virtual; abstract;
    function Merge(AOtherItem: TKnowledgeItem): Boolean; virtual; abstract;
  end;

  TSourceItem = class;

  TSource = class(TKnowledgeItem)
  public
    function ToString: UTF8String; virtual; abstract; reintroduce;
    function eof: Boolean;virtual; abstract;
    function ReadNextItem: TSourceItem; virtual; abstract;
  end;

  TSourceItem = class(TKnowledgeItem)
  end;

  { TSequenceInfo }

  TSequenceInfo = class(TKnowledgeItem)
  private
    FPrevious: TKnowledgeItem;
  public
    property PreviousItem: TKnowledgeItem read FPrevious;
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

  TKnowledgeBaseSubset = class
  protected
    function GetSuperset: TKnowledgeBaseSubset; virtual; abstract;
  public
    // subset content management
    function GetIterator: TKnowledgeIterator; virtual; abstract;

    // subset hierarchy
    property Superset: TKnowledgeBaseSubset read GetSuperset;
    procedure RegisterSubset(ASubset: TKnowledgeBaseSubset);
    procedure UnRegisterSubset(ASubset: TKnowledgeBaseSubset);
    procedure SupersetItemAdded(AItem: TKnowledgeItem);
    procedure SupersetItemChanged(AItem: TKnowledgeItem);
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

  TDetector = class
  public
    procedure Evalute(AKnowledgeBaseSubset: TKnowledgeBaseSubset; AKnowledgeItem: TKnowledgeItem); virtual; abstract;
  end;

  { TSourceDetector }

  TSourceDetector = class
  public
    procedure Evalute(AKnowledgeBaseSubset: TKnowledgeBaseSubset; AKnowledgeItem: TKnowledgeItem); override;
  end;

  TBrain = class(TKnowledgeBaseSubset)
  private
  protected
    function GetDetector(Index: Integer): TDetector; virtual; abstract;
    function GetSuperset: TKnowledgeBaseSubset; override;
  public
    function DetectorCount: Integer; virtual; abstract;
    property Detectors[Index: Integer]: TDetector read GetDetector;
  end;


implementation


resourcestring
  rsKnowledgeBasisInfo = ' (%d basis)';

{ TSourceDetector }

procedure TSourceDetector.Evalute(AKnowledgeBaseSubset: TKnowledgeBaseSubset;
  AKnowledgeItem: TKnowledgeItem);
begin
  inherited Evalute(AKnowledgeBaseSubset, AKnowledgeItem);
  if AKnowledgeItem is TSource then
    while not TSource(AKnowledgeItem).eof do
      AKnowledgeBaseSubset.Add(); тут типа надо Brain найти?
end;

{ TTabledKnowledgeIterator }

constructor TTabledKnowledgeIterator.Create(
  AKnowledgeBaseSubset: TTabledKnowledgeBaseSubset);
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

constructor TFunctionalKnowledgeIterator.Create(
  AKnowledgeBaseSubset: TTFunctionalKnowledgeBaseSubset);
begin
  inherited Create(AKnowledgeBaseSubset);
  FSupersetIterator := KnowledgeBaseSubset.GetIterator;
end;

procedure TFunctionalKnowledgeIterator.FindNext;
begin
  while not FSupersetIterator.eof do
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
  FindNext;
end;

function TFunctionalKnowledgeIterator.CurrentItem: TKnowledgeItem;
begin
  Assert(EOF, '{ABFF5494-6642-46AE-A6E4-90BEB348CDF4}');
  Result := FSupersetIterator.CurrentItem;
end;

{ TKnowledgeIterator }

constructor TKnowledgeIterator.Create(AKnowledgeBaseSubset: TTKnowledgeBaseSubset);
begin
  FKnowledgeBaseSubset := AKnowledgeBaseSubset;
end;

{ TSequenceInfo }

constructor TSequenceInfo.Create(APreviousItem: TKnowledgeItem);
begin
  FPreviousItem := APreviousItem;
end;

{ TBrain }

function TBrain.GetSource(Index: Integer): TSource;
begin
  Result := nil;
end;

function TBrain.GetSuperset: TKnowledgeBaseSubset;
begin
  Result := inherited GetSuperset;
end;

{ TKnowledgeItem }

procedure TKnowledgeItem.SetBasis(AValue: TKnowledgeBaseSubset);
begin
  if FBasis = AValue then
     Exit;
  FBasis := AValue;
end;

function TKnowledgeItem.InfoText: UTF8String;
begin
  Result := ToString + Format(rsKnowledgeBasisInfo, [ProofCount]);
end;

end.

