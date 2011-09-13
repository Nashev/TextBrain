unit Engine1Unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  TSourceInfo = class;

  TSourceItem = class
    function ToString: string; virtual; abstract;
    function GetSourceInfo: TSourceInfo; virtual; abstract;
  end;

  TSource = class
  public
    function ToString: string; virtual; abstract;
    function eof: Boolean;virtual; abstract;
    function GetNextItem: TSourceItem; virtual; abstract;
  end;

  TSourceInfo = class
  protected
    function GetSource: TSource; virtual; abstract;
  public
    function ToString: string; virtual; abstract;
    property Source: TSource read GetSource;
  end;

  TKnowledgeItem = class
  protected
    function GetProof(Index: Integer): TSourceInfo; virtual; abstract;
  public
    function ToString: string; virtual; abstract;
    function ProofCount: Integer; virtual; abstract;
    procedure AddProof(AProof: TSourceInfo); virtual; abstract;
    property Proof[Index: Integer]: TSourceInfo read GetProof;
    function Merge(AOtherItem: TKnowledgeItem): Boolean; virtual; abstract;
  end;

  TKnowledgeBaseSubset = class
  protected
    function GetItem(Index: Integer): TKnowledgeItem; virtual; abstract;
  public
    function Count: Integer; virtual; abstract;
    property Items[Index: Integer]: TKnowledgeItem read GetItem; default;
    procedure Add(AItem: TKnowledgeItem); virtual; abstract;
  end;

  TKnowledgeBase = class(TKnowledgeBaseSubset)
  public
  end;

  TDetector = class
  public
    procedure Evalute(AKnowledgeBaseSubset: TKnowledgeBaseSubset); virtual; abstract; overload;
    procedure Evalute(AKnowledgeBaseSubset: TKnowledgeBaseSubset; ASourceItem: TSourceItem); virtual; abstract; overload;
  end;
  TDetectorClass = class of TDetector;

  TDetectedFact = class(TKnowledgeItem)
  protected
    function GetLinkedKnowledge(Index: Integer): TKnowledgeItem; virtual; abstract;
    function GetDetectorClass: TDetectorClass; virtual; abstract;
  public
    property LinkedKnowledge[Index: Integer]: TKnowledgeItem read GetLinkedKnowledge;
    property DetectorClass: TDetectorClass read GetDetectorClass;
  end;

  TBrain = class
  protected
    function GetDetector(Index: Integer): TDetector; virtual; abstract;
    function GetSource(Index: Integer): TSource; virtual; abstract;
    function GetKnowledgeBase: TKnowledgeBase; virtual; abstract;
  public
    property KnowledgeBase: TKnowledgeBase read GetKnowledgeBase;
    function DetectorCount: Integer; virtual; abstract;
    property Detectors[Index: Integer]: TDetector read GetDetector;
    function SourceCount: Integer; virtual; abstract;
    property Sources[Index: Integer]: TSource read GetSource;
    procedure AddSource(ASource: TSource); virtual; abstract;
  end;


implementation


end.

