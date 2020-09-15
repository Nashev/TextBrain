unit Engine2.Interfaces;

interface
uses
  Spring.Collections;

type
  TIndex = LongWord;

  IKnowledgeItemTypeInfo = interface;
  IProperty = interface;
  IKnowledgeMakingEngineInfo = interface;

  /// <summary>
  ///   Any knowledge item. May be presented as RDF Subject with properties and objects.
  /// </summary>
  IKnowledgeItem = interface
    function GetTypeInfo: IKnowledgeItemTypeInfo;
    function GetPropertyValue(const AProperty: IProperty): IKnowledgeItem;
  end;

  /// <summary>
  ///   Ordered set of knowledge items. Auxilarry interface, not an knowledge item byself.
  /// </summary>
  ///  <remark>
  ///    I do not use IList<IKnowledgeItem>, to not implement all of it's
  ///    interface when I will need to provide items from not compatible source
  ///  </remark>
  IKnowledgeItems = interface
    function GetItem(AIndex: TIndex): IProperty;
    function GetItemCount: TIndex;
  end;

  /// <summary>
  ///   List of properties, to provide type info.
  ///   As also an item of knowledge, Type info have own properties, each with IProperty value (object):
  ///   Property1, Property2, ..., PropertyN etc.
  /// </summary>
  IKnowledgeItemTypeInfo = interface(IKnowledgeItem)
    function GetItemProperties(AIndex: TIndex): IProperty;
    function GetItemPropertiesCount: TIndex;
  end;

  INamedKnowledgeItem = interface(IKnowledgeItem)
    function GetName: string;
  end;

  IProperty = interface(INamedKnowledgeItem)
  end;

  IKnowledgeMakingEngineInfo = interface(INamedKnowledgeItem)
  end;

  IKnowledgeMakingStrategy = interface(INamedKnowledgeItem)
    function GetProductivityCounter: TIndex;
  end;

  IKnowledgeMakingStrateges =  interface(INamedKnowledgeItem)
    function GetStrategy(AIndex: TIndex): IKnowledgeMakingStrategy;
    function GetStrategyCount: TIndex;
  end;

  IKnowledgeMakingEngine = interface
    ['{B0529BEA-D7C7-42FC-A990-0F11AE11E4EB}']
    function GetInfo: IKnowledgeMakingEngineInfo;

    function GetMakingStrategies: IKnowledgeMakingStrateges;
    function ApplyStrategy(IKnowledgeMakingStrategy): IKnowledgeItems;
  end;

  /// <summary>
  ///   Knowledge Item Basis is a list of prepending Knowledge Items and info about an engine,
  ///   that was used to make this Item from that Items.
  /// </summary>
  IKnowledgeItemBasis = interface(IKnowledgeItem)
    ['{507AE114-75F5-450B-A921-73293CD5841E}']
    function GetBasisItems(AIndex: TIndex): IKnowledgeItem;
    function GetEngineInfo: IKnowledgeMakingEngineInfo;
  end;

  /// <summary>
  ///   Auxilary interface, collection of bases and consequences of a some KnowledgeItem
  ///   Used only in a context of one instance of IKnowledgeItem.
  /// </summary>
  IKnowledgeItemInfo = interface
    ['{9259D90D-4C0C-4E0A-B26D-FBAB8BA306F1}']
    function GetKnowledgeBasis(AIndex: TIndex): IKnowledgeItemBasis;
    function GetKnowledgeBasesCount: TIndex;

    /// <summary>
    ///   Consequence is a Basis for some findings, conclusions, made from this KnowledgeItem
    /// </summary>
    function GetKnowledgeConsequence(AIndex: TIndex): IKnowledgeItemBasis;
    function GetKnowledgeConsequencesCount: TIndex;

    procedure Append(const AMoreInfo: IKnowledgeItemInfo);
  end;

  IKnowledgeStorage = interface(IKnowledgeItem)
    ['{9A17650C-D160-4509-8E5D-2A944CAB04D0}']
    function GetKnowledgeItemInfo(const AKnowledgeItem: IKnowledgeItem): IKnowledgeItemInfo;
    procedure Append(const AKnowledgeItem: IKnowledgeItem);
  end;

implementation

end.
