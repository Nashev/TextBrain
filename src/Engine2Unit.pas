unit Engine2Unit;

interface
uses
  System.SysUtils,
  Spring,
  Spring.Services,
  Spring.Container.Common,
  Spring.Collections,
  Engine2.Interfaces
  ;

type
  [Implements(TypeInfo(IKnowledgeItem))]
  TDictBasedKnowledgeItem = class(TInterfacedObject, IKnowledgeItem)
  private
    FProperties: IDictionary<IProperty, IKnowledgeItem>;
    FTypeInfo: IKnowledgeItemTypeInfo;
  public
    constructor Create;

    function GetTypeInfo: IKnowledgeItemTypeInfo;
    function GetPropertyValue(const AProperty: IProperty): IKnowledgeItem;

    procedure SetProperty(const AProperty: IProperty; const AValue: IKnowledgeItem);
  end;

  [Implements(TypeInfo(IKnowledgeItemInfo))]
  TKnowledgeItemInfo = class(TInterfacedObject, IKnowledgeItemInfo)
  private
    FKnowledgeConsequence: IList<IKnowledgeItemBasis>;
    FKnowledgeBasis: IList<IKnowledgeItemBasis>;
  public
    constructor Create;
    function GetKnowledgeBasis(AIndex: TIndex): IKnowledgeItemBasis;
    function GetKnowledgeBasesCount: TIndex;
    function GetKnowledgeConsequence(AIndex: TIndex): IKnowledgeItemBasis;
    function GetKnowledgeConsequencesCount: TIndex;
    procedure Append(const AMoreInfo: IKnowledgeItemInfo);
  end;

  [Implements(TypeInfo(IKnowledgeStorage))]
  TAllLoadedKnowledgeStorages = class(TDictBasedKnowledgeItem, IKnowledgeStorage)
  private
    FInstantiatedStorages: IDictionary<string, IKnowledgeStorage>;
  public
    constructor Create;
    // IKnowledgeStorage
    function GetKnowledgeItemInfo(const AKnowledgeItem: IKnowledgeItem): IKnowledgeItemInfo;
    procedure Append(const AKnowledgeItem: IKnowledgeItem);
  end;

  [Implements(TypeInfo(IKnowledgeStorage), 'FileSystem')]
  TFileSystemKnowledgeStorage = class(TDictBasedKnowledgeItem, IKnowledgeStorage)
  private
    FFileName: string;
  public
    constructor Create(const AFileName: string);
    // IKnowledgeStorage
    function GetKnowledgeItemInfo(const AKnowledgeItem: IKnowledgeItem): IKnowledgeItemInfo;
    procedure Append(const AKnowledgeItem: IKnowledgeItem);
  end;

  [Implements(TypeInfo(IKnowledgeStorage), 'Memory')]
  TMemoryKnowledgeStorage = class(TDictBasedKnowledgeItem, IKnowledgeStorage)
  public
    constructor Create(const ASubsetName: string);
    // IKnowledgeStorage
    function GetKnowledgeItemInfo(const AKnowledgeItem: IKnowledgeItem): IKnowledgeItemInfo;
    procedure Append(const AKnowledgeItem: IKnowledgeItem);
  end;

implementation

constructor TDictBasedKnowledgeItem.Create;
begin
  FProperties := TCollections.CreateDictionary<IProperty, IKnowledgeItem>;
end;

function TDictBasedKnowledgeItem.GetPropertyValue(const AProperty: IProperty): IKnowledgeItem;
begin
  Result := FProperties[AProperty];
end;

function TDictBasedKnowledgeItem.GetTypeInfo: IKnowledgeItemTypeInfo;
begin
  if not Assigned(FTypeInfo) then
  begin
    var LTypeName := 'TypeByProps(';
    for var LProp in FProperties.Keys do
      LTypeName := LTypeName + LProp.GetName + ',';
    LTypeName := LTypeName.TrimEnd(',') + ')';

    FTypeInfo := ServiceLocator.GetService<IKnowledgeItemTypeInfo>(LTypeName)
  end;
  Result := FTypeInfo;
end;

procedure TDictBasedKnowledgeItem.SetProperty(const AProperty: IProperty; const AValue: IKnowledgeItem);
begin
  if Assigned(AValue) then
    FProperties.AddOrSetValue(AProperty, AValue)
  else
    FProperties.Remove(AProperty);
end;

{ TFileSystemKnowledgeStorage }

procedure TFileSystemKnowledgeStorage.Append(const AKnowledgeItem: IKnowledgeItem);
begin

end;

constructor TFileSystemKnowledgeStorage.Create(const AFileName: string);
begin
  FFileName := AFileName;
end;

function TFileSystemKnowledgeStorage.GetKnowledgeItemInfo(const AKnowledgeItem: IKnowledgeItem): IKnowledgeItemInfo;
begin

end;

{ TMemoryKnowledgeStorage }

procedure TMemoryKnowledgeStorage.Append(const AKnowledgeItem: IKnowledgeItem);
begin

end;

constructor TMemoryKnowledgeStorage.Create(const ASubsetName: string);
begin

end;

function TMemoryKnowledgeStorage.GetKnowledgeItemInfo(const AKnowledgeItem: IKnowledgeItem): IKnowledgeItemInfo;
begin

end;

{ TAllLoadedKnowledgeStorages }

procedure TAllLoadedKnowledgeStorages.Append(const AKnowledgeItem: IKnowledgeItem);
begin
  raise ENotSupportedException.Create('Append items to exact storage, not into this storage collection! ');
end;

constructor TAllLoadedKnowledgeStorages.Create;
begin
  FInstantiatedStorages := TCollections.CreateDictionary<string, IKnowledgeStorage>;
end;

function TAllLoadedKnowledgeStorages.GetKnowledgeItemInfo(const AKnowledgeItem: IKnowledgeItem): IKnowledgeItemInfo;
begin
  Result := Spring.Services<IKnowledgeItemInfo>;
  for var LStorage in FInstantiatedStorages.Values do
    Result.Append(LStorage.GetKnowledgeItemInfo(AKnowledgeItem));
end;

{ TKnowledgeItemInfo }

procedure TKnowledgeItemInfo.Append(const AMoreInfo: IKnowledgeItemInfo);
begin
  if AMoreInfo is TKnowledgeItemInfo then
  begin
    var LMoreInfo := AMoreInfo as TKnowledgeItemInfo;
    FKnowledgeBasis.AddRange(LMoreInfo.FKnowledgeBasis);
    FKnowledgeConsequence.AddRange(LMoreInfo.FKnowledgeConsequence);
  end
  else
  begin
    for var i := 0 to AMoreInfo.GetKnowledgeBasesCount - 1 do
      FKnowledgeBasis.Add(AMoreInfo.GetKnowledgeBasis(i));

    for var i := 0 to AMoreInfo.GetKnowledgeConsequencesCount - 1 do
      FKnowledgeConsequence.Add(AMoreInfo.GetKnowledgeConsequence(i));
  end;
end;

constructor TKnowledgeItemInfo.Create;
begin
  FKnowledgeConsequence := TCollections.CreateList<IKnowledgeItemBasis>;
  FKnowledgeBasis := TCollections.CreateList<IKnowledgeItemBasis>;
end;

function TKnowledgeItemInfo.GetKnowledgeBasesCount: TIndex;
begin
  Result := FKnowledgeBasis.Count;
end;

function TKnowledgeItemInfo.GetKnowledgeBasis(AIndex: TIndex): IKnowledgeItemBasis;
begin
  Result := FKnowledgeBasis.Items[AIndex];
end;

function TKnowledgeItemInfo.GetKnowledgeConsequence(AIndex: TIndex): IKnowledgeItemBasis;
begin
  Result := FKnowledgeConsequence.Items[AIndex];
end;

function TKnowledgeItemInfo.GetKnowledgeConsequencesCount: TIndex;
begin
  Result := FKnowledgeConsequence.Count;
end;

end.
