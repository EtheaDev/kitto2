unit EF.Classes;

{$I EF.Defines.inc}

interface

uses
  Types, SysUtils, Classes, DB, Generics.Collections,
  EF.Intf, EF.Types, EF.Macros, EF.Tree, EF.ObserverIntf;

const
  {
    Default value for TEFComponent.LogLevel.
  }
  DEFAULT_LOG_LEVEL = 1;

type
  TEFConfig = class(TEFNode);

  IEFComponent = interface(IEFInterface)
    ['{F4FA3F31-AE77-4847-A5DD-8F2DC9DDEBD1}']
    function GetConfig: TEFConfig;
    property Config: TEFConfig read GetConfig;
  end;

  IEFDBConnection = interface;
  IEFDBCommand = interface;
  IEFDBQuery = interface;

  {
    A connection to a database. It is in charge of
    database access and transaction management.
  }
  IEFDBConnection = interface(IEFComponent)
    ['{E084619B-D16D-4907-8E80-E33F55F60433}']
    {
      Connects to the database.
    }
    procedure Open;
    {
      Closes the connection to the database. As a result, open
      datasets might get closed as well or not, depending on the
      implementation.
    }
    procedure Close;
    {
      Returns True if the connection to the database is open, False otherwise.
    }
    function IsOpen: Boolean;
    {
      Executes a command and returns the number of affected records (assuming
      the database is able to return it). Implementors may make use of
      predefined functionality in a given database library, or create an
      instance of whatever class implements IEFDBCommand, set its CommandText
      and Execute the command, then destroy the object and return.
    }
    function ExecuteImmediate(const AStatement: string): Integer;
    {
      Starts a nEF transaction.
    }
    procedure StartTransaction;
    {
      Commits and ends a previously started transaction.
    }
    procedure CommitTransaction;
    {
      Rollbacks and ends a previously started transaction.
    }
    procedure RollbackTransaction;
    {
      Tells whether a transaction was started or not.
    }
    function IsInTransaction: Boolean;
    {
      Fetches and returns a nEF sequence generator value. Use only with
      databases that support sequence generators (IB/Fb, Oracle).
    }
    function FetchSequenceGeneratorValue(const ASequenceName: string): Int64;
    {
      Returns the last generated auto-inc value for a given table (or globally,
      if the database doesn't support getting auto-inc values per table). Use
      only with databases that support auto-inc semantics, and according to the
      particular database specification.
    }
    function GetLastAutoincValue(const ATableName: string = ''): Int64;
    {
      Creates and returns an instance of the concrete class that
      implements IEFDBCommand, linked to this connection.
    }
    function CreateDBCommand: IEFDBCommand;
    {
      Creates and returns an instance of the concrete class that
      implements IEFDBQuery, linked to this connection.
    }
    function CreateDBQuery: IEFDBQuery;
    {
      Formats the specified value according to the database rules for
      SQL. The value formatted in this way can be used in queries.

      This method should use the passed data items' DataType to decide how to
      format the value, which it then returns as a string.
    }
    function FormatValue(const AValue: TEFNode): string;
  end;

  {
    A data access component that needs a connection (and possibly a
    transaction) to work.
  }
  IEFDBComponent = interface(IEFComponent)
    ['{0BBA4B61-3696-4048-96B9-7155B75ACA38}']
    function GetConnection: IEFDBConnection;
    procedure SetConnection(const AValue: IEFDBConnection);
    property Connection: IEFDBConnection read GetConnection write SetConnection;
  end;

  {
    Executes a command that doesn't return any data.
  }
  IEFDBCommand = interface(IEFDBComponent)
    ['{DD90B918-0E57-4648-A1D8-6D9C01678C0D}']
    {
      Contains the text of the statement to execute.
    }
    function GetCommandText: string;
    procedure SetCommandText(const AValue: string);
    property CommandText: string read GetCommandText write SetCommandText;
    {
      Manages prepared statements. Not all database might benefit from it.
    }
    function GetPrepared: Boolean;
    procedure SetPrepared(const AValue: Boolean);
    property Prepared: Boolean read GetPrepared write SetPrepared;
    {
      Optional param values for CommandText.
    }
    function GetParams: TParams;
    procedure SetParams(const AValue: TParams);
    property Params: TParams read GetParams write SetParams;
    {
      Executes the command and returns the number of affected records,
      assuming the database is able to give this information.
    }
    function Execute: Integer;
  end;

  {
    Executes a command that returns a data set.
  }
  IEFDBQuery = interface(IEFDBCommand)
    ['{F6B0DAE8-F08D-406F-8EC4-98999C96D394}']
    {
      Executes the CommandText and reads the data into DataSet.
    }
    procedure Open;
    {
      Frees the memory used by the data and closes or frees DataSet.
    }
    procedure Close;
    {
      Returns True if the DataSet is available.
    }
    function IsOpen: Boolean;
    {
      Detail queries need a reference to their master query's DataSource for
      parameter binding.
    }
    function GetMasterSource: TDataSource;
    procedure SetMasterSource(const AValue: TDataSource);
    property MasterSource: TDataSource read GetMasterSource write SetMasterSource;
    {
      The data buffer.
      Don't access DataSet before calling Open, or checking IsOpen: some
      classes might not make it available in advance.
    }
    function GetDataSet: TDataSet;
    property DataSet: TDataSet read GetDataSet;
  end;

  IEFTreeItem = interface(IEFInterface)
    ['{1C109A68-2108-4679-A76A-FC019B883A8B}']
    {
      Reparents the specified item under the current object. The implementor
      decides where the nEF item will go and how to unlink it from its previous
      parent if any, as required. Items that are containers might host
      reparented items as children, others as siblings.
      AAfter optionally specifies a sibling item after which to add AItem
      in the container's collection.
    }
    procedure ReparentItem(const AItem: IEFTreeItem;
      const AAfter: IEFTreeItem = nil);
  end;

  { TODO : inherit from TEFSubjectAndObserver }
  TEFComponent = class(TEFNoRefCountObject, IInterface, IEFInterface,
    IEFSubject, IEFObserver, IEFComponent)
  private
    FOnLog: TEFLogEvent;
    FLogLevel: Integer;
    FObservers: TInterfaceList;
    FConfig: TEFConfig;
  protected
    {
      Override this method to enable Config auto-load upon first request.
      The default implementation returns '', which disables auto-load.
    }
    function GetConfigFileName: string; virtual;
    {
      Implements GetClassId.
    }
    class function InternalGetClassId: string; virtual;
    {
      Implements GetId.
    }
    function InternalGetId: string; virtual;
    {
      Fires OnLog.
    }
    procedure DoLog(const AString: string; const ALogLevel: Integer = DEFAULT_LOG_LEVEL); overload;
    {
      Calls DoLog for each line in AStrings, pre-pending ALinePrefix to
      each line.
    }
    procedure DoLog(const AStrings: TStrings; const ALinePrefix: string = '';
      const ALogLevel: Integer = DEFAULT_LOG_LEVEL); overload;
    {
      Checks that a proprerty has an assigned/valid value, and raises a
      generic exception otherwise. ACondition should evaluate to True if the
      property is assigned/valid, and to False otherwise. In this case, an
      exception with APropertyName in the message text is raised.
    }
    procedure CheckProperty(const ACondition: Boolean; const APropertyName: string);
    {
      Implements IEFSubject.Notify.
    }
    procedure NotifyObservers(const AContext: string = '');
    {
      Implements IEFObserver.Update.
    }
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); virtual;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    {
      Returns an identification string based on the class name. It can be used
      by a registry to hold string identifiers for different classes.
      By default it returns the class name without any 'T' or 'TEF' prefix.
    }
    class function GetClassId: string;
    {
      Returns an Id for the object. By default it returns the same as GetClassId.
    }
    function GetId: string;
    {
      From IEFInterface.
    }
    function AsObject: TObject;
    {
      Fired each time the object needs to log information about its work.
    }
    property OnLog: TEFLogEvent read FOnLog write FOnLog;
    {
      Sets the logging level for this component. Only messages that
      have a level lower than or equal to this setting will be logged.
    }
    property LogLevel: Integer read FLogLevel write FLogLevel default DEFAULT_LOG_LEVEL;
    {
      Implements IEFSubject.Attach.
    }
    procedure AttachObserver(const AObserver: IEFObserver);
    {
      Implements IEFSubject.Detach.
    }
    procedure DetachObserver(const AObserver: IEFObserver);

    function GetConfig: TEFConfig;
    property Config: TEFConfig read GetConfig;
  end;

  TEFComponentClass = class of TEFComponent;

  ///	<summary>
  ///	  Holds a list of registered classes and provides class references by
  ///	  string Ids.
  ///	</summary>
  TEFRegistry = class
  private
    FClasses: TDictionary<string, TClass>;
  protected
    {
      Called at the beginning of RegisterClass.
    }
    procedure BeforeRegisterClass(const AId: string; const AClass: TClass); virtual;
    {
      Called at the end of RegisterClass, after successful registration.
    }
    procedure AfterRegisterClass(const AId: string; const AClass: TClass); virtual;
    {
      Called at the beginning of UnregisterClass.
    }
    procedure BeforeUnregisterClass(const AId: string; const AClass: TClass); virtual;
    {
      Called at the end of UnregisterClass, after successful unregistration.
    }
    procedure AfterUnregisterClass(const AId: string; const AClass: TClass); virtual;
    {
      Holds the list of registered classes, each of which represented by its
      Id and holding the class reference in Objects[]. Descendants may need to
      access this object in order to add functionality.
    }
    property Classes: TDictionary<string, TClass> read FClasses;
  public
    constructor Create;
    destructor Destroy; override;
    {
      Adds a class to the registry.
    }
    procedure RegisterClass(const AId: string; const AClass: TClass);
    {
      Deletes a previously registered class from the registry.
    }
    procedure UnregisterClass(const AId: string);
    {
      Returns True if a given class Id is registered.
    }
    function HasClass(const AId: string): Boolean;
    {
      Returns a class reference to the class identified by AClassId, if registered.
      Otherwise an exception is raised.
    }
    function GetClass(const AId: string): TClass;
    {
      Returns a class reference to the class identified by AClassId, if registered.
      Otherwise returns nil.
    }
    function FindClass(const AId: string): TClass;
  end;

  {
    Uses the registry to create objects by class id.
    It is friend to a descendant of TEFRegistry.
  }
  TEFFactory = class
  private
    FRegistry: TEFRegistry;
  protected
    function DoCreateObject(const AClass: TClass): TObject; virtual;
    {
      A reference to the registry.
    }
    property Registry: TEFRegistry read Fregistry;
    {
      Called at the beginning of CreateObject, before checking if the class Id
      is registered.
    }
    procedure BeforeCreateObject(const AId: string); virtual;
    {
      Called after CreateObject has created the object and before it returns
      it to the caller.
    }
    procedure AfterCreateObject(const AId: string;
      const AClass: TClass; const AObject: TObject); virtual;
  public
    constructor Create(const ARegistry: TEFRegistry);
    {
      Creates and returns an instance of the class identified
      by AClassId. Raises an exception if said class is not registered.
    }
    function CreateObject(const AId: string): TObject;
    {
      Returns True if a given class Id is registered.
    }
    function HasClass(const AId: string): Boolean;
  end;

implementation

uses
  StrUtils, TypInfo,
  EF.SysUtils, EF.StrUtils, EF.Localization, EF.YAML;

{ TEFComponent }

procedure TEFComponent.AfterConstruction;
begin
  inherited;
  FLogLevel := DEFAULT_LOG_LEVEL;
  FObservers := TInterfaceList.Create;
end;

function TEFComponent.AsObject: TObject;
begin
  Result := Self;
end;

procedure TEFComponent.AttachObserver(const AObserver: IEFObserver);
begin
  if Assigned(AObserver) and Assigned(FObservers) then
  begin
    if FObservers.IndexOf(AObserver) < 0 then
      FObservers.Add(AObserver);
  end;
end;

procedure TEFComponent.CheckProperty(const ACondition: Boolean;
  const APropertyName: string);
begin
  if not ACondition then
    raise EEFError.CreateFmt(_('Unspecified property "%s".'), [APropertyName]);
end;

destructor TEFComponent.Destroy;
begin
  FreeAndNil(FConfig);
  FreeAndNil(FObservers);
  inherited;
end;

procedure TEFComponent.DetachObserver(const AObserver: IEFObserver);
begin
  if Assigned(AObserver) and Assigned(FObservers) then
    FObservers.Remove(AObserver);
end;

procedure TEFComponent.DoLog(const AString: string; const ALogLevel: Integer = DEFAULT_LOG_LEVEL);
begin
  if (FLogLevel >= ALogLevel) and Assigned(FOnLog) then
    FOnLog(Self, '[' + GetId + '] ' + AString);
end;

procedure TEFComponent.DoLog(const AStrings: TStrings; const ALinePrefix: string = '';
  const ALogLevel: Integer = DEFAULT_LOG_LEVEL);
var
  LLogLineIndex: Integer;
begin
  Assert(Assigned(AStrings));

  for LLogLineIndex := 0 to AStrings.Count - 1 do
    DoLog(ALinePrefix + AStrings[LLogLineIndex], ALogLevel);
end;

class function TEFComponent.GetClassId: string;
begin
  Result := InternalGetClassId;
end;

function TEFComponent.GetConfig: TEFConfig;
var
  LConfigFileName: string;
begin
  if not Assigned(FConfig) then
  begin
    LConfigFileName := GetConfigFileName;
    if LConfigFileName <> '' then
      FConfig := TEFTreeFactory.LoadFromFile<TEFConfig>(LConfigFileName)
    else
      FConfig := TEFConfig.Create;
  end;
  Result := FConfig;
end;

function TEFComponent.GetConfigFileName: string;
begin
  Result := '';
end;

function TEFComponent.GetId: string;
begin
  Result := InternalGetId;
end;

class function TEFComponent.InternalGetClassId: string;
begin
  // Strips standard prefix from the class name.
  // If the class name doesn't start with TEF, then let's at least
  // remove the T.
  Result := StripPrefixAndSuffix(ClassName, 'T', '');
  Result := StripPrefixAndSuffix(Result, 'EF', '');
end;

function TEFComponent.InternalGetId: string;
begin
  Result := GetClassId;
end;

procedure TEFComponent.NotifyObservers(const AContext: string = '');
var
  I: Integer;
begin
  if Assigned(FObservers) then
    for I := FObservers.Count - 1 downto 0 do
      (FObservers[I] as IEFObserver).UpdateObserver(Self, AContext);
end;

procedure TEFComponent.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
end;

{ TEFRegistry }

procedure TEFRegistry.BeforeRegisterClass(const AId: string; const AClass: TClass);
begin
end;

procedure TEFRegistry.AfterRegisterClass(const AId: string; const AClass: TClass);
begin
end;

procedure TEFRegistry.BeforeUnregisterClass(const AId: string; const AClass: TClass);
begin
end;

procedure TEFRegistry.AfterUnregisterClass(const AId: string; const AClass: TClass);
begin
end;

constructor TEFRegistry.Create;
begin
  inherited Create;
  FClasses := TDictionary<string, TClass>.Create;
end;

destructor TEFRegistry.Destroy;
begin
  FreeAndNil(FClasses);
  inherited;
end;

function TEFRegistry.FindClass(const AId: string): TClass;
begin
  if HasClass(AId) then
    Result := GetClass(AId)
  else
    Result := Default(TClass);
end;

function TEFRegistry.GetClass(const AId: string): TClass;
begin
  if HasClass(AId) then
    Result := FClasses[AId]
  else
    raise EEFError.CreateFmt('Class %s not found.', [AId]);
end;

procedure TEFRegistry.RegisterClass(const AId: string; const AClass: TClass);
begin
  BeforeRegisterClass(AId, AClass);
  FClasses.Add(AId, AClass);
  AfterRegisterClass(AId, AClass);
end;

function TEFRegistry.HasClass(const AId: string): Boolean;
begin
  Result := FClasses.ContainsKey(AId);
end;

procedure TEFRegistry.UnregisterClass(const AId: string);
begin
  Assert(AId <> '');

  if FClasses.ContainsKey(AId) then
    FClasses.Remove(AId);
end;

{ TEFFactory }

procedure TEFFactory.BeforeCreateObject(const AId: string);
begin
end;

procedure TEFFactory.AfterCreateObject(const AId: string;
  const AClass: TClass; const AObject: TObject);
begin
end;

constructor TEFFactory.Create(const ARegistry: TEFRegistry);
begin
  Assert(Assigned(ARegistry));

  inherited Create;
  FRegistry := ARegistry;
end;

function TEFFactory.CreateObject(const AId: string): TObject;
var
  LClass: TClass;
begin
  LClass := FRegistry.GetClass(AId);
  if Assigned(LClass) then
  begin
    BeforeCreateObject(AId);
    Result := DoCreateObject(LClass);
    AfterCreateObject(AId, LClass, Result);
  end
  else
    raise EEFError.CreateFmt(_('%s: Unknown class "%s".'), [ClassName, AId]);
end;

function TEFFactory.DoCreateObject(const AClass: TClass): TObject;
begin
  Result := AClass.Create;
end;

function TEFFactory.HasClass(const AId: string): Boolean;
begin
  Result := FRegistry.HasClass(AId);
end;

end.
