{
  The URI processor is a chain of URI handlers that are invoked in sequence to
  process a given URI. A URI handler is a class that is able to handle a family
  of URIs and do things depending on the passed URI. Together, these two classes
  make up a mechanism to:

  - decouple callers and callees.

  - stream calls as strings.

  Defining a new URI handler and registering it in the URI handler registry
  causes it to be called whenever a URI is passed to the URI processor. This
  allows to implement database-driven menus, HTML-like forms, indirect calls,
  dynamic systems, etc.

  This unit also defines a general-purpose URI class that is able to parse an
  URI string and split it into its components. This class extends Indy's
  TIdURI class.
}
unit EF.URIProcessor;

interface

uses
  IdGlobal, IdURI, Classes;

type
  {
    This class is used to parse an URI and split it into its components.
    This class adds to TIdURI's capabilities the feature of
    parsing URI parameters. Ex:

    proto://host/path/document#bookmark?param1=foo&param2=bar
    
    In the example above, ParamCount = 2, ParamStrings[0] = 'param1=foo',
    ParamValues[1] = 'bar', etc.
  }
  TEFURI = class(TIdURI)
  private
    FParamList: TStrings;
    procedure ParseParams;
    function GetParamString(const I: Integer): string;
    function GetParamCount: Integer;
    function GetParamName(const I: Integer): string;
    function GetParamValue(const I: Integer): string;
  public
    destructor Destroy; override;
    {
      Returns the parameter value; returns '' if the parameter is not found.
    }
    function ParamByName(const ParamName: string): string;
    {
      Number of parameters in the URI.
    }
    property ParamCount: Integer read GetParamCount;
    {
      Returns all the parameters in 'name=value' format.
    }
    property ParamStrings[const I: Integer]: string read GetParamString;
    {
      Returns all parameter names.
    }
    property ParamNames[const I: Integer]: string read GetParamName;
    {
      Returns all parameter values.
    }
    property ParamValues[const I: Integer]: string read GetParamValue;
  end;

  {
    Base class for all URI handlers. Inherit from this class, override
    InternalHandleURI and register the new class to add a custom URI handler
    to the chain.
  }
  TEFURIHandler = class
  protected
    {
      Override this method to handle the URI. In the method, set AHandled to
      True to break the chain. Note: this method can also modify the URI, and
      the modified version will be passed to the other handlers in the chain.
      
      The default implementation does nothing and sets AHandled to False.
    }
    procedure InternalHandleURI(var AURI: string; var AHandled: Boolean); virtual;
  public
    constructor Create; virtual;
    procedure HandleURI(var AURI: string; var AHandled: Boolean);
  end;
  TEFURIHandlerClass = class of TEFURIHandler;

  {
    Registry of URI handlers. Keeps a list of registered URI handler classes.
    It is used by URI handlers and by the URI processor.
  }
  TEFURIHandlerRegistry = class
  private
    FHandlerClasses: TStringList;
    function GetHandlerCount: Integer;
    function GetHandler(const AIndex: Integer): TEFURIHandlerClass;
  public
    constructor Create;
    destructor Destroy; override;
    {
      Number of currently registered handler classes.
    }
    property HandlerCount: Integer read GetHandlerCount;
    {
      Returns the registered handler classes by index.
    }
    property Handlers[const AIndex: Integer]: TEFURIHandlerClass read GetHandler;
    {
      Registers a URI handler class at a given position. URI handlers are
      invoked in ascending position order.
    }
    procedure RegisterHandler(const AHandlerClass: TEFURIHandlerClass; const APosition: Integer);
    {
      Unregisters a previously registered URI handler class.
    }
    procedure UnregisterHandler(const AHandlerClass: TEFURIHandlerClass);
  end;

  {
    Handles URIs by calling all registered URI handlers in a chain.
  }
  TEFURIProcessor = class
  private
    FRegistry: TEFURIHandlerRegistry;
  public
    {
      The processor needs a reference to a registry to work with.
    }
    constructor Create(const ARegistry: TEFURIHandlerRegistry);
    {
      Passes AURI to all registered handlers. Returns True if one of
      them handles it.
    }
    function HandleURI(var AURI: string): Boolean;
  end;

{
  Accesses the singleton URI processor.
}
function URIProcessor: TEFURIProcessor;

{
  Accesses the singleton URI handler registry, used by URIProcessor.
}
function URIHandlerRegistry: TEFURIHandlerRegistry;

implementation

uses
  SysUtils;

var
  _EFURIProcessor: TEFURIProcessor;
  _EFURIHandlerRegistry: TEFURIHandlerRegistry;
  
function URIProcessor: TEFURIProcessor;
begin
  if not Assigned(_EFURIProcessor) then
    _EFURIProcessor := TEFURIProcessor.Create(URIHandlerRegistry);
  Result := _EFURIProcessor;
end;

function URIHandlerRegistry: TEFURIHandlerRegistry;
begin
  if not Assigned(_EFURIHandlerRegistry) then
    _EFURIHandlerRegistry := TEFURIHandlerRegistry.Create;
  Result := _EFURIHandlerRegistry;
end;

{ TEFURI }

destructor TEFURI.Destroy;
begin
  FreeAndNil(FParamList);
  inherited;
end;

function TEFURI.GetParamString(const I: Integer): string;
begin
  ParseParams;
  Result := FParamList[I];
end;

function TEFURI.GetParamCount: Integer;
begin
  ParseParams;
  Result := FParamList.Count;
end;

function TEFURI.GetParamName(const I: Integer): string;
begin
  ParseParams;
  Result := FParamList.Names[I];
end;

function TEFURI.GetParamValue(const I: Integer): string;
begin
  ParseParams;
  Result := FParamList.ValueFromIndex[I];
end;

function TEFURI.ParamByName(const ParamName: string): string;
begin
  ParseParams;
  Result := FParamList.Values[ParamName];
end;

procedure TEFURI.ParseParams;
var
  CurrentParam, LParams: string;
begin
  if not Assigned(FParamList) then begin
    FParamList := TStringList.Create;
    try
      LParams := Params;
      // Strip any leading ?.
      Fetch(LParams, '?');
      repeat
        CurrentParam := Fetch(LParams, '&');
        if CurrentParam <> '' then
          FParamList.Add(CurrentParam);
      until CurrentParam = '';
    except
      FParamList.Free;
      raise;
    end;
  end;
end;

{ TEFURIHandlerRegistry }

constructor TEFURIHandlerRegistry.Create;
begin
  inherited Create;
  FHandlerClasses := TStringList.Create;
  FHandlerClasses.Sorted := True;
  FHandlerClasses.Duplicates := dupAccept;
end;

destructor TEFURIHandlerRegistry.Destroy;
begin
  FreeAndNil(FHandlerClasses);
  inherited;
end;

function TEFURIHandlerRegistry.GetHandlerCount: Integer;
begin
  Result := FHandlerClasses.Count;
end;

function TEFURIHandlerRegistry.GetHandler(const AIndex: Integer): TEFURIHandlerClass;
begin
  Result := TEFURIHandlerClass(FHandlerClasses.Objects[AIndex]);
end;

procedure TEFURIHandlerRegistry.RegisterHandler(const AHandlerClass: TEFURIHandlerClass;
  const APosition: Integer);
begin
  FHandlerClasses.AddObject(Format('%10d', [APosition]), TObject(AHandlerClass));
end;

procedure TEFURIHandlerRegistry.UnregisterHandler(const AHandlerClass: TEFURIHandlerClass);
var
  LClassIndex: Integer;
begin
  for LClassIndex := FHandlerClasses.Count - 1 downto 0 do
    if TEFURIHandlerClass(FHandlerClasses.Objects[LClassIndex]) = AHandlerClass then
      FHandlerClasses.Delete(LClassIndex);
end;

{ TEFURIProcessor }

constructor TEFURIProcessor.Create(const ARegistry: TEFURIHandlerRegistry);
begin
  inherited Create;
  FRegistry := ARegistry;
end;

function TEFURIProcessor.HandleURI(var AURI: string): Boolean;
var
  LClassIndex: Integer;
  LHandler: TEFURIHandler;
begin
  Result := False;
  for LClassIndex := 0 to FRegistry.HandlerCount - 1 do begin
    LHandler := FRegistry.Handlers[LClassIndex].Create;
    try
      LHandler.HandleURI(AURI, Result);
      if Result then
        Break;
    finally
      LHandler.Free;
    end;
  end;
end;

{ TEFURIHandler }

constructor TEFURIHandler.Create;
begin
  inherited Create;
end;

procedure TEFURIHandler.HandleURI(var AURI: string; var AHandled: Boolean);
begin
  InternalHandleURI(AURI, AHandled);
end;

procedure TEFURIHandler.InternalHandleURI(var AURI: string; var AHandled: Boolean);
begin
  AHandled := False;
end;

initialization

finalization
  FreeAndNil(_EFURIProcessor);
  FreeAndNil(_EFURIHandlerRegistry);

end.
