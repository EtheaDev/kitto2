///	<summary>
///	  <para>This unit contains the classes that make up Kitto's support for
///	  rules. A rule in Kitto is a business rule, or constraint, applied during
///	  data entry. Rules are associated to models, model fields, view tables and
///	  view table fiels, in a subnode <c>Rules</c> of the object
///	  definition.</para>
///	  <para>A rule influences Kitto's data entry behaviour at different times
///	  and places. For example, a rule that forces character case on a given
///	  field is applied on the client side through ExtJS, while a custom rule
///	  written in Delphi code that performs cross-field checks for data
///	  integrity before writing a record to the database is applied on the
///	  server after a form is submitted.</para>
///	  <para>Kitto includes a number of predefined rules for common validation
///	  tasks, defined both in this unit and in Kitto.Ext.Rule. You can add new
///	  rules by creating classes inherited from <c>TKRuleImpl</c> and
///	  registering them this way:</para>
///	  <code lang="Delphi">
///	initialization
///	  TKRuleImplRegistry.Instance.RegisterClass(TKExtMyRule.GetClassId, TKExtMyRule);
///	finalization
///	  TKRuleImplRegistry.Instance.UnregisterClass(TKExtMyRule.GetClassId);
///	</code>You then use it by mentioning it in the definition of a model, a
///	model field, a view table or a view table field. Rules are called (applied)
///	depending on where they are used:
///	  <list type="bullet">
///	    <item>A model-level rule is always applied.</item>
///	    <item>A table-view-level rule is always applied when editing data
///	    through the view, in addition to (and before) any model-level
///	    rules.</item>
///	    <item>A model-field-level rule is always applied unless a
///	    view-table-field-level rule of the same type is also defined (for
///	    example, you cannot force case to upper case in the model and lower
///	    case in the view - you have to do it only once where it's
///	    appropriate).</item>
///	    <item>A view-table-field-level rule is always applied when editing data
///	    through the view.</item>
///	  </list>
///	</summary>
///	<seealso cref="TKRuleImpl">How to create custom rules.</seealso>
unit Kitto.Rules;

interface

uses
  EF.Types, EF.Tree,
  Kitto.Types, Kitto.Metadata.Models, Kitto.Store;

type
  ///	<summary>Base class for all classes that implement rules.</summary>
  TKRuleImpl = class
  private
    FRule: TKRule;
  protected
    procedure SetRule(const AValue: TKRule); virtual;
    procedure CheckRuleParam(const APath: string);
    procedure CheckRuleValueParam;
    class function GetClassId: string; virtual;

    ///	<summary>Returns the error message, which can be customized through the
    ///	ErrorMessage parameter or kept as default value.</summary>
    ///	<remarks>Not all rules use error messages. Some rules constrain or
    ///	massage input without displaying errors.</remarks>
    function GetErrorMessage: string;

    ///	<summary>Override this method to provide a custom error message for the
    ///	rule.</summary>
    ///	<remarks>The custom error message is NOT used if the rule's
    ///	<c>ErrorMessage</c> parameter is specified.</remarks>
    function InternalGetErrorMessage: string; virtual;

    ///	<summary>Raises a validation error. If no message is passed, then the
    ///	result of GetErrorMessage is used.</summary>
    procedure RaiseError(const AMessage: string = '');
  public
    property Rule: TKRule read FRule write SetRule;

    ///	<summary>
    ///	  <para>Server side validation before writing to the database.
    ///	  Descendants should read the values in ARecord and call RaiseError
    ///	  (which will raise an exception with the default or a custom message)
    ///	  in order to stop the write operation and display an erro to the
    ///	  user.</para>
    ///	  <para>Descendants may also change values.</para>
    ///	</summary>
    ///	<remarks>If an exception is raised, any change is lost.</remarks>
    procedure BeforeWrite(const ARecord: TKRecord); virtual;
  end;
  TKRuleImplClass = class of TKRuleImpl;

  TKRuleImplRegistry = class(TEFRegistry)
  private
    class var FInstance: TKRuleImplRegistry;
    class function GetInstance: TKRuleImplRegistry; static;
  public
    class destructor Destroy;
    class property Instance: TKRuleImplRegistry read GetInstance;

    ///	<summary>Adds a rule implementation class to the registry.</summary>
    procedure RegisterClass(const AId: string; const AClass: TKRuleImplClass);
  end;

  TKRuleImplFactory = class(TEFFactory)
  private
    class var FInstance: TKRuleImplFactory;
    class function GetInstance: TKRuleImplFactory; static;
  public
    class destructor Destroy;
    class property Instance: TKRuleImplFactory read GetInstance;

    ///	<summary>Creates and returns an instance of the rule implementation class
    ///	identified by AClassId. Raises an exception if said class is not
    ///	registered.</summary>
    function CreateObject(const AClassId: string): TKRuleImpl;
  end;

  EKRuleError = class(EKError);

  EKValidationError = class(EKRuleError);

  TKApplyRuleProc = reference to procedure(const ARuleImpl: TKRuleImpl);

implementation

uses
  SysUtils, StrUtils,
  EF.StrUtils, EF.Localization;

{ TKRuleImpl }

procedure TKRuleImpl.BeforeWrite;
begin
end;

procedure TKRuleImpl.CheckRuleParam(const APath: string);
var
  LNode: TEFNode;
begin
  Assert(Assigned(Rule));

  LNode := Rule.FindNode(APath);
  if not Assigned(LNode) or LNode.IsNull or (LNode.AsString = '') then
    raise EKRuleError.CreateFmt('Missing parameter %s in rule %s', [APath, GetClassid]);
end;

procedure TKRuleImpl.CheckRuleValueParam;
begin
  Assert(Assigned(Rule));

  if Rule.IsNull or (Rule.AsString = '') then
    raise EKRuleError.CreateFmt('Missing value in rule %s', [GetClassid]);
end;

class function TKRuleImpl.GetClassId: string;
begin
  Result := StripPrefix(ClassName, 'TK');
end;

function TKRuleImpl.GetErrorMessage: string;
begin
  Result := Rule.GetExpandedString('ErrorMessage');
  if Result = '' then
    Result := InternalGetErrorMessage;
end;

function TKRuleImpl.InternalGetErrorMessage: string;
begin
  Result := Format(_('Rule %s failed.'), [GetClassId]);
end;

procedure TKRuleImpl.RaiseError(const AMessage: string);
begin
  raise EKValidationError.Create(IfThen(AMessage = '', GetErrorMessage, AMessage));
end;

procedure TKRuleImpl.SetRule(const AValue: TKRule);
begin
  FRule := AValue;
end;

{ TKRuleImplRegistry }

class destructor TKRuleImplRegistry.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TKRuleImplRegistry.GetInstance: TKRuleImplRegistry;
begin
  if FInstance = nil then
    FInstance := TKRuleImplRegistry.Create;
  Result := FInstance;
end;

procedure TKRuleImplRegistry.RegisterClass(const AId: string;
  const AClass: TKRuleImplClass);
begin
  inherited RegisterClass(AId, AClass);
end;

{ TKRuleImplFactory }

function TKRuleImplFactory.CreateObject(const AClassId: string): TKRuleImpl;
begin
  Result := inherited CreateObject(AClassId) as TKRuleImpl;
end;

class destructor TKRuleImplFactory.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TKRuleImplFactory.GetInstance: TKRuleImplFactory;
begin
  if FInstance = nil then
    FInstance := TKRuleImplFactory.Create(TKRuleImplRegistry.Instance);
  Result := FInstance;
end;

end.
