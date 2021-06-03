{-------------------------------------------------------------------------------
   Copyright 2012-2019 Ethea S.r.l.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-------------------------------------------------------------------------------}

/// <summary>
///  <para>
///   This unit contains the classes that make up Kitto's support for
///   rules. A rule in Kitto is a business rule, or constraint, applied during
///   data entry. Rules are associated to models, model fields, view tables and
///   view table fiels, in a subnode <c>Rules</c> of the object definition.
///  </para>
///  <para>
///   A rule influences Kitto's data entry behaviour at different times
///   and places. For example, a rule that forces character case on a given
///   field is applied on the client side through ExtJS, while a custom rule
///   written in Delphi code that performs cross-field checks for data
///   integrity before writing a record to the database is applied on the
///   server after a form is submitted.
///  </para>
///  <para>
///   Kitto includes a number of predefined rules for common validation
///   tasks, defined both in this unit and in Kitto.Ext.Rule. You can add new
///   rules by creating classes inherited from <c>TKRuleImpl</c> and
///   registering them this way:
///  </para>
///  <code lang="Delphi">
///   initialization
///     TKRuleImplRegistry.Instance.RegisterClass(TKExtMyRule.GetClassId, TKExtMyRule);
///   finalization
///     TKRuleImplRegistry.Instance.UnregisterClass(TKExtMyRule.GetClassId);
///  </code>
///  You then use it by mentioning it in the definition of a model, a
///  model field, a view table or a view table field. Rules are called (applied)
///  depending on where they are used:
///  <list type="bullet">
///   <item>A model-level rule is always applied.</item>
///   <item>A table-view-level rule is always applied when editing data
///    through the view, in addition to (and before) any model-level
///    rules.</item>
///   <item>A model-field-level rule is always applied unless a
///    view-table-field-level rule of the same type is also defined (for
///    example, you cannot force case to upper case in the model and lower
///    case in the view - you have to do it only once where it's
///    appropriate).</item>
///   <item>A view-table-field-level rule is always applied when editing data
///    through the view.</item>
///  </list>
/// </summary>
/// <seealso cref="TKRuleImpl">How to create custom rules.</seealso>
unit Kitto.Rules;

{$I Kitto.Defines.inc}

interface

uses
  Generics.Collections,
  EF.Types, EF.Tree,
  Kitto.Types, Kitto.Metadata.Models, Kitto.Store;

type
  EKRuleError = class(EKError);

  EKValidationError = class(EKRuleError);

  /// <summary>
  ///  Base class for all classes that implement rules.
  /// </summary>
  TKRuleImpl = class
  strict private
    FRule: TKRule;
    FReferencedModelStores: TDictionary<string, TKStore>;
  protected
    class function GetClassId: string; virtual;
  strict protected
    procedure SetRule(const AValue: TKRule); virtual;
    procedure CheckRuleParam(const APath: string);
    procedure CheckRuleValueParam;

    /// <summary>
    ///  Returns the error message, which can be customized through the
    ///  ErrorMessage parameter or kept as default value.
    /// </summary>
    /// <remarks>
    ///  Not all rules use error messages. Some rules constrain or
    ///  massage input without displaying errors.
    /// </remarks>
    function GetErrorMessage: string;

    /// <summary>
    ///  Override this method to provide a custom error message for the rule.
    /// </summary>
    /// <remarks>
    ///  The custom error message is NOT used if the rule's
    ///  <c>ErrorMessage</c> parameter is specified.
    /// </remarks>
    function InternalGetErrorMessage: string; virtual;

    /// <summary>
    ///  Raises a validation error. If no message is passed, then the
    ///  result of GetErrorMessage is used.
    /// </summary>
    procedure RaiseError(const AMessage: string = ''); overload;

    /// <summary>
    ///  Raises a validation error formatted with params. If no message
    ///  is passed, then the result of GetErrorMessage is used.
    /// </summary>
    procedure RaiseError(const AParams: array of const; const AMessage: string = ''); overload;

    /// <summary>
    ///  Called by both BeforeAdd and BeforeUpdate. The default
    ///  implementation does nothing.
    /// </summary>
    procedure BeforeAddOrUpdate(const ARecord: TKRecord); virtual;

    /// <summary>
    ///  Called by both AfterAdd and AfterUpdate. The default
    ///  implementation does nothing.
    /// </summary>
    procedure AfterAddOrUpdate(const ARecord: TKRecord); virtual;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    property Rule: TKRule read FRule write SetRule;

    /// <summary>
    ///  Client-side (Javascript) rules return True. The default implementation
    ///  returns False, assuming it is a server-side rule.
    /// </summary>
    function IsClientSide: Boolean; virtual;

    /// <summary>
    ///  Called when creating a new record before showing it in the
    ///  user interface. Descendants may set computed default values
    ///  (declarative default values are already applied when this method is
    ///  called). Calling RaiseError in this method displays an error message to
    ///  the user and aborts the insert operation.
    /// </summary>
    /// <param name="ARecord">
    ///  The record being created. It is usually an
    ///  instance of TKViewTableRecord.
    /// </param>
    /// <remarks>
    ///  If an exception is raised, the insert operation is aborted.
    /// </remarks>
    procedure NewRecord(const ARecord: TKRecord); virtual;

    /// <summary>
    ///  Called when editing a record in the user interface.
    ///  Descendants may set computed values.
    ///  Calling RaiseError in this method displays an error message to
    ///  the user and aborts the edit operation.
    /// </summary>
    /// <param name="ARecord">
    ///  The record being edited. It is usually an
    ///  instance of TKViewTableRecord.
    /// </param>
    /// <remarks>
    ///  If an exception is raised, the edit operation is aborted.
    /// </remarks>
    procedure EditRecord(const ARecord: TKRecord); virtual;

    /// <summary>
    ///  Called after creating Windows for editing a record in the user interface.
    ///  Descendants may set computed values.
    /// </summary>
    /// <param name="ARecord">
    ///  The record being edited. It is usually an
    ///  instance of TKViewTableRecord.
    /// </param>
    procedure AfterShowEditWindow(const ARecord: TKRecord); virtual;

    /// <summary>
    ///  <para>
    ///   Called when duplicating a record.
    ///   Descendants should read the values in ARecord and call RaiseError
    ///   (which will raise an exception with the default or a custom
    ///   message) in order to stop the duplicate operation and display an error
    ///   to the user.
    ///  </para>
    ///  <para>
    ///   Descendants may also change values.
    ///  </para>
    /// </summary>
    /// <param name="ARecord">
    ///  The duplicate record being created. It is usually an instance of
    ///  TKViewTableRecord.
    /// </param>
    /// <remarks>
    ///  If an exception is raised, any change is lost.
    /// </remarks>
    procedure DuplicateRecord(const ARecord: TKRecord); virtual;

    /// <summary>
    ///  <para>
    ///   Server side validation before writing a new record to the database.
    ///   Descendants should read the values in ARecord and call RaiseError
    ///   (which will raise an exception with the default or a custom
    ///   message) in order to stop the write operation and display an error
    ///   to the user.
    ///  </para>
    ///  <para>
    ///   Descendants may also change values.
    ///  </para>
    /// </summary>
    /// <param name="ARecord">
    ///  The record being created. It is usually an instance of
    ///  TKViewTableRecord.
    /// </param>
    /// <remarks>
    ///  If an exception is raised, any change is lost.
    /// </remarks>
    procedure BeforeAdd(const ARecord: TKRecord); virtual;

    /// <summary>
    ///  <para>
    ///   Server side validation before updating an existing database record.
    ///   Descendants should read the values in ARecord and call RaiseError
    ///   (which will raise an exception with the default or a custom
    ///   message) in order to stop the write operation and display an error
    ///   to the user.
    ///  </para>
    ///  <para>
    ///   Descendants may also change values.
    ///  </para>
    /// </summary>
    /// <param name="ARecord">
    ///  The record being written to the database. It is usually an instance
    ///  of TKViewTableRecord.
    /// </param>
    /// <remarks>
    ///  <para>
    ///   If an exception is raised, any change is lost.
    ///  </para>
    ///  <para>
    ///   Changing key values has the effect of updating a different record.
    ///  </para>
    /// </remarks>
    procedure BeforeUpdate(const ARecord: TKRecord); virtual;

    /// <summary>
    ///  <para>
    ///   Server side validation before�deleting a database record.
    ///   Descendants should read the values in ARecord and call RaiseError
    ///   (which will raise an exception with the default or a custom
    ///   message) in order to stop the delete operation and display an error
    ///   to the user.
    ///  </para>
    ///  <para>
    ///   Descendants may also change values, although the changes are
    ///   meaningless.
    ///  </para>
    /// </summary>
    /// <param name="ARecord">
    ///  The record being�deleted. It is usually an instance of
    ///  TKViewTableRecord.
    /// </param>
    /// <remarks>
    ///  Changing a key value will have the effect of deleting a different
    ///  record.
    /// </remarks>
    procedure BeforeDelete(const ARecord: TKRecord); virtual;

    /// <summary>
    ///  Called after successfully writing a new record to the database. This
    ///  method can still raise an exception (by calling RaiseError) causing
    ///  the transaction to be rolled back.
    /// </summary>
    /// <remarks>
    ///  Any values set or changed at the database level (by database
    ///  triggers, for example), are not available at this point. Changes
    ///  performed in BeforeAdd by this or another rule are.
    /// </remarks>
    procedure AfterAdd(const ARecord: TKRecord); virtual;

    /// <summary>
    ///  Called after successfully updating a record in the database. This
    ///  method can still raise an exception (by calling RaiseError) causing
    ///  the transaction to be rolled back.
    /// </summary>
    /// <remarks>
    ///  Any values set or changed at the database level (by database
    ///  triggers, for example), are not available at this point. Changes
    ///  performed in BeforeUpdate by this or another rule are.
    /// </remarks>
    procedure AfterUpdate(const ARecord: TKRecord); virtual;

    /// <summary>
    ///  Called after successfully deleting a record in�the database. This
    ///  method can still raise an exception (by calling RaiseError) causing
    ///  the transaction to be rolled back.
    /// </summary>
    procedure AfterDelete(const ARecord: TKRecord); virtual;

    /// <summary>
    ///  Called before changing a field's value. This method can silently disallow
    ///  the change by setting ADoIt to False, or it can call RaiseError to abort
    ///  with an error message, or even modify the new value before it is written
    ///  to the field by modifying ANewValue.
    /// </summary>
    procedure BeforeFieldChange(const AField: TKField; const AOldValue: Variant;
      var ANewValue: Variant; var ADoIt: Boolean); virtual;

    /// <summary>
    ///  Called after a field's value has been successfully changed.
    ///  This method can still abort the operation but the old field value will
    ///  not be restored automatically.
    /// </summary>
    procedure AfterFieldChange(const AField: TKField; const AOldValue, ANewValue: Variant); virtual;

    /// <summary>
    ///  Creates and returns a store with one record containing the values of
    ///  the model instance pointed to by the specified reference field. Raises
    ///  exceptions if the field is not found or is not a reference.
    /// </summary>
    /// <remarks>
    ///  If no referenced model instance is found, the returned store is empty.
    /// </remarks>
    /// <remarks>
    ///  The caller does not need to free the returned object - it will be freed
    ///  together with this object.
    /// </remarks>
    function GetReferencedModelInstance(const AReferenceName: string; const ARecord: TKRecord): TKStore;

    /// <summary>
    ///  Returns a value from the model instance pointed to by the specified
    ///  reference field. Raises exceptions if the field is not found or is not
    ///  a reference.
    /// </summary>
    function GetReferencedModelInstanceValue(const AReferenceName, AFieldName: string; const ARecord: TKRecord): Variant;

    procedure AfterRefreshReferenceField(const AField: TKField); virtual;

  end;
  TKRuleImplClass = class of TKRuleImpl;

  TKRuleImplRegistry = class(TEFRegistry)
  private
    class var FInstance: TKRuleImplRegistry;
    class function GetInstance: TKRuleImplRegistry; static;
  public
    class destructor Destroy;
    class property Instance: TKRuleImplRegistry read GetInstance;

    /// <summary>Adds a rule implementation class to the registry.</summary>
    procedure RegisterClass(const AId: string; const AClass: TKRuleImplClass);
  end;

  TKRuleImplFactory = class(TEFFactory)
  private
    class var FInstance: TKRuleImplFactory;
    class function GetInstance: TKRuleImplFactory; static;
  public
    class destructor Destroy;
    class property Instance: TKRuleImplFactory read GetInstance;

    /// <summary>
    ///  Creates and returns an instance of the rule implementation class
    ///  identified by AClassId. Raises an exception if said class is not
    ///  registered.
    /// </summary>
    function CreateObject(const AClassId: string): TKRuleImpl;
  end;

  TKEnforceRange = class(TKRuleImpl)
  protected
    procedure SetRule(const AValue: TKRule); override;
    procedure BeforeAddOrUpdate(const ARecord: TKRecord); override;
    function InternalGetErrorMessage: string; override;
  end;

implementation

uses
  SysUtils, StrUtils, Variants,
  EF.StrUtils, EF.Localization, EF.VariantUtils,
  Kitto.Metadata.DataView;

{ TKRuleImpl }

procedure TKRuleImpl.AfterAdd(const ARecord: TKRecord);
begin
  AfterAddOrUpdate(ARecord);
end;

procedure TKRuleImpl.AfterAddOrUpdate(const ARecord: TKRecord);
begin
end;

procedure TKRuleImpl.AfterConstruction;
begin
  inherited;
  FReferencedModelStores := TDictionary<string, TKStore>.Create;
end;

procedure TKRuleImpl.AfterDelete(const ARecord: TKRecord);
begin
end;

procedure TKRuleImpl.AfterFieldChange(const AField: TKField; const AOldValue, ANewValue: Variant);
begin
end;

procedure TKRuleImpl.AfterRefreshReferenceField(const AField: TKField);
begin
end;

procedure TKRuleImpl.AfterUpdate(const ARecord: TKRecord);
begin
  AfterAddOrUpdate(ARecord);
end;

procedure TKRuleImpl.BeforeAdd(const ARecord: TKRecord);
begin
  BeforeAddOrUpdate(ARecord);
end;

procedure TKRuleImpl.BeforeAddOrUpdate(const ARecord: TKRecord);
begin
end;

procedure TKRuleImpl.BeforeDelete(const ARecord: TKRecord);
begin
end;

procedure TKRuleImpl.BeforeFieldChange(const AField: TKField; const AOldValue: Variant;
  var ANewValue: Variant; var ADoIt: Boolean);
begin
end;

procedure TKRuleImpl.BeforeUpdate(const ARecord: TKRecord);
begin
  BeforeAddOrUpdate(ARecord);
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

destructor TKRuleImpl.Destroy;
var
  LStores: TArray<TKStore>;
  I: Integer;
begin
  LStores := FReferencedModelStores.Values.ToArray;
  for I := Low(LStores) to High(LStores) do
    LStores[I].Free;
  FreeAndNil(FReferencedModelStores);
  inherited;
end;

procedure TKRuleImpl.EditRecord(const ARecord: TKRecord);
begin
end;

procedure TKRuleImpl.AfterShowEditWindow(const ARecord: TKRecord);
begin
end;

procedure TKRuleImpl.DuplicateRecord(const ARecord: TKRecord);
begin
end;


class function TKRuleImpl.GetClassId: string;
begin
  if StartsText('TK', ClassName) then
    Result := StripPrefix(ClassName, 'TK')
  else
    Result := StripPrefix(ClassName, 'T');
end;

function TKRuleImpl.GetErrorMessage: string;
begin
  Result := Rule.GetExpandedString('ErrorMessage');
  if Result = '' then
    Result := InternalGetErrorMessage;
end;

function TKRuleImpl.GetReferencedModelInstance(const AReferenceName: string;
  const ARecord: TKRecord): TKStore;
var
  LReferenceField: TKModelField;
  LRecord: TKViewTableRecord;
begin
  if not FReferencedModelStores.TryGetValue(AReferenceName, Result) then
  begin
    LRecord := ARecord as TKViewTableRecord;
    LReferenceField := LRecord.FieldByName(AReferenceName).ModelField;
    Assert(LReferenceField.IsReference);
    Result := LRecord.ViewTable.FieldByName(AReferenceName).CreateReferencedModelStore(
      LRecord.GetFieldValues(LReferenceField.GetFieldNames));
    FReferencedModelStores.Add(AReferenceName, Result);
  end;
end;

function TKRuleImpl.GetReferencedModelInstanceValue(const AReferenceName,
  AFieldName: string; const ARecord: TKRecord): Variant;
var
  LStore: TKStore;
begin
  LStore := GetReferencedModelInstance(AReferenceName, ARecord);
  if LStore.IsEmpty then
    Result := Null
  else
    Result := LStore.Records[0].FieldByName(AFieldName).Value;
end;

function TKRuleImpl.InternalGetErrorMessage: string;
begin
  Result := Format(_('Rule %s failed.'), [GetClassId]);
end;

function TKRuleImpl.IsClientSide: Boolean;
begin
  Result := False;
end;

procedure TKRuleImpl.NewRecord(const ARecord: TKRecord);
begin
end;

procedure TKRuleImpl.RaiseError(const AParams: array of const; const AMessage: string);
begin
  raise EKValidationError.CreateFmt(IfThen(AMessage = '', GetErrorMessage, AMessage), AParams);
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

{ TKEnforceRange }

procedure TKEnforceRange.BeforeAddOrUpdate(const ARecord: TKRecord);
var
  LFrom: TKViewTableField;
  LTo: TKViewTableField;
begin
  inherited;
  LFrom := (ARecord as TKViewTableRecord).FieldByName(Rule.GetString('From'));
  LTo := (ARecord as TKViewTableRecord).FieldByName(Rule.GetString('To'));

  if not IsRange(LFrom.Value, LTo.Value) then
    RaiseError([LTo.ViewField.DisplayLabel, LFrom.ViewField.DisplayLabel]);
end;

function TKEnforceRange.InternalGetErrorMessage: string;
begin
  Result := _('%s must be greater than or equal to %s.');
end;

procedure TKEnforceRange.SetRule(const AValue: TKRule);
begin
  inherited;
  CheckRuleParam('From');
  CheckRuleParam('To');
end;

initialization
  TKRuleImplRegistry.Instance.RegisterClass(TKEnforceRange.GetClassId, TKEnforceRange);

finalization
  TKRuleImplRegistry.Instance.UnregisterClass(TKEnforceRange.GetClassId);

end.
