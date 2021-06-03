{-------------------------------------------------------------------------------
   Copyright 2012-2021 Ethea S.r.l.

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
unit KIDE.Editor;

interface

uses
  EF.Intf, EF.Types, EF.Tree,
  Classes, Controls;

type
  TEditorAction = (eaSave, eaReload, eaClose);

  ///	<summary>Represents a specific editor/designer open in the IDE.</summary>
  IEditor = interface(IEFInterface)
    ['{CAC22163-C7EA-487C-B73B-A885C0F5697B}']

    ///	<summary>Initializes the editor passing params such as a reference to
    ///	the object to edit or the original file name.</summary>
    ///	<param name="AParams">Object containing params. Which params to pass
    ///	varies with each editor.</param>
    ///	<remarks>The AParams object itself is guaranteed alive only during the
    ///	execution of this method. As the caller might free it upon return, the
    ///	editor is expected to copy any useful data in this method and never
    ///	hold or use references to the AParams object outside of here.</remarks>
    procedure InitEditor(const AParams: TEFNode);

    ///	<summary>Triggers a lighter re-initialization procedure. This method is
    ///	called when re-activating an already open editor.</summary>
    ///	<remarks>It is up to the implementation to decide whether to perform a
    ///	full initialization or not. Generally, since an open editor might have
    ///	pending changes, a full initialization which might lose them is not
    ///	performed when refreshing.</remarks>
    procedure RefreshEditor;

    procedure DisplayEmbedded(const AParent: TWinControl);
    procedure CloseEditor(const AForce: Boolean);

    ///	<summary>Returns True if the specified common action is enabled at any
    ///	given point.</summary>
    function IsEditorActionEnabled(const AAction: TEditorAction): Boolean;

    procedure ExecuteEditorAction(const AAction: TEditorAction);

    ///	<summary>Returns True if the editor matches the provided spec, which
    ///	might be a file name or something else that (preferably uniquely)
    ///	identifies an open editor (such as a URI).</summary>
    function EditorMatchesSpec(const ASpec: string): Boolean;

    ///	<summary>Returns True if the editor is suitable for the given spec and
    ///	params. To be called on a freshly created editor.</summary>
    ///	<remarks>Do not confuse with MatchesSpec, which is called on an already
    ///	operating editor to know if it is operating on a given spec. This
    ///	method is used to decide if an editor can be used for a spec &amp;
    ///	params.</remarks>
    function EditorSuits(const ASpec: string; const AParams: TEFNode): Boolean;

    ///	<summary>Returns editor specific properties useful for placing it
    ///	inside a context.</summary>
    ///	<param name="AName">Supported names so far: ShortTitle,
    ///	LongTitle.</param>
    function GetEditorProperty(const AName: string): Variant;

    ///	<summary>Editor spec, as passed when initializing the editor or
    ///	otherwise provided by the open editor itself.</summary>
    function GetSpec: string;
    property Spec: string read GetSpec;
  end;

  ///	<summary>A manager or container of editors.</summary>
  IEditContext = interface
    ['{C24640D2-FB95-4CD6-A6E4-39EBAA85EC78}']

    function FindEditor(const ASpec: string): IEditor;

    function OpenEditor(const ASpec: string; const AParams: TEFNode): IEditor;
  end;

  TEditorRegistry = class(TEFRegistry)
  private
    class var FInstance: TEditorRegistry;
    class function GetInstance: TEditorRegistry; static;
  protected
    procedure BeforeRegisterClass(const AId: string; const AClass: TClass);
      override;
  public
    class destructor Destroy;
    class property Instance: TEditorRegistry read GetInstance;
    property Classes;
  end;

  ///	<summary>
  ///	  Queries the registry to create editors by spec & params. It is
  ///	  friend to TEditorRegistry.
  ///	</summary>
  TEditorFactory = class
  private
    class var FInstance: TEditorFactory;
    class function GetInstance: TEditorFactory; static;
  public
    class destructor Destroy;
    class property Instance: TEditorFactory read GetInstance;

    ///	<summary>Creates an editor suitable for the specified spec and
    ///	params.</summary>
    function CreateEditor(const ASpec: string; const AParams: TEFNode;
      const AOwner: TComponent): IEditor;
  end;

implementation

uses
  SysUtils;

{ TEditorRegistry }

procedure TEditorRegistry.BeforeRegisterClass(const AId: string;
  const AClass: TClass);
begin
  if not Supports(AClass, IEditor) then
    raise Exception.CreateFmt('Cannot register class %s (Id %s). Class does not support IEditor.', [AClass.ClassName, AId]);
  inherited;
end;

class destructor TEditorRegistry.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TEditorRegistry.GetInstance: TEditorRegistry;
begin
  if FInstance = nil then
    FInstance := TEditorRegistry.Create;
  Result := FInstance;
end;

{ TEditorFactory }

function TEditorFactory.CreateEditor(const ASpec: string;
  const AParams: TEFNode; const AOwner: TComponent): IEditor;
var
  I: Integer;
  LObject: TObject;
begin
  Assert(ASpec <> '');
  Assert(Assigned(AParams));

  Result := nil;
  for I := 0 to TEditorRegistry.Instance.Classes.Count - 1 do
  begin
    LObject := TComponentClass(TEditorRegistry.Instance.Classes.Values.ToArray[I]).Create(AOwner);
    try
      NilEFIntf(Result);
      if not Supports(LObject, IEditor, Result) then
        Continue;

      if not Result.EditorSuits(ASpec, AParams) then
      begin
        FreeAndNil(LObject);
        NilEFIntf(Result);
      end
      else
        Break;
    except
      FreeAndNil(LObject);
      raise;
    end;
  end;

  if not Assigned(Result) then
    raise Exception.CreateFmt('Cannot edit %s. No suitable class found.', [ASpec]);
end;

class destructor TEditorFactory.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TEditorFactory.GetInstance: TEditorFactory;
begin
  if FInstance = nil then
    FInstance := TEditorFactory.Create;
  Result := FInstance;
end;

end.
