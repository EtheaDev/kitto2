unit EF.Intf;

interface

type
  ///	<summary>
  ///	  Base interface for all EF interfaces.
  ///	</summary>
  IEFInterface = interface
    ['{9E0408C7-0923-4DA4-86F8-CB0561D43B49}']

    ///	<summary>
    ///	  Gives access to the implementing object. Implementors should return
    ///	  Self.
    ///	</summary>
    function AsObject: TObject;
  end;

  ///	<summary>
  ///	  Base class for objects that implement interfaces but disable reference
  ///	  count.
  ///	</summary>
  TEFNoRefCountObject = class(TObject, IInterface, IEFInterface)
  public
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function AsObject: TObject;
  end;

///	<summary>
///	  Frees the object and sets the reference to nil. It is mandatory to use
///	  this function instead of FreeAnNil on references of type IEFInterface or
///	  descendants, which are *not* reference-counted. Don't pass anything else
///	  to this function.
///	</summary>
procedure FreeAndNilEFIntf(var AEFIntferface);

///	<summary>
///	  Sets the reference to nil but doesn't free the object.
///	  It is mandatory to use this function instead of simply setting the reference
///	  to nil on references of type IEFInterface or descendants, which are *not*
///	  reference-counted. Don't pass anything else to this function.
///	</summary>
procedure NilEFIntf(var AEFIntferface);

implementation

procedure FreeAndNilEFIntf(var AEFIntferface);
begin
  if Pointer(AEFIntferface) <> nil then
  begin
    IEFInterface(AEFIntferface).AsObject.Free;
    NilEFIntf(AEFIntferface);
  end;
end;

procedure NilEFIntf(var AEFIntferface);
begin
  // It is necessary to set the reference to nil and at the same time avoid
  // calling the pseudo-virtual method _Release, which the compiler calls
  // automatically (through _ClearIntf) when an interface variable is set to
  // nil or goes out of scope. Calling _Release on an already freed instance
  // may cause AVs. The cast to Pointer avoids this.
  Pointer(AEFIntferface) := nil;
end;

{ TEFNoRefCountObject }

function TEFNoRefCountObject.AsObject: TObject;
begin
  Result := Self;
end;

function TEFNoRefCountObject.QueryInterface(const IID: TGUID;
  out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TEFNoRefCountObject._AddRef: Integer;
begin
  Result := 0;
end;

function TEFNoRefCountObject._Release: Integer;
begin
  Result := 0;
end;

end.
