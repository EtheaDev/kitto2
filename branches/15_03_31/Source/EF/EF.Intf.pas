{-------------------------------------------------------------------------------
   Copyright 2012 Ethea S.r.l.

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

///	<summary>
///	  All interfaces in EF are based on EFInterface and disable reference
///	  counting. This unit defines the base interface and the norefcount-related
///	  classes and utility routines.
///	</summary>
unit EF.Intf;

{$I EF.Defines.inc}

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
  ///	  counting.
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
var
  LObject: TObject;
begin
  if Pointer(AEFIntferface) <> nil then
  begin
    LObject := IEFInterface(AEFIntferface).AsObject;
    NilEFIntf(AEFIntferface);
    LObject.Free;
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
  Result := -1;
end;

function TEFNoRefCountObject._Release: Integer;
begin
  Result := -1;
end;

end.
