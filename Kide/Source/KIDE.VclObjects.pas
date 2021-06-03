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
unit KIDE.VclObjects;

interface

uses
  Windows, Generics.Collections, SysUtils, Classes, Graphics;


Type
  TVclObjectList = class;

  {
  Ancestor of all classes and components for Kide2 layout editor.
  }
  TVclObject = class(TComponent)
  private
  protected
    FVclName : string;  // Internal Vcl name generated automatically
    function IsParent(CName : string): boolean;
    function ParamAsInteger(ParamName : string) : integer;
    function ParamAsDouble(ParamName : string) : double;
    function ParamAsBoolean(ParamName : string) : boolean;
    function ParamAsString(ParamName : string) : string;
    function ParamAsTDateTime(ParamName : string) : TDateTime;
    function ParamAsObject(ParamName : string) : TVclObject;
    procedure InitDefaults; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    IsChild : boolean;
    constructor Create(AOwner: TComponent); override;

    constructor CreateAndAddTo(List: TVclObjectList);

    procedure AddTo(List: TVclObjectList);

    procedure Delete;
    class function VclClassName : string; virtual;
    function CharsToPixels(const AChars: Integer; const AFont: TFont; const AOffset: Integer = 0): Integer;
    function LinesToPixels(const ALines: Integer; AFont: TFont): Integer;
    destructor Destroy; override;
    function FindVclObject(const AName: string): TObject;
    procedure SetCustomConfigItem(const AName, AValue: string);
  end;

  // List of TVclObjects.
  TVclObjectList = class(TVclObject)
  private
    FObjects: TObjectList<TVclObject>;
    FAttribute: string;
    function GetObject(I: Integer): TVclObject;
    function GetOwnerVclObject: TVclObject;
    function GetCount: Integer;
    property OwnerVclObject: TVclObject read GetOwnerVclObject;
  protected
  public
    procedure AfterConstruction; override;
    constructor CreateAsAttribute(const AOwner: TVclObject; const AAttribute: string);
    destructor Destroy; override;

    property Objects[I: Integer]: TVclObject read GetObject; default;
    function Add(const AObject: TVclObject): Integer;
    function AddInternal(const AObject: TVclObject): Integer;
    function Remove(const AObject: TVclObject): Integer;
    function IndexOf(const AObject: TVclObject): Integer;
    property Count: Integer read GetCount;

    function GetAddMethodName: string;
  end;

implementation

uses
  StrUtils, Math, Controls, TypInfo;

{ VclObjectList }

{
Creates a TVclObjectList instance.
@param pOwner TVclObject that owns this list
@param pAttribute JS attribute name in TVclObject to this list
}
constructor TVclObjectList.CreateAsAttribute(const AOwner: TVclObject; const AAttribute: string);
begin
  Assert(Assigned(AOwner));
  Assert(AAttribute <> '');

  FAttribute := AAttribute;
  inherited Create(AOwner);
end;

// Frees this list and all objects linked in it
destructor TVclObjectList.Destroy;
begin
  FreeAndNil(FObjects);
  inherited;
end;

function TVclObjectList.Add(const AObject: TVclObject): Integer;
begin
  Assert(Assigned(AObject));
  Assert(Assigned(OwnerVclObject));
  Assert(FAttribute <> '');
  Result := AddInternal(AObject);
end;

{
Returns the Ith object in the list, starts with 0.
@param I Position in list
@return <link TVclObject>
}
function TVclObjectList.GetAddMethodName: string;
begin
  // items -> add()
  // buttons -> addButton()
  if (FAttribute = '') or (FAttribute = 'items') then
    Result := 'add'
  else
  begin
    Result := FAttribute;
    if EndsText('s', Result) then
      System.Delete(Result, Length(Result), 1);
    Result[1] := UpperCase(Result[1])[1];
    Result := 'add' + Result;
  end;
end;

function TVclObjectList.GetCount: Integer;
begin
  Result := FObjects.Count;
end;

function TVclObjectList.GetObject(I: Integer): TVclObject;
begin
  Result := FObjects[I];
end;

function TVclObjectList.GetOwnerVclObject: TVclObject;
begin
  Result := Owner as TVclObject;
end;

function TVclObjectList.IndexOf(const AObject: TVclObject): Integer;
begin
  Result := FObjects.IndexOf(AObject);
end;

function TVclObjectList.Remove(const AObject: TVclObject): Integer;
begin
  Result := FObjects.Remove(AObject);
end;

function TVclObjectList.AddInternal(const AObject: TVclObject): Integer;
begin
  Assert(Assigned(AObject));

  Result := FObjects.Add(AObject);
end;

procedure TVclObjectList.AfterConstruction;
begin
  inherited;
  FObjects := TObjectList<TVclObject>.Create(False);
end;

{ VclObject }

procedure CalcStandardEditSize(Font : TFont;
  Ctl3D : boolean; out Width, Height : integer);
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  if NewStyleControls then
  begin
    if Ctl3D then I := 8 else I := 6;
    I := GetSystemMetrics(SM_CYBORDER) * I;
  end
  else
  begin
    I := SysMetrics.tmHeight;
    if I > Metrics.tmHeight then I := Metrics.tmHeight;
    I := I div 4 + GetSystemMetrics(SM_CYBORDER) * 4;
  end;
  Height := Metrics.tmHeight + I;
  Width := Metrics.tmAveCharWidth + 2;
end;

{
Converts a TExtFormField length in characters to pixels to use in Width property.
@param Chars Field length in characters
@return Pixels used by browser to render these Chars
}
function TVclObject.CharsToPixels(const AChars: Integer; const AFont: TFont;
  const AOffset: Integer = 0): Integer;
var
  LWidth, LHeight: Integer;
begin
  CalcStandardEditSize(AFont, False, LWidth, LHeight);
  // + 16 sort of compensates for text-to-border left and right margins.
  Result := (LWidth * AChars) + 16 + AOffset;
end;

{
Converts a TExtFormTextArea height in characters to pixels to use in Height property.
Uses dynamic JS in browser.
@param Lines TextArea height in characters.
@return Pixels used by browser to render these Lines
}
function TVclObject.LinesToPixels(const ALines: Integer; AFont: TFont): Integer;
var
  LWidth, LHeight: Integer;
begin
  CalcStandardEditSize(AFont, False, LWidth, LHeight);
  Result := LHeight * ALines; // * 0.8;
end;

// Deletes object from memory
procedure TVclObject.Delete;
begin
  inherited Free;
end;

procedure TVclObject.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = Self) and (Operation = opRemove) and Assigned(Owner) then
  begin
    // The owner is destroying this object, so we clear anything related from
    // the response. We do this in the destructor for those cases in which a component
    // in destroyed while still being owned, but we must do it here as well
    // for cases in which the owner is freeing its own components (which it
    // does AFTER calling RemoveComponent, thus at a time when it's not the owner
    // anymore.
    //ExtSession.ResponseItems.RemoveAll(Self);
  end;
end;

destructor TVclObject.Destroy;
begin
  inherited;
end;

{
Creates a TVclObject
}
constructor TVclObject.Create(AOwner: TComponent);
begin
  Assert(Assigned(AOwner));
  inherited Create(AOwner);
  InitDefaults;
end;

// Returns 'Object' that is the default class name for Ext JS objects
class function TVclObject.VclClassName : string;
begin
  Result := 'Object';
end;

{
Tests if a class name is parent of this object
@param CName Class name with "T" prefix
@return True if CName is parent of this object and false if not
}
function TVclObject.IsParent(CName : string) : boolean;
var
  Cls : TClass;
begin
  if (CName <> '') and (CName[1] = 'T') then begin
    Result := true;
    Cls    := ClassType;
    while Cls.ClassName <> 'TComponent' do begin
      if Cls.ClassName = CName then exit;
      Cls := Cls.ClassParent
    end;
  end;
  Result := false;
end;

procedure TVclObject.SetCustomConfigItem(const AName, AValue: string);
begin
  if IsPublishedProp(Self, AName) then
    SetStrProp(Self, AName, AValue);
end;

{
Adds this object in a list.
If called as constructor creates the object before adds it to the list.
@param List An instanciated <link TVclObjectList>
}
constructor TVclObject.CreateAndAddTo(List : TVclObjectList);
begin
  Create(List);
  AddTo(List);
end;

procedure TVclObject.InitDefaults;
begin
end;

procedure TVclObject.AddTo(List: TVclObjectList);
begin
  List.Add(Self);
end;

function TVclObject.ParamAsInteger(ParamName : string) : integer;
begin
  if IsPublishedProp(Self, ParamName) then
    Result := StrToIntDef(GetStrProp(Self, ParamName), 0)
  else
    Result := 0;
end;

// Aux function used internaly by ExtToPascal to override HandleEvent method
function TVclObject.ParamAsDouble(ParamName : string) : double;
begin
  if IsPublishedProp(Self, ParamName) then
    Result := GetFloatProp(Self, ParamName)
  else
    Result := 0;
end;

// Aux function used internaly by ExtToPascal to override HandleEvent method
function TVclObject.ParamAsBoolean(ParamName : string) : boolean;
begin
  if IsPublishedProp(Self, ParamName) then
    Result := GetOrdProp(Self, ParamName) = 1
  else
    Result := False;
end;

// Aux function used internaly by ExtToPascal to override HandleEvent method
function TVclObject.ParamAsString(ParamName : string) : string;
begin
  if IsPublishedProp(Self, ParamName) then
    Result := GetStrProp(Self, ParamName)
  else
    Result := '';
end;

// Aux function used internaly by ExtToPascal to override HandleEvent method
function TVclObject.ParamAsTDateTime(ParamName : string) : TDateTime;
begin
  if IsPublishedProp(Self, ParamName) then
    Result := GetFloatProp(Self, ParamName)
  else
    Result := 0;
end;

// Aux function used internaly by ExtToPascal to override HandleEvent method
function TVclObject.ParamAsObject(ParamName : string) : TVclObject;
begin
  if IsPublishedProp(Self, ParamName) then
    Result := TVclObject(GetObjectProp(Self, ParamName, TVclObject));
end;

function TVclObject.FindVclObject(const AName: string): TObject;
var
  I: Integer;
begin
  Assert(AName <> '');

  Result := nil;
  for I := 0 to ComponentCount - 1 do
  begin
    if Components[I] is TVclObject then
    begin
      if TVclObject(Components[I]).Name = AName then
        Result := TVclObject(Components[I])
      else
        Result := TVclObject(Components[I]).FindVclObject(AName);
      if Assigned(Result) then
        Break;
    end;
  end;
end;

end.
