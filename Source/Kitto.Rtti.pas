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

unit Kitto.Rtti;

{$I Kitto.Defines.inc}

interface

uses
  SysUtils
  , Kitto.JS.Types
  ;

function GetMethodName(const AMethod: TJSProcedure): string;

implementation

uses
  Rtti
  ;

function GetMethodName(const AMethod: TJSProcedure): string;
var
  LInfo: TRttiType;
  LMethod: TMethod;
  LRttiMethod: TRttiMethod;
  LObject: TObject;
begin
  Result := '';

  LMethod := TMethod(AMethod);
  LObject := LMethod.Data;

  LInfo := TRttiContext.Create.GetType(LObject.ClassType);
  for LRttiMethod in LInfo.GetMethods do
  begin
    if LRttiMethod.CodeAddress = LMethod.Code then
    begin
      Result := LRttiMethod.Name;
      Break;
    end;
  end;

  if Result = '' then
    raise Exception.Create('Method not found')
end;

end.
