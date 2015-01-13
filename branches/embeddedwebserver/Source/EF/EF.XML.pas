{-------------------------------------------------------------------------------
   Copyright 2014 Ethea S.r.l.

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
///	  Support for XML format.
///	</summary>
unit EF.XML;

{$I EF.Defines.inc}

interface

uses
  SysUtils, StrUtils,
  EF.Types;

const
  XMLHeader = '<?xml version="1.0" encoding="UTF-8" ?>';
  XMLTagFormat = '<%s>%s</%s>';

/// <summary>
///   Escapes control characters in the XML string.
/// </summary>
function XMLEscape(const AString: string): string;

implementation

uses
  EF.StrUtils;


function XMLEscape(const AString: string): string;
const
  EscStr = '&%s;';
var
  I: Integer;
  Esc: string;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(AString) do
  begin
    C := AString[I];
    if CharInSet(C, [#34, #38, #39, #60, #62]) then
    begin
      case C of
        #34:
          Esc := 'quot';
        #38:
          Esc := 'amp';
        #39:
          Esc := 'apos';
        #60:
          Esc := 'lt';
        #62:
          Esc := 'gt';
      end;
      Esc := Format(EscStr, [Esc]);
      Result := Result + Esc;
    end
    else
      Result := Result + C;
  end;
end;

end.
