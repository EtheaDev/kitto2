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
  DocTypeHeader = '<!DOCTYPE';
  XmlNameSpace = 'xmlns="';

/// <summary>
///   Escapes control characters in the XML string.
/// </summary>
function XMLEscape(const AString: string): string;

/// <summary>
///   Clear the XMLHeader from an XML string.
///   Returns true if the header was found and cleared
/// </summary>
function ClearXMLHeader(var Text: string): boolean;

/// <summary>
///   Returns the position of the XMLHeader if found
/// </summary>
function PosXMLHeader(const Text: string): integer;

/// <summary>
///   Clear the DOCTYPE node from an XML string.
///   Returns true if the DOCTYPE node was found and cleared
/// </summary>
function ClearDOCTYPE(var Text: string): boolean;

function ClearXmlNameSpaces(var Text: string): boolean;

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

function PosXMLHeader(const Text: string): integer;
begin
  Result := Pos(Copy(XMLHeader,1,36), Text);
end;

function ClearXMLHeader(var Text: string): boolean;
var
  LXmlHeaderPos, LClosedTagPos: Integer;
begin
  Result := False;
  LXmlHeaderPos := PosXMLHeader(Text);
  if LXmlHeaderPos > 0 then
  begin
    LClosedTagPos := Pos('>', Text);
    if LClosedTagPos > 0 then
    begin
      Text := Copy(Text, LClosedTagPos+1, MaxInt);
      Result := True;
    end;
  end;
end;

function ClearDOCTYPE(var Text: string): boolean;
var
  LDOCTYPEPos, LClosedTagPos: Integer;
begin
  Result := False;
  LDOCTYPEPos := Pos(DocTypeHeader, Text);
  if LDOCTYPEPos > 0 then
  begin
    LClosedTagPos := Pos('>', Text);
    if LClosedTagPos > 0 then
    begin
      Text := Copy(Text, LClosedTagPos+1, MaxInt);
      Result := True;
    end;
  end;
end;

function ClearXmlNameSpaces(var Text: string): boolean;
var
  LPos, LClosedBraket: Integer;
begin
  Result := False;
  LPos := Pos(XmlNameSpace, Text);
  if LPos > 0 then
  begin
    LClosedBraket := Pos('"', Copy(Text,LPos+length(XmlNameSpace),MaxInt))+LPos-1;
    if LClosedBraket > 0 then
    begin
      Text := Copy(Text, 1, LPos-1) + Copy(Text, LPos+1+LClosedBraket+1, MaxInt);
      Result := True;
      if not ClearXmlNameSpaces(Text) then
        Exit;
    end;
  end;
end;

end.
