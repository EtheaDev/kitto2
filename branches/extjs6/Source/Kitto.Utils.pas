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

unit Kitto.Utils;

{$I Kitto.Defines.inc}

interface

function HTMLEncode(const AString: string): string;
function URLDecodeUTF8(const AEncoded: string): string;
function URLDecode(const AEncoded: string): string;
function URLEncode(const ADecoded: string): string;

implementation

uses
  {$IFDEF D21+}System.NetEncoding{$ELSE}Web.HTTPApp{$ENDIF}
  , SysUtils
  , StrUtils
  ;

function HTMLEncode(const AString: string): string;
begin
  {$IFDEF D21+}
  Result := TNetEncoding.HTML.Encode(AString);
  {$ELSE}
  Result := Web.HTTPApp.HTMLEncode(AString);
  {$ENDIF}
end;

function URLDecode(const AEncoded: string): string;
//var
//  I : integer;
begin
  {$IFDEF D21+}
  Result := TNetEncoding.URL.Decode(AEncoded);
  {$ELSE}
  Result := Web.HTTPApp.HTTPDecode(AEncoded);
  {$ENDIF}
//  Result := AEncoded;
//  I := pos('%', Result);
//  while I <> 0 do begin
//    Result[I] := chr(StrToIntDef('$' + copy(Result, I+1, 2), 32));
//    Delete(Result, I+1, 2);
//    I := pos('%', Result);
//  end;
end;

// URLDecodeUTF8Impl adapted from http://koti.mbnet.fi/akini/delphi/urldecodeutf8/
function URLDecodeUTF8Impl(const s: PAnsiChar; const buf: PWideChar;
      var lenBuf: Cardinal): boolean; stdcall;
var
   sAnsi: ANSIString;    // normal ansi string
   sUtf8: UTF8String;    // utf8-bytes string
   sWide: WideString; // unicode string
   i: Integer;
   len: Cardinal;
   CharCode: Cardinal;
begin
 sAnsi := s; // null-terminated str to pascal str
 SetLength(sUtf8, Length(sAnsi));

 // Convert URLEncoded str to utf8 str, it must
 // use utf8 hex escaping for non us-ascii chars
 //    +      = space
 //    %2A    = *
 //    %C3%84 = Ä (A with diaeresis)
 i := 1;
 len := 1;
 while (i <= Length(sAnsi)) do
  begin
   if (sAnsi[i] <> '%') then
    begin
     if (sAnsi[i] = '+')
      then sUtf8[len] := ' '
      else sUtf8[len] := sAnsi[i];
     Inc(len);
    end else
    begin
     Inc(i); // skip the % char
     try
      CharCode := StrToInt('$' + Copy(string(sAnsi), i, 2));
      sUtf8[len] := AnsiChar(CharCode);
      Inc(len);
     except
     end;
     Inc(i); // skip ESC, another +1 at end of loop
    end;
   Inc(i);
  end;
 Dec(len); // -1 to fix length (num of characters)
 SetLength(sUtf8, len);

 sWide := UTF8ToWideString(sUtf8); // utf8 string to unicode
 len := Length(sWide);

 if Assigned(buf) and (len < lenBuf) then
  begin
   // copy result into the buffer, buffer must have
   // space for last null byte.
   //    lenBuf=num of chars in buffer, not counting null
   if (len > 0)
    then Move(sWide[1], buf^, (len+1) * SizeOf(WideChar));
   lenBuf := len;
   Result := True;
  end else
  begin
   // tell calling program how big the buffer
   // should be to store all decoded characters,
   // including trailing null value.
   if (len > 0)
    then lenBuf := len+1;
   Result := False;
  end;
end;

function URLDecodeUTF8(const AEncoded: string): string;
var
  LBuffer: array [0..2048] of WideChar;
  LBufferLength: Cardinal;
begin
   LBufferLength := Length(LBuffer);
   URLDecodeUTF8Impl(PAnsiChar(AnsiString(AEncoded)), LBuffer, LBufferLength);
   Result := LBuffer;
end;

function URLEncode(const ADecoded: string): string;
//const
//  Allowed = ['A'..'Z','a'..'z', '*', '@', '.', '_', '-', '0'..'9', '$', '!', '''', '(', ')'];
//var
//  I : integer;
begin
  {$IFDEF D21+}
  Result := TNetEncoding.URL.Encode(ADecoded);
  {$ELSE}
  Result := Web.HTTPApp.HTTPEncode(ADecoded);
  {$ENDIF}
//  Result := '';
//  for I := 1 to length(ADecoded) do
//    if CharInSet(ADecoded[I], Allowed) then
//      Result := Result + ADecoded[I]
//    else
//      Result := Result + '%' + IntToHex(ord(ADecoded[I]), 2);
end;

end.
