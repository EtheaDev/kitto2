(**********************************************************
*                                                         *
*     Perl Regular Expressions                            *
*                                                         *
*     Delphi wrapper around PCRE  -  http://www.pcre.org  *
*                                                         *
*     Copyright (C) 1999-2004 Jan Goyvaerts               *
*                                                         *
*     Design & implementation Jan Goyvaerts 1999-2001     *
*     Updated for PCRE 4.5, by Jan Goyvaerts, 2004        *
*                                                         *
**********************************************************)

// CHELPES.PAS include file

// These functions are required by pcre.obj and study.obj
// Unlike the DLL, this unit is not being linked agains the standard C libraries, so we have to provide them ourselves


{ Memory Allocation }

function _malloc(Size: Integer): Pointer; cdecl;
begin
  GetMem(Result, Size);
end;

procedure _free(P: Pointer); cdecl;
begin
  FreeMem(P);
end;

{ Memory Operation }

function _memset(P: Pointer; B: Byte; N: Integer): Pointer; cdecl;
begin
  FillChar(P^, N, B);
  Result := P
end;

function _memcpy(Dest, Src: Pointer; N: Integer): Pointer; cdecl;
begin
  Move(Src^, Dest^, N);
  Result := Dest;
end;

function _memmove(Dest, Src: Pointer; N: Integer): Pointer; cdecl;
begin
  Move(Src^, Dest^, N);
  Result := Dest;
end;

function _memcmp(S1, S2: PByteArray; N: Integer): Integer; cdecl;
var
  I: Integer;
begin
  I := 0;
  while (I < N) and (S1[I] = S2[I]) do Inc(I);
  if I >= N then Result := 0
    else if S1[I] < S2[I] then Result := -1
    else Result := 1
end;

{ Characters & Strings }

function _isalnum (c: AnsiChar): Boolean; cdecl;
begin
  Result := IsCharAlphaNumeric (c);
end;

function _isalpha (c: AnsiChar): Boolean; cdecl;
begin
  Result := IsCharAlpha (c);
end;

function _iscntrl (c: AnsiChar): Boolean; cdecl;
var
  CharType: Word;
begin
  GetStringTypeExA (LOCALE_USER_DEFAULT, CT_CTYPE1, @c, SizeOf (c), CharType);
  Result := (CharType and C1_CNTRL) <> 0;
end;

function _isdigit (c: AnsiChar): Boolean; cdecl;
var
  CharType: Word;
begin
  GetStringTypeExA (LOCALE_USER_DEFAULT, CT_CTYPE1, @c, SizeOf (c), CharType);
  Result := Boolean (CharType and C1_DIGIT);
end;

function _isgraph (c: AnsiChar): Boolean; cdecl;
begin
  Result := IsCharAlphaNumeric (c);
end;

function _islower (c: AnsiChar): Boolean; cdecl;
begin
  Result := IsCharLower (c);
end;

function _isprint (c: AnsiChar): Boolean; cdecl;
var
  CharType: Word;
begin
  GetStringTypeExA (LOCALE_USER_DEFAULT, CT_CTYPE1, @c, SizeOf (c), CharType);
  Result := (CharType and C1_CNTRL) = 0;
end;

function _ispunct (c: AnsiChar): Boolean; cdecl;
var
  CharType: Word;
begin
  GetStringTypeExA (LOCALE_USER_DEFAULT, CT_CTYPE1, @c, SizeOf (c), CharType);
  Result := (CharType and C1_PUNCT) <> 0;
end;

function _isspace (c: AnsiChar): Boolean; cdecl;
var
  CharType: Word;
begin
  GetStringTypeExA (LOCALE_USER_DEFAULT, CT_CTYPE1, @c, SizeOf (c), CharType);
  Result := (CharType and C1_SPACE) <> 0;
end;

function _isupper (c: AnsiChar): Boolean; cdecl;
begin
  Result := IsCharUpper (c);
end;

function _isxdigit (c: AnsiChar): Boolean; cdecl;
var
  CharType: Word;
begin
  GetStringTypeExA (LOCALE_USER_DEFAULT, CT_CTYPE1, @c, SizeOf (c), CharType);
  Result := (CharType and C1_XDIGIT) <> 0;
end;

function __ltolower (c: AnsiChar): AnsiChar; cdecl;
begin
  CharLower (@c);
  Result := c;
end;

function __ltoupper (c: AnsiChar): AnsiChar; cdecl;
begin
  CharUpper (@c);
  Result := c;
end;

function _strchr(S: PAnsiChar; C: AnsiChar): PAnsiChar; cdecl;
begin
  Result := StrScan(S, C);
end;

function _strncmp (const s1, s2: PAnsiChar; MaxLen: Cardinal): Integer; cdecl;
begin
  Result := StrLComp (s1, s2, MaxLen);
end;
