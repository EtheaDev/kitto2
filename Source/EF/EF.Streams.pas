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
///	  This unit contains a set of stream decorators to add buffering, textline
///	  support and other features to rtl streams.
///	</summary>
///	<remarks>
///	  Some code in this unit is based on code by Julian M. Bucknall and
///	  published on The Delphi Magazine.
///	</remarks>
unit EF.Streams;

{$I EF.Defines.inc}

interface

uses
  Classes, Generics.Collections;
  
type
  ///	<summary>
  ///	  Base class for all EF stream decorators and filters. It just forwards
  ///	  read and write requestes to an internal stream object, a reference to
  ///	  which is passed to the constructor.
  ///	</summary>
  TEFStreamDecorator = class(TStream)
  private
    FStream: TStream;
    FOwnsStream: Boolean;
    FOnEndOfStream: TNotifyEvent;
  protected
    property Stream: TStream read FStream;

    ///	<summary>
    ///	  Fires OnEndOfStream. Descendants that override Read without calling
    ///	  inherited are required to call this method to signal that the stream
    ///	  is over.
    ///	</summary>
    procedure DoEndOfStream;
  public
    ///	<summary>
    ///	  Creates an object that decorates AStream. If AOwnsStream is True,
    ///	  then the decorator acquires ownership of the stream and will destroy
    ///	  it when it is itself destroyed.
    ///	</summary>
    constructor Create(const AStream: TStream; const AOwnsStream: Boolean = True);
    destructor Destroy; override;
    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;

    ///	<summary>
    ///	  Fired when the end of the stream is reached while reading. IOW, when
    ///	  Read's return value is less than its Count argument. Handle this
    ///	  event if you do not have control over reads but still want to be
    ///	  notified when the stream is over.
    ///	</summary>
    property OnEndOfStream: TNotifyEvent read FOnEndOfStream write FOnEndOfStream;
  end;

  ///	<summary>
  ///	  A stream filter that only allows sequential reading.
  ///	</summary>
  TEFReadFilter = class(TEFStreamDecorator)
  private
    FGettingSize: Boolean;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  ///	<summary>
  ///	  Adds buffering to the read stream filter. Use this class for efficient
  ///	  reading from a file or other medium.
  ///	</summary>
  TEFBufferedReadFilter = class(TEFReadFilter)
  private
    FBuffer: PChar;
    FBufferLength: Longint;
    FBufferPosition: Longint;
  protected
    ///	<summary>
    ///	  Reads new data from the internal stream into the buffer and resets
    ///	  the buffer's current position. Returns True when it has read at least
    ///	  one byte, False otherwise.
    ///	</summary>
    function ReadNextBuffer: Boolean;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  ///	<summary>
  ///	  A stream decorator that is able to read and write text lines to the
  ///	  decorated stream. Use it with a buffered read filter to efficiently
  ///	  read text lines from a file.
  ///	</summary>
  TEFTextStream = class(TEFStreamDecorator)
  private
    FLineBreak: string;
  public
    procedure AfterConstruction; override;
  public
    const
      ///	<summary>
      ///	  ReadLn returns this value when there's no more text.
      ///	</summary>
      EOT: string = #4;

    ///	<summary>
    ///	  <para>
    ///	    Line breaking sequence used to terminate lines written by WriteLn.
    ///	    Defaults to sLineBreak.
    ///	  </para>
    ///	  <para>
    ///	    Note: Currently it is NOT used by ReadLn.
    ///	  </para>
    ///	</summary>
    property LineBreak: string read FLineBreak write FLineBreak;

    ///	<summary>
    ///	  <para>
    ///	    Reads text from the current position up to (but not including) the
    ///	    next LF character, skipping any CR characters found. This supports
    ///	    both LF (Linux) and CR+LF (Windows) line breaking styles. It
    ///	    doesn't currently support the CR-only line breaking style. Returns
    ///	    EOT when there's no more text.
    ///	  </para>
    ///	  <para>
    ///	    Note: the value of the LineBreak property is ignored.
    ///	  </para>
    ///	</summary>
    function ReadLn: string;

    ///	<summary>
    ///	  Writes AString plus LineBreak to the stream.
    ///	</summary>
    procedure WriteLn(const AString: string);
  end;

type
  ///	<summary>A stream decorator that is capable of outputting XML data such
  ///	as tags and attributes, keeping track of the indent.</summary>
  ///	<remarks>It only supports UTF-8 encoding.</remarks>
  TEFXMLOutputStream = class(TEFTextStream)
  private
    ///	<summary>The indentation level used by WriteLnIndented is calculated
    ///	based on the number of open tags. When this stream is used to write a
    ///	piece of XML code (as opposed to the complete document), it might be
    ///	useful to set an additional indent level; that's what this class does
    ///	when it decorates a stream of the same type.</summary>
    FAdditionalIndentLevel: Integer;
    FOpenTags: TStack<string>;
    function GetIndentLevel: Integer;
    function GetOpenTags: TStack<string>;

    ///	<summary>Encodes the attribute names and values as a string suitable
    ///	for inclusion in a XML tag. If the arrays are empty, returns '',
    ///	otherwise the return string includes a leading ' '.</summary>
    function EncodeAttributes(const AAttributeNames,
      AAttributeValues: array of string): string;
    property OpenTags: TStack<string> read GetOpenTags;
    procedure WriteLnIndented(const AString: string);

    ///	<summary>Encodes AString. Call this method before writing anything (tag
    ///	names, attribute names and values, characters). WriteLnIndented
    ///	automatically calls ithis method. Currently only supports UTF-8
    ///	encoding.</summary>
    function EncodeString(const AString: string): string;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    ///	<summary>Writes the XML prolog. Call this before writing anything else
    ///	if you are producing a well-formed XML document.</summary>
    procedure WriteProlog;

    ///	<summary>Opens a tag, optionally with attributes. AAttributeNames must
    ///	have the same length as AAttributeValues.</summary>
    procedure OpenTag(const ATagName: string); overload;
    procedure OpenTag(const ATagName: string;
      const AAttributeNames: array of string;
      const AAttributeValues: array of string); overload;

    ///	<summary>Opens a tag, writes ATagCharacters and closes it all in a
    ///	single call. If ATagCharacter is empty, it uses the "&lt;ATagName
    ///	/&gt;" syntax. Optionally writes also the attributes in the opening tag
    ///	(in which case the tag cannot be empty).</summary>
    procedure WriteTag(const ATagName: string; const ATagCharacters: string = ''); overload;
    procedure WriteTag(const ATagName: string;
      const AAttributeNames: array of string;
      const AAttributeValues: array of string;
      const ATagCharacters: string = ''); overload;

    ///	<summary>Like WriteTag, but embeds ATagCharacters in a CDATA
    ///	section.</summary>
    procedure WriteCDATATag(const ATagName, ATagCharacters: string);

    ///	<summary>Closes the last opened tag, if any, and returns its name. If
    ///	no tag to close was found, it returns ''.</summary>
    function CloseTag: string;

    ///	<summary>Iteratively calls CloseTag until the stack of open tags is
    ///	empty. It is called automatically upon destruction.</summary>
    procedure CloseAllOpenTags;
  end;

implementation

uses
  SysUtils, StrUtils, Math;

const
  {
    Default buffer size for buffered stream filters.
  }
  BUFFER_SIZE = 16384;

{ TEFStreamDecorator }

constructor TEFStreamDecorator.Create(const AStream: TStream;
  const AOwnsStream: Boolean = True);
begin
  Assert(Assigned(AStream));

  inherited Create;
  FStream := AStream;
  FOwnsStream := AOwnsStream;
end;

destructor TEFStreamDecorator.Destroy;
begin
  if FOwnsStream then
    FreeAndNil(FStream);
  inherited;
end;

procedure TEFStreamDecorator.DoEndOfStream;
begin
  if Assigned(FOnEndOfStream) then
    FOnEndOfStream(Self);
end;

function TEFStreamDecorator.Read(var Buffer; Count: Integer): Integer;
begin
  Result := FStream.Read(Buffer, Count);
  if Result < Count then
    DoEndOfStream;
end;

function TEFStreamDecorator.Seek(Offset: Integer; Origin: Word): Longint;
begin
  Result := FStream.Seek(Offset, Origin);
end;

function TEFStreamDecorator.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  Result := FStream.Seek(Offset, Origin);
end;

function TEFStreamDecorator.Write(const Buffer; Count: Integer): Integer;
begin
  Result := FStream.Write(Buffer, Count);
end;

{ TEFReadFilter }

function TEFReadFilter.Read(var Buffer; Count: Longint): Longint;
begin
  Assert(not FGettingSize);

  Result := inherited Read(Buffer, Count);
end;

function TEFReadFilter.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  // Intercept and handle the special Seek calls, that is those made while
  // getting the stream's size.
  case Origin of
    soBeginning:
      if FGettingSize then
      begin
        Result := FStream.Position;
        if Offset = Result then
          Exit;
        FGettingSize := False;
      end;
    soCurrent:
      if (Offset = 0) and not FGettingSize then
      begin
        Result := FStream.Position;
        Exit;
      end;
    soEnd:
      if (Offset = 0) and not FGettingSize then
      begin
        Result := FStream.Size;
        FGettingSize := True;
        Exit;
      end;
  end;
  raise Exception.Create('Seek not available in a read stream filter.');
end;

function TEFReadFilter.Write(const Buffer; Count: Longint): Longint;
begin
  raise Exception.Create('Write not available in a read stream filter.');
end;

{ TEFBufferedReadFilter }

procedure TEFBufferedReadFilter.AfterConstruction;
begin
  inherited;
  GetMem(FBuffer, BUFFER_SIZE);
end;

destructor TEFBufferedReadFilter.Destroy;
begin
  FreeMem(FBuffer, BUFFER_SIZE);
  inherited;
end;

function TEFBufferedReadFilter.ReadNextBuffer: Boolean;
begin
  FBufferLength := FStream.Read(FBuffer^, BUFFER_SIZE);
  FBufferPosition := 0;
  Result := FBufferLength <> 0;
end;

function TEFBufferedReadFilter.Read(var Buffer; Count: Longint): Longint;
var
  LPCharBuffer: PChar;
  LBytesToRead: Longint;
  LBytesToCopy: Longint;

  {
    Calculates and returns the number of bytes to copy from the internal
    buffer, depending on the requested number of bytes.
  }
  function HowManyBytesToCopy(const ARequestedBytes: Longint): Longint;
  begin
    Result := Min(FBufferLength - FBufferPosition, ARequestedBytes);
  end;

begin
  LPCharBuffer := @Buffer;
  Result := 0;
  // Fill the internal buffer if required.
  if FBufferPosition = FBufferLength then
    if not ReadNextBuffer then
      Exit;
  LBytesToRead := Count;
  // How many bytes should we copy from the internal buffer?
  LBytesToCopy := Min(FBufferLength - FBufferPosition, LBytesToRead);
  // Copy 'em.
  Move(FBuffer[FBufferPosition], LPCharBuffer^, LBytesToCopy);
  Inc(Result, LBytesToCopy);
  // Once the bytes are copied, adjust the counters.
  Inc(FBufferPosition, LBytesToCopy);
  Dec(LBytesToRead, LBytesToCopy);
  // Still bytes to read? Read and copy 'em.
  while LBytesToRead <> 0 do
  begin
    // Go right after the data we just copied.
    Inc(LPCharBuffer, LBytesToCopy);
    // The internal buffer was copied entirely, so read another one.
    if not ReadNextBuffer then
    begin
      if Result < Count then
        DoEndOfStream;
      Exit;
    end;
    // How many bytes should we copy from the internal buffer?
    LBytesToCopy := Min(FBufferLength - FBufferPosition, LBytesToRead);
    // Copy 'em.
    Move(FBuffer^, LPCharBuffer^, LBytesToCopy);
    Inc(Result, LBytesToCopy);
    // Once the bytes are copied, adjust the counters.
    Inc(FBufferPosition, LBytesToCopy);
    Dec(LBytesToRead, LBytesToCopy);
  end;
  if Result < Count then
    DoEndOfStream;
end;

function TEFBufferedReadFilter.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  if (Offset = 0) and (Origin = soCurrent) then
    Result := FStream.Position - FBufferLength + FBufferPosition
  else
    Result := inherited Seek(Offset, Origin);
end;

{ TEFTextStream }

procedure TEFTextStream.AfterConstruction;
begin
  inherited;
  FLineBreak := sLineBreak;
end;

function TEFTextStream.ReadLn: string;
const
  CR = #13;
  LF = #10;
var
  LCurrentChar: Char;
  LBytesRead: Longint;
begin
  Result := '';
  LBytesRead := Read(LCurrentChar, SizeOf(LCurrentChar));
  if LBytesRead = 0 then
  begin
    Result := EOT;
    Exit;
  end;
  while (LBytesRead <> 0) and (LCurrentChar <> LF) do
  begin
    if LCurrentChar <> CR then
      Result := Result + LCurrentChar;
    LBytesRead := Read(LCurrentChar, SizeOf(LCurrentChar));
  end;
end;

procedure TEFTextStream.WriteLn(const AString: string);
var
  LBuffer: UTF8String;
begin
  LBuffer := UTF8String(AString + FLineBreak);
  Write(LBuffer[1], Length(LBuffer));
end;

{ TEFXMLOutputStream }

procedure TEFXMLOutputStream.AfterConstruction;
begin
  inherited;
  if Stream is TEFXMLOutputStream then
    FAdditionalIndentLevel := TEFXMLOutputStream(Stream).GetIndentLevel;
end;

procedure TEFXMLOutputStream.CloseAllOpenTags;
begin
  while CloseTag <> '' do
    ;
end;

function TEFXMLOutputStream.CloseTag: string;
begin
  if OpenTags.Count = 0 then
    Result := ''
  else
  begin
    Result := OpenTags.Pop;
    WriteLnIndented('</' + Result + '>');
  end;
end;

destructor TEFXMLOutputStream.Destroy;
begin
  CloseAllOpenTags;
  FreeAndNil(FOpenTags);
  inherited;
end;

function TEFXMLOutputStream.GetIndentLevel: Integer;
begin
  Result := OpenTags.Count * 2;
end;

function TEFXMLOutputStream.GetOpenTags: TStack<string>;
begin
  if not Assigned(FOpenTags) then
    FOpenTags := TStack<string>.Create;
  Result := FOpenTags;
end;

function TEFXMLOutputStream.EncodeAttributes(const AAttributeNames: array of string;
  const AAttributeValues: array of string): string;
var
  LAttributeIndex: Integer;
begin
  Assert(Length(AAttributeNames) = Length(AAttributeValues));
  if Length(AAttributeNames) = 0 then
    Result := ''
  else
  begin
    Result := ' ';
    for LAttributeIndex := Low(AAttributeNames) to High(AAttributeNames) do
      Result := Result + AAttributeNames[LAttributeIndex] + '="' +
        AAttributeValues[LAttributeIndex] + '" ';
    // Remove the trailing space.
    Delete(Result, Length(Result), 1);
  end;
end;

function TEFXMLOutputStream.EncodeString(const AString: string): string;
begin
  // See TEFTextStream.WriteLn.
  {$IFDEF UNICODE}
  Result := AString;
  {$ELSE}
  Result := AnsiToUtf8(AString);
  {$ENDIF}
end;

procedure TEFXMLOutputStream.OpenTag(const ATagName: string;
  const AAttributeNames: array of string;
  const AAttributeValues: array of string);
begin
  WriteLnIndented('<' + ATagName
    + EncodeAttributes(AAttributeNames, AAttributeValues) + '>');
  OpenTags.Push(ATagName);
end;

procedure TEFXMLOutputStream.OpenTag(const ATagName: string);
begin
  OpenTag(ATagName, [], []);
end;

procedure TEFXMLOutputStream.WriteTag(const ATagName: string;
  const AAttributeNames, AAttributeValues: array of string;
  const ATagCharacters: string);
begin
  if ATagCharacters <> '' then
    WriteLnIndented('<' + ATagName
      + EncodeAttributes(AAttributeNames, AAttributeValues) + '>'
      + ATagCharacters + '</' + ATagName + '>')
  else
    WriteLnIndented('<' + ATagName
      + EncodeAttributes(AAttributeNames, AAttributeValues) + ' />');
end;

procedure TEFXMLOutputStream.WriteTag(const ATagName: string;
  const ATagCharacters: string = '');
begin
  WriteTag(ATagName, [], [], ATagCharacters);
end;

procedure TEFXMLOutputStream.WriteCDATATag(const ATagName: string;
  const ATagCharacters: string);
begin
  WriteTag(ATagName, [], [], '<![CDATA[' + ATagCharacters + ']]>');
end;

procedure TEFXMLOutputStream.WriteLnIndented(const AString: string);
var
  LIndentStr: string;
begin
  LIndentStr := DupeString(' ', GetIndentLevel + FAdditionalIndentLevel);
  WriteLn(LIndentStr + EncodeString(AString));
end;

procedure TEFXMLOutputStream.WriteProlog;
begin
  WriteLn('<?xml version="1.0" encoding="UTF-8" ?>');
end;

end.
