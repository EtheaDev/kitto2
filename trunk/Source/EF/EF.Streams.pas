{*******************************************************************}
{                                                                   }
{   Ethea Foundation                                                }
{   Stream-related classes, decorators, filters and utilities       }
{                                                                   }
{   Copyright (c) 2006-2010 Ethea Srl                               }
{   ALL RIGHTS RESERVED / TUTTI I DIRITTI RISERVATI                 }
{                                                                   }
{*******************************************************************}
{                                                                   }
{   The entire contents of this file is protected by                }
{   International Copyright Laws. Unauthorized reproduction,        }
{   reverse-engineering, and distribution of all or any portion of  }
{   the code contained in this file is strictly prohibited and may  }
{   result in severe civil and criminal penalties and will be       }
{   prosecuted to the maximum extent possible under the law.        }
{                                                                   }
{   RESTRICTIONS                                                    }
{                                                                   }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED      }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE        }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE       }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT  }
{   AND PERMISSION FROM ETHEA S.R.L.                                }
{                                                                   }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON       }
{   ADDITIONAL RESTRICTIONS.                                        }
{                                                                   }
{*******************************************************************}
{                                                                   }
{   Il contenuto di questo file è protetto dalle leggi              }
{   internazionali sul Copyright. Sono vietate la riproduzione, il  }
{   reverse-engineering e la distribuzione non autorizzate di tutto }
{   o parte del codice contenuto in questo file. Ogni infrazione    }
{   sarà perseguita civilmente e penalmente a termini di legge.     }
{                                                                   }
{   RESTRIZIONI                                                     }
{                                                                   }
{   SONO VIETATE, SENZA IL CONSENSO SCRITTO DA PARTE DI             }
{   ETHEA S.R.L., LA COPIA, LA VENDITA, LA DISTRIBUZIONE E IL       }
{   TRASFERIMENTO A TERZI, A QUALUNQUE TITOLO, DEL CODICE SORGENTE  }
{   CONTENUTO IN QUESTO FILE E ALTRI FILE AD ESSO COLLEGATI.        }
{                                                                   }
{   SI FACCIA RIFERIMENTO ALLA LICENZA D'USO PER INFORMAZIONI SU    }
{   EVENTUALI RESTRIZIONI ULTERIORI.                                }
{                                                                   }
{*******************************************************************} 

{
  This unit contains a set of stream decorators to add buffering, textline
  support and other features to VCL streams.
}
unit EF.Streams;

{
  Some code in this unit is based on code by Julian M. Bucknall and
  published on The Delphi Magazine.
}

interface

uses
  Classes;
  
type
  {
    Base class for all EF stream decorators and filters. It just forwards read
    and write requestes to an internal stream object, a reference to which
    is passed to the constructor.
  }
  TEFStreamDecorator = class(TStream)
  private
    FStream: TStream;
    FOwnsStream: Boolean;
    FOnEndOfStream: TNotifyEvent;
  protected
    property Stream: TStream read FStream;
    {
      Fires OnEndOfStream. Descendants that override Read without calling
      inherited are required to call this method to signal that the stream is
      over.
    }
    procedure DoEndOfStream;
  public
    {
      Creates an object that decorates AStream. If AOwnsStream is True, then
      the decorator acquires ownership of the stream and will destroy it when
      it is itself destroyed.
    }
    constructor Create(const AStream: TStream; const AOwnsStream: Boolean = True);
    destructor Destroy; override;
    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    {
      Fired when the end of the stream is reached while reading. IOW, when
      Read's return value is less than its Count argument. Handle this event if
      you do not have control over reads but still want to be notified when
      the stream is over. 
    }
    property OnEndOfStream: TNotifyEvent read FOnEndOfStream write FOnEndOfStream;
  end;

  {
    A stream filter that only allows sequential reading.
  }
  TEFReadFilter = class(TEFStreamDecorator)
  private
    FGettingSize: Boolean;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  {
    Adds buffering to the read stream filter. Use this class for efficient
    reading from a file or other medium.
  }
  TEFBufferedReadFilter = class(TEFReadFilter)
  private
    FBuffer: PChar;
    FBufferLength: Longint;
    FBufferPosition: Longint;
  protected
    {
      Reads new data from the internal stream into the buffer and resets the
      buffer's current position. Returns True when it has read at least one
      byte, False otherwise.
    }
    function ReadNextBuffer: Boolean;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  {
    A stream decorator that is able to read and write text lines to the
    decorated stream. Use it with a buffered read filter to efficiently
    read text lines from a file.
  }
  TEFTextStream = class(TEFStreamDecorator)
  private
    FLineBreak: string;
  public
    {
      ReadLn returns this value when there's no more text.
    }
    const EOT: string = #4;

    procedure AfterConstruction; override;
    {
      Line breaking sequence used to terminate lines written by WriteLn.
      Defaults to sLineBreak.

      Note: Currently it is NOT used by ReadLn.
    }
    property LineBreak: string read FLineBreak write FLineBreak;
    {
      Reads text from the current position up to (but not including) the
      next LF character, skipping any CR characters found. This supports both
      LF (Linux) and CR+LF (Windows) line breaking styles. It doesn't currently
      support the CR-only line breaking style.
      Returns EOT when there's no more text.

      Note: the value of the LineBreak property is ignored.
    }
    function ReadLn: string;
    {
      Writes AString plus LineBreak to the stream.
    }
    procedure WriteLn(const AString: string);
  end;

implementation

uses
  SysUtils, Math;

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

end.
