unit uMd5;

// Unit by Steve Schafer [mailto:s t e v e @ f e n e s t r a . c o m]
// No license information available. Assuming public domain.

{$Q-}
{$R-}

interface

uses
  Windows,
  SysUtils,
  Classes;

type
  TMd5Digest = array[0..3] of LongWord;

  TMd5Stream = class(TStream)
    private
      BlockCount: Int64;
      BufCount: Cardinal;
      Buffer: array[0..15] of LongWord;
      A: LongWord;
      B: LongWord;
      C: LongWord;
      D: LongWord;
      function F(X, Y, Z: LongWord): LongWord;
      function G(X, Y, Z: LongWord): LongWord;
      function H(X, Y, Z: LongWord): LongWord;
      function I(X, Y, Z: LongWord): LongWord;
      function Rotate(L: LongWord; NumBits: Cardinal): LongWord;
    protected
      procedure AddByte(B: Byte); virtual;
      function GetDigest: TMd5Digest; virtual;
      function GetDigestString: String; virtual;
      procedure Initialize; virtual;
      procedure UpdateDigest; virtual;
    public
      constructor Create;
      function Read(var Buffer; Count: Longint): Longint; override;
      function Seek(Offset: Longint; Origin: Word): Longint; override;
      function Write(const Buffer; Count: Longint): Longint; override;
      property Digest: TMd5Digest read GetDigest;
      property DigestString: String read GetDigestString; end;

implementation

const
  T: array[1..64] of LongWord = ($D76AA478, $E8C7B756, $242070DB,
    $C1BDCEEE, $F57C0FAF, $4787C62A, $A8304613, $FD469501, $698098D8,
    $8B44F7AF, $FFFF5BB1, $895CD7BE, $6B901122, $FD987193, $A679438E,
    $49B40821, $F61E2562, $C040B340, $265E5A51, $E9B6C7AA, $D62F105D,
    $02441453, $D8A1E681, $E7D3FBC8, $21E1CDE6, $C33707D6, $F4D50D87,
    $455A14ED, $A9E3E905, $FCEFA3F8, $676F02D9, $8D2A4C8A, $FFFA3942,
    $8771F681, $6D9D6122, $FDE5380C, $A4BEEA44, $4BDECFA9, $F6BB4B60,
    $BEBFBC70, $289B7EC6, $EAA127FA, $D4EF3085, $04881D05, $D9D4D039,
    $E6DB99E5, $1FA27CF8, $C4AC5665, $F4292244, $432AFF97, $AB9423A7,
    $FC93A039, $655B59C3, $8F0CCC92, $FFEFF47D, $85845DD1, $6FA87E4F,
    $FE2CE6E0, $A3014314, $4E0811A1, $F7537E82, $BD3AF235, $2AD7D2BB,
    $EB86D391);

constructor TMd5Stream.Create;
  begin
  inherited Create;
  Initialize end;

procedure TMd5Stream.AddByte(B: Byte);
  begin
  case BufCount mod 4 of
    0:
      Buffer[BufCount div 4] := Buffer[BufCount div 4] or B;
    1:
      Buffer[BufCount div 4] := Buffer[BufCount div 4] or (B shl 8);
    2:
      Buffer[BufCount div 4] := Buffer[BufCount div 4] or (B shl 16);
    3:
      Buffer[BufCount div 4] := Buffer[BufCount div 4] or (B shl 24)
    end;
  Inc(BufCount);
  if BufCount = 64 then
    UpdateDigest end;

function TMd5Stream.F(X, Y, Z: LongWord): LongWord;
  begin
  Result := (X and Y) or ((not X) and Z) end;

function TMd5Stream.G(X, Y, Z: LongWord): LongWord;
  begin
  Result := (X and Z) or (Y and (not Z)) end;

function TMd5Stream.H(X, Y, Z: LongWord): LongWord;
  begin
  Result := X xor Y xor Z end;

function TMd5Stream.I(X, Y, Z: LongWord): LongWord;
  begin
  Result := Y xor (X or (not Z)) end;

function TMd5Stream.GetDigest: TMd5Digest;
  var
    L: Int64;
    MsgLen: Int64;
  begin
  MsgLen := BlockCount * 512 + BufCount * 8;
  AddByte($80);
  L := 0;
  if BufCount > 56 then
    Write(L, SizeOf(L));
  while BufCount < 56 do
    AddByte(0);
  Write(MsgLen, SizeOf(MsgLen));
  Result[0] := A;
  Result[1] := B;
  Result[2] := C;
  Result[3] := D;
  Initialize end;

function TMd5Stream.GetDigestString: String;
  var
    D: TMd5Digest;
  begin
  D := GetDigest;
  SetLength(Result, SizeOf(D));
  Move(D, Result[1], Length(Result)) end;

procedure TMd5Stream.Initialize;
  begin
  BlockCount := 0;
  BufCount := 0;
  FillChar(Buffer, SizeOf(Buffer), 0);
  A := $67452301;
  B := $EFCDAB89;
  C := $98BADCFE;
  D := $10325476 end;

function TMd5Stream.Read(var Buffer; Count: LongInt): LongInt;
  begin
  Result := 0 end;

function TMd5Stream.Rotate(L: LongWord; NumBits: Cardinal): LongWord;
  begin
  Result := (L shl NumBits) + (L shr (32 - NumBits)) end;

function TMd5Stream.Seek(Offset: LongInt; Origin: Word): LongInt;
  begin
  Result := BlockCount * 16 + BufCount end;

procedure TMd5Stream.UpdateDigest;
  var
    AA: LongWord;
    BB: LongWord;
    CC: LongWord;
    DD: LongWord;
  begin
  Inc(BlockCount);
  AA := A;
  BB := B;
  CC := C;
  DD := D;

  A := B + Rotate(A + F(B, C, D) + Buffer[ 0] + T[ 1],  7);
  D := A + Rotate(D + F(A, B, C) + Buffer[ 1] + T[ 2], 12);
  C := D + Rotate(C + F(D, A, B) + Buffer[ 2] + T[ 3], 17);
  B := C + Rotate(B + F(C, D, A) + Buffer[ 3] + T[ 4], 22);
  A := B + Rotate(A + F(B, C, D) + Buffer[ 4] + T[ 5],  7);
  D := A + Rotate(D + F(A, B, C) + Buffer[ 5] + T[ 6], 12);
  C := D + Rotate(C + F(D, A, B) + Buffer[ 6] + T[ 7], 17);
  B := C + Rotate(B + F(C, D, A) + Buffer[ 7] + T[ 8], 22);
  A := B + Rotate(A + F(B, C, D) + Buffer[ 8] + T[ 9],  7);
  D := A + Rotate(D + F(A, B, C) + Buffer[ 9] + T[10], 12);
  C := D + Rotate(C + F(D, A, B) + Buffer[10] + T[11], 17);
  B := C + Rotate(B + F(C, D, A) + Buffer[11] + T[12], 22);
  A := B + Rotate(A + F(B, C, D) + Buffer[12] + T[13],  7);
  D := A + Rotate(D + F(A, B, C) + Buffer[13] + T[14], 12);
  C := D + Rotate(C + F(D, A, B) + Buffer[14] + T[15], 17);
  B := C + Rotate(B + F(C, D, A) + Buffer[15] + T[16], 22);

  A := B + Rotate(A + G(B, C, D) + Buffer[ 1] + T[17],  5);
  D := A + Rotate(D + G(A, B, C) + Buffer[ 6] + T[18],  9);
  C := D + Rotate(C + G(D, A, B) + Buffer[11] + T[19], 14);
  B := C + Rotate(B + G(C, D, A) + Buffer[ 0] + T[20], 20);
  A := B + Rotate(A + G(B, C, D) + Buffer[ 5] + T[21],  5);
  D := A + Rotate(D + G(A, B, C) + Buffer[10] + T[22],  9);
  C := D + Rotate(C + G(D, A, B) + Buffer[15] + T[23], 14);
  B := C + Rotate(B + G(C, D, A) + Buffer[ 4] + T[24], 20);
  A := B + Rotate(A + G(B, C, D) + Buffer[ 9] + T[25],  5);
  D := A + Rotate(D + G(A, B, C) + Buffer[14] + T[26],  9);
  C := D + Rotate(C + G(D, A, B) + Buffer[ 3] + T[27], 14);
  B := C + Rotate(B + G(C, D, A) + Buffer[ 8] + T[28], 20);
  A := B + Rotate(A + G(B, C, D) + Buffer[13] + T[29],  5);
  D := A + Rotate(D + G(A, B, C) + Buffer[ 2] + T[30],  9);
  C := D + Rotate(C + G(D, A, B) + Buffer[ 7] + T[31], 14);
  B := C + Rotate(B + G(C, D, A) + Buffer[12] + T[32], 20);

  A := B + Rotate(A + H(B, C, D) + Buffer[ 5] + T[33],  4);
  D := A + Rotate(D + H(A, B, C) + Buffer[ 8] + T[34], 11);
  C := D + Rotate(C + H(D, A, B) + Buffer[11] + T[35], 16);
  B := C + Rotate(B + H(C, D, A) + Buffer[14] + T[36], 23);
  A := B + Rotate(A + H(B, C, D) + Buffer[ 1] + T[37],  4);
  D := A + Rotate(D + H(A, B, C) + Buffer[ 4] + T[38], 11);
  C := D + Rotate(C + H(D, A, B) + Buffer[ 7] + T[39], 16);
  B := C + Rotate(B + H(C, D, A) + Buffer[10] + T[40], 23);
  A := B + Rotate(A + H(B, C, D) + Buffer[13] + T[41],  4);
  D := A + Rotate(D + H(A, B, C) + Buffer[ 0] + T[42], 11);
  C := D + Rotate(C + H(D, A, B) + Buffer[ 3] + T[43], 16);
  B := C + Rotate(B + H(C, D, A) + Buffer[ 6] + T[44], 23);
  A := B + Rotate(A + H(B, C, D) + Buffer[ 9] + T[45],  4);
  D := A + Rotate(D + H(A, B, C) + Buffer[12] + T[46], 11);
  C := D + Rotate(C + H(D, A, B) + Buffer[15] + T[47], 16);
  B := C + Rotate(B + H(C, D, A) + Buffer[ 2] + T[48], 23);

  A := B + Rotate(A + I(B, C, D) + Buffer[ 0] + T[49],  6);
  D := A + Rotate(D + I(A, B, C) + Buffer[ 7] + T[50], 10);
  C := D + Rotate(C + I(D, A, B) + Buffer[14] + T[51], 15);
  B := C + Rotate(B + I(C, D, A) + Buffer[ 5] + T[52], 21);
  A := B + Rotate(A + I(B, C, D) + Buffer[12] + T[53],  6);
  D := A + Rotate(D + I(A, B, C) + Buffer[ 3] + T[54], 10);
  C := D + Rotate(C + I(D, A, B) + Buffer[10] + T[55], 15);
  B := C + Rotate(B + I(C, D, A) + Buffer[ 1] + T[56], 21);
  A := B + Rotate(A + I(B, C, D) + Buffer[ 8] + T[57],  6);
  D := A + Rotate(D + I(A, B, C) + Buffer[15] + T[58], 10);
  C := D + Rotate(C + I(D, A, B) + Buffer[ 6] + T[59], 15);
  B := C + Rotate(B + I(C, D, A) + Buffer[13] + T[60], 21);
  A := B + Rotate(A + I(B, C, D) + Buffer[ 4] + T[61],  6);
  D := A + Rotate(D + I(A, B, C) + Buffer[11] + T[62], 10);
  C := D + Rotate(C + I(D, A, B) + Buffer[ 2] + T[63], 15);
  B := C + Rotate(B + I(C, D, A) + Buffer[ 9] + T[64], 21);

  A := A + AA;
  B := B + BB;
  C := C + CC;
  D := D + DD;
  BufCount := 0;
  FillChar(Buffer, SizeOf(Buffer), 0) end;

function TMd5Stream.Write(const Buffer; Count: LongInt): LongInt;
  var
    I: Cardinal;
    P: PByteArray;
  begin
  P := @Buffer;
  for I := 0 to Count - 1 do
    AddByte(P^[I]);
  Result := Count end;

end.
