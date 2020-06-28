unit hash;
{
  Fast hash functions
  http://www.sanmayce.com/Fastest_Hash/
}

{$ifdef FPC}
{$mode delphi}
{$endif}

{$inline on}
{$pointermath on}

interface

uses
  Classes, SysUtils;

function Hash_DJB(key: PChar; keyLen: NativeUInt): UInt32; inline;

function FNV1A_Hash_Jesteress(key: PChar; keyLen: NativeUInt): UInt32; inline;
function FNV1A_Hash_Meiyan(key: PChar; keyLen: NativeUInt): UInt32; inline;
function FNV1A_Hash_Mantis(key: PChar; keyLen: NativeUInt): UInt32; inline;
function FNV1A_Hash_Yorikke(key: PChar; keyLen: NativeUInt): UInt32; inline;

implementation
{$ifdef FPC}
{$macro on}
{$define PRIME_A := 709607}
{$define BASE_A := 2166136261}
{$else}
const
  PRIME_A = 709607;
  BASE_A = 2166136261;
{$endif}

{$Q-}{$R-}

function Hash_DJB(key: PChar; keyLen: NativeUInt): UInt32;
var
  hash32: UInt32;
begin
  hash32 := 5381;
  while keyLen > 0 do
  begin
    hash32 := (hash32 shl 5) + hash32 + PByte(key)^;
    Inc(key);
    Dec(keyLen);
  end;

  Result := hash32;
end;

function FNV1A_Hash_Jesteress(key: PChar; keyLen: NativeUInt): UInt32; inline;
var
  hash32: UInt32;
begin
  hash32 := BASE_A;

  while keyLen >= SizeOf(UInt64) do (* 8 *)
  begin
    hash32 := (hash32 xor (
      (* FPC RolDWord() *)
      (((PUInt32(key))^ shl 5) or ((PUInt32(key))^ shr 27)) xor
      (PUInt32(key + SizeOf(UInt32)))^ (* 4 *)
      )) * PRIME_A;
    Dec(keyLen, SizeOf(UInt64)); (* 8 *)
    Inc(key, SizeOf(UInt64)); (* 8 *)
  end;

  (* Remaining bytes - 0..7 *)
  if (keyLen and SizeOf(UInt32)) <> 0 then (* 4 *)
  begin
    hash32 := (hash32 xor (PUInt32(key))^) * PRIME_A;
    Inc(key, SizeOf(UInt32)); (* 4 *)
  end;

  if (keyLen and SizeOf(Word)) <> 0 then (* 2 *)
  begin
    hash32 := (hash32 xor (PWord(key))^) * PRIME_A;
    Inc(key, SizeOf(Word)); (* 2 *)
  end;

  if (keyLen and SizeOf(Byte)) <> 0 then (* 1 *)
    hash32 := (hash32 xor (PByte(key))^) * PRIME_A;

  Result := hash32 xor (hash32 shr 16);
end;

function FNV1A_Hash_Meiyan(key: PChar; keyLen: NativeUInt): UInt32; inline;
var
  hash32: UInt32;
begin
  hash32 := BASE_A;

  while keyLen >= SizeOf(UInt64) do (* 8 *)
  begin
    hash32 := (hash32 xor (
      (* FPC RolDWord() *)
      (((PUInt32(key))^ shl 5) or ((PUInt32(key))^ shr 27)) xor
      (PUInt32(key + SizeOf(UInt32)))^ (* 4 *)
      )) * PRIME_A;

    Dec(keyLen, SizeOf(UInt64)); (* 8 *)
    Inc(key, SizeOf(UInt64)); (* 8 *)
  end;

  (* Remaining bytes - 0..7 *)
  if (keyLen and SizeOf(UInt32)) <> 0 then (* 4 *)
  begin
    hash32 := (hash32 xor (PWord(key))^) * PRIME_A;
    Inc(key, SizeOf(Word)); (* 2 *)
    hash32 := (hash32 xor (PWord(key))^) * PRIME_A;
    Inc(key, SizeOf(Word)); (* 2 *)
  end;

  if (keyLen and SizeOf(Word)) <> 0 then (* 2 *)
  begin
    hash32 := (hash32 xor (PWord(key))^) * PRIME_A;
    Inc(key, SizeOf(Word)); (* 2 *)
  end;

  if (keyLen and SizeOf(Byte)) <> 0 then (* 1 *)
    hash32 := (hash32 xor (PByte(key))^) * PRIME_A;

  Result := hash32 xor (hash32 shr 16);
end;

function FNV1A_Hash_Mantis(key: PChar; keyLen: NativeUInt): UInt32; inline;
var
  hash32: UInt32;
  p: PChar;
begin
  hash32 := BASE_A;
  p := key;

  (* Leading bytes - 0..7 *)
  if (keyLen and SizeOf(UInt32)) <> 0 then (* 4 *)
  begin
    hash32 := (hash32 xor (PWord(p))^) * PRIME_A;
    Inc(p, SizeOf(Word)); (* 2 *)
    hash32 := (hash32 xor (PWord(p))^) * PRIME_A;
    Inc(p, SizeOf(Word)); (* 2 *)
  end;

  if (keyLen and SizeOf(Word)) <> 0 then (* 2 *)
  begin
    hash32 := (hash32 xor (PWord(p))^) * PRIME_A;
    Inc(p, SizeOf(Word)); (* 2 *)
  end;

  if (keyLen and SizeOf(Byte)) <> 0 then (* 1 *)
  begin
    hash32 := (hash32 xor (PByte(p))^) * PRIME_A;
    Inc(p, SizeOf(Byte)); (* 1 *)
  end;

  Dec(keyLen, p - key);

  while keyLen > SizeOf(UInt64) do (* 8 *)
  begin
    hash32 := (hash32 xor (
      (* FPC RolDWord() *)
      (((PUInt32(p))^ shl 5) or ((PUInt32(p))^ shr 27)) xor
      (PUInt32(p + SizeOf(UInt32)))^ (* 4 *)
      )) * PRIME_A;
    Dec(keyLen, SizeOf(UInt64)); (* 8 *)
    Inc(p, SizeOf(UInt64)); (* 8 *)
  end;

  (* Last 8 bytes *)
  if keyLen <> 0 then
  begin
    hash32 := (hash32 xor (PWord(p + 0 * SizeOf(Word)))^) * PRIME_A; (* 0 *)
    hash32 := (hash32 xor (PWord(p + 1 * SizeOf(Word)))^) * PRIME_A; (* 2 *)
    hash32 := (hash32 xor (PWord(p + 2 * SizeOf(Word)))^) * PRIME_A; (* 4 *)
    hash32 := (hash32 xor (PWord(p + 3 * SizeOf(Word)))^) * PRIME_A; (* 6 *)
  end;

  Result := hash32 xor (hash32 shr 16);
end;

function FNV1A_Hash_Yorikke(key: PChar; keyLen: NativeUInt): UInt32; inline;
var
  hash32: UInt32;
  hash32B: UInt32;
begin
  hash32 := BASE_A;
  hash32B := BASE_A;

  while keyLen >= 2*SizeOf(UInt64) do (* 16 = 2*8 *)
  begin
    hash32 := (hash32 xor (
      (* FPC RolDWord() *)
      (((PUInt32(key))^ shl 5) or ((PUInt32(key))^ shr 27)) xor
      (PUInt32(key + SizeOf(UInt32)))^  (* 4 *)
      )) * PRIME_A;

    hash32B := (hash32B xor (
      (* FPC RolDWord() *)
      (((PUInt32(key + SizeOf(UInt64)))^ shl 5) or ((PUInt32(key + SizeOf(UInt64)))^ shr 27)) xor
      (PUInt32(key + (SizeOf(UInt64) + SizeOf(UInt32))))^
      )) * PRIME_A;

    Dec(keyLen, 2*SizeOf(UInt64)); (* 16 = 2*8 *)
    Inc(key, 2*SizeOf(UInt64)); (* 16 = 2*8 *)
  end;

  (* Remaining bytes - 0..15 *)
  if (keyLen and SizeOf(UInt64)) <> 0 then (* 8 *)
  begin
    hash32 := (hash32 xor (PUInt32(key))^) * PRIME_A;
    hash32B := (hash32B xor (PUInt32(key + SizeOf(UInt32)))^) * PRIME_A; (* 4 *)
    Inc(key, SizeOf(UInt64)); (* 8 *)
  end;

  if (keyLen and SizeOf(UInt32)) <> 0 then (* 4 *)
  begin
    hash32 := (hash32 xor (PWord(key))^) * PRIME_A;
    hash32B := (hash32B xor (PWord(key + SizeOf(Word)))^) * PRIME_A; (* 2 *)
    Inc(key, SizeOf(UInt32)); (* 4 *)
  end;

  if (keyLen and SizeOf(Word)) <> 0 then (* 2 *)
  begin
    hash32 := (hash32 xor (PWord(key))^) * PRIME_A;
    Inc(key, SizeOf(Word)); (* 2 *)
  end;

  if (keyLen and SizeOf(Byte)) <> 0 then (* 1 *)
    hash32 := (hash32 xor (PByte(key))^) * PRIME_A;

  (* FPC RolDWord() *)
  hash32 := (hash32 xor ((hash32B shl 5) or (hash32B shr 27))) * PRIME_A;
  Result := hash32 xor (hash32 shr 16);
end;

{$Q+}{$R+}

initialization

finalization

end.
