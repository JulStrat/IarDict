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

function FNV1A_Hash_Jesteress(key: PChar; keyLen: NativeUInt): DWord; inline;
function FNV1A_Hash_Meiyan(key: PChar; keyLen: NativeUInt): DWord; inline;
function FNV1A_Hash_Mantis(key: PChar; keyLen: NativeUInt): DWord; inline;
function FNV1A_Hash_Yorikke(key: PChar; keyLen: NativeUInt): DWord; inline;

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

function FNV1A_Hash_Jesteress(key: PChar; keyLen: NativeUInt): DWord; inline;
var
  hash32: DWord;
begin
  hash32 := BASE_A;

  while keyLen >= SizeOf(QWord) do
  begin
    hash32 := (hash32 xor (
      (* FPC RolDWord() *)
      (((PDWord(key))^ shl 5) or ((PDWord(key))^ shr 27)) xor
      (PDWord(key + SizeOf(DWord)))^
      )) * PRIME_A;
    Dec(keyLen, SizeOf(QWord));
    Inc(key, SizeOf(QWord));
  end;

  if (keyLen and SizeOf(DWord)) <> 0 then
  begin
    hash32 := (hash32 xor (PDWord(key))^) * PRIME_A;
    Inc(key, SizeOf(DWord));
  end;

  if (keyLen and SizeOf(Word)) <> 0 then
  begin
    hash32 := (hash32 xor (PWord(key))^) * PRIME_A;
    Inc(key, SizeOf(Word));
  end;

  if (keyLen and SizeOf(Byte)) <> 0 then
    hash32 := (hash32 xor (PByte(key))^) * PRIME_A;

  Result := hash32 xor (hash32 shr 16);

end;

function FNV1A_Hash_Meiyan(key: PChar; keyLen: NativeUInt): DWord; inline;
var
  hash32: DWord;
begin
  hash32 := BASE_A;

  while keyLen >= SizeOf(QWord) do
  begin
    hash32 := (hash32 xor (
      (* FPC RolDWord() *)
      (((PDWord(key))^ shl 5) or ((PDWord(key))^ shr 27)) xor
      (PDWord(key + SizeOf(DWord)))^
      )) * PRIME_A;

    Dec(keyLen, SizeOf(QWord));
    Inc(key, SizeOf(QWord));
  end;

  if (keyLen and SizeOf(DWord)) <> 0 then
  begin
    hash32 := (hash32 xor (PWord(key))^) * PRIME_A;
    Inc(key, SizeOf(Word));
    hash32 := (hash32 xor (PWord(key))^) * PRIME_A;
    Inc(key, SizeOf(Word));
  end;

  if (keyLen and SizeOf(Word)) <> 0 then
  begin
    hash32 := (hash32 xor (PWord(key))^) * PRIME_A;
    Inc(key, SizeOf(Word));
  end;

  if (keyLen and SizeOf(Byte)) <> 0 then
    hash32 := (hash32 xor (PByte(key))^) * PRIME_A;

  Result := hash32 xor (hash32 shr 16);

end;

function FNV1A_Hash_Mantis(key: PChar; keyLen: NativeUInt): DWord; inline;
var
  hash32: DWord;
  p: PChar;
begin
  hash32 := BASE_A;
  p := key;

  if (keyLen and SizeOf(DWord)) <> 0 then
  begin
    hash32 := (hash32 xor (PWord(p))^) * PRIME_A;
    Inc(p, SizeOf(Word));
    hash32 := (hash32 xor (PWord(p))^) * PRIME_A;
    Inc(p, SizeOf(Word));
  end;

  if (keyLen and SizeOf(Word)) <> 0 then
  begin
    hash32 := (hash32 xor (PWord(p))^) * PRIME_A;
    Inc(p, SizeOf(Word));
  end;

  if (keyLen and SizeOf(Byte)) <> 0 then
  begin
    hash32 := (hash32 xor (PByte(p))^) * PRIME_A;
    Inc(p, SizeOf(Byte));
  end;

  Dec(keyLen, p - key);

  while keyLen > SizeOf(QWord) do
  begin
    hash32 := (hash32 xor (
      (* FPC RolDWord() *)
      (((PDWord(p))^ shl 5) or ((PDWord(p))^ shr 27)) xor
      (PDWord(p + SizeOf(DWord)))^
      )) * PRIME_A;
    Dec(keyLen, SizeOf(QWord));
    Inc(p, SizeOf(QWord));
  end;

  if keyLen <> 0 then
  begin
    hash32 := (hash32 xor (PWord(p + 0 * SizeOf(Word)))^) * PRIME_A;
    hash32 := (hash32 xor (PWord(p + 1 * SizeOf(Word)))^) * PRIME_A;
    hash32 := (hash32 xor (PWord(p + 2 * SizeOf(Word)))^) * PRIME_A;
    hash32 := (hash32 xor (PWord(p + 3 * SizeOf(Word)))^) * PRIME_A;
  end;

  Result := hash32 xor (hash32 shr 16);

end;

function FNV1A_Hash_Yorikke(key: PChar; keyLen: NativeUInt): DWord; inline;
var
  hash32: DWord;
  hash32B: DWord;
begin
  hash32 := BASE_A;
  hash32B := BASE_A;

  while keyLen >= 2*SizeOf(QWord) do
  begin
    hash32 := (hash32 xor (
      (* FPC RolDWord() *)
      (((PDWord(key))^ shl 5) or ((PDWord(key))^ shr 27)) xor
      (PDWord(key + SizeOf(DWord)))^
      )) * PRIME_A;

    hash32B := (hash32B xor (
      (* FPC RolDWord() *)
      (((PDWord(key + 8))^ shl 5) or ((PDWord(key))^ shr 27)) xor
      (PDWord(key + (8 + SizeOf(DWord))))^
      )) * PRIME_A;

    Dec(keyLen, 2*SizeOf(QWord));
    Inc(key, 2*SizeOf(QWord));
  end;

  if (keyLen and SizeOf(QWord)) <> 0 then
  begin
    hash32 := (hash32 xor (PDWord(key))^) * PRIME_A;
    hash32B := (hash32B xor (PDWord(key + 4))^) * PRIME_A;
    Inc(key, SizeOf(QWord));
  end;

  if (keyLen and SizeOf(DWord)) <> 0 then
  begin
    hash32 := (hash32 xor (PWord(key))^) * PRIME_A;
    hash32B := (hash32B xor (PWord(key + 2))^) * PRIME_A;
    Inc(key, SizeOf(DWord));
  end;

  if (keyLen and SizeOf(Word)) <> 0 then
  begin
    hash32 := (hash32 xor (PWord(key))^) * PRIME_A;
    Inc(key, SizeOf(Word));
  end;

  if (keyLen and SizeOf(Byte)) <> 0 then
    hash32 := (hash32 xor (PByte(key))^) * PRIME_A;

  (* FPC RolDWord() *)
  hash32 := (hash32 xor ((hash32B shl 5) or (hash32B shr 27))) * PRIME_A;
  Result := hash32 xor (hash32 shr 16);
end;

{$Q+}{$R+}

initialization

finalization

end.
