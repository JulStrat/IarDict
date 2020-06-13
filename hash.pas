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

function FNV1A_Hash_Jesteress(key: PChar; keyLen: integer): DWord; inline;
function FNV1A_Hash_Meiyan(key: PChar; keyLen: integer): DWord; inline;
function FNV1A_Hash_Mantis(key: PChar; keyLen: integer): DWord; inline;

implementation

{$Q-}{$R-}

function FNV1A_Hash_Jesteress(key: PChar; keyLen: integer): DWord;
var
  hash32: DWord;
begin
  hash32 := 2166136261;

  while keyLen >= SizeOf(QWord) do
  begin
    hash32 := (hash32 xor (
      (((PDWord(key))^ shl 5) or ((PDWord(key))^ shr 27)) xor
      (PDWord(key + SizeOf(DWord)))^
      )) * 709607;
    Dec(keyLen, SizeOf(QWord));
    Inc(key, SizeOf(QWord));
  end;

  if (keyLen and SizeOf(DWord)) <> 0 then
  begin
    hash32 := (hash32 xor (PDWord(key))^) * 709607;
    Inc(key, SizeOf(DWord));
  end;

  if (keyLen and SizeOf(Word)) <> 0 then
  begin
    hash32 := (hash32 xor (PWord(key))^) * 709607;
    Inc(key, SizeOf(Word));
  end;

  if (keyLen and SizeOf(Byte)) <> 0 then
    hash32 := (hash32 xor (PByte(key))^) * 709607;

  Result := hash32 xor (hash32 shr 16);

end;

function FNV1A_Hash_Meiyan(key: PChar; keyLen: integer): DWord;
var
  hash32: DWord;
begin
  hash32 := 2166136261;

  while keyLen >= SizeOf(QWord) do
  begin
    hash32 := (hash32 xor (
      (((PDWord(key))^ shl 5) or ((PDWord(key))^ shr 27)) xor
      (PDWord(key + SizeOf(DWord)))^
      )) * 709607;
    Dec(keyLen, SizeOf(QWord));
    Inc(key, SizeOf(QWord));
  end;

  if (keyLen and SizeOf(DWord)) <> 0 then
  begin
    hash32 := (hash32 xor (PWord(key))^) * 709607;
    Inc(key, SizeOf(Word));
    hash32 := (hash32 xor (PWord(key))^) * 709607;
    Inc(key, SizeOf(Word));
  end;

  if (keyLen and SizeOf(Word)) <> 0 then
  begin
    hash32 := (hash32 xor (PWord(key))^) * 709607;
    Inc(key, SizeOf(Word));
  end;

  if (keyLen and SizeOf(Byte)) <> 0 then
    hash32 := (hash32 xor (PByte(key))^) * 709607;

  Result := hash32 xor (hash32 shr 16);

end;

function FNV1A_Hash_Mantis(key: PChar; keyLen: integer): DWord;
var
  hash32: DWord;
  p: PChar;
begin
  hash32 := 2166136261;
  p := key;

  if (keyLen and SizeOf(DWord)) <> 0 then
  begin
    hash32 := (hash32 xor (PWord(p))^) * 709607;
    Inc(p, SizeOf(Word));
    hash32 := (hash32 xor (PWord(p))^) * 709607;
    Inc(p, SizeOf(Word));
  end;

  if (keyLen and SizeOf(Word)) <> 0 then
  begin
    hash32 := (hash32 xor (PWord(p))^) * 709607;
    Inc(p, SizeOf(Word));
  end;

  if (keyLen and SizeOf(Byte)) <> 0 then
  begin
    hash32 := (hash32 xor (PByte(p))^) * 709607;
    Inc(p, SizeOf(Byte));
  end;

  Dec(keyLen, p - key);

  while keyLen > SizeOf(QWord) do
  begin
    hash32 := (hash32 xor (
      (((PDWord(p))^ shl 5) or ((PDWord(p))^ shr 27)) xor
      (PDWord(p + SizeOf(DWord)))^
      )) * 709607;
    Dec(keyLen, SizeOf(QWord));
    Inc(p, SizeOf(QWord));
  end;

  if keyLen <> 0 then
  begin
    hash32 := (hash32 xor (PWord(p + 0 * SizeOf(Word)))^) * 709607;
    hash32 := (hash32 xor (PWord(p + 1 * SizeOf(Word)))^) * 709607;
    hash32 := (hash32 xor (PWord(p + 2 * SizeOf(Word)))^) * 709607;
    hash32 := (hash32 xor (PWord(p + 3 * SizeOf(Word)))^) * 709607;
  end;

  Result := hash32 xor (hash32 shr 16);

end;

{$Q+}{$R+}

initialization

finalization

end.
