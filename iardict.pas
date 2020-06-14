unit iardict;
{
  Simple Hash Table
}

{$ifdef FPC}
{$mode delphi}
{$endif}

{$inline on}
{$pointermath on}

interface

uses
  Classes, SysUtils;

type
  THash = function(key: PChar; keyLen: NativeUInt): DWord;

{
  PPKeyNode = ^PKeyNode;
  PKeyNode = ^TKeyNode;


  TKeyNode = record
    key: PChar;
    Value: Pointer;
    Next: PKeyNode;
    keyLen: integer;
    function Create(key: PChar; keyLen: integer): PKeyNode; inline;
    procedure Destroy(node: PKeyNode); inline;
  end;
}
  PIarDict = ^TIarDict;

  { TIarDict }

  TIarDict = record
    private
    table: PPChar;
    icap: NativeUInt;
    FCapacity, FKeyNum: NativeUInt;
    FHash: THash;

    procedure Grow; inline;
    procedure Shrink; inline;
    procedure Resize(cap: NativeUInt);
    function FindNode(key: PChar; keyLen: NativeUInt;
      out block: NativeUInt; out prev: PChar): PChar; inline;

    public
    procedure Init(hash: THash);
    procedure Clear;
    function UsedBlocks: NativeUInt; inline;
    function Find(key: PChar; keyLen: NativeUInt; out value: Pointer): boolean;
    function Remove(key: PChar; keyLen: NativeUInt): boolean;
    function Insert(key: PChar; keyLen: NativeUInt; value: Pointer = nil): integer;

    property keyNum: NativeUInt read FKeyNum;
    property capacity: NativeUInt read FCapacity;
  end;

  function KN_Create(key: PChar; keyLen: NativeUInt): PChar; inline;
  function KN_Next(kn: PChar): PPChar; inline;
  function KN_KeyLen(kn: PChar): PNativeUInt; inline;
  function KN_Key(kn: PChar): PChar; inline;
  function KN_Value(kn: PChar): PPointer; inline;

implementation

const
  BASE_CAPACITY = 1 shl 3; (* 8 *)

{ TIarDict }

procedure TIarDict.Grow;
begin
  if FKeyNum > FCapacity then
    Resize(2*FCapacity);
end;

procedure TIarDict.Shrink;
begin
  if 4*FKeyNum < FCapacity then
    if FCapacity >= 2*BASE_CAPACITY then
      Resize(FCapacity div 2);
end;

procedure TIarDict.Resize(cap: NativeUInt);
var
  i, bn: NativeUInt;
  tbl: PPChar;
  kn, nn: PChar;
begin
  tbl := AllocMem(SizeOf(PChar) * cap);
  for i := 0 to FCapacity - 1 do
    begin
      kn := table[i];
      while kn <> nil do
      begin
        nn := KN_Next(kn)^;
        (* Rehash and insert *)
        bn := FHash(KN_Key(kn), KN_KeyLen(kn)^) and (cap - 1);
        KN_Next(kn)^ := tbl[bn];
        tbl[bn] := kn;
        kn := nn;
      end;
    end;

  FreeMem(table);
  table := tbl;

  FCapacity := cap;
end;

function TIarDict.FindNode(key: PChar; keyLen: NativeUInt;
  out block: NativeUInt; out prev: PChar): PChar;
var
  bn: NativeUInt;
  kn, pn: PChar;
begin
  pn := nil;
  bn := FHash(key, keyLen) and (FCapacity - 1);
  kn := table[bn];
  block := bn;

  while kn <> nil do
  begin
    if (KN_KeyLen(kn)^ = keylen) and (CompareMem(KN_Key(kn), key, keyLen) = True) then
    begin
      prev := pn;
      Exit(kn);
    end;
    pn := kn;
    kn := KN_Next(kn)^;
  end;

  prev := pn;
  Exit(kn);
end;

procedure TIarDict.Init(hash: THash);
begin
  icap := 0;
  FCapacity := BASE_CAPACITY;
  FKeyNum := 0;
  table := AllocMem(SizeOf(PChar) * FCapacity);
  Fhash := hash;
end;

procedure TIarDict.Clear;
var
  i: integer;
  kn, nn: PChar;
begin
  for i := 0 to FCapacity - 1 do
  begin
    kn := table[i];
    while kn <> nil do
    begin
      nn := KN_Next(kn)^;
      FreeMem(kn);
      kn := nn;
    end;
  end;

  FreeMem(table);
end;

function TIarDict.UsedBlocks: NativeUInt;
var
  i: NativeUInt;
begin
  Result := 0;
  for i := 0 to FCapacity - 1 do
    if table[i] <> nil then
      Inc(Result);
end;

function TIarDict.Find(key: PChar; keyLen: NativeUInt; out value: Pointer): boolean;
var
  bn: NativeUInt;
  kn, pn: PChar;
begin
  value := nil;
  if (key = nil) or (keyLen = 0) then
    Exit(false);

  kn := FindNode(key, keyLen, bn, pn);
  if kn <> nil then
  begin
    value := KN_value(kn)^;
    Result := true;
  end
  else
    Result := false;
end;

function TIarDict.Remove(key: PChar; keyLen: NativeUInt): boolean;
var
  bn: NativeUInt;
  kn, pn: PChar;
begin
  if (key = nil) or (keyLen = 0) then
    Exit(false);

  kn := FindNode(key, keyLen, bn, pn);
  if kn <> nil then
  begin
    if pn <> nil then
      KN_Next(pn)^ := KN_Next(kn)^
    else
      table[bn] := KN_Next(kn)^;
    Freemem(kn);
    Dec(FKeyNum);
    Shrink();
    Result := true;
  end
  else
    Result := false;
end;

function TIarDict.Insert(key: PChar; keyLen: NativeUInt; value: Pointer = nil): integer;
var
  bn: NativeUInt;
  kn, pn: PChar;
begin
  if (key = nil) or (keyLen = 0) then
    Exit(-1);

  kn := FindNode(key, keyLen, bn, pn);
  if kn <> nil then
  begin
    KN_Value(kn)^ := value;
    Exit(0);
  end
  else
  begin
    Inc(FKeyNum);
    kn := KN_Create(key, keyLen);
    KN_Next(kn)^ := table[bn];
    KN_Value(kn)^ := value;
    table[bn] := kn;
    Grow();
    Exit(1);
  end;

end;

{ KeyNode Layout
next   - PChar       offset - 0
keeLen - NativeUInt  offset - SizeOf(PChar)
key    - char[]      offset - SizeOf(PChar) + SizeOf(NativeUInt)
value  - Pointer     offset - SizeOf(PChar) + SizeOf(NativeUInt) + keyLen

Total -  SizeOf(PChar) + SizeOf(NativeUInt) + SizeOf(Pointer) + keyLen
}

function KN_Create(key: PChar; keyLen: NativeUInt): PChar;
var
  kn: PChar;
begin
  kn := AllocMem(SizeOf(PChar) + SizeOf(NativeUInt) + SizeOf(Pointer) + keyLen);
  (* Copy key *)
  Move(key^, (kn + SizeOf(PChar) + SizeOf(NativeUInt))^, keyLen);
  PNativeUInt(kn + SizeOf(PChar))^ := keyLen;
  Result := kn;
end;

function KN_Next(kn: PChar): PPChar;
begin
  Result := PPChar(kn);
end;

function KN_KeyLen(kn: PChar): PNativeUInt;
begin
  Result := PNativeUInt(kn + SizeOf(PChar));
end;

function KN_Key(kn: PChar): PChar;
begin
  Result := kn + SizeOf(PChar) + SizeOf(NativeUInt);
end;

function KN_Value(kn: PChar): PPointer;
begin
  Result := PPointer(kn + SizeOf(PChar) + SizeOf(NativeUInt) + KN_KeyLen(kn)^);
end;

initialization

finalization

end.
