unit iardict;
{
  TIarDict (Iaramaz Dictionary) - Separate chaining Hash Table
  https://en.wikipedia.org/wiki/Hash_table#Separate_chaining
}

{$ifdef FPC}
{$mode delphi}
{$macro on}
{$endif}

{$inline on}
{$pointermath on}
{$undef USE_MACRO}

interface

uses
  Classes, SysUtils;

type
  THash = function(key: PChar; keyLen: NativeUInt): DWord;

  PIarDict = ^TIarDict;

  { TIarDict }

  TIarDict = record
    private
    FTable: PPChar;
    //icap: NativeUInt;
    FCapacity, FKeyNum: NativeUInt;
    FHash: THash;

    procedure Grow; inline;
    procedure Shrink; inline;
    procedure Resize(cap: NativeUInt);
    function FindNode(key: PChar; keyLen: NativeUInt;
      out block: NativeUInt; out prev: PChar): PChar;

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
{$ifdef USE_MACRO}
  {$define KN_NextKN := PPChar(kn) }
  {$define KN_NextPN := PPChar(pn) }
  {$define KN_KeyLen := PNativeUInt(kn + SizeOf(PChar)) }
  {$define KN_Key := (kn + (SizeOf(PChar) + SizeOf(NativeUInt))) }
  {$define KN_Value := PPointer(kn + (SizeOf(PChar) + SizeOf(NativeUInt)) + PNativeUInt(kn + SizeOf(PChar))^) }
{$else}
  function KN_Next(kn: PChar): PPChar; inline;
  function KN_KeyLen(kn: PChar): PNativeUInt; inline;
  function KN_Key(kn: PChar): PChar; inline;
  function KN_Value(kn: PChar): PPointer; inline;
{$endif}


{  A fast alternative to the modulo reduction.
   https://github.com/lemire/fastrange  }
  function FastRange32(x: DWord; r: DWord): DWord; inline;

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
      kn := FTable[i];
      while kn <> nil do
      begin
        (* Rehash and insert *)
        {$ifdef USE_MACRO}
        nn := KN_NextKN^;
        (* bn := FHash(KN_Key(kn), KN_KeyLen(kn)^) and (cap - 1); *)
        bn := FastRange32(FHash(KN_Key, KN_KeyLen^), cap);
        KN_NextKN^ := tbl[bn];
        {$else}
        nn := KN_Next(kn)^;
        bn := FastRange32(FHash(KN_Key(kn), KN_KeyLen(kn)^), cap);
        KN_Next(kn)^ := tbl[bn];
        {$endif}
        tbl[bn] := kn;
        kn := nn;
      end;
    end;

  FreeMem(FTable);
  FTable := tbl;

  FCapacity := cap;
end;

function TIarDict.FindNode(key: PChar; keyLen: NativeUInt;
  out block: NativeUInt; out prev: PChar): PChar;
var
  bn: NativeUInt;
  kn, pn: PChar;
begin
  pn := nil;
  //bn := FHash(key, keyLen) and (FCapacity - 1);
  bn := FastRange32(FHash(key, keyLen), FCapacity);
  kn := FTable[bn];
  block := bn;

  while kn <> nil do
  begin
    {$ifdef USE_MACRO}
    if (KN_KeyLen^ = keylen) and (CompareMem(KN_Key, key, keyLen) = True) then
    {$else}
    if (KN_KeyLen(kn)^ = keylen) and (CompareMem(KN_Key(kn), key, keyLen) = True) then
    {$endif}
    begin
      prev := pn;
      Exit(kn);
    end;
    pn := kn;
    {$ifdef USE_MACRO} kn := KN_NextKN^;
    {$else} kn := KN_Next(kn)^; {$endif}
  end;

  prev := pn;
  Exit(kn);
end;

procedure TIarDict.Init(hash: THash);
begin
  //icap := 0;
  FCapacity := BASE_CAPACITY;
  FKeyNum := 0;
  FTable := AllocMem(SizeOf(PChar) * FCapacity);
  Fhash := hash;
end;

procedure TIarDict.Clear;
var
  i: NativeUInt;
  kn, nn: PChar;
begin
  for i := 0 to FCapacity - 1 do
  begin
    kn := FTable[i];
    while kn <> nil do
    begin
      {$ifdef USE_MACRO} nn := KN_NextKN^;
      {$else} nn := KN_Next(kn)^; {$endif}
      FreeMem(kn);
      kn := nn;
    end;
  end;

  FreeMem(FTable);
end;

function TIarDict.UsedBlocks: NativeUInt;
var
  i: NativeUInt;
begin
  Result := 0;
  for i := 0 to FCapacity - 1 do
    if FTable[i] <> nil then
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
    {$ifdef USE_MACRO} value := KN_Value^;
    {$else} value := KN_value(kn)^; {$endif}
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
    {$ifdef USE_MACRO}
    if pn <> nil then
      KN_NextPN^ := KN_NextKN^
    else
      FTable[bn] := KN_NextKN^;
    {$else}
    if pn <> nil then
      KN_Next(pn)^ := KN_Next(kn)^
    else
      FTable[bn] := KN_Next(kn)^;
    {$endif}

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
    {$ifdef USE_MACRO} KN_Value^ := value;
    {$else} KN_Value(kn)^ := value; {$endif}
    Exit(0);
  end
  else
  begin
    Inc(FKeyNum);
    kn := KN_Create(key, keyLen);
    {$ifdef USE_MACRO}
    KN_NextKN^ := FTable[bn];
    KN_Value^ := value;
    {$else}
    KN_Next(kn)^ := FTable[bn];
    KN_Value(kn)^ := value;
    {$endif}

    FTable[bn] := kn;
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
  kn := GetMem((SizeOf(PChar) + SizeOf(NativeUInt) + SizeOf(Pointer)) + keyLen);
  (* Copy key *)
  KN_Next(kn)^ := nil;
  KN_KeyLen(kn)^ := keyLen;
  Move(key^, (kn + (SizeOf(PChar) + SizeOf(NativeUInt)))^, keyLen);
  KN_Value(kn)^ := nil;
  Result := kn;
end;

{$ifndef USE_MACRO}
function KN_Next(kn: PChar): PPChar; inline;
begin
  Result := PPChar(kn);
end;

function KN_KeyLen(kn: PChar): PNativeUInt; inline;
begin
  Result := PNativeUInt(kn + SizeOf(PChar));
end;

function KN_Key(kn: PChar): PChar; inline;
begin
  Result := kn + (SizeOf(PChar) + SizeOf(NativeUInt));
end;

function KN_Value(kn: PChar): PPointer; inline;
begin
  Result := PPointer(kn + (SizeOf(PChar) + SizeOf(NativeUInt)) +
    PNativeUInt(kn + SizeOf(PChar))^);
end;
{$endif}

function FastRange32(x: DWord; r: DWord): DWord; inline;
begin
  Result := DWord((QWord(x) * QWord(r)) shr 32);
end;

initialization

finalization

end.
