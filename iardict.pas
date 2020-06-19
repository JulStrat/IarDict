unit iardict;
{
  TIarDict (Iaramaz Dictionary) - Simple & Bad separate chaining Hash Table.
  Copyright (C) 2020, I. Kakoulidis, all right reserved.
}

{$ifdef FPC}
{$mode delphi}
{$macro on}
{$undef USE_MACRO}
{$endif}

{$inline on}
{$pointermath on}


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
    FCapacity, FKeyNum: NativeUInt;
    FHash: THash;

    procedure Grow; inline;
    procedure Shrink; inline;
    procedure Resize(cap: NativeUInt);
    function FindNode(key: PChar; keyLen: NativeUInt;
      out block: NativeUInt; out prev: PChar): PChar;

    public
    procedure Init(hash: THash); overload;
    procedure Init(hash: THash; cap: NativeUInt); overload;
    procedure Clear;
    function UsedBlocks: NativeUInt; inline;
    function Find(key: PChar; keyLen: NativeUInt; out value: Pointer): boolean;
    function Remove(key: PChar; keyLen: NativeUInt): boolean;
    function Insert(key: PChar; keyLen: NativeUInt; value: Pointer = nil): integer;
    function SetValues(value: Pointer): NativeUInt;

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

(* function CmpMem(a, b: PChar; len: NativeUInt): boolean; inline; *)
function MemEq(a, b: PChar; len: NativeUInt): boolean; inline;
(* procedure MemCpy(src, dst: PChar; len: NativeUInt); inline; *)

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
    Resize(7*(FCapacity shr 2));
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
    if (KN_KeyLen(kn)^ = keylen) and (MemEq(KN_Key(kn), key, keyLen) = True) then
    //if (KN_KeyLen(kn)^ = keylen) and (CompareMem(KN_Key(kn), key, keyLen) = True) then
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
  FCapacity := BASE_CAPACITY;
  FKeyNum := 0;
  FTable := AllocMem(SizeOf(PChar) * FCapacity);
  Fhash := hash;
end;

procedure TIarDict.Init(hash: THash; cap: NativeUInt);
begin
  if cap < BASE_CAPACITY then
    cap := BASE_CAPACITY;
  FCapacity := cap;
  FKeyNum := 0;
  FTable := AllocMem(SizeOf(PChar) * cap);
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

function TIarDict.SetValues(value: Pointer): NativeUInt;
var
  i: NativeUInt;
  kn, nn: PChar;
begin
  Result := 0;
  for i := 0 to FCapacity - 1 do
  begin
    kn := FTable[i];
    while kn <> nil do
    begin
      {$ifdef USE_MACRO}
      nn := KN_NextKN^;
      KN_Value^ := value;
      {$else}
      nn := KN_Next(kn)^;
      KN_Value(kn)^ := value;
      {$endif}
      Inc(Result);
      kn := nn;
    end;
  end;
end;

{ KeyNode Layout
next   - PChar       offset - 0
value  - Pointer     offset - SizeOf(PChar)
keeLen - NativeUInt  offset - SizeOf(PChar) + SizeOf(NativeUInt)
key    - char[]      offset - SizeOf(PChar) + SizeOf(NativeUInt) + SizeOf(NativeUInt)

Total -  SizeOf(PChar) + SizeOf(NativeUInt) + SizeOf(NativeUInt) + keyLen
}

function KN_Create(key: PChar; keyLen: NativeUInt): PChar;
var
  kn: PChar;
begin
  kn := GetMem((SizeOf(PChar) + SizeOf(NativeUInt) + SizeOf(Pointer)) + keyLen);
  (* Copy key *)
  {$ifndef USE_MACRO}
  KN_Next(kn)^ := nil;
  KN_Value(kn)^ := nil;
  KN_KeyLen(kn)^ := keyLen;

  {$else}
  KN_NextKN^ := nil;
  KN_KeyLen^ := keyLen;
  KN_Value^ := nil;
  {$endif}

  Move(key^, (kn + (SizeOf(PChar) + SizeOf(NativeUInt) + SizeOf(NativeUInt)))^, keyLen);
  (* MemCpy(key, kn + (SizeOf(PChar) + SizeOf(NativeUInt) + SizeOf(NativeUInt)), keyLen); *)
  Result := kn;
end;

{$ifndef USE_MACRO}
function KN_Next(kn: PChar): PPChar; inline;
begin
  Result := PPChar(kn);
end;

function KN_Value(kn: PChar): PPointer; inline;
begin
  Result := PPointer(kn + SizeOf(PChar));
end;

function KN_KeyLen(kn: PChar): PNativeUInt; inline;
begin
  Result := PNativeUInt(kn + (SizeOf(PChar) + SizeOf(NativeUInt)));
end;

function KN_Key(kn: PChar): PChar; inline;
begin
  Result := kn + (SizeOf(PChar) + SizeOf(NativeUInt) + SizeOf(NativeUInt));
end;

{$endif}

function FastRange32(x: DWord; r: DWord): DWord; inline;
begin
  Result := DWord((QWord(x) * QWord(r)) shr 32);
end;

function MemEq(a, b: PChar; len: NativeUInt): boolean; inline;
begin
  while len >= 16 do
  begin
    if PDWord(a)^ <> PDWord(b)^ then Exit(false);
    Inc(a, 4); Inc(b, 4); Dec(len, 4);
    if PDWord(a)^ <> PDWord(b)^ then Exit(false);
    Inc(a, 4); Inc(b, 4); Dec(len, 4);
    if PDWord(a)^ <> PDWord(b)^ then Exit(false);
    Inc(a, 4); Inc(b, 4); Dec(len, 4);
    if PDWord(a)^ <> PDWord(b)^ then Exit(false);
    Inc(a, 4); Inc(b, 4); Dec(len, 4);
  end;

  if (len and 8) <> 0 then
  begin
    if PDWord(a)^ <> PDWord(b)^ then Exit(false);
    Inc(a, 4); Inc(b, 4); Dec(len, 4);
    if PDWord(a)^ <> PDWord(b)^ then Exit(false);
    Inc(a, 4); Inc(b, 4); Dec(len, 4);
  end;  

  if (len and 4) <> 0 then
  begin
    if PDWord(a)^ <> PDWord(b)^ then Exit(false);
    Inc(a, 4); Inc(b, 4); Dec(len, 4);
  end;

  if (len and 2) <> 0 then
  begin
    if PWord(a)^ <> PWord(b)^ then Exit(false);
    Inc(a, 2); Inc(b, 2); Dec(len, 2);
  end;

  if (len and 1) <> 0 then
  begin
    if a^ <> b^ then Exit(false);
  end;
  Exit(true);

end;
(*
procedure MemCpy(src, dst: PChar; len: NativeUInt); inline;
begin
  while len >= 16 do
  begin
    PDWord(dst)^ := PDWord(src)^;
    Inc(src, 4); Inc(dst, 4); Dec(len, 4);
    PDWord(dst)^ := PDWord(src)^;
    Inc(src, 4); Inc(dst, 4); Dec(len, 4);
    PDWord(dst)^ := PDWord(src)^;
    Inc(src, 4); Inc(dst, 4); Dec(len, 4);
    PDWord(dst)^ := PDWord(src)^;
    Inc(src, 4); Inc(dst, 4); Dec(len, 4);
  end;
  
  if (len and 8) <> 0 then
  begin
    PDWord(dst)^ := PDWord(src)^;
    Inc(src, 4); Inc(dst, 4); Dec(len, 4);
    PDWord(dst)^ := PDWord(src)^;
    Inc(src, 4); Inc(dst, 4); Dec(len, 4);
  end;
  
  if (len and 4) <> 0 then
  begin
    PDWord(dst)^ := PDWord(src)^;
    Inc(src, 4); Inc(dst, 4); Dec(len, 4);
  end;

  if (len and 2) <> 0 then
  begin
    PWord(dst)^ := PWord(src)^;
    Inc(src, 2); Inc(dst, 2); Dec(len, 2);
  end;

  if (len and 1) <> 0 then
  begin
    dst^ := src^;
  end;

end;
*)
initialization

finalization

end.
