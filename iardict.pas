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

  { KeyNode Layout
  next   - PChar       offset - 0
  keeLen - NativeUInt  offset - SizeOf(PChar)
  key    - char[]      offset - SizeOf(PChar) + SizeOf(NativeUInt)
  value  - Pointer     offset - SizeOf(PChar) + SizeOf(NativeUInt) + keyLen

  Total -  SizeOf(PChar) + SizeOf(NativeUInt) + SizeOf(Pointer) + keyLen
  }

  PPKeyNode = ^PKeyNode;
  PKeyNode = ^TKeyNode;

  { TKeyNode }

  TKeyNode = record
    key: PChar;
    Value: Pointer;
    Next: PKeyNode;
    keyLen: integer;
    function Create(key: PChar; keyLen: integer): PKeyNode; inline;
    procedure Destroy(node: PKeyNode); inline;
  end;

  PIarDict = ^TIarDict;

  { TIarDict }

  TIarDict = record
    private
    table: PPKeyNode;
    icap: integer;
    FCapacity, FKeyNum: integer;
    FHash: THash;

    procedure Grow; inline;
    procedure Shrink; inline;
    procedure Resize(cap: integer);
    function FindNode(key: PChar; keyLen: integer;
      out block: integer; out prev: PKeyNode): PKeyNode; inline;

    public
    procedure Init(hash: THash);
    procedure Clear;
    function UsedBlocks: integer; inline;
    function Find(key: PChar; keyLen: integer; out value: Pointer): boolean;
    function Remove(key: PChar; keyLen: integer): boolean;
    function Insert(key: PChar; keyLen: integer; value: Pointer = nil): integer;

    property keyNum: integer read FKeyNum;
    property capacity: integer read FCapacity;
  end;

implementation

const
  BASE_CAPACITY = 1 shl 3; (* 8 *)

{ TKeyNode }

function TKeyNode.Create(key: PChar; keyLen: integer): PKeyNode;
var
  kn: PKeyNode;
begin
  kn := AllocMem(SizeOf(PChar) + SizeOf(NativeUInt) + SizeOf(Pointer) + keyLen);
  (* Copy key *)
  Move(key^, (kn + SizeOf(PChar) + SizeOf(NativeUInt))^, keyLen);

  kn.keyLen := keyLen;
  Result := kn;
end;

procedure TKeyNode.Destroy(node: PKeyNode);
var
  nn: PKeyNode;
begin
  while node <> nil do
  begin
    nn := node.Next;

    if node.key <> nil then
      FreeMem(node.key);
    FreeMem(node);
    node := nn;
  end;
end;

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

procedure TIarDict.Resize(cap: integer);
var
  i, bn: integer;
  tbl: PPKeyNode;
  kn, nn: PKeyNode;
begin
  tbl := AllocMem(SizeOf(PKeyNode) * cap);
  for i := 0 to FCapacity - 1 do
    begin
      kn := table[i];
      while kn <> nil do
      begin
        nn := kn.Next;
        (* Rehash and insert *)
        bn := FHash(kn.key, kn.keyLen) and (cap - 1);
        kn.Next := tbl[bn];
        tbl[bn] := kn;
        kn := nn;
      end;
    end;

  FreeMem(table);
  table := tbl;

  FCapacity := cap;
end;

function TIarDict.FindNode(key: PChar; keyLen: integer;
  out block: integer; out prev: PKeyNode): PKeyNode;
var
  bn: integer;
  kn, pn: PKeyNode;
begin
  pn := nil;
  bn := FHash(key, keyLen) and (FCapacity - 1);
  kn := table[bn];
  block := bn;

  while kn <> nil do
  begin
    if (kn.keyLen = keylen) and (CompareMem(kn.key, key, keyLen) = True) then
    begin
      prev := pn;
      Exit(kn);
    end;
    pn := kn;
    kn := kn.Next;
  end;

  prev := pn;
  Exit(kn);
end;

procedure TIarDict.Init(hash: THash);
begin
  icap := 0;
  FCapacity := BASE_CAPACITY;
  FKeyNum := 0;
  table := AllocMem(SizeOf(PKeyNode) * FCapacity);
  Fhash := hash;
end;

procedure TIarDict.Clear;
var
  i: integer;
begin
  for i := 0 to FCapacity - 1 do
    if table[i] <> nil then
      TKeyNode.Destroy(table[i]);

  FreeMem(table);
end;

function TIarDict.UsedBlocks: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FCapacity - 1 do
    if table[i] <> nil then
      Inc(Result);
end;

function TIarDict.Find(key: PChar; keyLen: integer; out value: Pointer): boolean;
var
  bn: integer;
  kn, pn: PKeyNode;
begin
  value := nil;
  if (key = nil) or (keyLen = 0) then
    Exit(false);

  kn := FindNode(key, keyLen, bn, pn);
  if kn <> nil then
  begin
    value := kn.Value;
    Result := true;
  end
  else
    Result := false;
end;

function TIarDict.Remove(key: PChar; keyLen: integer): boolean;
var
  bn: integer;
  kn, pn: PKeyNode;
begin
  if (key = nil) or (keyLen = 0) then
    Exit(false);

  kn := FindNode(key, keyLen, bn, pn);
  if kn <> nil then
  begin
    if pn <> nil then
      pn.Next := kn.Next
    else
      table[bn] := kn.Next;
    FreeMem(kn.key);
    Freemem(kn);
    Dec(FKeyNum);
    Shrink();
    Result := true;
  end
  else
    Result := false;
end;

function TIarDict.Insert(key: PChar; keyLen: integer; value: Pointer = nil): integer;
var
  bn: integer;
  kn, pn: PKeyNode;
begin
  if (key = nil) or (keyLen = 0) then
    Exit(-1);

  kn := FindNode(key, keyLen, bn, pn);
  if kn <> nil then
  begin
    kn.Value := value;
    Exit(0);
  end
  else
  begin
    Inc(FKeyNum);
    kn := TKeyNode.Create(key, keyLen);
    kn.Next := table[bn];
    kn.Value := value;
    table[bn] := kn;
    Grow();
    Exit(1);
  end;

end;

initialization

finalization

end.
