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

function MeiYan(key: PChar; keyLen: integer): DWord; inline;

type
  PPKeyNode = ^PKeyNode;
  PKeyNode = ^TKeyNode;

  { TKeyNode }

  TKeyNode = record
    key: PChar;
    Value: Pointer;
    Next: PKeyNode;
    keyLen: integer;
    function Create(key: PChar; keyLen: integer): PKeyNode;
    procedure Destroy(node: PKeyNode);
  end;

  PIarDict = ^TIarDict;

  { TIarDict }

  TIarDict = record
    private
    table: PPKeyNode;
    capacity, FKeyNum: integer;
    FValue: Pointer;
    (* Double capacity [AddOrUpdate] when keyNum > capacity *)
    procedure Grow;
    (* Helper *)
    procedure Resize(cap: integer);
    procedure FindEx(key: PChar; keyLen: integer; out block: integer; out node: PKeyNode); inline;
    function UBlocks: integer; inline;
    public
    property value: Pointer read FValue;
    property keyNum: integer read FKeyNum;
    property usedBlocks: integer read UBlocks;
    procedure Init;
    procedure Clear;
    function Find(key: PChar; keyLen: integer): boolean; inline;
    function AddOrUpdate(key: PChar; keyLen: integer; v: Pointer = nil): integer;

  end;

implementation

const
  BASE_CAPACITY = 11;

{$Q-}{$R-}
function MeiYan(key: PChar; keyLen: integer): DWord;
var
  h: DWord;
begin
  h := $811C9DC5;
  while keyLen >= 8 do
  begin
    h := (h xor ((PDWord(key)^ shl 5) or (PDWord(key)^ shr 27) xor
      PDWord(key + 4)^)) * $AD3E7;
    Inc(key, 8);
    Dec(keyLen, 8);
  end;

  if (keyLen and 4) <> 0 then
  begin
    h := (h xor PWord(key)^) * $AD3E7;
    Inc(key, 2);
    h := (h xor PWord(key)^) * $AD3E7;
    Inc(key, 2);
  end;

  if (keyLen and 2) <> 0 then
  begin
    h := (h xor PWord(key)^) * $AD3E7;
    Inc(key, 2);
  end;

  if (keyLen and 1) <> 0 then
    h := (h xor PByte(key)^) * $AD3E7;

  Result := h xor (h shr 16);
end;

{$Q+}{$R+}

{ TKeyNode }

function TKeyNode.Create(key: PChar; keyLen: integer): PKeyNode;
var
  kn: PKeyNode;
begin
  kn := AllocMem(SizeOf(TKeyNode));
  GetMem(kn.key, keyLen);
  Move(key^, (kn.key)^, keyLen);
  kn.keyLen := keyLen;
  Result := kn;
end;

procedure TKeyNode.Destroy(node: PKeyNode);
begin
  if node.key <> nil then
    FreeMem(node.key);
  if node.Next <> nil then
    Destroy(node.Next);
  FreeMem(node);
end;

{ TIarDict }

procedure TIarDict.Grow;
begin
  Resize(2*capacity+1);
end;

procedure TIarDict.Resize(cap: integer);
var
  i, bn: integer;
  tbl: PPKeyNode;
  kn, nn: PKeyNode;
begin
  tbl := AllocMem(SizeOf(PKeyNode) * cap);
  for i := 0 to capacity - 1 do
    begin
      kn := table[i];
      while kn <> nil do
      begin
        nn := kn.Next;
        (* Rehash and insert *)
        bn := MeiYan(kn.key, kn.keyLen) mod cap;
        kn.Next := tbl[bn];
        tbl[bn] := kn;
        kn := nn;
      end;
    end;

  FreeMem(table);
  table := tbl;
  capacity := cap;
end;

procedure TIarDict.FindEx(key: PChar; keyLen: integer; out block: integer; out
  node: PKeyNode);
var
  bn: integer;
  kn: PKeyNode;
begin
    if (key = nil) or (keyLen = 0) then
    begin
      block := -1;
      node := nil;
      Exit();
    end;

    bn := MeiYan(key, keyLen) mod capacity;
    kn := table[bn];
    block := bn;

    while kn <> nil do
    begin
      if (kn.keyLen = keylen) and (CompareMem(kn.key, key, keyLen) = True) then
      begin
        FValue := kn.Value;
        node := kn;
        Exit();
      end;
      kn := kn.Next;
    end;

    FValue := nil;
    node := nil;
    Exit();
end;

procedure TIarDict.Init;
begin
  capacity := BASE_CAPACITY;
  FKeyNum := 0;
  FValue := nil;
  table := AllocMem(SizeOf(PKeyNode) * BASE_CAPACITY);
end;

procedure TIarDict.Clear;
var
  i: integer;
begin
  for i := 0 to capacity - 1 do
    if table[i] <> nil then
      TKeyNode.Destroy(table[i]);
  FreeMem(table);
end;

function TIarDict.UBlocks: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to capacity - 1 do
    if table[i] <> nil then
      Inc(Result);
end;

function TIarDict.Find(key: PChar; keyLen: integer): boolean;
var
  bn: integer;
  kn: PKeyNode;
begin
  FindEx(key, keyLen, bn, kn);
  if kn <> nil then
    Result := true
  else
    Result := false;
end;

function TIarDict.AddOrUpdate(key: PChar; keyLen: integer; v: Pointer = nil): integer;
var
  bn: integer;
  kn: PKeyNode;
begin
  if (key = nil) or (keyLen = 0) then
    Exit(-1);
  FindEx(key, keyLen, bn, kn);
  if kn <> nil then
  begin
    kn.Value := v;
    FValue := v;
    Exit(0);
  end
  else
  begin
    Inc(FKeyNum);
    kn := TKeyNode.Create(key, keyLen);
    kn.Next := table[bn];
    kn.Value := v;
    table[bn] := kn;
    FValue := v;

    if FKeyNum > capacity then
      Grow();
    Exit(1);
  end;

end;

initialization

finalization

end.
