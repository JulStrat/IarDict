program benchmark;
{$ifdef FPC}
{$mode delphi}
{$endif}

{$apptype console}

{$undef USE_FASTMM}

{$define STL_INTERFACE_EXT}

uses
  {$ifdef USE_FASTMM}FastMM4 ,{$endif}
  Classes, SysUtils, iardict, ghashmap, generics.collections, hash ;

type
  TRnd = array[0..252] of Byte;
  TArrRnd = array[0..1000000-1] of TRnd;

  {
  THint = class
    class function hash(key: TRnd; n: SizeUInt): SizeUInt;
    class function equal(const a, b: TRnd): boolean;
  end;

  class function THint.hash(key: TRnd; n:SizeUInt): SizeUInt;
  begin
    hash := FNV1A_Hash_Meiyan(@key[0], SizeOf(TRnd)) and (n-1);
  end;

  class function THint.equal(const a, b: TRnd): boolean;
  begin
    Result := MemEq(@a[0], @b[0], SizeOf(TRnd));
  end;
  }
var
  t: TIarDict;
  counter: longint;
  start: QWord;
  rnd: TRnd;
  gdict: TDictionary<TRnd, NativeUInt>;
  { ghm: ghashmap.THashmap<TRnd, NativeUInt, THint>; }
  v: NativeUInt;
  i: integer;

  intKey: int64;
  frDict: TStringList;
  wrd: String;
  arrRnd: TArrRnd;

begin

  Randomize();
  for counter := 0 to 1000000-1 do
    for i := 0 to Length(TRnd) - 1 do
      arrRnd[counter][i] := Random(256);

  start := GetTickCount64();

  {
  for counter := 1 to 10000000 do
  begin
    t.AddOrUpdate('Hellz', 5);
    t.AddOrUpdate('AbraKadabra', 11);

    t.AddOrUpdate('Halli', 5);
    t.AddOrUpdate('AbrzKadabr0', 11);
  end;
  WriteLn(t.keyNum);
  for counter := 1 to 10000000 do
  begin
    t.Find('Hello', 5);
    t.Find('Hellz', 5);
    t.Find('Halli', 5);
    t.Find('AbraKadabra', 11);
  end;

  WriteLn(t.Find('Hello', 5));
  WriteLn(t.Find('Hellz', 5));
  WriteLn(t.Find('Halli', 5));
  WriteLn(t.Find('AbraKadabra', 11));

  WriteLn('Ticks - ', GetTickCount64() - start);
  }

  start := GetTickCount64();
  t.Init(FNV1A_Hash_Meiyan, 1000000);
  WriteLn('Key size - ', SizeOf(TRnd));
  WriteLn('--------');
  WriteLn('IarDict - Insert 1000000 random ...');
  for counter := 0 to 1000000-1 do
  begin
    t.Insert(PChar(@arrRnd[counter][0]), SizeOf(TRnd), i);

    if not t.Find(PChar(@arrRnd[counter][0]), SizeOf(TRnd), v) then
    begin
      WriteLn('Error.');
      break;
    end;
    if v <> i then
    begin
      WriteLn('Error.');
      break;
    end;
{
    for i := 0 to Length(rnd) - 1 do
      rnd[i] := Random();
    t.Remove(PChar(@rnd[0]), SizeOf(rnd));
}
  end;
  WriteLn('      Ticks - ', GetTickCount64() - start);
  WriteLn('       Keys - ', t.keyNum);
  WriteLn('   Capacity - ', t.capacity);
  WriteLn('Used blocks - ', t.UsedBlocks);
  WriteLn(Format('Load factor - %g', [t.keyNum / t.capacity]));

  t.SetValues(0);
  WriteLn('Ticks with SetValues - ', GetTickCount64() - start);

  t.Clear();
  WriteLn('Ticks with free - ', GetTickCount64() - start);

  //halt(70);

  gdict := TDictionary<TRnd, NativeUInt>.Create(1000000);
  start := GetTickCount64();
  WriteLn('.......................................');
  WriteLn('TDictionary - Insert 1000000 random ...');
  for counter := 0 to 1000000-1 do
  begin
    gdict.AddOrSetValue(arrRnd[counter], i);

    if not gdict.TryGetValue(arrRnd[counter], v) then
    begin
      WriteLn('Error.');
      break;
    end;
    if v <> i then
    begin
      WriteLn('Error.');
      break;
    end;
{
    for i := 0 to Length(rnd) - 1 do
      rnd[i] := Random();
    gdict.Remove(rnd);
}
  end;
  WriteLn('Ticks - ', GetTickCount64() - start);
  WriteLn('Keys - ', gdict.Count);
  WriteLn('Capacity - ', gdict.Capacity);
  FreeAndNil(gdict);
  WriteLn('Ticks wiht free - ', GetTickCount64() - start);

  (* Keys - int64 *)
  WriteLn('');
  WriteLn('....................');
  WriteLn('Integer keys test...');
  start := GetTickCount64();
  t.Init(FNV1A_Hash_Meiyan);
  for i := 0 to 10000000 do
  begin
    intKey := Random(1 shl 16);
    t.Insert(@intkey, SizeOf(int64), 0);
  end;
  WriteLn('      Ticks - ', GetTickCount64() - start);
  WriteLn('       Keys - ', t.keyNum);
  WriteLn('   Capacity - ', t.capacity);
  WriteLn('Used blocks - ', t.UsedBlocks);
  WriteLn(Format('Load factor - %g', [t.keyNum / t.capacity]));
  t.Clear();
  WriteLn('Ticks with free - ', GetTickCount64() - start);

  WriteLn('');
  WriteLn('.................................');
  WriteLn('IarDict - Insert French words ...');

  frDict := TStringList.Create();
  frDict.LoadFromFile('dic_fr.txt');
  start := GetTickCount64();
  t.Init(FNV1A_Hash_Meiyan);
  for wrd in frDict do
    t.Insert(PChar(wrd), Length(wrd), 0);
  WriteLn('Ticks - ', GetTickCount64() - start);
  WriteLn('Keys - ', t.keyNum);
  WriteLn('Used blocks - ', t.UsedBlocks);

  WriteLn('Find insert order ...');
  for wrd in frDict do
    if not t.Find(PChar(wrd), Length(wrd), v) then
    begin
      WriteLn('Err');
      break;
    end;
  WriteLn('Ticks - ', GetTickCount64() - start);

  for wrd in frDict do
    t.Remove(PChar(wrd), Length(wrd));

  WriteLn('Keys - ', t.keyNum);
  WriteLn('Ticks - ', GetTickCount64() - start);

  t.Clear();
  WriteLn('Ticks with free - ', GetTickCount64() - start);

  frDict.free();

end.

