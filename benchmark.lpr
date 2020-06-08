program benchmark;
{$ifdef FPC}
{$mode delphi}
{$endif}

{$apptype console}

uses
  Classes, SysUtils, iardict;

var
  t: TIarDict;
  counter: longint;
  start: QWord;
  rnd: array[0..2] of Extended;

begin
  start := GetTickCount64();
  t.Init();

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

  start := GetTickCount64();
  WriteLn('Insert 1000000 random ...');
  for counter := 1 to 1000000 do
  begin
    rnd[0] := Random(); rnd[1] := Random(); rnd[2] := Random();
    t.AddOrUpdate(PChar(@rnd[0]), SizeOf(rnd));
    if not t.Find(PChar(@rnd[0]), SizeOf(rnd)) then
    begin
      WriteLn('Error.');
      break;
    end;
  end;
  WriteLn('Ticks - ', GetTickCount64() - start);
  WriteLn('Keys - ', t.keyNum);
  WriteLn('Used blocks - ', t.UsedBlocks);

  t.Clear();
end.

