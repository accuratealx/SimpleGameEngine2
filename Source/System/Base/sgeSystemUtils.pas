{
Пакет             Simple Game Engine 2
Файл              sgeSystemUtils.pas
Версия            1.6
Создан            24.02.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Вспомогательные Функции
}
{$Include Defines.inc}

unit sgeSystemUtils;

{$mode objfpc}{$H+}

interface


function  sgeTrim(const S: string): string;
function  sgeTrimLeft(const S: string): string;
function  sgeTrimRight(const S: string): string;

function  sgeIntToStr(Value: Longint): string;
function  sgeStrToInt(const S: string): Integer;
function  sgeTryStrToInt(const S: string; out Int: Longint): boolean;

function  sgeFloatToStr(Value: Extended): String;
function  sgeStrToFloat(const S: String): Extended;
function  sgeTryStrToFloat(const S: String; Out Value: Double): Boolean;

function  sgeBoolToStr(Value: Boolean; TrueStr: String = 'True'; FalseStr: String = 'False'): String;

function  sgeCompareMem(P1, P2: Pointer; Length: PtrUInt): Boolean;

procedure sgeFreeAndNil(var Obj);

function  sgePos(const Substr, Source: String; Offset: SizeInt = 1): SizeInt;



implementation

uses
  SysUtils;


const
  WhiteSpace = [#0..' '];


function sgeTrim(const S: string): string;
var
  Ofs, Len: integer;
begin
  len := Length(S);
  while (Len > 0) and (S[Len] in WhiteSpace) do dec(Len);
  Ofs := 1;
  while (Ofs <= Len) and (S[Ofs] in WhiteSpace) do Inc(Ofs);
  result := Copy(S, Ofs, 1 + Len - Ofs);
end;


function sgeTrimLeft(const S: string): string;
var
  i, l: integer;
begin
  l := length(s);
  i := 1;
  while (i <= l) and (s[i] in whitespace) do inc(i);
  Result := copy(s, i, l);
end;


function sgeTrimRight(const S: string): string;
var
  l: integer;
begin
  l := length(s);
  while (l > 0) and (s[l] in whitespace) do dec(l);
  result := copy(s, 1, l);
end;


function sgeIntToStr(Value: Longint): string;
begin
  System.Str(Value, Result);
end;


function sgeStrToInt(const S: string): Integer;
var
  Err: Word;
begin
  Result := 0;
  System.Val(S, Result, Err);
end;


function sgeTryStrToInt(const S: string; out Int: Longint): boolean;
var
  Err: Word;
begin
  System.Val(S, Int, Err);
  Result := Err = 0;
end;


function sgeFloatToStr(Value: Extended): String;
begin
  Result := SysUtils.FloatToStr(Value);
end;


function sgeStrToFloat(const S: String): Extended;
var
  d: Double;
begin
  d := 0;
  sgeTryStrToFloat(S, d);
  Result := d;
end;


function sgeTryStrToFloat(const S: String; out Value: Double): Boolean;
var
  E: Integer;
Begin
  Val(sgeTrim(S), Value, E);
  Result := (E = 0);
end;


function sgeBoolToStr(Value: Boolean; TrueStr: String; FalseStr: String): String;
begin
  if Value then Result := TrueStr else Result := FalseStr;
end;


function sgeCompareMem(P1, P2: Pointer; Length: PtrUInt): Boolean;
begin
  if P1 = P2 then Result := True else Result := CompareByte(P1^, P2^, Length) = 0;
end;


procedure sgeFreeAndNil(var Obj);
begin
  TObject(Obj).Free;
  Pointer(Obj) := nil;
end;


function sgePos(const Substr, Source: String; Offset: SizeInt): SizeInt;
var
  i, MaxLen: SizeInt;
  pc: PAnsiChar;
begin
  Result := 0;
  if (Length(SubStr) > 0) and (Offset > 0) and (Offset <= Length(Source)) then
    begin
    MaxLen := Length(source) - Length(SubStr);
    i := Offset - 1;
    pc := @source[Offset];
    while (i <= MaxLen) do
      begin
      inc(i);
      if (SubStr[1]= pc^) and (CompareByte(Substr[1], pc^, Length(SubStr)) = 0) then
        begin
        Result := i;
        exit;
        end;
      inc(pc);
      end;
    end;
end;







end.

