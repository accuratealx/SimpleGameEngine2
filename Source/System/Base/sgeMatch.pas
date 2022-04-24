{
Пакет             Simple Game Engine 2
Файл              sgeMatch.pas
Версия            1.0
Создан            20.04.2022
Автор             Творческий человек  (accuratealx@gmail.com), CheeryProgrammer  (medulla_261@mail.ru)
Описание          Класс поиска совпадения по маске
}
{$Include Defines.inc}

unit sgeMatch;

{$mode ObjFPC}{$H+}

interface

uses
  sgeMatchMask;

type
  TsgeMatch = class
  private
    FMask: TsgeMatchMask;
    FIgnoreCase: Boolean;

  public
    constructor Create(Mask: String = ''; IgnoreCase: Boolean = True);
    destructor  Destroy; override;

    function Match(Str: String): Boolean;
  end;



implementation


constructor TsgeMatch.Create(Mask: String; IgnoreCase: Boolean);
begin
  FIgnoreCase := IgnoreCase;
  if IgnoreCase then
    Mask := LowerCase(Mask);
  FMask := TsgeMatchMask.Create(Mask);
end;


destructor TsgeMatch.Destroy;
begin
  FMask.Free;
end;


function TsgeMatch.Match(Str: String): Boolean;
var
  StrEnd, MaskEnd: Integer;

  function M(StrPos, MaskPos: Integer): Boolean;
  var
    i: Integer;
  begin
    if (StrPos = StrEnd) and (MaskPos = MaskEnd) then
      Exit(True);

    if (MaskPos = MaskEnd) and (FMask.Item[MaskPos].&Type = mmitAsterisk) then
      Exit(True);

    if (StrPos = StrEnd) or (MaskPos = MaskEnd) then
      Exit(False);

    case FMask.Item[MaskPos].&Type of
      mmitQuestion:
        Result := M(StrPos + 1, MaskPos + 1);

      mmitSymbol:
      begin
        if Str[StrPos] = FMask.Item[MaskPos].Symbol then
          Result := M(StrPos + 1, MaskPos + 1)
        else
          Exit(False);
      end;

      mmitAsterisk:
      begin
        for i := 0 to StrEnd - StrPos do
			    if M(StrPos + i, MaskPos + 1) then
		    		Exit(True);
		    Exit(False);
      end;
    end;

  end;

begin
  if FMask.Count = 0 then
    Exit(False);

  if FIgnoreCase then
    Str := LowerCase(Str);

  StrEnd := Length(Str);
  MaskEnd := FMask.Count - 1;
  Result := M(1, 0);
end;



end.

