{
Пакет             Simple Game Engine 2
Файл              sgeMatchMask.pas
Версия            1.0
Создан            23.04.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс разбора маски на токены
}
{$Include Defines.inc}

unit sgeMatchMask;

{$mode ObjFPC}{$H+}

interface

type
  //Тип символа в маске
  TsgeMatchMaskItemType = (mmitSymbol, mmitQuestion, mmitAsterisk);

  //Элемент маски
  TsgeMatchMaskItem = record
    &Type: TsgeMatchMaskItemType; //Тип элемента
    Symbol: String;               //Символ
  end;

  //Маска
  TsgeMatchMask = class
  private
    Fcount: Integer;
    FMask: String;
    FItemList: array of TsgeMatchMaskItem;

    function GetItem(Index: Integer): TsgeMatchMaskItem;
    procedure Parse(aMask: String);

    procedure AddItem(aType: TsgeMatchMaskItemType; Symbol: String = '');
    procedure SetMask(aMask: String = '');
  public
    constructor Create(Mask: String);
    destructor Destroy; override;

    procedure Clear;

    property Count: Integer read Fcount;
    property Item[Index: Integer]: TsgeMatchMaskItem read GetItem;
    property Mask: String read FMask write SetMask;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils;

const
  _UNITNAME = 'MatchMask';

  Err_IndexOutOfBounds = 'IndexOutOfBounds';


procedure TsgeMatchMask.AddItem(aType: TsgeMatchMaskItemType; Symbol: String);
begin
  Inc(Fcount);
  SetLength(FItemList, Fcount);
  FItemList[Fcount - 1].&Type := aType;
  FItemList[Fcount - 1].Symbol := Symbol;
end;


function TsgeMatchMask.GetItem(Index: Integer): TsgeMatchMaskItem;
begin
  if (Index < 0) or (Index > Fcount - 1) then ;
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  Result := FItemList[Index];
end;


procedure TsgeMatchMask.Parse(aMask: String);
const
  sAsterisk = '*';
  sQuestion = '?';
  sShield = '`';
var
  c, Index: Integer;
  Shield: Boolean;
  s: String;
begin
  c := Length(aMask);
  Shield := False;
  Index := 1;

  while Index <= c do
  begin
    s := aMask[Index];

    if (s = sShield) and not Shield then
    begin
      Inc(Index);
      Shield := True;
      Continue;
    end;

    if Shield then
    begin
      AddItem(mmitSymbol, s);
      Shield := False;
    end
    else
      case s of
        sAsterisk:
        begin
          s := '';
          if Index + 1 <= c then
            s := aMask[Index + 1];
          if s = sAsterisk then
          begin
            Inc(Index);
            Continue;
          end;

          AddItem(mmitAsterisk, sAsterisk);
        end;

        sQuestion:
          AddItem(mmitQuestion, sQuestion);

        else
          AddItem(mmitSymbol, s);
      end;

    Inc(Index);
  end;
end;


procedure TsgeMatchMask.SetMask(aMask: String);
begin
  if FMask = aMask then
    Exit;

  FMask := aMask;
  Parse(FMask);
end;


constructor TsgeMatchMask.Create(Mask: String);
begin
  Fcount := 0;
  SetMask(Mask);
end;


destructor TsgeMatchMask.Destroy;
begin
  Clear;
end;


procedure TsgeMatchMask.Clear;
begin
  SetLength(FItemList, 0);
  Fcount := 0;
end;



end.

