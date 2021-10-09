{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyHorizontalAlign.pas
Версия            1.1
Создан            01.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Горизонтальное выравнивание
}
{$Include Defines.inc}

unit sgeGUIPropertyHorizontalAlign;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleParameters,
  sgeGUIProperty;


type
  //Тип выравнивания
  TsgeGUIPropertyHorizontalAlignMode = (hamLeft, hamMiddle, hamRight, hamUser);


  TsgeGUIPropertyHorizontalAlign = class(TsgeGUIProperty)
  private
    FMode: TsgeGUIPropertyHorizontalAlignMode;
    FOffset: Integer;

    procedure SetMode(AMode: TsgeGUIPropertyHorizontalAlignMode);
    procedure SetOffset(AOffset: Integer);
  public
    constructor Create(AOwner: TObject); override;

    property Mode: TsgeGUIPropertyHorizontalAlignMode read FMode write SetMode;
    property Offset: Integer read FOffset write SetOffset;
  end;


  TsgeGUIPropertyHorizontalAlignExt = class(TsgeGUIPropertyHorizontalAlign)
  public
    procedure LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String = '');
    function GetOffset(BaseWidth, ElementWidth: Integer): Single;
  end;



implementation


procedure TsgeGUIPropertyHorizontalAlign.SetMode(AMode: TsgeGUIPropertyHorizontalAlignMode);
begin
  if FMode = AMode then Exit;

  FMode := AMode;
  UpdateParent;
end;


procedure TsgeGUIPropertyHorizontalAlign.SetOffset(AOffset: Integer);
begin
  if FOffset = AOffset then Exit;

  FOffset := AOffset;
  UpdateParent;
end;


constructor TsgeGUIPropertyHorizontalAlign.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FMode := hamLeft;
  FOffset := 0;
end;


procedure TsgeGUIPropertyHorizontalAlignExt.LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String);
var
  ParamName, s: String;
begin
  //Mode
  ParamName := Prefix + 'Mode';
  if Parameters.Exist[ParamName] then
    begin
    s := LowerCase(Parameters.GetValue(ParamName, ''));
    case s of
      'left'  : FMode := hamLeft;
      'middle': FMode := hamMiddle;
      'right' : FMode := hamRight;
      'user'  : FMode := hamUser;
    end;
    end;

  //Offset
  ParamName := Prefix + 'Offset';
  if Parameters.Exist[ParamName] then FOffset := Parameters.GetValue(ParamName, 0);
end;


function TsgeGUIPropertyHorizontalAlignExt.GetOffset(BaseWidth, ElementWidth: Integer): Single;
begin
  case FMode of
    hamLeft   : Result := 0;
    hamMiddle : Result := BaseWidth / 2 - ElementWidth / 2;
    hamRight  : Result := BaseWidth - ElementWidth;
    hamUser   : Result := FOffset;
  end;
end;


end.

