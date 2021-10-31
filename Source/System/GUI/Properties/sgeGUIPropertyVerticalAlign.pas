{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyVerticalAlign.pas
Версия            1.1
Создан            01.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Вертикальное выравнивание
}
{$Include Defines.inc}

unit sgeGUIPropertyVerticalAlign;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleParameters,
  sgeGUIProperty;


type
  //Тип выравнивания
  TsgeGUIPropertyVerticalAlignMode = (vamTop, vamMiddle, vamBottom, vamUser);


  TsgeGUIPropertyVerticalAlign = class(TsgeGUIProperty)
  private
    FMode: TsgeGUIPropertyVerticalAlignMode;
    FOffset: Integer;

    procedure SetMode(AMode: TsgeGUIPropertyVerticalAlignMode);
    procedure SetOffset(AOffset: Integer);
  public
    constructor Create(AOwner: TObject); override;

    property Mode: TsgeGUIPropertyVerticalAlignMode read FMode write SetMode;
    property Offset: Integer read FOffset write SetOffset;
  end;


  TsgeGUIPropertyVerticalAlignExt = class(TsgeGUIPropertyVerticalAlign)
  public
    procedure LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String = '');
    function GetOffset(BaseHeight, ElementHeight: Integer): Single;
  end;



implementation


procedure TsgeGUIPropertyVerticalAlign.SetMode(AMode: TsgeGUIPropertyVerticalAlignMode);
begin
  if FMode = AMode then Exit;

  FMode := AMode;
  UpdateParent;
end;


procedure TsgeGUIPropertyVerticalAlign.SetOffset(AOffset: Integer);
begin
  if FOffset = AOffset then Exit;

  FOffset := AOffset;
  UpdateParent;
end;


constructor TsgeGUIPropertyVerticalAlign.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FMode := vamTop;
  FOffset := 0;
end;


procedure TsgeGUIPropertyVerticalAlignExt.LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String);
var
  ParamName, s: String;
begin
  //Mode
  ParamName := Prefix + 'Mode';
  if Parameters.Exist[ParamName] then
    begin
    s := LowerCase(Parameters.GetValue(ParamName, ''));
    case s of
      'top'   : FMode := vamTop;
      'middle': FMode := vamMiddle;
      'bottom': FMode := vamBottom;
      'user'  : FMode := vamUser;
    end;
    end;

  //Offset
  ParamName := Prefix + 'Offset';
  if Parameters.Exist[ParamName] then FOffset := Parameters.GetValue(ParamName, 0);
end;


function TsgeGUIPropertyVerticalAlignExt.GetOffset(BaseHeight, ElementHeight: Integer): Single;
begin
  case FMode of
    vamTop    : Result := 0;
    vamMiddle : Result := BaseHeight / 2 - ElementHeight / 2;
    vamBottom : Result := BaseHeight - ElementHeight;
    vamUser   : Result := FOffset;
  end;
end;


end.

